library(sf)
library(terra)
library(future)
library(future.apply)
library(progressr)

# 1. Set up parallel backend
# Reduced to 2 workers to prevent GDAL I/O bottlenecks on Windows
num_workers <- 2
plan(multisession, workers = num_workers)

handlers(global = TRUE)
handlers("progress")

setwd("C:/Users/sebastiaan_verbessel/Downloads")

# Output directory
output_full <- "E:/covariates_full_dataset/"
dir.create(output_full, showWarnings = FALSE, recursive = TRUE)

# 2. Read metadata on main thread
grid_512 <- st_read("512_pixel_grid.gpkg")
covariates_path <- "predictors_ARD2_filled.tif"

# Get raster info directly
cov_rast_main <- rast(covariates_path)
cov_crs <- crs(cov_rast_main)
grid_512 <- st_transform(grid_512, cov_crs)

# Extract Extents and IDs as simple data frames
# Passing raw bounding box coordinates avoids SpatVector/C++ serialization bugs
bboxes <- st_bbox(grid_512[1, ])
n_tiles <- nrow(grid_512)

tile_data <- lapply(seq_len(n_tiles), function(i) {
  box <- st_bbox(grid_512[i, ])
  list(
    id = as.character(grid_512$id[i]),
    xmin = box[["xmin"]],
    xmax = box[["xmax"]],
    ymin = box[["ymin"]],
    ymax = box[["ymax"]]
  )
})

# Close main raster handle explicitly
rm(cov_rast_main)
gc()

# 3. Parallel Execution
process_tiles <- function() {
  p <- progressor(steps = n_tiles)
  
  future_lapply(tile_data, function(item) {
    p()
    
    poly_id <- item$id
    out_full_path <- file.path(output_full, paste0("Covariates_tile_id_", poly_id, ".tif"))
    
    if (file.exists(out_full_path)) return("skipped")
    
    # Configure isolated worker temp folder
    worker_temp <- file.path(tempdir(), paste0("terra_w_", Sys.getpid()))
    dir.create(worker_temp, showWarnings = FALSE, recursive = TRUE)
    
    terra::terraOptions(memfrac = 0.2, todisk = TRUE, tempdir = worker_temp)
    
    res <- tryCatch({
      # Build extent bounding box directly from numbers
      e <- terra::ext(item$xmin, item$xmax, item$ymin, item$ymax)
      
      # Open raster connection inside worker
      r <- terra::rast(covariates_path)
      
      # Crop using spatial extent
      cov_crop <- terra::crop(r, e)
      
      if (!is.null(cov_crop) && terra::ncell(cov_crop) > 0) {
        terra::writeRaster(cov_crop, filename = out_full_path, overwrite = TRUE)
      }
      
      # Clean up worker memory completely
      rm(r, cov_crop)
      gc()
      "success"
    }, error = function(e) {
      "failed"
    })
    
    return(res)
  }, future.seed = TRUE, future.packages = c("terra"))
}

# 4. Execute
results <- with_progress({
  process_tiles()
})

plan(sequential)

cat("\n--- Execution Summary ---\n")
cat("Successfully processed:", sum(results == "success"), "\n")
cat("Skipped (already existing):", sum(results == "skipped"), "\n")
cat("Failed tiles:", sum(results == "failed"), "\n")
