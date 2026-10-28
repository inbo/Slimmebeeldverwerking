library(sf)
library(terra)
library(future)
library(future.apply)
library(progressr)

setwd("C:/Users/sebastiaan_verbessel/Downloads")

# 1. Define paths & directories
output_vae      <- "covariates_VAE_dataset"
ref_dir         <- "./blocks"
covariates_path <- "predictors_ARD2_filled.tif"
grid_path       <- "512_pixel_grid.gpkg"

dir.create(output_vae, showWarnings = FALSE, recursive = TRUE)

# 2. Map reference files cleanly
ref_files <- list.files(
  path = ref_dir, 
  pattern = "^Reference_tile_id_.*\\.tif$", 
  full.names = TRUE, 
  recursive = TRUE
)

ref_ids <- gsub("^Reference_tile_id_(.*)\\.tif$", "\\1", basename(ref_files))

# Create a clean lookup table instead of named vector
ref_df <- data.frame(
  id = as.character(ref_ids),
  path = ref_files,
  stringsAsFactors = FALSE
)

# 3. Read grid & filter matching IDs
grid_subset <- st_read(grid_path)
grid_subset$id <- as.character(grid_subset$id)
tasks <- merge(grid_subset[, "id"], ref_df, by = "id")

# Convert tasks to a simple list of task items (avoids exporting sf object to workers)
task_list <- split(tasks, seq_len(nrow(tasks)))

# 4. Setup Parallel Processing safely
# Lower worker count if system memory is low (e.g. 2 or 3 instead of 4)
num_workers <- 4 
plan(multisession, workers = num_workers)

handlers(global = TRUE)
handlers("progress")

# 5. Parallel Execution Loop
with_progress({
  p <- progressor(steps = length(task_list))
  
  future_lapply(task_list, function(task) {
    poly_id  <- task$id
    ref_path <- task$path
    out_vae_path <- file.path(output_vae, paste0("Covariates_vae_tile_id_", poly_id, ".tif"))
    
    # Skip if file already exists (allows quick restarts)
    if (file.exists(out_vae_path)) {
      p()
      return(NULL)
    }
    
    tryCatch({
      # Configure safe terra parameters inside worker
      terraOptions(memfrac = 0.1, todisk = TRUE)
      setGDALconfig("GDAL_NUM_THREADS", "1")
      
      # Load rasters directly from disk per worker
      covariates_rast <- rast(covariates_path)
      ref_tile        <- rast(ref_path)
      
      # Match CRS if required
      if (crs(ref_tile) != crs(covariates_rast)) {
        ref_tile <- project(ref_tile, crs(covariates_rast))
      }
      
      # Crop covariates to reference tile extent
      cov_cropped <- crop(covariates_rast, ref_tile, snap = "near", mask = FALSE)
      
      # Resample aligned raster
      cov_aligned <- resample(cov_cropped, ref_tile, method = "near")
      
      # Mask values where reference raster equals -1
      mask_layer <- ref_tile == -1
      
      # Check if mask layer contains TRUE values
      val_counts <- global(mask_layer, "sum", na.rm = TRUE)
      has_values_to_mask <- any(val_counts > 0, na.rm = TRUE)
      
      if (has_values_to_mask) {
        cov_vae <- mask(cov_aligned, mask_layer, maskvalues = TRUE)
      } else {
        cov_vae <- cov_aligned
      }
      
      # Write output to disk
      writeRaster(cov_vae, filename = out_vae_path, overwrite = TRUE)
      
    }, error = function(e) {
      warning(paste0("Error on tile ID ", poly_id, ": ", e$message))
    }, finally = {
      # Memory cleanup
      gc(verbose = FALSE)
      p()
    })
    
    return(NULL)
  }, future.seed = TRUE)
})

# Close parallel workers cleanly
plan(sequential)
