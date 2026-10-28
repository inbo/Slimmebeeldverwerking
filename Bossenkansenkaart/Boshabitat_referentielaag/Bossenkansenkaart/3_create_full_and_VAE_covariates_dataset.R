library(sf)
library(terra)

# Set terra options to manage RAM safely and clear temporary files
terraOptions(memfrac = 0.6, todisk = TRUE)

setwd("G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/Bossenkansenkaart/Boshabitat_covariabelen/blocks")

# Define directories and file paths
output_full <- "covariates_full_dataset"
output_vae  <- "covariates_VAE_dataset"
ref_dir     <- "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/Bossenkansenkaart/Boshabitat_referentielaag"

dir.create(output_full, showWarnings = FALSE, recursive = TRUE)
dir.create(output_vae,  showWarnings = FALSE, recursive = TRUE)

# 1. Load grid and full prediction raster
grid_512 <- st_read("G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/Bossenkansenkaart/grids/512_pixel_grid.gpkg")
covariates_rast <- rast("G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/predictors_ARD2.tif")

# Ensure coordinate reference systems match
grid_512 <- st_transform(grid_512, crs(covariates_rast))

# Convert grid polygons once to spatVector to avoid recreating it in every iteration
grid_vect <- vect(grid_512)

# 2. Get list of existing reference tiles and map their polygon IDs
ref_files <- list.files(
  path = ref_dir, 
  pattern = "^Reference_tile_id_.*\\.tif$", 
  full.names = TRUE, 
  recursive = TRUE
)

ref_ids <- gsub("^Reference_tile_id_(.*)\\.tif$", "\\1", basename(ref_files))
ref_map <- setNames(ref_files, ref_ids)

# 3. Process each polygon in the grid
for (i in 823:nrow(grid_512)) {
  
  poly <- grid_vect[i, ]
  poly_id <- as.character(grid_512$id[i]) 
  
  out_full_path <- file.path(output_full, paste0("Covariates_tile_id_", poly_id, ".tif"))
  out_vae_path  <- file.path(output_vae,  paste0("Covariates_vae_tile_id_", poly_id, ".tif"))
  
  # --- Task 1: Crop and mask for full dataset ---
  # Remove mask=TRUE inside crop() when using raster extents to avoid alignment warnings
  cov_full <- crop(covariates_rast, poly, mask = TRUE)
  writeRaster(cov_full, filename = out_full_path, overwrite = TRUE)
  
  # --- Task 2: Process VAE dataset if a matching reference tile exists ---
  if (poly_id %in% names(ref_map)) {
    ref_tile <- rast(ref_map[[poly_id]])
    
    # 1. Crop to spatial extent first (without mask)
    cov_cropped <- crop(covariates_rast, ref_tile, mask = FALSE)
    
    # 2. Resample to match reference grid geometry exactly
    cov_aligned <- resample(cov_cropped, ref_tile, method = "near")
    
    # 3. Mask pixels where reference tile is -1
    cov_vae <- mask(cov_aligned, ref_tile == -1, maskvalues = TRUE)
    
    writeRaster(cov_vae, filename = out_vae_path, overwrite = TRUE)
    
    # Clean up intermediate raster objects from memory
    rm(ref_tile, cov_cropped, cov_aligned, cov_vae)
  }
  
  # Clean up loop object and force garbage collection periodically
  rm(cov_full)
  if (i %% 50 == 0) {
    gc()
    
    tmpFiles(current = TRUE, orphan = TRUE, old = TRUE, remove = TRUE)
  }
}
