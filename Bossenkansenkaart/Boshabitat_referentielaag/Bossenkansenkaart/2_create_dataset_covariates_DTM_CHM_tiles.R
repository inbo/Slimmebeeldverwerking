### Load the libraries ####
library(terra)
library(tidyterra)

### Load the datasets ####
base_dir <- "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/Bossenkansenkaart"



boshab <- rast(file.path(base_dir, "Boshabitat_referentielaag/BWK_2025_bosraster.tif"))

originalcrs <- crs(boshab)


#tile_grid <- vect(file.path(base_dir, "grids/512_pixel_grid.gpkg"))
block_grid <- vect(file.path(base_dir, "grids/50km_spatial_blok.gpkg"))


### Load Covariables ####
dtm <- rast("S:/Vlaanderen/Hoogte/DHMVII/DHMVIIDTMRAS1m.tif")
chm <- rast("S:/Vlaanderen/Hoogte/DHMVII/DHMVIInDSMRAS1m.tif")

targetcrs <- crs(dtm)


if (crs(boshab) != targetcrs) {
  boshab <- project(boshab,targetcrs)
}

if (crs(tile_grid) != targetcrs) {
  tile_grid <- project(tile_grid,targetcrs)
}

if (crs(block_grid) != targetcrs) {
  block_grid <- project(block_grid,targetcrs)
}

if (crs(chm) != targetcrs) {
  chm <- project(chm,targetcrs)
}

### Set ouput path location ###
output_base_path <- file.path(base_dir, "Boshabitat_covariabelen/blocks")


### DTM - max ###

for (i in 1:nrow(block_grid)) {
  #i <- 1
  block_sel <- block_grid[i, ]
  block_buffer <- buffer(block_sel,50) # create a 50 meter buffer
  
  # crop reference layer
  rast_crop <- crop(boshab, block_sel, mask = TRUE)
  
  # crop covariables
  dtm_crop <- crop(dtm, block_buffer, mask = TRUE)
  
  # align rasters
  # Step 1: Aggregate r2 from 1m to 10m by taking the maximum value (10x10 cell block)
  dtm_crop_max <- aggregate(dtm_crop, fact = 10, fun = max)
  
  # Step 2: Crop and align r2 to the exact extent, resolution, and origin of r1
  dtm_aligned <- resample(dtm_crop_max, rast_crop, method = "near")
  
  # Optional: Ensure the spatial extent is strictly clipped to r1's bounding box
  dtm_final <- crop(dtm_aligned, rast_crop)
  
  dtm_final <- project(dtm_final,originalcrs)
  
  plot(dtm_final)
  
  
  newfolder <- paste0("block", i)
  newpath <- file.path(output_base_path, newfolder)
  dir.create(newpath, recursive = TRUE, showWarnings = FALSE)
  
  outpath <- file.path(newpath, paste0("DTM_max_block_", i, ".tif"))
  
  writeRaster(
    dtm_final,
    outpath,
    gdal = c("COMPRESS=LZW"),
    overwrite = TRUE)
  
  
}

rm(dtm_crop_max)


### DTM - median ###

for (i in 1:nrow(block_grid)) {
  #i <- 1
  block_sel <- block_grid[i, ]
  block_buffer <- buffer(block_sel,50) # create a 50 meter buffer
  
  # crop reference layer
  rast_crop <- crop(boshab, block_sel, mask = TRUE)
  
  # crop covariables
  dtm_crop <- crop(dtm, block_buffer, mask = TRUE)
  
  # align rasters
  # Step 1: Aggregate r2 from 1m to 10m by taking the maximum value (10x10 cell block)
  dtm_crop_median <- aggregate(dtm_crop, fact = 10, fun = median)
  
  # Step 2: Crop and align r2 to the exact extent, resolution, and origin of r1
  dtm_aligned <- resample(dtm_crop_median, rast_crop, method = "near")
  
  # Optional: Ensure the spatial extent is strictly clipped to r1's bounding box
  dtm_final <- crop(dtm_aligned, rast_crop)
  
  dtm_final <- project(dtm_final,originalcrs)
  
  plot(dtm_final)
  
  
  newfolder <- paste0("block", i)
  newpath <- file.path(output_base_path, newfolder)
  dir.create(newpath, recursive = TRUE, showWarnings = FALSE)
  
  outpath <- file.path(newpath, paste0("DTM_median_block_", i, ".tif"))
  
  writeRaster(
    dtm_final,
    outpath,
    gdal = c("COMPRESS=LZW"),
    overwrite = TRUE)
  
  
}



rm(dtm_crop_median)

### DTM - IQR ###

for (i in 1:nrow(block_grid)) {
  #i <- 1
  block_sel <- block_grid[i, ]
  block_buffer <- buffer(block_sel,50) # create a 50 meter buffer
  
  # crop reference layer
  rast_crop <- crop(boshab, block_sel, mask = TRUE)
  
  # crop covariables
  dtm_crop <- crop(dtm, block_buffer, mask = TRUE)
  
  # align rasters
  # Step 1: Aggregate r2 from 1m to 10m by taking the maximum value (10x10 cell block)
  dtm_crop_IQR <- aggregate(dtm_crop, fact = 10, fun = function(x, ...) IQR(x, na.rm = TRUE))
  
  # Step 2: Crop and align r2 to the exact extent, resolution, and origin of r1
  dtm_aligned <- resample(dtm_crop_IQR, rast_crop, method = "near")
  
  # Optional: Ensure the spatial extent is strictly clipped to r1's bounding box
  dtm_final <- crop(dtm_aligned, rast_crop)
  
  dtm_final <- project(dtm_final,originalcrs)
  
  plot(dtm_final)
  
  
  newfolder <- paste0("block", i)
  newpath <- file.path(output_base_path, newfolder)
  dir.create(newpath, recursive = TRUE, showWarnings = FALSE)
  
  outpath <- file.path(newpath, paste0("DTM_IQR_block_", i, ".tif"))
  
  writeRaster(
    dtm_final,
    outpath,
    gdal = c("COMPRESS=LZW"),
    overwrite = TRUE)
  
  
}

rm(dtm_crop_IQR,dtm_aligned,dtm_final)


### CHM - max ###

for (i in 1:nrow(block_grid)) {
  #i <- 1
  block_sel <- block_grid[i, ]
  block_buffer <- buffer(block_sel,50) # create a 50 meter buffer
  
  # crop reference layer
  rast_crop <- crop(boshab, block_sel, mask = TRUE)
  
  # crop covariables
  chm_crop <- crop(chm, block_buffer, mask = TRUE)
  
  # align rasters
  # Step 1: Aggregate r2 from 1m to 10m by taking the maximum value (10x10 cell block)
  chm_crop_max <- aggregate(chm_crop, fact = 10, fun = max)
  
  # Step 2: Crop and align r2 to the exact extent, resolution, and origin of r1
  chm_aligned <- resample(chm_crop_max, rast_crop, method = "near")
  
  # Optional: Ensure the spatial extent is strictly clipped to r1's bounding box
  chm_final <- crop(chm_aligned, rast_crop)
  
  chm_final <- project(chm_final,originalcrs)
  
  plot(chm_final)
  
  
  newfolder <- paste0("block", i)
  newpath <- file.path(output_base_path, newfolder)
  dir.create(newpath, recursive = TRUE, showWarnings = FALSE)
  
  outpath <- file.path(newpath, paste0("chm_max_block_", i, ".tif"))
  
  writeRaster(
    chm_final,
    outpath,
    gdal = c("COMPRESS=LZW"),
    overwrite = TRUE)
  
  
}

rm(chm_crop_max)


### CHM - median ###

for (i in 1:nrow(block_grid)) {
  #i <- 1
  block_sel <- block_grid[i, ]
  block_buffer <- buffer(block_sel,50) # create a 50 meter buffer
  
  # crop reference layer
  rast_crop <- crop(boshab, block_sel, mask = TRUE)
  
  # crop covariables
  chm_crop <- crop(chm, block_buffer, mask = TRUE)
  
  # align rasters
  # Step 1: Aggregate r2 from 1m to 10m by taking the maximum value (10x10 cell block)
  chm_crop_median <- aggregate(chm_crop, fact = 10, fun = median)
  
  # Step 2: Crop and align r2 to the exact extent, resolution, and origin of r1
  chm_aligned <- resample(chm_crop_median, rast_crop, method = "near")
  
  # Optional: Ensure the spatial extent is strictly clipped to r1's bounding box
  chm_final <- crop(chm_aligned, rast_crop)
  
  chm_final <- project(chm_final,originalcrs)
  
  plot(chm_final)
  
  
  newfolder <- paste0("block", i)
  newpath <- file.path(output_base_path, newfolder)
  dir.create(newpath, recursive = TRUE, showWarnings = FALSE)
  
  outpath <- file.path(newpath, paste0("chm_median_block_", i, ".tif"))
  
  writeRaster(
    chm_final,
    outpath,
    gdal = c("COMPRESS=LZW"),
    overwrite = TRUE)
  
  
}



rm(chm_crop_median)

### CHM - IQR ###

for (i in 1:nrow(block_grid)) {
  #i <- 1
  block_sel <- block_grid[i, ]
  block_buffer <- buffer(block_sel,50) # create a 50 meter buffer
  
  # crop reference layer
  rast_crop <- crop(boshab, block_sel, mask = TRUE)
  
  # crop covariables
  chm_crop <- crop(chm, block_buffer, mask = TRUE)
  
  # align rasters
  # Step 1: Aggregate r2 from 1m to 10m by taking the maximum value (10x10 cell block)
  chm_crop_IQR <- aggregate(chm_crop, fact = 10, fun = function(x, ...) IQR(x, na.rm = TRUE))
  
  # Step 2: Crop and align r2 to the exact extent, resolution, and origin of r1
  chm_aligned <- resample(chm_crop_IQR, rast_crop, method = "near")
  
  # Optional: Ensure the spatial extent is strictly clipped to r1's bounding box
  chm_final <- crop(chm_aligned, rast_crop)
  
  chm_final <- project(chm_final,originalcrs)
  
  plot(chm_final)
  
  
  newfolder <- paste0("block", i)
  newpath <- file.path(output_base_path, newfolder)
  dir.create(newpath, recursive = TRUE, showWarnings = FALSE)
  
  outpath <- file.path(newpath, paste0("chm_IQR_block_", i, ".tif"))
  
  writeRaster(
    chm_final,
    outpath,
    gdal = c("COMPRESS=LZW"),
    overwrite = TRUE)
  
  
}