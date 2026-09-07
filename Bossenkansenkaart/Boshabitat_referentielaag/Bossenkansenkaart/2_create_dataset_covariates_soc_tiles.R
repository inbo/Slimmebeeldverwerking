### Load the libraries ####
library(terra)
library(tidyterra)

### Load the datasets ####
base_dir <- "//Client/G$/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/Bossenkansenkaart" #"G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/Bossenkansenkaart"



boshab <- rast(file.path(base_dir, "Boshabitat_referentielaag/BWK_2025_bosraster.tif"))

originalcrs <- crs(boshab)


#tile_grid <- vect(file.path(base_dir, "grids/512_pixel_grid.gpkg"))
block_grid <- vect(file.path(base_dir, "grids/50km_spatial_blok.gpkg"))


### Load Covariables ####
soc <- rast("//Client/G$/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/OCst_30_2026_gemiddelde_percentielen/processed/OCst_30_2026_gemiddelde_percentielen/OCst_30_2026_gemiddelde_percentielen.tiff")
#rast("G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/OCst_30_2026_gemiddelde_percentielen/processed/OCst_30_2026_gemiddelde_percentielen/OCst_30_2026_gemiddelde_percentielen.tiff")



targetcrs <- crs(soc)


if (crs(boshab) != targetcrs) {
  boshab <- project(boshab,targetcrs)
}

#if (crs(tile_grid) != targetcrs) {
#  tile_grid <- project(tile_grid,targetcrs)
#}

if (crs(block_grid) != targetcrs) {
  block_grid <- project(block_grid,targetcrs)
}

soc_median <- soc[[1]]
soc_iqr <- soc[[3]] - soc[[2]]

### Set ouput path location ###
output_base_path <- file.path(base_dir, "Boshabitat_covariabelen/blocks")


### SOC median ###

for (i in 1:nrow(block_grid)) {
  #i <- 1
  block_sel <- block_grid[i, ]
  block_buffer <- buffer(block_sel,50) # create a 50 meter buffer
  
  # crop reference layer
  rast_crop <- crop(boshab, block_sel, mask = TRUE)
  
  # crop covariables
  soc_median_crop <- crop(soc_median, block_buffer, mask = TRUE)
  
  # align rasters
  # Crop and align r2 to the exact extent, resolution, and origin of r1
  soc_median_aligned <- resample(soc_median_crop, rast_crop, method = "bilinear")
  
  # Optional: Ensure the spatial extent is strictly clipped to r1's bounding box
  soc_median_final <- crop(soc_median_aligned, rast_crop)
  
  soc_median_final <- project(soc_median_final,originalcrs)
  
  newfolder <- paste0("block", i)
  newpath <- file.path(output_base_path, newfolder)
  dir.create(newpath, recursive = TRUE, showWarnings = FALSE)
  
  outpath <- file.path(newpath, paste0("SOC_median_block_", i, ".tif"))
  
  writeRaster(
    soc_median_final,
    outpath,
    gdal = c("COMPRESS=LZW"),
    overwrite = TRUE)
  
  
}
rm(soc_median_final,soc_median_aligned,soc_median_crop)

### SOC iqr ###

for (i in 1:nrow(block_grid)) {
  #i <- 1
  block_sel <- block_grid[i, ]
  block_buffer <- buffer(block_sel,50) # create a 50 meter buffer
  
  # crop reference layer
  rast_crop <- crop(boshab, block_sel, mask = TRUE)
  
  # crop covariables
  soc_iqr_crop <- crop(soc_iqr, block_buffer, mask = TRUE)
  
  # align rasters
  # Crop and align r2 to the exact extent, resolution, and origin of r1
  soc_iqr_aligned <- resample(soc_iqr_crop, rast_crop, method = "bilinear")
  
  # Optional: Ensure the spatial extent is strictly clipped to r1's bounding box
  soc_iqr_final <- crop(soc_iqr_aligned, rast_crop)
  
  soc_iqr_final <- project(soc_iqr_final,originalcrs)
  
  newfolder <- paste0("block", i)
  newpath <- file.path(output_base_path, newfolder)
  dir.create(newpath, recursive = TRUE, showWarnings = FALSE)
  
  outpath <- file.path(newpath, paste0("SOC_IQR_block_", i, ".tif"))
  
  writeRaster(
    soc_iqr_final,
    outpath,
    gdal = c("COMPRESS=LZW"),
    overwrite = TRUE)
  
  
}
rm(soc_iqr_final,soc_iqr_aligned,soc_iqr_crop)
