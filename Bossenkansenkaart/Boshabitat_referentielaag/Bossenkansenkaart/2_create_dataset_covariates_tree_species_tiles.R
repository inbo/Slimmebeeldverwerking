### Load the libraries ####
library(terra)
library(tidyterra)

### Load the datasets ####
base_dir <- "//Client/G$/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/Bossenkansenkaart"

base_dir <- "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/Bossenkansenkaart"

boshab <- rast(file.path(base_dir, "Boshabitat_referentielaag/BWK_2025_bosraster.tif"))

originalcrs <- crs(boshab)


#tile_grid <- vect(file.path(base_dir, "grids/512_pixel_grid.gpkg"))
block_grid <- vect(file.path(base_dir, "grids/50km_spatial_blok.gpkg"))


### Load Covariables ####
trsp <- rast("G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/Bossenkansenkaart/Data Margot Verhulst/2022/Bosmonitoring-boomsoort_2022-01-01.tif")


trsp[trsp == -9999] <- NA

plot(trsp)

targetcrs <- crs(trsp)

if (crs(boshab) != targetcrs) {
  boshab <- project(boshab,targetcrs)
}

#if (crs(tile_grid) != targetcrs) {
#  tile_grid <- project(tile_grid,targetcrs)
#}

if (crs(block_grid) != targetcrs) {
  block_grid <- project(block_grid,targetcrs)
}


### Set ouput path location ###
output_base_path <- file.path(base_dir, "Boshabitat_covariabelen/blocks")


### Bomenrijen ###
for (i in 1:nrow(block_grid)) {
  #i <- 1
  block_sel <- block_grid[i, ]
  block_buffer <- buffer(block_sel,50) # create a 50 meter buffer
  
  # crop reference layer
  rast_crop <- crop(boshab, block_sel, mask = TRUE)
  
  # crop covariables
  trsp_crop <- crop(trsp, block_buffer, mask = TRUE)
  
  # align rasters
  # Crop and align r2 to the exact extent, resolution, and origin of r1
  trsp_aligned <- resample(trsp_crop, rast_crop, method = "bilinear")
  
  # Optional: Ensure the spatial extent is strictly clipped to r1s bounding box
  trsp_final <- crop(trsp_aligned, rast_crop)
  
  trsp_final <- project(trsp_final,originalcrs)
  
  newfolder <- paste0("block", i)
  newpath <- file.path(output_base_path, newfolder)
  dir.create(newpath, recursive = TRUE, showWarnings = FALSE)
  
  outpath <- file.path(newpath, paste0("TreeSpecies_block_", i, ".tif"))
  
  writeRaster(
    trsp_final,
    outpath,
    gdal = c("COMPRESS=LZW"),
    overwrite = TRUE)
  
  
}

