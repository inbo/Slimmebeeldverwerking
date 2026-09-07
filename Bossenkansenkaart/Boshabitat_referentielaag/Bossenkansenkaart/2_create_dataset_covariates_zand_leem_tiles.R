### Load the libraries ####
library(terra)
library(tidyterra)

### Load the datasets ####
base_dir <- "//Client/G$/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/Bossenkansenkaart"



boshab <- rast(file.path(base_dir, "Boshabitat_referentielaag/BWK_2025_bosraster.tif"))

originalcrs <- crs(boshab)


#tile_grid <- vect(file.path(base_dir, "grids/512_pixel_grid.gpkg"))
block_grid <- vect(file.path(base_dir, "grids/50km_spatial_blok.gpkg"))


### Load Covariables ####
zand <- rast("//Client/G$/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/fractie zand/topsoil_bdbstat__fractie_zand_basisdata_bodemkartering_samengevoegd.tif")
zand_onz <- rast("//Client/G$/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/fractie zand/topsoil_unc_bdbstat__fractie_zand_basisdata_bodemkartering_samengevoegd.tif")
NAflag(zand_onz) <- 0

targetcrs <- crs(zand)


if (crs(boshab) != targetcrs) {
  boshab <- project(boshab,targetcrs)
}

#if (crs(tile_grid) != targetcrs) {
#  tile_grid <- project(tile_grid,targetcrs)
#}

if (crs(block_grid) != targetcrs) {
  block_grid <- project(block_grid,targetcrs)
}

if (crs(zand_onz) != targetcrs) {
  zand_onz <- project(zand_onz,targetcrs)
}

### Set ouput path location ###
output_base_path <- file.path(base_dir, "Boshabitat_covariabelen/blocks")


### Fractie zand ###

#for (i in 1:nrow(block_grid)) {
for (i in c(5,6)) {
  #i <- 1
  block_sel <- block_grid[i, ]
  block_buffer <- buffer(block_sel,50) # create a 50 meter buffer
  
  # crop reference layer
  rast_crop <- crop(boshab, block_sel, mask = TRUE)
  
  # crop covariables
  zand_crop <- crop(zand, block_buffer, mask = TRUE)
  
  # align rasters
  # Crop and align r2 to the exact extent, resolution, and origin of r1
  zand_aligned <- resample(zand_crop, rast_crop, method = "bilinear")
  
  # Optional: Ensure the spatial extent is strictly clipped to r1's bounding box
  zand_final <- crop(zand_aligned, rast_crop)
  
  zand_final <- project(zand_final,originalcrs)
  
  newfolder <- paste0("block", i)
  newpath <- file.path(output_base_path, newfolder)
  dir.create(newpath, recursive = TRUE, showWarnings = FALSE)
  
  outpath <- file.path(newpath, paste0("FracZand_block_", i, ".tif"))
  
  writeRaster(
    zand_final,
    outpath,
    gdal = c("COMPRESS=LZW"),
    overwrite = TRUE)
  
  
}
rm(zand_final,zand_aligned,zand_crop)

### Fractie zand - onzekerheid ###
#for (i in 1:nrow(block_grid)) {
for (i in c(5,6)) {
  #i <- 1
  block_sel <- block_grid[i, ]
  block_buffer <- buffer(block_sel,50) # create a 50 meter buffer
  
  # crop reference layer
  rast_crop <- crop(boshab, block_sel, mask = TRUE)
  
  # crop covariables
  zand_onz_crop <- crop(zand_onz, block_buffer, mask = TRUE)
  
  # align rasters
  # Crop and align r2 to the exact extent, resolution, and origin of r1
  zand_onz_aligned <- resample(zand_onz_crop, rast_crop, method = "bilinear")
  
  # Optional: Ensure the spatial extent is strictly clipped to r1's bounding box
  zand_onz_final <- crop(zand_onz_aligned, rast_crop)
  
  zand_onz_final <- project(zand_onz_final,originalcrs)
  
  newfolder <- paste0("block", i)
  newpath <- file.path(output_base_path, newfolder)
  dir.create(newpath, recursive = TRUE, showWarnings = FALSE)
  
  outpath <- file.path(newpath, paste0("FracZand_onz_block_", i, ".tif"))
  
  writeRaster(
    zand_onz_final,
    outpath,
    gdal = c("COMPRESS=LZW"),
    overwrite = TRUE)
  
  
}
rm(zand_onz_final,zand_onz_aligned,zand_onz_crop)
rm(zand,zand_onz)

leem <- rast("//Client/G$/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/fractie_leem/topsoil_bdbstat__fractie_leem_basisdata_bodemkartering_samengevoegd.tif")
leem_onz <- rast("//Client/G$/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/fractie_leem/topsoil_unc_bdbstat__fractie_leem_basisdata_bodemkartering_samengevoegd.tif")


### Fractie leem ###

#for (i in 1:nrow(block_grid)) {
for (i in c(5,6)) {
  #i <- 1
  block_sel <- block_grid[i, ]
  block_buffer <- buffer(block_sel,50) # create a 50 meter buffer
  
  # crop reference layer
  rast_crop <- crop(boshab, block_sel, mask = TRUE)
  
  # crop covariables
  leem_crop <- crop(leem, block_buffer, mask = TRUE)
  
  # align rasters
  # Crop and align r2 to the exact extent, resolution, and origin of r1
  leem_aligned <- resample(leem_crop, rast_crop, method = "bilinear")
  
  # Optional: Ensure the spatial extent is strictly clipped to r1's bounding box
  leem_final <- crop(leem_aligned, rast_crop)
  
  leem_final <- project(leem_final,originalcrs)
  
  newfolder <- paste0("block", i)
  newpath <- file.path(output_base_path, newfolder)
  dir.create(newpath, recursive = TRUE, showWarnings = FALSE)
  
  outpath <- file.path(newpath, paste0("FracLeem_block_", i, ".tif"))
  
  writeRaster(
    leem_final,
    outpath,
    gdal = c("COMPRESS=LZW"),
    overwrite = TRUE)
  
  
}
rm(leem_final,leem_aligned,leem_crop)

### Fractie leem - onzekerheid ###
for (i in 1:nrow(block_grid)) {
  #i <- 1
  block_sel <- block_grid[i, ]
  block_buffer <- buffer(block_sel,50) # create a 50 meter buffer
  
  # crop reference layer
  rast_crop <- crop(boshab, block_sel, mask = TRUE)
  
  # crop covariables
  leem_onz_crop <- crop(leem_onz, block_buffer, mask = TRUE)
  
  # align rasters
  # Crop and align r2 to the exact extent, resolution, and origin of r1
  leem_onz_aligned <- resample(leem_onz_crop, rast_crop, method = "bilinear")
  
  # Optional: Ensure the spatial extent is strictly clipped to r1's bounding box
  leem_onz_final <- crop(leem_onz_aligned, rast_crop)
  
  leem_onz_final <- project(leem_onz_final,originalcrs)
  
  newfolder <- paste0("block", i)
  newpath <- file.path(output_base_path, newfolder)
  dir.create(newpath, recursive = TRUE, showWarnings = FALSE)
  
  outpath <- file.path(newpath, paste0("FracLeem_onz_block_", i, ".tif"))
  
  writeRaster(
    leem_onz_final,
    outpath,
    gdal = c("COMPRESS=LZW"),
    overwrite = TRUE)
  
  
}

rm(leem_onz_final,leem_onz_aligned,leem_onz_crop)
rm(leem,leem_onz)
