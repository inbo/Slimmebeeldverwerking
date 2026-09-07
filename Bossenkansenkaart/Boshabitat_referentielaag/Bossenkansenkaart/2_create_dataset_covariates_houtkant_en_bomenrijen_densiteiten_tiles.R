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
BR1778 <- rast("G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/FOREDGEMAP/HKBR1778/2_densiteit/HKBR1778_bomenrij_density200m_warp_clip.tif")
HK1778 <- rast("G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/FOREDGEMAP/HKBR1778/2_densiteit/HKBR1778_houtkant_density100m_warp_clip.tif")

BR1873 <- rast("G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/FOREDGEMAP/HKBR1873/2_densiteit/HKBR1873_bomenrij_density200m_warp_clip.tif")
HK1873 <- rast("G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/FOREDGEMAP/HKBR1873/2_densiteit/HKBR1873_houtkant_density100m_warp_clip.tif")

BR1939 <- rast("G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/FOREDGEMAP/HKBR1939/2_densiteit/HKBR1939_bomenrij_density_200m_clip.tif")
HK1939 <- rast("G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/FOREDGEMAP/HKBR1939/2_densiteit/HKBR1939_houtkant_density_100m_clip.tif")

BR1969 <- rast("G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/FOREDGEMAP/HKBR1969/2_densiteit/HKBR1969_bomenrij_density200m_warp_clip.tif")
HK1969 <- rast("G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/FOREDGEMAP/HKBR1969/2_densiteit/HKBR1969_houtkant_density_100m_clip.tif")


targetcrs <- crs(BR1778)

if (crs(boshab) != targetcrs) {
  boshab <- project(boshab,targetcrs)
}

#if (crs(tile_grid) != targetcrs) {
#  tile_grid <- project(tile_grid,targetcrs)
#}

if (crs(block_grid) != targetcrs) {
  block_grid <- project(block_grid,targetcrs)
}


if (crs(HK1778) != targetcrs) {
  HK1778 <- project(HK1778,targetcrs)
}

if (crs(HK1873) != targetcrs) {
  HK1873 <- project(HK1873,targetcrs)
}

if (crs(HK1939) != targetcrs) {
  HK1939 <- project(HK1939,targetcrs)
}

if (crs(HK1969) != targetcrs) {
  HK1969 <- project(HK1969,targetcrs)
}


if (crs(BR1873) != targetcrs) {
  BR1873 <- project(BR1873,targetcrs)
}

if (crs(BR1939) != targetcrs) {
  BR1939 <- project(BR1939,targetcrs)
}

if (crs(BR1969) != targetcrs) {
  BR1969 <- project(BR1969,targetcrs)
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
  BR_crop <- crop(BR1778, block_buffer, mask = TRUE)
  
  # align rasters
  # Crop and align r2 to the exact extent, resolution, and origin of r1
  BR_aligned <- resample(BR_crop, rast_crop, method = "bilinear")
  
  # Optional: Ensure the spatial extent is strictly clipped to r1s bounding box
  BR_final <- crop(BR_aligned, rast_crop)
  
  BR_final <- project(BR_final,originalcrs)
  
  newfolder <- paste0("block", i)
  newpath <- file.path(output_base_path, newfolder)
  dir.create(newpath, recursive = TRUE, showWarnings = FALSE)
  
  outpath <- file.path(newpath, paste0("BR_dens_1778_block_", i, ".tif"))
  
  writeRaster(
    BR_final,
    outpath,
    gdal = c("COMPRESS=LZW"),
    overwrite = TRUE)
  
  
}


for (i in 1:nrow(block_grid)) {
  #i <- 1
  block_sel <- block_grid[i, ]
  block_buffer <- buffer(block_sel,50) # create a 50 meter buffer
  
  # crop reference layer
  rast_crop <- crop(boshab, block_sel, mask = TRUE)
  
  # crop covariables
  BR_crop <- crop(BR1873, block_buffer, mask = TRUE)
  
  # align rasters
  # Crop and align r2 to the exact extent, resolution, and origin of r1
  BR_aligned <- resample(BR_crop, rast_crop, method = "bilinear")
  
  # Optional: Ensure the spatial extent is strictly clipped to r1s bounding box
  BR_final <- crop(BR_aligned, rast_crop)
  
  BR_final <- project(BR_final,originalcrs)
  
  newfolder <- paste0("block", i)
  newpath <- file.path(output_base_path, newfolder)
  dir.create(newpath, recursive = TRUE, showWarnings = FALSE)
  
  outpath <- file.path(newpath, paste0("BR_dens_1873_block_", i, ".tif"))
  
  writeRaster(
    BR_final,
    outpath,
    gdal = c("COMPRESS=LZW"),
    overwrite = TRUE)
  
  
}

for (i in 1:nrow(block_grid)) {
  #i <- 1
  block_sel <- block_grid[i, ]
  block_buffer <- buffer(block_sel,50) # create a 50 meter buffer
  
  # crop reference layer
  rast_crop <- crop(boshab, block_sel, mask = TRUE)
  
  # crop covariables
  BR_crop <- crop(BR1939, block_buffer, mask = TRUE)
  
  # align rasters
  # Crop and align r2 to the exact extent, resolution, and origin of r1
  BR_aligned <- resample(BR_crop, rast_crop, method = "bilinear")
  
  # Optional: Ensure the spatial extent is strictly clipped to r1s bounding box
  BR_final <- crop(BR_aligned, rast_crop)
  
  BR_final <- project(BR_final,originalcrs)
  
  newfolder <- paste0("block", i)
  newpath <- file.path(output_base_path, newfolder)
  dir.create(newpath, recursive = TRUE, showWarnings = FALSE)
  
  outpath <- file.path(newpath, paste0("BR_dens_1939_block_", i, ".tif"))
  
  writeRaster(
    BR_final,
    outpath,
    gdal = c("COMPRESS=LZW"),
    overwrite = TRUE)
  
  
}

for (i in 1:nrow(block_grid)) {
  #i <- 1
  block_sel <- block_grid[i, ]
  block_buffer <- buffer(block_sel,50) # create a 50 meter buffer
  
  # crop reference layer
  rast_crop <- crop(boshab, block_sel, mask = TRUE)
  
  # crop covariables
  BR_crop <- crop(BR1969, block_buffer, mask = TRUE)
  
  # align rasters
  # Crop and align r2 to the exact extent, resolution, and origin of r1
  BR_aligned <- resample(BR_crop, rast_crop, method = "bilinear")
  
  # Optional: Ensure the spatial extent is strictly clipped to r1s bounding box
  BR_final <- crop(BR_aligned, rast_crop)
  
  BR_final <- project(BR_final,originalcrs)
  
  newfolder <- paste0("block", i)
  newpath <- file.path(output_base_path, newfolder)
  dir.create(newpath, recursive = TRUE, showWarnings = FALSE)
  
  outpath <- file.path(newpath, paste0("BR_dens_1969_block_", i, ".tif"))
  
  writeRaster(
    BR_final,
    outpath,
    gdal = c("COMPRESS=LZW"),
    overwrite = TRUE)
  
  
}

rm(BR_crop, BR_aligned, BR_final)


### Houtkanten ###
for (i in 1:nrow(block_grid)) {
  #i <- 1
  block_sel <- block_grid[i, ]
  block_buffer <- buffer(block_sel,50) # create a 50 meter buffer
  
  # crop reference layer
  rast_crop <- crop(boshab, block_sel, mask = TRUE)
  
  # crop covariables
  HK_crop <- crop(HK1778, block_buffer, mask = TRUE)
  
  # align rasters
  # Crop and align r2 to the exact extent, resolution, and origin of r1
  HK_aligned <- resample(HK_crop, rast_crop, method = "bilinear")
  
  # Optional: Ensure the spatial extent is strictly clipped to r1s bounding box
  HK_final <- crop(HK_aligned, rast_crop)
  
  HK_final <- project(HK_final,originalcrs)
  
  newfolder <- paste0("block", i)
  newpath <- file.path(output_base_path, newfolder)
  dir.create(newpath, recursive = TRUE, showWarnings = FALSE)
  
  outpath <- file.path(newpath, paste0("HK_dens_1778_block_", i, ".tif"))
  
  writeRaster(
    HK_final,
    outpath,
    gdal = c("COMPRESS=LZW"),
    overwrite = TRUE)
  
  
}


for (i in 1:nrow(block_grid)) {
  #i <- 1
  block_sel <- block_grid[i, ]
  block_buffer <- buffer(block_sel,50) # create a 50 meter buffer
  
  # crop reference layer
  rast_crop <- crop(boshab, block_sel, mask = TRUE)
  
  # crop covariables
  HK_crop <- crop(HK1873, block_buffer, mask = TRUE)
  
  # align rasters
  # Crop and align r2 to the exact extent, resolution, and origin of r1
  HK_aligned <- resample(HK_crop, rast_crop, method = "bilinear")
  
  # Optional: Ensure the spatial extent is strictly clipped to r1s bounding box
  HK_final <- crop(HK_aligned, rast_crop)
  
  HK_final <- project(HK_final,originalcrs)
  
  newfolder <- paste0("block", i)
  newpath <- file.path(output_base_path, newfolder)
  dir.create(newpath, recursive = TRUE, showWarnings = FALSE)
  
  outpath <- file.path(newpath, paste0("HK_dens_1873_block_", i, ".tif"))
  
  writeRaster(
    HK_final,
    outpath,
    gdal = c("COMPRESS=LZW"),
    overwrite = TRUE)
  
  
}

for (i in 1:nrow(block_grid)) {
  #i <- 1
  block_sel <- block_grid[i, ]
  block_buffer <- buffer(block_sel,50) # create a 50 meter buffer
  
  # crop reference layer
  rast_crop <- crop(boshab, block_sel, mask = TRUE)
  
  # crop covariables
  HK_crop <- crop(HK1939, block_buffer, mask = TRUE)
  
  # align rasters
  # Crop and align r2 to the exact extent, resolution, and origin of r1
  HK_aligned <- resample(HK_crop, rast_crop, method = "bilinear")
  
  # Optional: Ensure the spatial extent is strictly clipped to r1s bounding box
  HK_final <- crop(HK_aligned, rast_crop)
  
  HK_final <- project(HK_final,originalcrs)
  
  newfolder <- paste0("block", i)
  newpath <- file.path(output_base_path, newfolder)
  dir.create(newpath, recursive = TRUE, showWarnings = FALSE)
  
  outpath <- file.path(newpath, paste0("HK_dens_1939_block_", i, ".tif"))
  
  writeRaster(
    HK_final,
    outpath,
    gdal = c("COMPRESS=LZW"),
    overwrite = TRUE)
  
  
}

for (i in 1:nrow(block_grid)) {
  #i <- 1
  block_sel <- block_grid[i, ]
  block_buffer <- buffer(block_sel,50) # create a 50 meter buffer
  
  # crop reference layer
  rast_crop <- crop(boshab, block_sel, mask = TRUE)
  
  # crop covariables
  HK_crop <- crop(HK1969, block_buffer, mask = TRUE)
  
  # align rasters
  # Crop and align r2 to the exact extent, resolution, and origin of r1
  HK_aligned <- resample(HK_crop, rast_crop, method = "bilinear")
  
  # Optional: Ensure the spatial extent is strictly clipped to r1s bounding box
  HK_final <- crop(HK_aligned, rast_crop)
  
  HK_final <- project(HK_final,originalcrs)
  
  newfolder <- paste0("block", i)
  newpath <- file.path(output_base_path, newfolder)
  dir.create(newpath, recursive = TRUE, showWarnings = FALSE)
  
  outpath <- file.path(newpath, paste0("HK_dens_1969_block_", i, ".tif"))
  
  writeRaster(
    HK_final,
    outpath,
    gdal = c("COMPRESS=LZW"),
    overwrite = TRUE)
  
  
}
