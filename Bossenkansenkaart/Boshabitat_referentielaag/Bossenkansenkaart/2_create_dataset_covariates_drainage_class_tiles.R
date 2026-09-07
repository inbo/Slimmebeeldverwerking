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
soil <- vect("G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/bodemkaart-drainage/Drainage_klassen_filtered.gpkg")


# Define the ordered drainage categories
drainage_levels <- c("a", "a-b", "b", "a-d", "c", "c-d", "d", "e", "e-f", "f", "g", "h", "h-i", "i")
drainage_level_extra <- "e-i"

# Assign evenly spaced numeric values from 0 to 1
drainage_num <- seq(0, 1, length.out = length(drainage_levels))

drainage_level_extra_num <- drainage_num[11]



# Create a lookup mapping table
drainage_key <- data.frame(
  drainage_class = c(drainage_levels, drainage_level_extra),
  drainage_val   = c(drainage_num, drainage_level_extra_num)
)


targetcrs <- crs(soil)


if (crs(boshab) != targetcrs) {
  boshab <- project(boshab,targetcrs)
}

#if (crs(tile_grid) != targetcrs) {
#  tile_grid <- project(tile_grid,targetcrs)
#}

if (crs(block_grid) != targetcrs) {
  block_grid <- project(block_grid,targetcrs)
}


# Check vector attribute names to match your soil column name (e.g., "DRAINAGE")
# Merge numeric values into the vector attribute table
soil_merged <- merge(soil, drainage_key, by.x = "Drainage_c", by.y = "drainage_class", all.x = TRUE)

# Rasterize using the template grid and your numeric attribute
drainage_rast <- rasterize(
  x = soil_merged,
  y = boshab,
  field = "drainage_val",
  filename = "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/bodemkaart-drainage/flanders_drainage_10m.tif",  # Stream directly to disk for large files
  overwrite = TRUE
)

drainage_rast <- rasterize(
  x = soil_merged,
  y = boshab,
  touches = TRUE,
  field = "drainage_val",
  filename = "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/bodemkaart-drainage/flanders_drainage_10m_touches.tif",  # Stream directly to disk for large files
  overwrite = TRUE
)


soil_raster <- rast("G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/bodemkaart-drainage/flanders_drainage_10m.tif")




### Set ouput path location ###
output_base_path <- file.path(base_dir, "Boshabitat_covariabelen/blocks")


### drainage ###
for (i in 1:nrow(block_grid)) {
  #i <- 1
  block_sel <- block_grid[i, ]
  block_buffer <- buffer(block_sel,50) # create a 50 meter buffer
  
  # crop reference layer
  rast_crop <- crop(boshab, block_sel, mask = TRUE)
  
  # crop covariables
  drain_crop <- crop(soil_raster, block_buffer, mask = TRUE)
  
  # align rasters
  # Crop and align r2 to the exact extent, resolution, and origin of r1
  drain_aligned <- resample(drain_crop, rast_crop, method = "bilinear")
  
  # Optional: Ensure the spatial extent is strictly clipped to r1s bounding box
  drain_final <- crop(drain_aligned, rast_crop)
  
  drain_final <- project(drain_final,originalcrs)
  
  newfolder <- paste0("block", i)
  newpath <- file.path(output_base_path, newfolder)
  dir.create(newpath, recursive = TRUE, showWarnings = FALSE)
  
  outpath <- file.path(newpath, paste0("Drainage_block_", i, ".tif"))
  
  writeRaster(
    drain_final,
    outpath,
    gdal = c("COMPRESS=LZW"),
    overwrite = TRUE)
  
  
}

