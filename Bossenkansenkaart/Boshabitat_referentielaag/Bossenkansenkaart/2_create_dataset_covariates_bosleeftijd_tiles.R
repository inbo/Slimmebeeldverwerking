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
bosleeftijd <- vect("G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/Bosleeftijd_Opname_1771_2001_actualisatie_2021_GewVLA_Shapefile/Shapefile/Blftd.shp")


# Define the ordered drainage categories
leeftijd_levels <- c("Bos ontstaan voor 1775", "Bos ontstaan tussen 1775 en 1850", "Bos ontstaan tussen 1850 en +/- 1930", "Bos ontstaan tussen 1850 en +/- 1940", "Bos ontstaan na +/- 1930", "Bos ontstaan na +/- 1940")

# Assign evenly spaced numeric values from 0 to 1
leeftijd_num <- seq(0, 1, length.out = length(leeftijd_levels))



# Create a lookup mapping table
leeftijd_key <- data.frame(
  leeftijd_class = leeftijd_levels,
  leeftijd_val   = leeftijd_num
)


targetcrs <- crs(bosleeftijd)


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
bosleeftijd_merged <- merge(bosleeftijd, leeftijd_key, by.x = "BLK", by.y = "leeftijd_class", all.x = TRUE)

# Rasterize using the template grid and your numeric attribute
bosleeftijd_rast <- rasterize(
  x = bosleeftijd_merged,
  y = boshab,
  field = "leeftijd_val",
  filename = "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/Bosleeftijd_Opname_1771_2001_actualisatie_2021_GewVLA_Shapefile/bosleeftijd.tif",  # Stream directly to disk for large files
  overwrite = TRUE
)




### Set ouput path location ###
output_base_path <- file.path(base_dir, "Boshabitat_covariabelen/blocks")


### Bosleeftijd ###
for (i in 1:nrow(block_grid)) {
  #i <- 1
  block_sel <- block_grid[i, ]
  block_buffer <- buffer(block_sel,50) # create a 50 meter buffer
  
  # crop reference layer
  rast_crop <- crop(boshab, block_sel, mask = TRUE)
  
  # crop covariables
  boslf_crop <- crop(bosleeftijd_rast, block_buffer, mask = TRUE)
  
  # align rasters
  # Crop and align r2 to the exact extent, resolution, and origin of r1
  boslf_aligned <- resample(boslf_crop, rast_crop, method = "bilinear")
  
  # Optional: Ensure the spatial extent is strictly clipped to r1s bounding box
  boslf_final <- crop(boslf_aligned, rast_crop)
  
  boslf_final <- project(boslf_final,originalcrs)
  
  newfolder <- paste0("block", i)
  newpath <- file.path(output_base_path, newfolder)
  dir.create(newpath, recursive = TRUE, showWarnings = FALSE)
  
  outpath <- file.path(newpath, paste0("Bosleeftijd_block_", i, ".tif"))
  
  writeRaster(
    boslf_final,
    outpath,
    gdal = c("COMPRESS=LZW"),
    overwrite = TRUE)
  
  
}

