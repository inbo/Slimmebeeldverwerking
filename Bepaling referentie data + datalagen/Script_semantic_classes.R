library(terra)
library(sf)
library(dplyr)

setwd("C:/Users/sebastiaan_verbessel/Downloads")
# Read data
r <- rast("boswijzer_meerdaal.tif")
pol <- st_read("test sites.gpkg")   # or shapefile

pol <- pol %>% st_transform(crs(r))
pol <- vect(pol)

#-------------------------------------------------------
# Step 1: Reclassify raster
#-------------------------------------------------------

# 0 -> -1
# 1 -> 1
# 2 -> -9999
r_class <- classify(
  r,
  rbind(
    c(0, 0),
    c(1,  1),
    c(2, -9999)
  )
)

#-------------------------------------------------------
# Step 2: Rasterize polygon classes
#-------------------------------------------------------

# Create numeric codes for polygon classes
pol$class <- NA_integer_
pol$class[pol$HABLEGENDE == "ohab"] <- 2
pol$class[pol$HABLEGENDE == "phab"] <- 3
pol$class[pol$HABLEGENDE == "hab"]  <- 4   # change if another value is desired

poly_rast <- rasterize(pol, r, field = "class")

#-------------------------------------------------------
# Step 3: Update only cells that were originally 1
#-------------------------------------------------------

idx <- r_class == 1 & !is.na(poly_rast)

r_class[idx] <- poly_rast[idx]

#-------------------------------------------------------
# Save result
#-------------------------------------------------------

writeRaster(r_class, "classified_raster.tif", overwrite = TRUE)


#-------------------------------------------------------
# Step 4: Convert -1 back to 0
#-------------------------------------------------------

r_class[r_class == -1] <- 0

#-------------------------------------------------------
# Step 5: Prepare raster for smoothing
#-------------------------------------------------------

# Keep track of which cells should be updated
update_cells <- r_class %in% c(1, 2, 3, 4)

# Copy raster for smoothing
r_smooth <- r_class

# Exclude nodata from the smoothing
r_smooth[r_smooth == -9999] <- NA

#-------------------------------------------------------
# Step 6: 5x5 moving-window mean
#-------------------------------------------------------

w <- matrix(1, 5, 5)

r_mean <- focal(
  r_smooth,
  w = w,
  fun = mean,
  na.policy = "omit",
  na.rm = TRUE
)

#-------------------------------------------------------
# Step 7: Update only habitat cells
#-------------------------------------------------------

r_final <- r_class
r_final[update_cells] <- r_mean[update_cells]

# Restore nodata value
r_final[is.na(r_final)] <- -9999

#-------------------------------------------------------
# Save
#-------------------------------------------------------

writeRaster(r_final, "classified_smoothed.tif", overwrite = TRUE)
