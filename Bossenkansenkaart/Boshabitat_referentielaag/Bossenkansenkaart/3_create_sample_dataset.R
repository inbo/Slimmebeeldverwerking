library(terra)

# Define the root folder
root_dir <- "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/Bossenkansenkaart/Boshabitat_referentielaag/blocks"

# Recursively find all raster .tif files across subfolders
tif_files <- list.files(
  path = root_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

tif_files_first <- tif_files[1:100] 
tif_files_second <- tif_files[101:200]
tif_files_third <- tif_files[201:300]
tif_files_fourth <- tif_files[301:400]
tif_files_fifth <- tif_files[401:500]
tif_files_sixth <- tif_files[501:600]
tif_files_seventh <- tif_files[601:700]
tif_files_eigth <- tif_files[701:800]
tif_files_nineth <- tif_files[801:900]
tif_files_tenth <- tif_files[901:976]

#### First ####
# Convert each raster file into a point vector layer (automatically excludes NaN/NA)
points_list <- lapply(tif_files_first, function(file_path) {
  r <- rast(file_path)
  
  # as.points converts valid raster pixels into points
  # na.rm = TRUE drops all NaN/NA values by default
  pt <- as.points(r, na.rm = TRUE)
  return(pt)
})

# Combine all individual SpatVector objects into one single point layer
all_points <- vect(svc(points_list))

# Optional: Save the combined point vector layer to disk (e.g., as a geopackage)
writeVector(all_points, "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/Bossenkansenkaart/Boshabitat_referentielaag/combined_points_first.gpkg", overwrite = TRUE)

#### Second ####
# Convert each raster file into a point vector layer (automatically excludes NaN/NA)
points_list <- lapply(tif_files_second, function(file_path) {
  r <- rast(file_path)
  
  # as.points converts valid raster pixels into points
  # na.rm = TRUE drops all NaN/NA values by default
  pt <- as.points(r, na.rm = TRUE)
  return(pt)
})

# Combine all individual SpatVector objects into one single point layer
all_points <- vect(svc(points_list))

# Optional: Save the combined point vector layer to disk (e.g., as a geopackage)
writeVector(all_points, "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/Bossenkansenkaart/Boshabitat_referentielaag/combined_points_second.gpkg", overwrite = TRUE)



#### Third ####
# Convert each raster file into a point vector layer (automatically excludes NaN/NA)
points_list <- lapply(tif_files_third, function(file_path) {
  r <- rast(file_path)
  
  # as.points converts valid raster pixels into points
  # na.rm = TRUE drops all NaN/NA values by default
  pt <- as.points(r, na.rm = TRUE)
  return(pt)
})

# Combine all individual SpatVector objects into one single point layer
all_points <- vect(svc(points_list))

# Optional: Save the combined point vector layer to disk (e.g., as a geopackage)
writeVector(all_points, "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/Bossenkansenkaart/Boshabitat_referentielaag/combined_points_third.gpkg", overwrite = TRUE)


#### Fourth ####
# Convert each raster file into a point vector layer (automatically excludes NaN/NA)
points_list <- lapply(tif_files_fourth, function(file_path) {
  r <- rast(file_path)
  
  # as.points converts valid raster pixels into points
  # na.rm = TRUE drops all NaN/NA values by default
  pt <- as.points(r, na.rm = TRUE)
  return(pt)
})

# Combine all individual SpatVector objects into one single point layer
all_points <- vect(svc(points_list))

# Optional: Save the combined point vector layer to disk (e.g., as a geopackage)
writeVector(all_points, "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/Bossenkansenkaart/Boshabitat_referentielaag/combined_points_fourth.gpkg", overwrite = TRUE)

#### fifth ####
# Convert each raster file into a point vector layer (automatically excludes NaN/NA)
points_list <- lapply(tif_files_fifth, function(file_path) {
  r <- rast(file_path)
  
  # as.points converts valid raster pixels into points
  # na.rm = TRUE drops all NaN/NA values by default
  pt <- as.points(r, na.rm = TRUE)
  return(pt)
})

# Combine all individual SpatVector objects into one single point layer
all_points <- vect(svc(points_list))

# Optional: Save the combined point vector layer to disk (e.g., as a geopackage)
writeVector(all_points, "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/Bossenkansenkaart/Boshabitat_referentielaag/combined_points_fifth.gpkg", overwrite = TRUE)


#### sixth ####
# Convert each raster file into a point vector layer (automatically excludes NaN/NA)
points_list <- lapply(tif_files_sixth, function(file_path) {
  r <- rast(file_path)
  
  # as.points converts valid raster pixels into points
  # na.rm = TRUE drops all NaN/NA values by default
  pt <- as.points(r, na.rm = TRUE)
  return(pt)
})

# Combine all individual SpatVector objects into one single point layer
all_points <- vect(svc(points_list))

# Optional: Save the combined point vector layer to disk (e.g., as a geopackage)
writeVector(all_points, "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/Bossenkansenkaart/Boshabitat_referentielaag/combined_points_sixth.gpkg", overwrite = TRUE)

#### seventh ####
# Convert each raster file into a point vector layer (automatically excludes NaN/NA)
points_list <- lapply(tif_files_seventh, function(file_path) {
  r <- rast(file_path)
  
  # as.points converts valid raster pixels into points
  # na.rm = TRUE drops all NaN/NA values by default
  pt <- as.points(r, na.rm = TRUE)
  return(pt)
})

# Combine all individual SpatVector objects into one single point layer
all_points <- vect(svc(points_list))

# Optional: Save the combined point vector layer to disk (e.g., as a geopackage)
writeVector(all_points, "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/Bossenkansenkaart/Boshabitat_referentielaag/combined_points_seventh.gpkg", overwrite = TRUE)


#### eigth ####
# Convert each raster file into a point vector layer (automatically excludes NaN/NA)
points_list <- lapply(tif_files_eigth, function(file_path) {
  r <- rast(file_path)
  
  # as.points converts valid raster pixels into points
  # na.rm = TRUE drops all NaN/NA values by default
  pt <- as.points(r, na.rm = TRUE)
  return(pt)
})

# Combine all individual SpatVector objects into one single point layer
all_points <- vect(svc(points_list))

# Optional: Save the combined point vector layer to disk (e.g., as a geopackage)
writeVector(all_points, "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/Bossenkansenkaart/Boshabitat_referentielaag/combined_points_eigth.gpkg", overwrite = TRUE)


#### nineth ####
# Convert each raster file into a point vector layer (automatically excludes NaN/NA)
points_list <- lapply(tif_files_nineth, function(file_path) {
  r <- rast(file_path)
  
  # as.points converts valid raster pixels into points
  # na.rm = TRUE drops all NaN/NA values by default
  pt <- as.points(r, na.rm = TRUE)
  return(pt)
})

# Combine all individual SpatVector objects into one single point layer
all_points <- vect(svc(points_list))

# Optional: Save the combined point vector layer to disk (e.g., as a geopackage)
writeVector(all_points, "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/Bossenkansenkaart/Boshabitat_referentielaag/combined_points_nineth.gpkg", overwrite = TRUE)


#### tenth ####
# Convert each raster file into a point vector layer (automatically excludes NaN/NA)
points_list <- lapply(tif_files_tenth, function(file_path) {
  r <- rast(file_path)
  
  # as.points converts valid raster pixels into points
  # na.rm = TRUE drops all NaN/NA values by default
  pt <- as.points(r, na.rm = TRUE)
  return(pt)
})

# Combine all individual SpatVector objects into one single point layer
all_points <- vect(svc(points_list))

# Optional: Save the combined point vector layer to disk (e.g., as a geopackage)
writeVector(all_points, "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/Bossenkansenkaart/Boshabitat_referentielaag/combined_points_tenth.gpkg", overwrite = TRUE)
