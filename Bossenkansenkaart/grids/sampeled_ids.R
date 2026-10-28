library(sf)
library(stringr)
library(dplyr)

# ============================================================
# INPUT / OUTPUT
# ============================================================

# Original GeoPackage
gpkg_file <- "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/Bossenkansenkaart/grids/512_pixel_grid.gpkg"

# Directory containing the 10 subdirectories with TIFF files
tif_dir <- "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/Bossenkansenkaart/Boshabitat_referentielaag/blocks"

# Output GeoPackage
output_gpkg <- "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/Bossenkansenkaart/grids/sampled_ids.gpkg"

# Name of the layer in the input GeoPackage
# Set to NULL if the GPKG contains only one layer
input_layer <- NULL


# ============================================================
# 1. READ THE GEOPACKAGE
# ============================================================

if (is.null(input_layer)) {
  layers <- st_layers(gpkg_file)$name
  
  if (length(layers) != 1) {
    stop(
      "The GeoPackage contains multiple layers. ",
      "Please specify 'input_layer'."
    )
  }
  
  input_layer <- layers[1]
}

polygons <- st_read(
  gpkg_file,
  layer = input_layer,
  quiet = TRUE
)

# Check that the ID column exists
if (!"id" %in% names(polygons)) {
  stop("The GeoPackage does not contain a column named 'id'.")
}


# ============================================================
# 2. FIND ALL TIFF FILES
# ============================================================

tif_files <- list.files(
  path = tif_dir,
  pattern = "\\.tif$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(tif_files) == 0) {
  stop("No .tif files were found.")
}

cat("Found", length(tif_files), "TIFF files.\n")


# ============================================================
# 3. EXTRACT IDs FROM TIFF FILENAMES
# ============================================================
#
# Expected filename:
#
# Reference_tile_id_12345.tif
#
# where 12345 is the ID from the GeoPackage.
#
# Everything after "Reference_tile_id_" and before ".tif"
# is interpreted as the ID.
#

tif_names <- basename(tif_files)

tif_ids <- str_match(
  tif_names,
  "^Reference_tile_id_(.+)\\.tif$"
)[, 2]

# Check for filenames that don't match the expected pattern
unmatched_files <- tif_files[is.na(tif_ids)]

if (length(unmatched_files) > 0) {
  warning(
    length(unmatched_files),
    " TIFF files did not match the expected filename pattern."
  )
  
  print(basename(unmatched_files))
}


# Remove unmatched files
tif_ids <- tif_ids[!is.na(tif_ids)]

# Remove duplicates
tif_ids <- unique(tif_ids)

cat("Found", length(tif_ids), "unique IDs in the TIFF files.\n")


# ============================================================
# 4. MATCH TIFF IDs TO THE GEOPACKAGE IDs
# ============================================================

# Convert both to character to avoid problems if one is numeric
gpkg_ids <- as.character(polygons$id)
tif_ids  <- as.character(tif_ids)

sampled_ids <- intersect(gpkg_ids, tif_ids)

cat(
  "Matched",
  length(sampled_ids),
  "IDs between the TIFF files and GeoPackage.\n"
)


# ============================================================
# 5. SELECT THE MATCHING POLYGONS
# ============================================================

sampled_polygons <- polygons %>%
  filter(as.character(id) %in% sampled_ids)


# ============================================================
# 6. REPORT UNMATCHED IDs
# ============================================================

ids_without_polygon <- setdiff(tif_ids, gpkg_ids)

ids_without_tif <- setdiff(gpkg_ids, tif_ids)

cat(
  "IDs in TIFFs but not in GeoPackage:",
  length(ids_without_polygon),
  "\n"
)

cat(
  "IDs in GeoPackage but not in TIFFs:",
  length(ids_without_tif),
  "\n"
)


# ============================================================
# 7. WRITE NEW GEOPACKAGE
# ============================================================

# Remove existing output file if it exists
if (file.exists(output_gpkg)) {
  file.remove(output_gpkg)
}

st_write(
  sampled_polygons,
  output_gpkg,
  layer = "sampled_ids",
  quiet = TRUE
)

cat("\nDone!\n")
cat(
  "Output written to:\n",
  output_gpkg,
  "\n"
)

cat(
  "Number of polygons in output:",
  nrow(sampled_polygons),
  "\n"
)
