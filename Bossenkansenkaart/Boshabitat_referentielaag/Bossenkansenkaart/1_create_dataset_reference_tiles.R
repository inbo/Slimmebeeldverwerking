### Load the libraries ####
library(terra)
library(tidyterra)

### Load the datasets ####
base_dir <- "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/Bossenkansenkaart"

boshab <- rast(file.path(base_dir, "Boshabitat_referentielaag/BWK_2025_bosraster.tif"))
tile_grid <- vect(file.path(base_dir, "grids/512_pixel_grid.gpkg"))
block_grid <- vect(file.path(base_dir, "grids/50km_spatial_blok.gpkg"))

output_base_path <- file.path(base_dir, "Boshabitat_referentielaag/blocks")

### Loop through each spatial block ####
for (i in 1:nrow(block_grid)) {
  
  block_sel <- block_grid[i, ] # Fixed hardcoded index [1,] -> [i,]
  
  newfolder <- paste0("block", i)
  newpath <- file.path(output_base_path, newfolder)
  dir.create(newpath, recursive = TRUE, showWarnings = FALSE)
  
  # Find tiles completely inside the current block
  rel_matrix <- relate(tile_grid, block_sel, "coveredby")
  inside_indices <- which(apply(rel_matrix, 1, any))
  
  tiles_in_block <- tile_grid[inside_indices, ]
  
  # Track valid tiles containing actual raster data
  valid_tile_indices <- c()
  
  # Shuffle indices to randomly test tiles until 100 valid ones are found
  candidate_indices <- sample(seq_len(nrow(tiles_in_block)))
  
  for (idx in candidate_indices) {
    candidate_tile <- tiles_in_block[idx, ]
    
    # Crop without masking first (faster speed check)
    tile_crop <- crop(boshab, candidate_tile, mask = TRUE)
    
    # Calculate non-NA cell counts
    valid_cells <- global(tile_crop, "notNA", na.rm = TRUE)$notNA
    
    if (valid_cells > 0) {
      valid_tile_indices <- c(valid_tile_indices, idx)
    }
    
    # Stop searching once 100 valid tiles are found
    if (length(valid_tile_indices) == 100) break
  }
  
  if (length(valid_tile_indices) < 100) {
    warning(paste0("Block ", i, " only contained ", length(valid_tile_indices), " valid tiles."))
  }
  
  # Selected valid tiles for export
  selected_polygons <- tiles_in_block[valid_tile_indices, ]
  
  # Process and write valid rasters
  for (j in seq_len(nrow(selected_polygons))) {
    sel_poly <- selected_polygons[j, ]
    tile_id <- sel_poly$id
    
    rast_crop <- crop(boshab, sel_poly, mask = TRUE)
    
    # Reclassify NAs to 255 for PyTorch ignore_index
    mask_encoded <- classify(rast_crop, cbind(NA, 255))
    
    outpath <- file.path(newpath, paste0("Reference_tile_id_", tile_id, ".tif"))
    
    writeRaster(
      mask_encoded,
      outpath,
      datatype = "INT1U",
      NAflag = 255,
      gdal = c("COMPRESS=LZW"),
      overwrite = TRUE
    )
  }
}