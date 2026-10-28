library(terra)
library(gstat)
library(sf)

# ----------------------------------------------------------------------
# 1. Load Raster & Mask NoData
# ----------------------------------------------------------------------
r <- rast("G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/Bossenkansenkaart/Boshabitat_referentielaag/BWK_2025_bosraster.tif")
r[r == -1] <- NA

# ----------------------------------------------------------------------
# 2. Optimized Range Calculation Function
# ----------------------------------------------------------------------
get_class_range <- function(raster_obj, class_val, sample_size = 50000) {
  message("Starting process for class: ", class_val)
  
  # Step A: Fast spatial sampling directly from the raster
  # 'random' sampling across non-NA cells avoids reading the full raster into memory at once
  set.seed(42)
  pts_sf <- spatSample(
    raster_obj, 
    size = sample_size, 
    method = "random", 
    as.points = TRUE, 
    values = TRUE, 
    na.rm = TRUE
  ) |> st_as_sf()
  
  # Stop if sampling returned no data
  if (nrow(pts_sf) == 0) return(NULL)
  
  # Step B: Create binary indicator on the sampled points
  raster_col <- names(pts_sf)[1]
  pts_sf$indicator <- as.integer(pts_sf[[raster_col]] == class_val)
  
  # Step C: Compute experimental variogram
  exp_vrio <- variogram(indicator ~ 1, data = pts_sf)
  
  # Step D: Fit spherical model
  init_sill <- var(pts_sf$indicator, na.rm = TRUE)
  init_range <- max(exp_vrio$dist) / 3
  
  fit_vrio <- tryCatch({
    fit.variogram(exp_vrio, model = vgm(psill = init_sill, "Sph", range = init_range, nugget = 0))
  }, error = function(e) {
    return(NULL)
  })
  
  fitted_range <- if (!is.null(fit_vrio)) fit_vrio$range[2] else NA
  
  return(list(
    class = class_val,
    range_distance = fitted_range,
    experimental = exp_vrio,
    model = fit_vrio
  ))
}

# ----------------------------------------------------------------------
# 3. Run Analysis
# ----------------------------------------------------------------------
classes <- c(0, 1, 2, 3)

# Increased sample_size to 50,000 for better statistical representation
results <- lapply(classes, function(cl) get_class_range(r, class_val = cl, sample_size = 50000))

# ----------------------------------------------------------------------
# 4. View Results
# ----------------------------------------------------------------------
summary_df <- data.frame(
  LandCover_Class = sapply(results, function(x) x$class),
  Range_Distance  = sapply(results, function(x) x$range_distance)
)

summary_df$Range_Distance_km <- summary_df$Range_Distance/1000

print("Spatial Autocorrelation Range (Distance in meters and kilometers):")
print(summary_df)

# Plot variogram for Class 1 as an example
plot(results[[4]]$experimental, model = results[[4]]$model, 
     main = "Indicator Variogram for Land Cover Class 3")

plot(results[[3]]$experimental, model = results[[3]]$model, 
     main = "Indicator Variogram for Land Cover Class 2")


plot(results[[2]]$experimental, model = results[[2]]$model, 
     main = "Indicator Variogram for Land Cover Class 1")

plot(results[[1]]$experimental, model = results[[1]]$model, 
     main = "Indicator Variogram for Land Cover Class 0")
