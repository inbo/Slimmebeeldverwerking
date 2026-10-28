library(terra)

landcover_raster <- rast("G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/Bossenkansenkaart/Boshabitat_referentielaag/BWK_2025_bosraster.tif")


# ============================================================
# INDICATOR SEMIVARIOGRAMS FOR A VERY LARGE LAND-COVER RASTER
# ============================================================
#
# INPUT:
#   landcover_raster
#
# Raster characteristics:
#   9,098 x 23,689 cells
#   10 x 10 m resolution
#   ~215.5 million cells
#   Belgian Lambert 72 / EPSG:31370
#
# LAND-COVER CLASSES:
#   0, 1, 2, 3
#
# METHOD:
#   - Keep original 10-m land-cover values
#   - Sample one pixel every 200 m in x and y
#   - ~54,000 sample locations
#   - Create binary indicator for each class
#   - Calculate empirical indicator semivariogram
#   - Fit spherical model
#   - Extract range, nugget and sill
#   - Calculate RMSE, SSE and R2
#
# IMPORTANT:
#   The 200-m spacing is the sampling interval.
#   It does NOT aggregate the original 10-m land-cover raster.
# ============================================================


# ============================================================
# 1. PACKAGES
# ============================================================

library(terra)
library(gstat)
library(sp)


# ============================================================
# 2. INPUT RASTER
# ============================================================

# Your existing raster
r <- landcover_raster

# Alternatively:
# r <- rast("path/to/landcover.tif")


# ------------------------------------------------------------
# Check raster
# ------------------------------------------------------------

cat("\n================ RASTER INFORMATION ================\n")

print(r)

cat("\nDimensions:\n")
print(dim(r))

cat("\nResolution:\n")
print(res(r))

cat("\nCRS:\n")
print(crs(r))


# ============================================================
# 3. CHECK RESOLUTION AND CRS
# ============================================================

if (nlyr(r) != 1) {
  stop("The raster must contain exactly one layer.")
}

if (abs(res(r)[1] - 10) > 0.001 ||
    abs(res(r)[2] - 10) > 0.001) {
  
  warning(
    "The raster is not exactly 10 x 10 m. ",
    "The script will still run, but check the sampling interval."
  )
}


# ============================================================
# 4. ANALYSIS SETTINGS
# ============================================================

# ------------------------------------------------------------
# Land-cover classes
# ------------------------------------------------------------

classes <- c(0, 1, 2, 3)


# ------------------------------------------------------------
# Sampling interval
#
# Original raster = 10 m
# Sampling interval = 200 m
#
# Therefore:
#
#   200 / 10 = 20
#
# We take every 20th pixel in each direction.
# ------------------------------------------------------------

sample_spacing <- 200

sampling_factor <- sample_spacing / res(r)[1]

if (sampling_factor != round(sampling_factor)) {
  
  stop(
    "Sampling interval must be an integer multiple ",
    "of raster resolution."
  )
  
}

sampling_factor <- as.integer(sampling_factor)


# ------------------------------------------------------------
# Maximum variogram distance
# ------------------------------------------------------------

max_distance <- 10000


# ------------------------------------------------------------
# Variogram lag width
# ------------------------------------------------------------

lag_width <- 200


# ------------------------------------------------------------
# Optional maximum number of points
#
# NULL = use every 200-m sample point.
#
# If variogram calculation becomes too slow, set this to
# e.g. 30000 or 20000.
# ------------------------------------------------------------

max_variogram_points <- NULL


# ------------------------------------------------------------
# Random seed
# ------------------------------------------------------------

set.seed(123)



# ============================================================
# 5. SAMPLE ORIGINAL 10-m PIXELS EVERY 200 m
# ============================================================

pixel_size <- res(r)[1]

sampling_factor <- as.integer(sample_spacing / pixel_size)

if (sampling_factor < 1) {
  stop("sample_spacing must be >= raster pixel size.")
}

rows <- seq(1, nrow(r), by = sampling_factor)
cols <- seq(1, ncol(r), by = sampling_factor)

cat("Number of sampled rows:", length(rows), "\n")
cat("Number of sampled columns:", length(cols), "\n")

grid_indices <- expand.grid(
  row = rows,
  col = cols
)

cat("Total candidate sample points:",
    nrow(grid_indices), "\n")


# ------------------------------------------------------------
# Convert row/column positions to cell numbers
# ------------------------------------------------------------

sample_cells <- terra::cellFromRowCol(
  r,
  row = grid_indices$row,
  col = grid_indices$col
)


# ------------------------------------------------------------
# Get coordinates of the sampled ORIGINAL pixels
# ------------------------------------------------------------

sample_xy <- terra::xyFromCell(
  r,
  sample_cells
)


# ------------------------------------------------------------
# Create a SpatVector of points
# ------------------------------------------------------------

sample_points <- terra::vect(
  data.frame(
    x = sample_xy[, 1],
    y = sample_xy[, 2]
  ),
  geom = c("x", "y"),
  crs = terra::crs(r)
)


# ------------------------------------------------------------
# Extract raster values at those points
#
# IMPORTANT:
# We do NOT use ID = FALSE because some terra versions
# handle that argument differently.
# ------------------------------------------------------------

sample_values <- terra::extract(
  r,
  sample_points
)


# The first column is the point ID.
# The second column is the raster value.
sample_values <- sample_values[[2]]


# ------------------------------------------------------------
# Build sampling data frame
# ------------------------------------------------------------

sample_df <- data.frame(
  x = sample_xy[, 1],
  y = sample_xy[, 2],
  landcover = sample_values
)


# ------------------------------------------------------------
# Remove NA cells
# ------------------------------------------------------------

sample_df <- sample_df[
  !is.na(sample_df$landcover),
]


# ------------------------------------------------------------
# Keep only the expected land-cover classes
# ------------------------------------------------------------

sample_df <- sample_df[
  sample_df$landcover %in% classes,
]


# Make sure class values are numeric
sample_df$landcover <- as.numeric(
  sample_df$landcover
)


cat(
  "Number of valid sampled pixels:",
  nrow(sample_df),
  "\n"
)


# ============================================================
# 6. OPTIONAL: LIMIT NUMBER OF POINTS USED BY GSTAT
# ============================================================

# At 200-m spacing your raster produces roughly 539,000
# candidate points. Calculating all pairwise distances for
# that many points can be extremely expensive.
#
# Therefore, retain a random subset for the variogram.
#
# IMPORTANT:
# These are still ORIGINAL 10-m raster pixel values.
# We are only reducing the number of locations used to
# calculate the variogram.

max_variogram_points <- 20000

if (
  !is.null(max_variogram_points) &&
  nrow(sample_df) > max_variogram_points
) {
  
  set.seed(123)
  
  sample_df <- sample_df[
    sample(
      seq_len(nrow(sample_df)),
      size = max_variogram_points
    ),
  ]
  
  rownames(sample_df) <- NULL
  
  cat(
    "Points retained for variogram:",
    nrow(sample_df),
    "\n"
  )
}


# ============================================================
# 7. CREATE INDICATOR VARIABLES
# ============================================================

for (cl in classes) {
  
  sample_df[[paste0("I_", cl)]] <-
    as.numeric(sample_df$landcover == cl)
  
}


cat("\nFinal number of points used:",
    nrow(sample_df), "\n")

cat("\nClass proportions:\n")
print(
  prop.table(
    table(sample_df$landcover)
  )
)



# ============================================================
# 8. GET COORDINATES
# ============================================================

sample_xy <- terra::xyFromCell(
  r,
  sample_cells
)


# ============================================================
# 9. CREATE DATA FRAME
# ============================================================

sample_df <- data.frame(
  x = sample_xy[, 1],
  y = sample_xy[, 2],
  landcover = sample_values
)


# Remove NA cells

sample_df <- sample_df[
  !is.na(sample_df$landcover),
]


# Keep only classes 0-3

sample_df <- sample_df[
  sample_df$landcover %in% classes,
]


# Make sure landcover is numeric

sample_df$landcover <-
  as.numeric(sample_df$landcover)


cat(
  "\nValid sampled locations:",
  nrow(sample_df),
  "\n"
)


# ============================================================
# 10. CHECK CLASS DISTRIBUTION
# ============================================================

cat("\n================ CLASS DISTRIBUTION ================\n")

class_counts <- table(
  factor(
    sample_df$landcover,
    levels = classes
  )
)

print(class_counts)

class_proportions <-
  prop.table(class_counts)

cat("\nClass proportions:\n")

print(
  round(
    class_proportions,
    4
  )
)


# ============================================================
# 11. OPTIONAL REDUCTION OF SAMPLE SIZE
# ============================================================

# For your raster, ~54,000 points is reasonable.
#
# However, if the variogram calculations are too slow,
# you can reduce this to e.g. 20,000 points.
#
# The reduction is performed after the systematic sampling.

if (!is.null(max_variogram_points) &&
    nrow(sample_df) > max_variogram_points) {
  
  cat(
    "\nReducing sample to",
    max_variogram_points,
    "points...\n"
  )
  
  selected <- sample(
    seq_len(nrow(sample_df)),
    size = max_variogram_points,
    replace = FALSE
  )
  
  sample_df <- sample_df[selected, ]
  
}


# ============================================================
# 12. CONVERT TO SpatialPointsDataFrame
# ============================================================

coordinates(sample_df) <- ~x + y


# Use the CRS of the raster.
#
# Your raster is Belgian Lambert 72 / EPSG:31370.

proj4string(sample_df) <-
  CRS(SRS_string = "EPSG:31370")


# ============================================================
# 13. PRINT FINAL SAMPLE SIZE
# ============================================================

cat("\n================ FINAL SAMPLE ================\n")

cat(
  "Number of points:",
  nrow(sample_df),
  "\n"
)

cat(
  "Sampling interval:",
  sample_spacing,
  "m\n"
)

cat(
  "Maximum variogram distance:",
  max_distance,
  "m\n"
)

cat(
  "Lag width:",
  lag_width,
  "m\n"
)


# ============================================================
# 14. FUNCTION TO CALCULATE INDICATOR VARIOGRAM
# ============================================================

calculate_indicator_variogram <- function(
    class_value,
    data,
    cutoff,
    width
) {
  
  
  cat("\n\n")
  cat("====================================================\n")
  cat("CLASS", class_value, "\n")
  cat("====================================================\n")
  
  
  # ----------------------------------------------------------
  # Create indicator variable
  #
  # 1 = pixel belongs to class
  # 0 = pixel belongs to another class
  # ----------------------------------------------------------
  
  data$indicator <-
    ifelse(
      data$landcover == class_value,
      1,
      0
    )
  
  
  # ----------------------------------------------------------
  # Class proportion
  # ----------------------------------------------------------
  
  class_proportion <-
    mean(data$indicator)
  
  cat(
    "Class proportion:",
    round(class_proportion, 4),
    "\n"
  )
  
  
  # ----------------------------------------------------------
  # Empirical indicator semivariogram
  # ----------------------------------------------------------
  
  cat(
    "Calculating empirical semivariogram...\n"
  )
  
  
  v_emp <- gstat::variogram(
    indicator ~ 1,
    data = data,
    cutoff = cutoff,
    width = width,
    cloud = FALSE
  )
  
  
  # ----------------------------------------------------------
  # Remove invalid bins
  # ----------------------------------------------------------
  
  v_emp <- v_emp[
    is.finite(v_emp$gamma) &
      is.finite(v_emp$dist) &
      v_emp$np > 0,
  ]
  
  
  # Need at least several variogram bins
  
  if (nrow(v_emp) < 5) {
    
    warning(
      "Too few variogram bins for class ",
      class_value
    )
    
    return(NULL)
    
  }
  
  
  # ==========================================================
  # 15. INITIAL SPHERICAL MODEL
  # ==========================================================
  
  # Initial nugget
  #
  # Use first empirical semivariance.
  
  nugget_start <-
    v_emp$gamma[1]
  
  
  # Initial total sill
  #
  # Use 90th percentile to avoid one noisy final bin.
  
  sill_start <-
    as.numeric(
      quantile(
        v_emp$gamma,
        probs = 0.90,
        na.rm = TRUE
      )
    )
  
  
  # Make sure sill > nugget
  
  if (sill_start <= nugget_start) {
    
    sill_start <-
      max(
        v_emp$gamma,
        na.rm = TRUE
      )
    
  }
  
  
  if (sill_start <= nugget_start) {
    
    sill_start <-
      nugget_start + 0.01
    
  }
  
  
  # Initial range
  
  range_start <-
    cutoff * 0.25
  
  
  # ----------------------------------------------------------
  # Initial spherical model
  # ----------------------------------------------------------
  
  initial_model <- gstat::vgm(
    psill =
      sill_start - nugget_start,
    model = "Sph",
    range = range_start,
    nugget = nugget_start
  )
  
  
  # ==========================================================
  # 16. FIT SPHERICAL MODEL
  # ==========================================================
  
  cat(
    "Fitting spherical model...\n"
  )
  
  
  fitted_model <- tryCatch(
    
    gstat::fit.variogram(
      v_emp,
      model = initial_model
    ),
    
    error = function(e) {
      
      cat(
        "Model fitting failed:\n",
        e$message,
        "\n"
      )
      
      return(NULL)
      
    }
    
  )
  
  
  # ----------------------------------------------------------
  # Check model
  # ----------------------------------------------------------
  
  if (is.null(fitted_model)) {
    
    return(NULL)
    
  }
  
  
  # ----------------------------------------------------------
  # Check for singular fit
  # ----------------------------------------------------------
  
  if (
    "singular" %in%
    names(fitted_model)
  ) {
    
    if (
      isTRUE(
        attr(
          fitted_model,
          "singular"
        )
      )
    ) {
      
      warning(
        "Spherical model may have a singular fit for class ",
        class_value
      )
      
    }
    
  }
  
  
  # ==========================================================
  # 17. EXTRACT MODEL PARAMETERS
  # ==========================================================
  
  nugget <- fitted_model$psill[
    fitted_model$model == "Nug"
  ]
  
  
  partial_sill <- fitted_model$psill[
    fitted_model$model == "Sph"
  ]
  
  
  range <- fitted_model$range[
    fitted_model$model == "Sph"
  ]
  
  
  total_sill <-
    nugget + partial_sill
  
  
  # ==========================================================
  # 18. PREDICTIONS FROM SPHERICAL MODEL
  # ==========================================================
  
  fitted_line <- gstat::variogramLine(
    fitted_model,
    dist_vector = v_emp$dist
  )
  
  
  observed <-
    v_emp$gamma
  
  predicted <-
    fitted_line$gamma
  
  
  # Remove invalid values
  
  keep <-
    is.finite(observed) &
    is.finite(predicted)
  
  
  observed <-
    observed[keep]
  
  predicted <-
    predicted[keep]
  
  
  # ==========================================================
  # 19. GOODNESS OF FIT
  # ==========================================================
  
  # Sum of squared errors
  
  SSE <-
    sum(
      (observed - predicted)^2
    )
  
  
  # RMSE
  
  RMSE <-
    sqrt(
      mean(
        (observed - predicted)^2
      )
    )
  
  
  # R-squared
  
  denominator <-
    sum(
      (observed - mean(observed))^2
    )
  
  
  if (denominator > 0) {
    
    R2 <-
      1 - SSE / denominator
    
  } else {
    
    R2 <- NA
    
  }
  
  
  # ==========================================================
  # 20. REPORT RESULTS
  # ==========================================================
  
  cat("\n")
  cat("---------------- RESULTS ----------------\n")
  
  cat(
    "Class:         ",
    class_value,
    "\n"
  )
  
  cat(
    "Proportion:    ",
    round(class_proportion, 4),
    "\n"
  )
  
  cat(
    "Nugget:        ",
    round(nugget, 6),
    "\n"
  )
  
  cat(
    "Partial sill:  ",
    round(partial_sill, 6),
    "\n"
  )
  
  cat(
    "Total sill:    ",
    round(total_sill, 6),
    "\n"
  )
  
  cat(
    "Range:         ",
    round(range, 2),
    "m\n"
  )
  
  cat(
    "RMSE:          ",
    round(RMSE, 6),
    "\n"
  )
  
  cat(
    "SSE:           ",
    round(SSE, 6),
    "\n"
  )
  
  cat(
    "R-squared:     ",
    round(R2, 4),
    "\n"
  )
  
  
  # ==========================================================
  # 21. PLOT EMPIRICAL + FITTED VARIOGRAM
  # ==========================================================
  
  plot(
    v_emp$dist, 
    v_emp$gamma,
    main = paste("Indicator semivariogram - Class", class_value),
    xlab = "Distance (m)",
    ylab = "Semivariance",
    pch = 16,
    ylim = c(0, max(v_emp$gamma, na.rm = TRUE) * 1.1)
  )
  
  # Add fitted spherical curve
  lines(
    v_emp$dist,
    predicted,
    col = "blue",
    lwd = 2
  )
  
  # Add spatial range vertical line
  abline(
    v = range,
    col = "red",
    lty = 2,
    lwd = 2
  )
  
  # Add range text annotation
  text(
    x = range,
    y = max(v_emp$gamma, na.rm = TRUE) * 0.8,
    labels = paste0("Range = ", round(range), " m"),
    pos = 4,
    col = "red"
  )
  
  
  # ==========================================================
  # 22. RETURN RESULTS
  # ==========================================================
  
  return(
    list(
      
      class = class_value,
      
      proportion =
        class_proportion,
      
      empirical =
        v_emp,
      
      model =
        fitted_model,
      
      range =
        range,
      
      nugget =
        nugget,
      
      partial_sill =
        partial_sill,
      
      total_sill =
        total_sill,
      
      RMSE =
        RMSE,
      
      SSE =
        SSE,
      
      R2 =
        R2
      
    )
  )
  
}


# ============================================================
# 23. RUN FOR ALL CLASSES
# ============================================================

results <- list()


for (cl in classes) {
  
  results[[as.character(cl)]] <-
    
    calculate_indicator_variogram(
      class_value = cl,
      data = sample_df,
      cutoff = max_distance,
      width = lag_width
    )
  
}


# ============================================================
# 24. CREATE SUMMARY TABLE
# ============================================================

summary_list <- lapply(
  results,
  function(x) {
    
    if (is.null(x)) {
      return(NULL)
    }
    
    data.frame(
      
      Class =
        x$class,
      
      Proportion =
        x$proportion,
      
      Nugget =
        x$nugget,
      
      Partial_Sill =
        x$partial_sill,
      
      Total_Sill =
        x$total_sill,
      
      Range_m =
        x$range,
      
      RMSE =
        x$RMSE,
      
      SSE =
        x$SSE,
      
      R2 =
        x$R2
      
    )
    
  }
)


summary_table <-
  do.call(
    rbind,
    summary_list
  )


rownames(summary_table) <- NULL


# ============================================================
# 25. PRINT FINAL SUMMARY
# ============================================================

cat("\n\n")
cat("====================================================\n")
cat("FINAL INDICATOR VARIOGRAM RESULTS\n")
cat("====================================================\n\n")

print(summary_table)


# ============================================================
# 26. SAVE SUMMARY
# ============================================================

write.csv(
  summary_table,
  "indicator_semivariogram_results.csv",
  row.names = FALSE
)


cat("\n")
cat(
  "Results saved to:\n",
  "indicator_semivariogram_results.csv\n"
)


# ============================================================
# 27. OPTIONAL: SAVE THE EMPIRICAL VARIOGRAMS
# ============================================================

# This creates a single CSV containing the empirical
# semivariogram values for all classes.

empirical_variograms <- do.call(
  rbind,
  lapply(
    results,
    function(x) {
      
      if (is.null(x)) {
        return(NULL)
      }
      
      data.frame(
        
        Class =
          x$class,
        
        Distance_m =
          x$empirical$dist,
        
        Semivariance =
          x$empirical$gamma,
        
        Number_of_pairs =
          x$empirical$np
        
      )
      
    }
  )
)


write.csv(
  empirical_variograms,
  "indicator_empirical_variograms.csv",
  row.names = FALSE
)


cat(
  "Empirical variograms saved to:\n",
  "indicator_empirical_variograms.csv\n"
)


# ### What this script is actually doing
# 
# Your original raster remains at **10 m resolution**. We're selecting cells like:
# 
# 
# 10 m raster:
# 
# x x x x x x x x x x x x x x x x x x x x
#                     ↑
#                   sample
#                     ↓
# x x x x x x x x x x x x x x x x x x x x
#                     ↑
#                   sample
# 
# 
# with a **200 m interval**, rather than creating a 200-m raster.
# 
# For your raster, that produces approximately:
# 
# $$
# \frac{9098}{20}\times\frac{23689}{20}
# \approx 53800
# $$
# 
# sample locations.
# 
# So you're retaining the **actual 10-m land-cover classification** at each sampled location rather than replacing a 200 × 200 m area with its modal class.
# 
# ### One important caveat
# 
# There's a potentially significant computational issue with the current settings:
# 
# 
# max_distance <- 10000
# lag_width <- 200
# 
# With ~54,000 points, `gstat` can still have to consider a **very large number of point pairs** within 10 km. If the variogram calculation is slow or runs out of memory, change:
# 
# 
# max_variogram_points <- NULL
# 
# 
# to:
# 
# 
# max_variogram_points <- 20000
# 
# 
# That will randomly reduce the 54,000 systematic sample points to 20,000 before calculating the variograms.
# 
# **I would first try the full ~54,000 points**, though.
# 
# Also, when you get the results, don't just look at `R2`. For this particular application, I would inspect whether the empirical variogram actually approaches a sill. A spherical model can return a numerical "range" even when the data don't strongly constrain that range. If you send me your resulting `summary_table`, I can help assess whether the four estimated ranges are statistically/structurally convincing.
