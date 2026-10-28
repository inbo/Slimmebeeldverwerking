library(terra)
library(vegan)
library(pairwiseAdonis) # devtools::install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(caret)

# Set seed for reproducibility
set.seed(42)

# -----------------------------------------------------------------------------
# 1. Spatial Sampling with Minimum Distance Constraint (500m)
# -----------------------------------------------------------------------------
# Assuming 'landcover' is a SpatRaster and 'covariates' is a SpatRaster with 35 layers

landcover_raster <- rast("G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/Bossenkansenkaart/Boshabitat_referentielaag/BWK_2025_bosraster.tif")
covariates_raster <- rast("G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/predictors_ARD2_filled.tif")

crs(landcover_raster)
res(landcover_raster)
dim(landcover_raster)
table(freq(landcover_raster))


sample_class_with_min_dist <- function(class_val, raster_lc, n_samples = 5000, min_dist = 1000) {
  # Mask raster so only the targeted class contains values (others become NA)
  class_mask <- mask(raster_lc, raster_lc == class_val, maskvalues = FALSE)
  
  # Sample strictly from non-NA cells
  class_points <- spatSample(class_mask, size = n_samples * 10, method = "random", 
                             as.points = TRUE, values = FALSE, na.rm = TRUE)
  
  if (length(class_points) == 0) {
    stop(paste("No cells found for class", class_val))
  }
  
  # Select points maintaining minimum distance
  selected_indices <- c(1)
  
  for (i in 2:length(class_points)) {
    if (length(selected_indices) >= n_samples) break
    
    # Calculate distance to already selected points
    dist_matrix <- distance(class_points[i], class_points[selected_indices])
    
    if (min(dist_matrix) >= min_dist) {
      selected_indices <- c(selected_indices, i)
    }
  }
  
  if (length(selected_indices) < n_samples) {
    warning(paste("Only managed to sample", length(selected_indices), "points for class", class_val))
  }
  
  sample_pts <- class_points[selected_indices]
  sample_pts$class <- class_val
  return(sample_pts)
}

# Run spatial sampling for classes 0 through 3
sampled_pts_list <- lapply(0:3, function(cl) {
  sample_class_with_min_dist(class_val = cl, raster_lc = landcover_raster, 
                             n_samples = 1000, min_dist = 2000)
})



# Combine all spatial samples
all_samples <- do.call(rbind, sampled_pts_list)

# Determine the minimum count among all classes
min_n <- min(table(all_samples$class))

# Sample equal rows from each class directly on the SpatVector
balanced_list <- lapply(unique(all_samples$class), function(cl) {
  # Get indices for current class
  idx <- which(all_samples$class == cl)
  # Randomly sample min_n indices
  sampled_idx <- sample(idx, min_n)
  # Subset SpatVector
  all_samples[sampled_idx, ]
})

# 3. Combine SpatVector objects using terra's rbind
balanced_samples <- do.call(rbind, balanced_list)
table(all_samples$class)
table(balanced_samples$class)


plot(balanced_samples ,col=as.factor(balanced_samples $class))

plot(landcover_raster)
plot(landcover_raster==2)

plot(balanced_samples [balanced_samples $class==2,],add=T,col="red")

# Extract covariate values at sampled point locations
covariates_extracted <- terra::extract(covariates_raster, balanced_samples , df = TRUE)
data_df <- cbind(class = as.factor(balanced_samples $class), covariates_extracted[, -1])

# Remove any rows with missing values
data_df <- na.omit(data_df)

write.csv(data_df,"G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/Bossenkansenkaart/Boshabitat_referentielaag/Bossenkansenkaart/resultaten permanova/dataset_permanova.csv")



#### Repeat analysis #####

data_df <- read.csv("G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/Bossenkansenkaart/Boshabitat_referentielaag/Bossenkansenkaart/resultaten permanova/dataset_permanova.csv", header = T, sep=",")


set.seed(42)
str(data_df)
data_df$class <- as.factor(data_df$class)
data_df <- data_df[,-1]

landcover_raster <- rast("G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/Bossenkansenkaart/Boshabitat_referentielaag/BWK_2025_bosraster.tif")
covariates_raster <- rast("G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/predictors_ARD2_filled.tif")

# -----------------------------------------------------------------------------
# 2. PERMANOVA Analysis
# -----------------------------------------------------------------------------
# Standardize continuous variables (and Gower/Euclidean distance calculation)
# Using Gower distance as covariates contain both binary and continuous data
covar_matrix <- data_df[, -1]
dist_matrix <- vegdist(covar_matrix, method = "gower")

# Run PERMANOVA using adonis2
permanova_res <- adonis2(dist_matrix ~ class, data = data_df, permutations = 999)
print("--- PERMANOVA Results ---")
print(permanova_res)

# Check Multivariate Homogeneity of Groups Dispersions (PERMDISP)
dispersion <- betadisper(dist_matrix, data_df$class)
print("--- PERMDISP Test ---")
print(anova(dispersion))

# -----------------------------------------------------------------------------
# 3. Post-Hoc Pairwise PERMANOVA
# -----------------------------------------------------------------------------
posthoc_res <- pairwise.adonis2(dist_matrix ~ class, data = data_df, nperm = 999)
print("--- Post-Hoc Pairwise PERMANOVA ---")
print(posthoc_res)


# Power analyse op basis van de piloot studie
library(vegan)

# Stel dat 'X' je matrix van 35 variabelen is en 'group' de factor met 4 niveaus
simulate_power <- function(data, group_factor, n_per_group, n_sim = 100) {
  p_values <- numeric(n_sim)
  
  for (i in 1:n_sim) {
    # Trek een willekeurige steekproef per groep
    sampled_indices <- unlist(lapply(unique(group_factor), function(g) {
      sample(which(group_factor == g), n_per_group, replace = TRUE)
    }))
    
    sub_data <- data[sampled_indices, ]
    sub_group <- group_factor[sampled_indices]
    
    # Voer PERMANOVA uit
    res <- adonis2(sub_data ~ sub_group, method = "gower", permutations = 199)
    p_values[i] <- res$`Pr(>F)`[1]
  }
  
  # Power is het aandeel simulaties waarin p < 0.05
  power <- mean(p_values < 0.05)
  return(power)
}

# Voorbeelduitvoering:
power_result <- simulate_power(data_df[,-1], data_df$class, n_per_group = 77, n_sim = 200)
power_result


# -----------------------------------------------------------------------------
# 4. Visualizations
# -----------------------------------------------------------------------------

# A. PCoA / Ordination Plot
pcoa_res <- cmdscale(dist_matrix, k = 2, eig = TRUE)
pcoa_df <- data.frame(
  PCoA1 = pcoa_res$points[, 1],
  PCoA2 = pcoa_res$points[, 2],
  Class = data_df$class
)

# Variance explained
var_pcoa1 <- round(pcoa_res$eig[1] / sum(pcoa_res$eig) * 100, 1)
var_pcoa2 <- round(pcoa_res$eig[2] / sum(pcoa_res$eig) * 100, 1)

ggplot(pcoa_df, aes(x = PCoA1, y = PCoA2, color = Class, fill = Class)) +
  stat_ellipse(geom = "polygon", alpha = 0.2, level = 0.95) +
  geom_point(alpha = 0.4, size = 1) +
  labs(
    title = "PCoA Ordination of Landcover Classes across Covariates",
    x = paste0("PCoA 1 (", var_pcoa1, "%)"),
    y = paste0("PCoA 2 (", var_pcoa2, "%)")
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")

# B. Distance to Centroid (Dispersion Plot)
plot(dispersion, main = "PCoA - Group Dispersions (PERMDISP)", hull = FALSE, ellipse = TRUE)



library(ggplot2)
library(dplyr)
library(tidyr)
library(pheatmap)
library(RColorBrewer)

# Assumed 'data_df' from the previous step:
# First column 'class' (factor with levels 0, 1, 2, 3), followed by 35 covariate columns.

# Separate continuous and binary variable column names
# (Adjust these vector definitions to match your dataset)
binary_vars <- names(data_df)[sapply(data_df, function(x) all(x %in% c(0, 1, NA)))]
continuous_vars <- setdiff(names(data_df)[-1], binary_vars)


# =============================================================================
# 1. Heatmap: Mean Values per Class for ALL 35 Covariates
# =============================================================================
# Standardizing (Z-score) continuous variables across groups allows direct visual 
# comparison on a single color scale alongside binary proportions (0-1).

# Calculate Z-scores for continuous, raw proportions for binary
df_scaled <- data_df
df_scaled[continuous_vars] <- scale(df_scaled[continuous_vars])

# Calculate average profile per class
class_means <- df_scaled %>%
  group_by(class) %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE))) %>%
  column_to_rownames(var = "class")

# Transpose matrix for display (Covariates as rows, Classes as columns)
heatmap_matrix <- t(class_means)

# Plot Heatmap
pheatmap(
  heatmap_matrix,
  cluster_cols = FALSE,      # Keep class order (0, 1, 2, 3)
  cluster_rows = TRUE,       # Cluster covariates with similar response patterns
  scale = "none", 
  color = colorRampPalette(c("#2166ac", "#f7f7f7", "#b2182b"))(100),
  main = "Covariate Profile Heatmap Across Landcover Classes (Standardized Continuous Z-Scores & Binary Proportions)",
  fontsize_row = 8,
  fontsize_col = 10
)


# =============================================================================
# 2. Continuous Covariates: Faceted Boxplots
# =============================================================================
# Reshape continuous variables into long format for ggplot
data_long_cont <- data_df %>%
  select(class, all_of(continuous_vars)) %>%
  pivot_longer(
    cols = -class, 
    names_to = "Covariate", 
    values_to = "Value"
  )

# Option A: Standardized Z-Score Boxplots on one unified panel grid
data_long_cont_scaled <- data_long_cont %>%
  group_by(Covariate) %>%
  mutate(Value_Scaled = scale(Value)) %>%
  ungroup()

ggplot(data_long_cont_scaled, aes(x = class, y = Value_Scaled, fill = class)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.7) +
  facet_wrap(~ Covariate, scales = "fixed") +
  labs(
    title = "Continuous Covariate Distributions by Class",
    x = "Landcover Class",
    y = "Standardized Value (Z-Score)"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(strip.text = element_text(size = 8, face = "bold"))


# =============================================================================
# 3. Binary Covariates: Proportion / Occurrence Bar Plots
# =============================================================================
if (length(binary_vars) > 0) {
  
  binary_summary <- data_df %>%
    select(class, all_of(binary_vars)) %>%
    pivot_longer(
      cols = -class, 
      names_to = "Covariate", 
      values_to = "Presence"
    ) %>%
    group_by(class, Covariate) %>%
    summarise(Proportion = mean(Presence, na.rm = TRUE), .groups = "drop")
  
  ggplot(binary_summary, aes(x = class, y = Proportion, fill = class)) +
    geom_col(alpha = 0.85, color = "black", linewidth = 0.2) +
    facet_wrap(~ Covariate, scales = "fixed") +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    labs(
      title = "Binary Covariate Prevalence by Class",
      x = "Landcover Class",
      y = "Presence Rate (%)"
    ) +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal() +
    theme(strip.text = element_text(size = 8, face = "bold"))
}

