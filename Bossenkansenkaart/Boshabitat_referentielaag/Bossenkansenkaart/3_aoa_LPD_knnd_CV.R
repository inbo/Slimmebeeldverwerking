library(CAST)
library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(caret)
library(blockCV)

set.seed(44)

setwd("G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen")


# 1. Read your samples sf object and extract its target CRS
samples <- read_sf("G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/Bossenkansenkaart/Boshabitat_referentielaag/combined_points_first.gpkg")
target_crs <- crs(samples)

# 2. Define vector of file paths (also cleaned up duplicate DTM entries)
raster_files <- c(
  "./bodemkaart-drainage/flanders_drainage_10m.tif",
  "./Bosleeftijd_Opname_1771_2001_actualisatie_2021_GewVLA_Shapefile/bosleeftijd.tif",
  "./boswijzer/boswijzer2009.tif",
  "./boswijzer/boswijzer2012.tif",
  "S:/Vlaanderen/Natuur_Bos/Boswijzer_Groenkaart/Boswijzer_2015.tif",
  "S:/Vlaanderen/Natuur_Bos/Boswijzer_Groenkaart/Boswijzer_2018.tif",
  "S:/Vlaanderen/Natuur_Bos/Boswijzer_Groenkaart/Boswijzer_2021.tif",
  "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Slimmebeeldverwerking/Bossenkansenkaart/Data Margot Verhulst/2022/Bosmonitoring-boomsoort_2022-01-01.tif",
  "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/DTM-CHM/DTM_max.tif",
  "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/DTM-CHM/DTM_median.tif",
  "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/DTM-CHM/DTM_IQR.tif",
  "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/DTM-CHM/CHM_max.tif",
  "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/DTM-CHM/CHM_median.tif",
  "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/DTM-CHM/CHM_IQR.tif",
  "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/FOREDGEMAP/HKBR1778/2_densiteit/HKBR1778_bomenrij_density200m_warp_clip.tif",
  "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/FOREDGEMAP/HKBR1778/2_densiteit/HKBR1778_houtkant_density100m_warp_clip.tif",
  "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/FOREDGEMAP/HKBR1873/2_densiteit/HKBR1873_bomenrij_density200m_warp_clip.tif",
  "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/FOREDGEMAP/HKBR1873/2_densiteit/HKBR1873_houtkant_density100m_warp_clip.tif",
  "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/FOREDGEMAP/HKBR1939/2_densiteit/HKBR1939_bomenrij_density_200m_clip.tif",
  "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/FOREDGEMAP/HKBR1939/2_densiteit/HKBR1939_houtkant_density_100m_clip.tif",
  "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/FOREDGEMAP/HKBR1969/2_densiteit/HKBR1969_bomenrij_density200m_warp_clip.tif",
  "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/FOREDGEMAP/HKBR1969/2_densiteit/HKBR1969_houtkant_density_100m_clip.tif",
  "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/fractie zand/topsoil_bdbstat__fractie_zand_basisdata_bodemkartering_samengevoegd.tif",
  "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/fractie zand/topsoil_unc_bdbstat__fractie_zand_basisdata_bodemkartering_samengevoegd.tif",
  "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/fractie_leem/topsoil_bdbstat__fractie_leem_basisdata_bodemkartering_samengevoegd.tif",
  "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/fractie_leem/topsoil_unc_bdbstat__fractie_leem_basisdata_bodemkartering_samengevoegd.tif",
  "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/OCst_30_2026_gemiddelde_percentielen/processed/OCst_30_2026_gemiddelde_percentielen/OCst_30_2026_gemiddelde_percentielen_median.tif",
  "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/OCst_30_2026_gemiddelde_percentielen/processed/OCst_30_2026_gemiddelde_percentielen/OCst_30_2026_gemiddelde_percentielen_IQR.tif"
)

# 3. Read each raster separately into a list
raster_list <- lapply(raster_files, rast)

# 4. Set reference raster grid (uses the first raster converted to target CRS)
ref_raster <- project(raster_list[[1]], target_crs)

# 5. Reproject and align all rasters to match ref_raster's CRS, extent, and resolution
processed_rasters <- lapply(raster_list, function(r) {
  # Check if raster contains categorical data or integer/boolean types
  is_categorical <- any(is.factor(r)) || any(datatype(r) %in% c("INT1U", "INT1S", "INT2U", "INT2S", "INT4U", "INT4S", "LOG1S"))
  
  m <- if (is_categorical) "near" else "bilinear"
  
  project(r, ref_raster, method = m)
})

# 6. Combine all processed layers into a single SpatRaster object
predictors <- rast(processed_rasters)

response <- "BWK_2025_bosraster"
predictor_names <- names(predictors)

plot(predictors)
plot(samples)

# Extract predictor values at sample locations and prepare model training data
samples_df <- terra::extract(predictors, samples, ID = FALSE)
samples_df <- cbind(samples, samples_df)
train_data <- samples_df |> 
  st_drop_geometry() |>
  dplyr::select(all_of(predictor_names))

# 1) AoA based only on the training data ------------------------------------------------
# See https://doi.org/10.1111/2041-210X.13650 for details on the method and interpretation of results
aoa_data <- CAST::aoa(
  newdata = predictors,
  train = train_data,
  variables = predictor_names,
  verbose = FALSE
)

# DI -- dissimilarity index -- values close to 0 indicate that the predictor values at a location are similar to those in the training data, while higher values indicate increasing dissimilarity
plot(aoa_data) +
  ggtitle("Distributions of training data and predictors in predictor space (DI values)")

# DI map
plot(aoa_data$DI, main = "Distribution of DI values across the study area")
