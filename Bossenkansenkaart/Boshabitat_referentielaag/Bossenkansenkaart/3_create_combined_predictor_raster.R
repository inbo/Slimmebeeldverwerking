library(sf)
library(terra)



setwd("G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen")


# Define vector of file paths (also cleaned up duplicate DTM entries)
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

# Read each raster separately into a list
raster_list <- lapply(raster_files, rast)

# Set reference raster grid (uses the first raster converted to target CRS)
ref_raster <- project(raster_list[[1]], target_crs)

# Reproject and align all rasters to match ref_raster's CRS, extent, and resolution
processed_rasters <- lapply(raster_list, function(r) {
  # Check if raster contains categorical data or integer/boolean types
  is_categorical <- any(is.factor(r)) || any(datatype(r) %in% c("INT1U", "INT1S", "INT2U", "INT2S", "INT4U", "INT4S", "LOG1S"))
  
  m <- if (is_categorical) "near" else "bilinear"
  
  project(r, ref_raster, method = m)
})

# Combine all processed layers into a single SpatRaster object
predictors <- rast(processed_rasters)

response <- "BWK_2025_bosraster"
predictor_names <- names(predictors)

plot(predictors)
#plot(samples)

#outpath = "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/predictors.tif"
#writeRaster(
#  predictors,
#  outpath,
#  overwrite = TRUE)


###################
# Start from here #
###################
setwd("C:/Users/sebastiaan_verbessel/Downloads")
#predictors <- rast("G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/predictors.tif")

predictors <- rast("predictors.tif")

win.graph()
plot(predictors)
names(predictors)

#gewest <- vect("G:/Mijn Drive/Dennensterfte/Labelme/CropSAR_TS/Grens Vlaams Gewest/Shapefile/Refgew.shp")
gewest <- vect("Refgew.shp")
plot(gewest)

predictors_cropped <- crop(predictors, gewest, mask = TRUE)
rm(predictors)
win.graph()
plot(predictors_cropped)
plot(predictors_cropped[[17:32]])

# Correction on the drainage layer
predictors_cropped[[1]] <- ifel(predictors_cropped[[1]] < 0.0769, 0, predictors_cropped[[1]])

# Set value 2 to NA for bands 5, 6, and 7
predictors_cropped[[5:7]] <- ifel(predictors_cropped[[5:7]] == 2, NA, predictors_cropped[[5:7]])

# Convert bands 3 to 7 to binary (0 and 1)
# Values > 0 become 1; 0 remains 0; NA values are preserved
predictors_cropped[[3:7]] <- ifel(predictors_cropped[[3:7]] > 0, 1, 0)

# (Optional) If you strictly require terra to define them as categorical factor layers:
# predictors[[3:7]] <- as.factor(predictors[[3:7]])

# Min-Max scale bands 16 to 32 between 0 and 1
b_min <- global(predictors_cropped[[16:32]], "min", na.rm = TRUE)[, 1]
b_max <- global(predictors_cropped[[16:32]], "max", na.rm = TRUE)[, 1]

predictors_cropped[[16:32]] <- (predictors_cropped[[16:32]] - b_min) / (b_max - b_min)

plot(predictors_cropped)
plot(predictors_cropped[[16:32]])

writeRaster(
  predictors_cropped,
  "C:/Users/sebastiaan_verbessel/Downloads/predictors_ARD.tif",
  overwrite = TRUE)




####### Modify last raster band layer #######
library(sf)
library(terra)



setwd("G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen")


predictors <- rast("predictors_ARD.tif")
summary(predictors)
names(predictors)


b_min <- global(predictors[[33:35]], "min", na.rm = TRUE)[, 1]
b_max <- global(predictors[[33:35]], "max", na.rm = TRUE)[, 1]

predictors[[33:35]] <- (predictors[[33:35]] - b_min) / (b_max - b_min)

writeRaster(
  predictors,
  "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/predictors_ARD2.tif",
  overwrite = TRUE)


####### Modify raster bands by filling in NA values (within Flanders) #######
library(sf)
library(terra)



setwd("G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen")


predictors <- rast("predictors_ARD.tif")
Fl <- vect("G:/Mijn Drive/Dennensterfte/Labelme/CropSAR_TS/Grens Vlaams Gewest/Shapefile/Refgew.shp")
summary(predictors)
names(predictors)

leeft_old <- unique(predictors$leeftijd_val)
predictors$leeftijd_val <- ifel(predictors$leeftijd_val < 0.2, 0, predictors$leeftijd_val)

predictors$leeftijd_val <- ifel(predictors$leeftijd_val == 0.8, 2.5/3, predictors$leeftijd_val)
predictors$leeftijd_val <- ifel(predictors$leeftijd_val == 0.6, 2/3, predictors$leeftijd_val)
predictors$leeftijd_val <- ifel(predictors$leeftijd_val == 0.4, 0.5, predictors$leeftijd_val)
predictors$leeftijd_val <- ifel(predictors$leeftijd_val == 0.2, 1/3, predictors$leeftijd_val)
predictors$leeftijd_val <- ifel(predictors$leeftijd_val == 0, 0.5/3, predictors$leeftijd_val)



#predictors$drainage_val
#predictors$leeftijd_val


# Check if CRS match and transform if necessary
if (crs(Fl) != crs(predictors)) {
  Fl <- project(Fl, crs(predictors))
}

# Extract raster cell indices within the polygon boundaries
cells_in_fl <- cells(predictors[["leeftijd_val"]], Fl)[, "cell"]

# Identify which of those cells currently have NA values
na_mask <- is.na(predictors[["leeftijd_val"]][cells_in_fl])

# Update ONLY the NA values inside the polygons to 0
predictors[["leeftijd_val"]][cells_in_fl[na_mask]] <- 0



# Extract raster cell indices within the polygon boundaries
cells_in_fl <- cells(predictors[["drainage_val"]], Fl)[, "cell"]

# Identify which of those cells currently have NA values
na_mask <- is.na(predictors[["drainage_val"]][cells_in_fl])

# Update ONLY the NA values inside the polygons to 0
predictors[["drainage_val"]][cells_in_fl[na_mask]] <- 0



# Extract raster cell indices within the polygon boundaries
cells_in_fl <- cells(predictors[["Bosmonitoring-boomsoort_2022-01-01_1"]], Fl)[, "cell"]

# Identify which of those cells currently have NA values
na_mask <- is.na(predictors[["Bosmonitoring-boomsoort_2022-01-01_1"]][cells_in_fl])

# Update ONLY the NA values inside the polygons to 0
predictors[["Bosmonitoring-boomsoort_2022-01-01_1"]][cells_in_fl[na_mask]] <- 0



# Extract raster cell indices within the polygon boundaries
cells_in_fl <- cells(predictors[["Bosmonitoring-boomsoort_2022-01-01_2"]], Fl)[, "cell"]

# Identify which of those cells currently have NA values
na_mask <- is.na(predictors[["Bosmonitoring-boomsoort_2022-01-01_2"]][cells_in_fl])

# Update ONLY the NA values inside the polygons to 0
predictors[["Bosmonitoring-boomsoort_2022-01-01_2"]][cells_in_fl[na_mask]] <- 0


rm(cells_in_fl,na_mask)


writeRaster(
  predictors,
  "G:/Gedeelde drives/PRJ_SlimmeBeeldverwerking/PRJ_2026_SlimmeBeeldverwerking/Bewerkte covariabelen/predictors_ARD2_filled.tif",
  overwrite = TRUE)

summary(predictors)
