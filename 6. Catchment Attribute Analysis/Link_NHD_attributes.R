#################### Lauren Bolotin - bolotinljb@gmail.com ##############################################################
## Link project data with land cover, geology, etc. via NHD ComID (Wieczorek & Schwarz 2019)
# NOTE re GBCO data: there are still some WQP sites without a ComID (13 of them)
library(tidyverse)
## WUS DATA: ##################################################################################
## Bring in files that have SiteID's and ComID's
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
usgs_comid <- readRDS("WUS_USGS_ComID.rds")

# Bring in the NHD Attribute Data 
# downloaded at https://www.sciencebase.gov/catalog/item/5669a79ee4b08895842a1d47

# Land Use/Hydrologic Modifications ####

# (NLCD) National Land Cover Database (2016) 
# reach catchments accumulated upstream proportional land cover through the river network
setwd("/Volumes/Blaszczak Lab/FSS/NHD Attributes/Land Use:Hydrologic Modifications") 
nlcd_acc <- read.csv("NLCD16_ACC_CONUS.csv")
head(nlcd_acc)
# Subset NLCD by our sites of interest
nlcd_acc <- subset(nlcd_acc, nlcd_acc$COMID %in% usgs_comid$COMID) # the number of rows should be = to the number of rows in usgs_comid
# Create one df with all necessary info
usgs_nlcd <- merge(usgs_comid, nlcd_acc, by = "COMID", all = TRUE)
# Rename columns to describe land use classes
names(usgs_nlcd)
colnames(usgs_nlcd) <- c("COMID", "SiteID", "OpenWater_pct", "PerennialIceSnow_pct", 
                         "DevelopedOpenSpace_pct", "DevelopedLowIntensity_pct", 
                         "DevelopedMedIntensity_pct", "DevelopedHiIntensity_pct", "BarrenLand_pct",
                         "DeciduousForest_pct", "EvergreenForest_pct", "MixedForest_pct",
                         "ShrubScrub_pct", "GrasslandHerbaceous_pct", "PastureHay_pct",
                         "CultivatedCrops_pct", "WoodyWetlands_pct", "EmergentHerbWetlands_pct",
                         "NLCD_NoData")
## Save output
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(usgs_nlcd, "WUS_USGS_NLCD16.rds")
rm(nlcd_acc, usgs_nlcd)

# Irrigated Agriculture 
# 2012 MODIS Irrigated Agricultural Data (MIrAD) data
# Percent of all land that’s irrigated agriculture in 2012 accumulated upstream through the river network
setwd("/Volumes/Blaszczak Lab/FSS/NHD Attributes/Land Use:Hydrologic Modifications") 
ag_acc <- read.csv("MIRAD_2012_CONUS.csv")
head(ag_acc)
# Subset NLCD by our sites of interest
ag_acc <- subset(ag_acc, ag_acc$COMID %in% usgs_comid$COMID) # the number of rows should be = to the number of rows in usgs_comid
# Create one df with all necessary info
usgs_ag <- merge(usgs_comid, ag_acc, by = "COMID", all = TRUE)
# Rename columns to describe land use classes
names(usgs_ag)
usgs_ag <- select(usgs_ag, c("COMID", "SiteID", "ACC_MIRAD_2012", "ACC_NODATA"))
# For more info on each agogic class, see the metadata for this specific file in the link at the top of the script
colnames(usgs_ag) <- c("COMID", "SiteID", "MIRAD_Irrig_Ag_Land_pct", "MIRAD_NoData")
# Save Output:
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(usgs_ag, "WUS_USGS_Irrigated_Ag.rds")
rm(usgs_ag, ag_acc)

# National Inventory of Dams (NID)
# 1990:
setwd("/Volumes/Blaszczak Lab/FSS/NHD Attributes/Land Use:Hydrologic Modifications") 
NID_acc <- read.csv("NID_1990_CONUS.csv")
head(NID_acc)
# Subset NLCD by our sites of interest
NID_acc <- subset(NID_acc, NID_acc$COMID %in% usgs_comid$COMID) # the number of rows should be = to the number of rows in usgs_comid
# Create one df with all necessary info
usgs_NID <- merge(usgs_comid, NID_acc, by = "COMID", all = TRUE)
# Rename columns to describe land use classes
names(usgs_NID)
usgs_NID <- select(usgs_NID, c("COMID", "SiteID", "ACC_NDAMS1990", "ACC_NID_STORAGE1990", 
                               "ACC_NORM_STORAGE1990", "ACC_MAJOR1990"))
# For more info on each NIDogic class, see the metadata for this specific file in the link at the top of the script
colnames(usgs_NID) <- c("COMID", "SiteID", "NDAMS1990", "NID_STORAGE1990", 
                        "NORM_STORAGE1990", "MAJOR1990")
# Save Output:
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(usgs_NID, "WUS_USGS_NID_1990.rds")
rm(usgs_NID, NID_acc)

# 2000:
setwd("/Volumes/Blaszczak Lab/FSS/NHD Attributes/Land Use:Hydrologic Modifications") 
NID_acc <- read.csv("NID_2000_CONUS.csv")
head(NID_acc)
# Subset NLCD by our sites of interest
NID_acc <- subset(NID_acc, NID_acc$COMID %in% usgs_comid$COMID) # the number of rows should be = to the number of rows in usgs_comid
# Create one df with all necessary info
usgs_NID <- merge(usgs_comid, NID_acc, by = "COMID", all = TRUE)
# Rename columns to describe land use classes
names(usgs_NID)
usgs_NID <- select(usgs_NID, c("COMID", "SiteID", "ACC_NDAMS2000", "ACC_NID_STORAGE2000", 
                               "ACC_NORM_STORAGE2000", "ACC_MAJOR2000"))
# For more info on each NIDogic class, see the metadata for this specific file in the link at the top of the script
colnames(usgs_NID) <- c("COMID", "SiteID", "NDAMS2000", "NID_STORAGE2000", 
                        "NORM_STORAGE2000", "MAJOR2000")
# Save Output:
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(usgs_NID, "WUS_USGS_NID_2000.rds")
rm(usgs_NID, NID_acc)

#2010: 
setwd("/Volumes/Blaszczak Lab/FSS/NHD Attributes/Land Use:Hydrologic Modifications") 
NID_acc <- read.csv("NID_2010_CONUS.csv")
head(NID_acc)
# Subset NLCD by our sites of interest
NID_acc <- subset(NID_acc, NID_acc$COMID %in% usgs_comid$COMID) # the number of rows should be = to the number of rows in usgs_comid
# Create one df with all necessary info
usgs_NID <- merge(usgs_comid, NID_acc, by = "COMID", all = TRUE)
# Rename columns to describe land use classes
names(usgs_NID)
usgs_NID <- select(usgs_NID, c("COMID", "SiteID", "ACC_NDAMS2010", "ACC_NID_STORAGE2010", 
                               "ACC_NORM_STORAGE2010", "ACC_MAJOR2010"))
# For more info on each NIDogic class, see the metadata for this specific file in the link at the top of the script
colnames(usgs_NID) <- c("COMID", "SiteID", "NDAMS2010", "NID_STORAGE2010", 
                        "NORM_STORAGE2010", "MAJOR2010")
# Save Output:
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(usgs_NID, "WUS_USGS_NID_2010.rds")
rm(usgs_NID, NID_acc)

#2013:
setwd("/Volumes/Blaszczak Lab/FSS/NHD Attributes/Land Use:Hydrologic Modifications") 
NID_acc <- read.csv("NID_2013_CONUS.csv")
head(NID_acc)
# Subset NLCD by our sites of interest
NID_acc <- subset(NID_acc, NID_acc$COMID %in% usgs_comid$COMID) # the number of rows should be = to the number of rows in usgs_comid
# Create one df with all necessary info
usgs_NID <- merge(usgs_comid, NID_acc, by = "COMID", all = TRUE)
# Rename columns to describe land use classes
names(usgs_NID)
usgs_NID <- select(usgs_NID, c("COMID", "SiteID", "ACC_NDAMS2013", "ACC_NID_STORAGE2013", 
                               "ACC_NORM_STORAGE2013", "ACC_MAJOR2013"))
# For more info on each NIDogic class, see the metadata for this specific file in the link at the top of the script
colnames(usgs_NID) <- c("COMID", "SiteID", "NDAMS2013", "NID_STORAGE2013", 
                        "NORM_STORAGE2013", "MAJOR2013")
# Save Output:
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(usgs_NID, "WUS_USGS_NID_2013.rds")
rm(usgs_NID, NID_acc)


# Geology ####
# Bush & Reed (2001) Generalized Geology types:
# accumulated upstream through the river network
setwd("/Volumes/Blaszczak Lab/FSS/NHD Attributes/Geology") 
geol_acc <- read.csv("BUSHREED_ACC_CONUS.csv")
head(geol_acc)
# Subset NLCD by our sites of interest
geol_acc <- subset(geol_acc, geol_acc$COMID %in% usgs_comid$COMID) # the number of rows should be = to the number of rows in usgs_comid
# Create one df with all necessary info
usgs_geol <- merge(usgs_comid, geol_acc, by = "COMID", all = TRUE)
# Rename columns to describe land use classes
names(usgs_geol)
# For more info on each geologic class, see the metadata for this specific file in the link at the top of the script
colnames(usgs_geol) <- c("COMID", "SiteID", "BR_Gneiss", "BR_Granitic", "BR_Ultramafic",
                         "BR_Quarternary", "BR_Sedimentary", "BR_Volcanic", "BR_Water", "BR_Anorthositic",
                         "BR_Intermediate", "BR_NoData")
# Save output:
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(usgs_geol, "WUS_USGS_Gen_Geology.rds")
rm(usgs_geol, geol_acc)

# Olson Geology:
setwd("/Volumes/Blaszczak Lab/FSS/NHD Attributes/Geology") 
geol_acc <- read.csv("OLSON_ACC_CONUS.csv")
head(geol_acc)
# Subset NLCD by our sites of interest
geol_acc <- subset(geol_acc, geol_acc$COMID %in% usgs_comid$COMID) # the number of rows should be = to the number of rows in usgs_comid
# Create one df with all necessary info
usgs_geol <- merge(usgs_comid, geol_acc, by = "COMID", all = TRUE)
# Rename columns to describe land use classes
names(usgs_geol)
colnames(usgs_geol) <- c("COMID", "SiteID", "OLSON_K2O", "OLSON_CaO", "OLSON_Fe203",
                         "OLSON_MgO", "OLSON_P2O5", "OLSON_S", "OLSON_SiO2", "OLSON_CompressStrength",
                         "OLSON_HydroConductivity", "OLSON_NoData")
# Save Output:
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(usgs_geol, "WUS_USGS_Geochem_Geology.rds")
rm(usgs_geol, geol_acc)

# Water Balance/Climate ####
#
# Baseflow Index (BFI)
# accumulated upstream through the river network
setwd("/Volumes/Blaszczak Lab/FSS/NHD Attributes/Water Balance:Climate") 
bfi_acc <- read.csv("BFI_CONUS.csv")
head(bfi_acc)
# Subset NLCD by our sites of interest
bfi_acc <- subset(bfi_acc, bfi_acc$COMID %in% usgs_comid$COMID) # the number of rows should be = to the number of rows in usgs_comid
# Create one df with all necessary info
usgs_bfi <- merge(usgs_comid, bfi_acc, by = "COMID", all = TRUE)
# Rename columns to describe land use classes
names(usgs_bfi)
usgs_bfi <- select(usgs_bfi, c("COMID", "SiteID", "ACC_BFI", "ACC_NODATA"))
# For more info on each bfiogic class, see the metadata for this specific file in the link at the top of the script
colnames(usgs_bfi) <- c("COMID", "SiteID", "BFI_pct", "BFI_NoData")
# Save Output:
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(usgs_bfi, "WUS_USGS_BFI.rds")
rm(usgs_bfi, bfi_acc)

# Water Balance Model
# Wolock and McCabe (2017) averaged over the years 2000 through 2014
# accumulated upstream through the river network
setwd("/Volumes/Blaszczak Lab/FSS/NHD Attributes/Water Balance:Climate") 
waterbal_acc <- read.csv("WBM_ACC_CONUS.csv")
head(waterbal_acc)
# Subset by our sites of interest
waterbal_acc <- subset(waterbal_acc, waterbal_acc$COMID %in% usgs_comid$COMID) # the number of rows should be = to the number of rows in usgs_comid
# Create one df with all necessary info
usgs_waterbal <- merge(usgs_comid, waterbal_acc, by = "COMID", all = TRUE)
# Rename columns to describe land use classes
names(usgs_waterbal)
# For more info on each waterbalogic class, see the metadata for this specific file in the link at the top of the script
colnames(usgs_waterbal) <- c("COMID", "SiteID", "WBM_AET", "WBM_PET", "WBM_PRCP", "WBM_RUNOFF", 
                             "WBM_PRSNOW", "WBM_SM_STRG", "WBM_TAVG",
                             "WBM_NoData")
# Save Output:
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(usgs_waterbal, "WUS_USGS_Water_Balance.rds")
rm(waterbal_acc, usgs_waterbal)

# Percent Snow
setwd("/Volumes/Blaszczak Lab/FSS/NHD Attributes/Water Balance:Climate") 
prsnow_acc <- read.csv("PRSNOW_CONUS.csv")
head(prsnow_acc)
# Subset by our sites of interest
prsnow_acc <- subset(prsnow_acc, prsnow_acc$COMID %in% usgs_comid$COMID) # the number of rows should be = to the number of rows in usgs_comid
# Create one df with all necessary info
usgs_prsnow <- merge(usgs_comid, prsnow_acc, by = "COMID", all = TRUE)
# Rename columns to describe land use classes
names(usgs_prsnow)
usgs_prsnow <- select(usgs_prsnow, c("COMID", "SiteID", "ACC_PRSNOW", "ACC_NODATA"))
# For more info on each prsnowogic class, see the metadata for this specific file in the link at the top of the script
colnames(usgs_prsnow) <- c("COMID", "SiteID", "PRSNOW", "PRSNOW_NoData")
# Save Output:
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(usgs_prsnow, "WUS_USGS_Percent_Snow.rds")
rm(prsnow_acc, usgs_prsnow)




# Basin Characteristics  ####
# 
# accumulated upstream through the river network
setwd("/Volumes/Blaszczak Lab/FSS/NHD Attributes/Basin Characteristics") 
basinchar_acc <- read.csv("BASIN_CHAR_ACC_CONUS.csv")
head(basinchar_acc)
# Subset NLCD by our sites of interest
basinchar_acc <- subset(basinchar_acc, basinchar_acc$COMID %in% usgs_comid$COMID) # the number of rows should be = to the number of rows in usgs_comid
# Create one df with all necessary info
usgs_basinchar <- merge(usgs_comid, basinchar_acc, by = "COMID", all = TRUE)
# Rename columns to describe land use classes
names(usgs_basinchar)
# For more info on each basincharogic class, see the metadata for this specific file in the link at the top of the script
colnames(usgs_basinchar) <- c("COMID", "SiteID", "Basin_Area_sqkm", "Stream_Slope_pct",
                              "Basin_Slope_pct", "Elevation_Mean_m", "Elevation_Min_m", "Elevation_Max_m",
                              "Flowline_Length_km")
# Save Output:
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(usgs_basinchar, "WUS_USGS_Basin_Characteristics.rds")
rm(usgs_basinchar, basinchar_acc)

# Soil Characteristics ####
# Texture Attributes:
setwd("/Volumes/Blaszczak Lab/FSS/NHD Attributes/STATSGO Soil Attributes")
soil_acc <- read.csv("STATSGO_TEXT_ACC_CONUS.csv")
head(soil_acc)
# Subset NLCD by our sites of interest
soil_acc <- subset(soil_acc, soil_acc$COMID %in% usgs_comid$COMID) # the number of rows should be = to the number of rows in usgs_comid
# Create one df with all necessary info
usgs_soil <- merge(usgs_comid, soil_acc, by = "COMID", all = TRUE)
# Rename columns to describe land use classes
names(usgs_soil)
# For more info on each soilogic class, see the metadata for this specific file in the link at the top of the script
colnames(usgs_soil) <- c("COMID", "SiteID", "Silt_avg", "Clay_avg","Sand_avg", "Texture_NoData")
# Save Output:
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(usgs_soil, "WUS_USGS_Soil_Texture.rds")
rm(usgs_soil, soil_acc)

# Soil Salinity:
setwd("/Volumes/Blaszczak Lab/FSS/NHD Attributes/STATSGO Soil Attributes")
soil_acc <- read.csv("SALINAVE_CONUS.csv")
head(soil_acc)
# Subset NLCD by our sites of interest
soil_acc <- subset(soil_acc, soil_acc$COMID %in% usgs_comid$COMID) # the number of rows should be = to the number of rows in usgs_comid
# Create one df with all necessary info
usgs_soil <- merge(usgs_comid, soil_acc, by = "COMID", all = TRUE)
# Rename columns to describe land use classes
names(usgs_soil)
usgs_soil <- select(usgs_soil, c("COMID", "SiteID", "ACC_SALINAVE", "ACC_NODATA"))
# For more info on each soilogic class, see the metadata for this specific file in the link at the top of the script
colnames(usgs_soil) <- c("COMID", "SiteID", "Salinity", "Salinity_NoData")
# Save Output:
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(usgs_soil, "WUS_USGS_Soil_Salinity.rds")
rm(usgs_soil, soil_acc)

# Soil pH:
setwd("/Volumes/Blaszczak Lab/FSS/NHD Attributes/STATSGO Soil Attributes")
soil_acc <- read.csv("STATSGO2_pH_CONUS.csv")
head(soil_acc)
# Subset NLCD by our sites of interest
soil_acc <- subset(soil_acc, soil_acc$COMID %in% usgs_comid$COMID) # the number of rows should be = to the number of rows in usgs_comid
# Create one df with all necessary info
usgs_soil <- merge(usgs_comid, soil_acc, by = "COMID", all = TRUE)
# Rename columns to describe land use classes
names(usgs_soil)
usgs_soil <- select(usgs_soil, c("COMID", "SiteID", "ACC_pH", "ACC_NODATA"))
# For more info on each soilogic class, see the metadata for this specific file in the link at the top of the script
colnames(usgs_soil) <- c("COMID", "SiteID", "pH", "pH_NoData")
# Save output:
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(usgs_soil, "WUS_USGS_Soil_pH.rds")
rm(usgs_soil, soil_acc)

# Soil layer attributes:
setwd("/Volumes/Blaszczak Lab/FSS/NHD Attributes/STATSGO Soil Attributes")
soil_acc <- read.csv("STATSGO_LAYER_ACC_CONUS.csv")
head(soil_acc)
# Subset NLCD by our sites of interest
soil_acc <- subset(soil_acc, soil_acc$COMID %in% usgs_comid$COMID) # the number of rows should be = to the number of rows in usgs_comid
# Create one df with all necessary info
usgs_soil <- merge(usgs_comid, soil_acc, by = "COMID", all = TRUE)
# Rename columns to describe land use classes
names(usgs_soil)
usgs_soil <- select(usgs_soil, c("COMID", "SiteID", "ACC_BDAVE", "ACC_ROCKDEP", "ACC_PERMAVE", "ACC_OM"))
# For more info on each soilogic class, see the metadata for this specific file in the link at the top of the script
colnames(usgs_soil) <- c("COMID", "SiteID", "Avg_Bulk_Density", "Thickness", "Permeability", "OM_Content")
# Save output:
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(usgs_soil, "WUS_USGS_Soil_Layer_Attributes.rds")
rm(usgs_soil, soil_acc)

# CONCATENATE DATA FRAMES ####
setwd("/Volumes/Blaszczak Lab/FSS/All Data")

# Geologic Attributes:
geol_1 <- readRDS("WUS_USGS_Gen_Geology.rds")
geol_2 <- readRDS("WUS_USGS_Geochem_Geology.rds")
geol <- merge(geol_1, geol_2, all = TRUE)
rm(geol_1, geol_2)
names(geol)
saveRDS(geol, "WUS_USGS_Geologic_Attributes.rds")
write.csv(geol, "WUS_USGS_Geologic_Attributes.csv")
rm(geol)

# Water Balance/Climate Attributes
wbc_1 <- readRDS("WUS_USGS_Water_Balance.rds")
wbc_2 <- readRDS("WUS_USGS_BFI.rds")
wbc_3 <- readRDS("WUS_USGS_Percent_Snow.rds")
wbc <- merge(wbc_1, wbc_2, all = TRUE)
wbc <- merge(wbc, wbc_3, all = TRUE)
rm(wbc_1, wbc_2, wbc_3)
names(wbc)
saveRDS(wbc, "WUS_USGS_Water_Bal_Climate_Attributes.rds")
write.csv(wbc, "WUS_USGS_Water_Bal_Climate_Attributes.csv")
rm(wbc)

# Soil Characteristics:
soil_1 <- readRDS("WUS_USGS_Soil_Texture.rds")
soil_2 <- readRDS("WUS_USGS_Soil_pH.rds")
soil_3 <- readRDS("WUS_USGS_Soil_Salinity.rds")
soil_4 <- readRDS("WUS_USGS_Soil_Layer_Attributes.rds")
soil <- merge(soil_1, soil_2, all = TRUE)
soil <- merge(soil, soil_3, all = TRUE)
soil <- merge(soil, soil_4, all = TRUE)
rm(soil_1, soil_2, soil_3, soil_4)
names(soil)
saveRDS(soil, "WUS_USGS_Soil_Attributes.rds")
write.csv(soil, "WUS_USGS_Soil_Attributes.csv")
rm(soil)

# Physiographic Characteristics
basin_1 <- readRDS("WUS_USGS_Basin_Characteristics.rds")
names(basin_1)
colnames(basin_1) <- c("COMID", "SiteID", "Basin_Area", "Stream_Slope", "Basin_Slope", 
                       "Elevation_Mean", "Elevation_Min", "Elevation_Max", "Flowline_Length")
saveRDS(basin_1, "WUS_USGS_Physiographic_Attributes.rds")
write.csv(basin_1, "WUS_USGS_Physiographic_Attributes.csv")
rm(basin_1)

# Land Use Attributes
land_1 <- readRDS("WUS_USGS_NLCD16.rds")
land_2 <- readRDS("WUS_USGS_Irrigated_Ag.rds")
land <- merge(land_1, land_2, all = TRUE)
saveRDS(land, "WUS_USGS_Land_Use_Attributes.rds")
write.csv(land, "WUS_USGS_Land_Use_Attributes.csv")
rm(land_1, land_2, land)

# Hydrologic Modification Attributes
mod_1 <- readRDS("WUS_USGS_NID_1990.rds")
mod_2 <- readRDS("WUS_USGS_NID_2000.rds")
mod_3 <- readRDS("WUS_USGS_NID_2010.rds")
mod_4 <- readRDS("WUS_USGS_NID_2013.rds")
mod <- merge(mod_1, mod_2, all = TRUE)
mod <- merge(mod, mod_3, all = TRUE)
mod <- merge(mod, mod_4, all = TRUE)
rm(mod_1, mod_2, mod_3, mod_4)
names(mod)
saveRDS(mod, "WUS_USGS_Hydrologic_Modification_Attributes.rds")
write.csv(mod, "WUS_USGS_Hydrologic_Modification_Attributes.csv")
rm(mod)














# GBCO DATA: #################################################################################
## Bring in files that have SiteID's and ComID's
  # I am keeping USGS and WQP separate for now
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
usgs_comid <- readRDS("USGS_SC_ComID.rds")
wqp_comid <- readRDS("WQP_SC_ComID.rds")

# Bring in the NHD Attribute Data 
  # downloaded at https://www.sciencebase.gov/catalog/item/5669a79ee4b08895842a1d47
# NLCD 
# National Land Cover Database (2016) 
    # reach catchments accumulated upstream proportional land cover through the river network
    setwd("/Volumes/Blaszczak Lab/FSS/NLCD") 
    nlcd_acc <- read.csv("NLCD16_ACC_CONUS.csv")
    head(nlcd_acc)
# Subset NLCD by our sites of interest
    nlcd_acc <- subset(nlcd_acc, nlcd_acc$COMID %in% usgs_comid$COMID) # the number of rows should be = to the number of rows in usgs_comid
# Create one df with all necessary info
    usgs_nlcd <- merge(usgs_comid, nlcd_acc, by = "COMID", all = TRUE)
# Rename columns to describe land use classes
    names(usgs_nlcd)
    colnames(usgs_nlcd) <- c("COMID", "SiteID", "OpenWater_pct", "PerennialIceSnow_pct", 
                             "DevelopedOpenSpace_pct", "DevelopedLowIntensity_pct", 
                             "DevelopedMedIntensity_pct", "DevelopedHiIntensity_pct", "BarrenLand_pct",
                             "DeciduousForest_pct", "EvergreenForest_pct", "MixedForest_pct",
                             "ShrubScrub_pct", "GrasslandHerbaceous_pct", "PastureHay_pct",
                             "CultivatedCrops_pct", "WoodyWetlands_pct", "EmergentHerbWetlands_pct",
                             "NoData")
    
    # individual reach catchment proportional land cover
    # setwd("/Volumes/Blaszczak Lab/FSS/NLCD/NLCD16_CAT_CONUS")
    # nlcd_cat <- read.csv("NLCD16_CAT_CONUS.csv")
    # head(nlcd_cat)
    # nlcd_cat <- subset(nlcd_cat, nlcd_cat$COMID %in% usgs_comid$COMID)
## Save output
    setwd("/Volumes/Blaszczak Lab/FSS/All Data")
    saveRDS(usgs_nlcd, "USGS_SC_NLCD.rds")
    
## Follow same procedure for WQP data and for other catchment attributes
    setwd("/Volumes/Blaszczak Lab/FSS/NLCD") 
    nlcd_acc <- read.csv("NLCD16_ACC_CONUS.csv")
    nlcd_acc <- subset(nlcd_acc, nlcd_acc$COMID %in% wqp_comid$COMID) # becomes 13,166 rows
    unique(wqp_comid$COMID) # there are only 13,167 unique ComID's in WQP, so that about explains it
    # Create one df with all necessary info
    wqp_nlcd <- merge(wqp_comid, nlcd_acc, by = "COMID", all = TRUE)
    # Rename columns to describe land use classes
    names(wqp_nlcd)
    colnames(wqp_nlcd) <- c("COMID", "SiteID", "OpenWater_pct", "PerennialIceSnow_pct", 
                             "DevelopedOpenSpace_pct", "DevelopedLowIntensity_pct", 
                             "DevelopedMedIntensity_pct", "DevelopedHiIntensity_pct", "BarrenLand_pct",
                             "DeciduousForest_pct", "EvergreenForest_pct", "MixedForest_pct",
                             "ShrubScrub_pct", "GrasslandHerbaceous_pct", "PastureHay_pct",
                             "CultivatedCrops_pct", "WoodyWetlands_pct", "EmergentHerbWetlands_pct",
                             "NoData")
    setwd("/Volumes/Blaszczak Lab/FSS/All Data")
    saveRDS(wqp_nlcd, "WQP_SC_NLCD.rds")


