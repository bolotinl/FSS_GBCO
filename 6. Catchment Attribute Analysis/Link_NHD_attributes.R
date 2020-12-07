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

# NLCD ####
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
saveRDS(usgs_nlcd, "WUS_USGS_NLCD16.rds")
rm(nlcd_acc, usgs_nlcd)

# Generalized Geology ####
# Bush & Reed (2001) generalized geology types
# accumulated upstream through the river network
setwd("/Volumes/Blaszczak Lab/FSS/Geology") 
geol_acc <- read.csv("BUSHREED_ACC_CONUS.csv")
head(geol_acc)
# Subset NLCD by our sites of interest
geol_acc <- subset(geol_acc, geol_acc$COMID %in% usgs_comid$COMID) # the number of rows should be = to the number of rows in usgs_comid
# Create one df with all necessary info
usgs_geol <- merge(usgs_comid, geol_acc, by = "COMID", all = TRUE)
# Rename columns to describe land use classes
names(usgs_geol)
# For more info on each geologic class, see the metadata for this specific file in the link at the top of the script
colnames(usgs_geol) <- c("COMID", "SiteID", "Gneiss", "Granitic", "Ultramafic",
                         "Quarternary", "Sedimentary", "Volcanic", "Water", "Anorthositic",
                         "Intermediate", "NoData")

setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(usgs_geol, "WUS_USGS_Gen_Geology.rds")
rm(usgs_geol, geol_acc)


# Water Balance Model ####
# Wolock and McCabe (2017) averaged over the years 2000 through 2014
# accumulated upstream through the river network
setwd("/Volumes/Blaszczak Lab/FSS/Water Balance") 
waterbal_acc <- read.csv("WBM_ACC_CONUS.csv")
head(waterbal_acc)
# Subset by our sites of interest
waterbal_acc <- subset(waterbal_acc, waterbal_acc$COMID %in% usgs_comid$COMID) # the number of rows should be = to the number of rows in usgs_comid
# Create one df with all necessary info
usgs_waterbal <- merge(usgs_comid, waterbal_acc, by = "COMID", all = TRUE)
# Rename columns to describe land use classes
names(usgs_waterbal)
# For more info on each waterbalogic class, see the metadata for this specific file in the link at the top of the script
colnames(usgs_waterbal) <- c("COMID", "SiteID", "AET_mm_yr", "PET_mm_yr", "PRCP_mm_yr", "RUNOFF_mm_yr", 
                             "SNO_pct_prcp", "SM_STRG_mm_yr", "TAVG_degC",
                             "NoData")

setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(usgs_waterbal, "WUS_USGS_Water_Balance.rds")
rm(waterbal_acc, usgs_waterbal)


# Irrigated Agriculture ####
# 2012 MODIS Irrigated Agricultural Data (MIrAD) data
# Percent of all land that’s irrigated agriculture in 2012 accumulated upstream through the river network
setwd("/Volumes/Blaszczak Lab/FSS/Irrigated Agriculture") 
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
colnames(usgs_ag) <- c("COMID", "SiteID", "Irrigated_Ag_Land_pct", "NoData")

setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(usgs_ag, "WUS_USGS_Irrigated_Ag.rds")
rm(usgs_ag, ag_acc)


# Generalized Geology ####
# Bush & Reed (2001) generalized geology types
# accumulated upstream through the river network
setwd("/Volumes/Blaszczak Lab/FSS/BFI") 
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
colnames(usgs_bfi) <- c("COMID", "SiteID", "BFI_pct", "NoData")

setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(usgs_bfi, "WUS_USGS_BFI.rds")
rm(usgs_bfi, bfi_acc)

# GBCO DATA: #################################################################################
## Bring in files that have SiteID's and ComID's
  # I am keeping USGS and WQP separate for now
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
usgs_comid <- readRDS("USGS_SC_ComID.rds")
wqp_comid <- readRDS("WQP_SC_ComID.rds")

# Bring in the NHD Attribute Data 
  # downloaded at https://www.sciencebase.gov/catalog/item/5669a79ee4b08895842a1d47
# NLCD ####
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


