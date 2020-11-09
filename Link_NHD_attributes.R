#################### Lauren Bolotin - bolotinljb@gmail.com ##############################################################
## Link project data with land cover, geology, etc. via NHD ComID (Wieczorek & Schwarz 2019)
#########################################################################################################################
# NOTE: there are still some WQP sites without a ComID (13 of them)
library(tidyverse)
## Bring in files that have SiteID's and ComID's
  # I am keeping USGS and WQP separate for now
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
usgs_comid <- readRDS("USGS_SC_ComID.rds")
wqp_comid <- readRDS("WQP_SC_ComID.rds")

# Bring in the NHD Attribute Data 
  # downloaded at https://www.sciencebase.gov/catalog/item/5669a79ee4b08895842a1d47
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


