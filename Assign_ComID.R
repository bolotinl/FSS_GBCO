####################### Lauren Bolotin bolotinljb@gmail.com ##############################
# Assign NHD ComID's to USGS and Water Quality Data Portal (WQP) sites
##########################################################################################
## Bring in packages
x <- c("sf", "rgdal", "raster", "tidyverse", "nhdplusTools", "beepr")
lapply(x, require, character.only = TRUE)
rm(x)

## Bring in time series data from the project so we know what sites we need to assign ComID's to
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
dat <- readRDS("all_SC_data.rds")
## Separate USGS and WQP data because we will process them differently
USGS <- subset(dat, dat$Source == "USGS")
WQP <- subset(dat, dat$Source == "WQP")

## BRING IN LOCATION DATA  ############################################################################################
# WQP:
WQP_meta <- readRDS("WQP_location_data_NAD83.rds")
  # Subset WQP metadata for our sites
WQP_meta <- subset(WQP_meta, WQP_meta$SiteID %in% WQP$SiteID)
# USGS:
setwd("/Volumes/Blaszczak Lab/FSS/NHD/USGS_Streamgages-NHD_Locations_Shape")
NHD <- st_read("USGS_Streamgages-NHD_Locations.shp")
  # Format SiteID column to match time series data and feed into our code later
NHD$SITE_NO <- paste0("USGS-", NHD$SITE_NO)
colnames(NHD)[3] <- "SiteID"
  # Subset NHD for our sites
NHD <- subset(NHD, NHD$SiteID %in% USGS$SiteID)
head(NHD)
NHD <- select(NHD, c("SiteID", "LON_NHD", "LAT_NHD", "HUC", "STATE_CD", "DA_SQ_MILE"))
  # See what USGS sites from our data were not in NHD
remainder <- setdiff(USGS$SiteID, NHD$SiteID)
  # Bring in other USGS metadata for these sites
setwd("/Users/laurenbolotin/Desktop/Blaszczak Lab/GB CO WQ Data/USGS Data Retrieval from Phil")
USGS_meta <- readRDS("GBCO_SC_sites.rds") # This file is a subset from the specific conductance site query (only sites in the GB and CO River basin)
USGS_meta$Site_ID <- ifelse(USGS_meta$Site_ID < 10000000, paste0("0", USGS_meta$Site_ID), paste0(USGS_meta$Site_ID))
USGS_meta$Site_ID <- paste0("USGS-", USGS_meta$Site_ID)
colnames(USGS_meta)[1] <- "SiteID"
USGS_meta <- subset(USGS_meta, USGS_meta$SiteID %in% remainder)
USGS_meta_list <- unique(USGS_meta$SiteID)
head(USGS_meta)
USGS_meta <- select(USGS_meta, c("SiteID", "Lon", "Lat", "huc_cd"))
USGS_meta <- unique(USGS_meta)

### USGS SITES ############################################################################################################
## Prepare a data frame where we will put the ComID's
USGS$SiteID <- factor(USGS$SiteID)
USGS_sites <- levels(USGS$SiteID)%>%
  as.data.frame()
USGS_sites$COMID <- "" # leave blank for now, this is what we are going to populate
colnames(USGS_sites)[1] <- "SiteID"
head(USGS_sites)
## Create function to find ComID for multiple sites...
  # By NHD Lat/Long ####
  findCOMID_NHD_coords <- function(x){ # x = USGS SiteID
  tryCatch((point <- st_sfc(st_point(c((NHD$LON_NHD[which(NHD$SiteID == x)]), (NHD$LAT_NHD[which(NHD$SiteID == x)]))), crs = 4269)), error = function(e) NULL)
  tryCatch((USGS_sites$COMID[which(USGS_sites$SiteID == x)] <<- discover_nhdplus_id(point)), error = function(e) NULL)
}

  findCOMID_NHD_coords('USGS-09041400') # test for one site
  lapply(USGS_sites$SiteID, findCOMID_NHD_coords)
  # If you look at the output dataframe, only the 15 USGS sites that were not in NHD were not assigned a ComID, so we will do those next using a different approach:
  View(USGS_sites)

  # By USGS SiteID ####
  findCOMID_USGS_ID <- function(x){
  tryCatch((nldi_nwis <- list(featureSource = "nwissite", featureID = x)), error = function(e) NULL)
  tryCatch((USGS_sites$COMID[which(USGS_sites$SiteID == x )] <<- discover_nhdplus_id(nldi_feature = nldi_nwis)), error = function(e) NULL)
  }
  lapply(USGS_meta$SiteID, findCOMID_USGS_ID)
  # If you look at the output dataframe, there are still 3 sites that were not indexed
  View(USGS_sites)

  # By USGS Lat/Long ####
  findCOMID_USGS_coords <- function(x){ # x = USGS SiteID
  tryCatch((point <- st_sfc(st_point(c((USGS_meta$Lon[which(USGS_meta$SiteID == x)]), (USGS_meta$Lat[which(USGS_meta$SiteID == x)]))), crs = 4269)), error = function(e) NULL)
  tryCatch((USGS_sites$COMID[which(USGS_sites$SiteID == x)] <<- discover_nhdplus_id(point)), error = function(e) NULL)
  }
  lapply(USGS_meta$SiteID, findCOMID_USGS_coords)
  # All USGS sites have now been assigned a ComID:
  View(USGS_sites)

  ## Remove stuff we no longer need 
  rm(USGS_meta, USGS, remainder, USGS_meta_list, findCOMID_NHD_coords, findCOMID_USGS_coords, findCOMID_USGS_ID, NHD)
# NOTE: USGS_sites is now a dataframe of all the USGS SiteID's and their ComIDs. We will now find ComID's for all WQP
  # sites and then join these two dataframes for a dataframe with all SiteID's and their ComID's
### WQP SITES ############################################################################################################
  head(WQP_meta)
  WQP_meta <- select(WQP_meta, c("SiteID", "Lon_NAD83", "Lat_NAD83", "HUCEightDigitCode", "WatershedArea_sqkm", "State"))
## Prepare a df where we will put the ComID's
  WQP_sites <- WQP_meta$SiteID
  WQP_sites <- as.data.frame(WQP_sites)
  WQP_sites$COMID <- ""
  colnames(WQP_sites)[1] <- "SiteID"
  
  # By WQP Lat/Long ####
  findCOMID_WQP_coords <- function(x){ # x = USGS SiteID
    tryCatch(point <- st_sfc(st_point(c((WQP_meta$Lon_NAD83[which(WQP_meta$SiteID == x)]), 
                                        (WQP_meta$Lat_NAD83[which(WQP_meta$SiteID == x)]))), crs = 4269), error = function(e) NULL)
    tryCatch((WQP_sites$COMID[which(WQP_sites$SiteID == x)] <<- discover_nhdplus_id(point)),
             error = function(e) NULL)
  }
  lapply(WQP_sites$SiteID, findCOMID_WQP_coords) # < 2 hrs
  
beep() # makes a noise when the function is done running (since it takes over an hour)

# NOTE: in the end, we probably could have kept the WQP and USGS data combined since we were able to use lat/long for all
  # WQP sites and either lat/long or USGS Site ID for all USGS sites. The main reason you would potentially process them
  # separately is if you could not assign a ComID for a WQP site using lat/long so you needed to use the WQP SiteID. 
  # To do that, you would create a function similar to the one above that uses the USGS SiteID, but you would input the 
  # WQP SiteID instead and you would change featureSource to = "WQP" and the featureID would = the WQP SiteID,
  # as shown below in a generalized example.

# nldi_feature <- list(featureSource = "WQP", featureID = WQP$SiteID)
# discover_nldi_navigation(nldi_feature)

### SAVE/COMBINE OUPUTS ###############################################################################################
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(WQP_sites, "WQP_SC_ComID.rds")
saveRDS(USGS_sites, "USGS_SC_ComID.rds")

# Combine all outputs and save as a different file
# new_df <- rbind(df1, df2, deparse.level = 1)
# saveRDS(new_df, "output_file.rds")
  