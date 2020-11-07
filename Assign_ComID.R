####################### Lauren Bolotin bolotinljb@gmail.com ##############################
# Assign NHD ComID's to USGS and Water Quality Data Portal (WQP) sites
##########################################################################################

## Bring in packages
x <- c("sf", "rgdal", "raster", "tidyverse", "nhdplusTools")
lapply(x, require, character.only = TRUE)
rm(x)

## Bring in time series data so we know what sites we need to assign ComID's to
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
dat <- readRDS("all_SC_data.rds")
## Separate USGS and WQP data because we will process them differently
USGS <- subset(dat, dat$Source == "USGS")
WQP <- subset(dat, dat$Source == "WQP")

## Bring in location data
# WQP:
WQP_meta <- readRDS("WQP_location_data_NAD83.rds")
# USGS:
setwd("/Volumes/Blaszczak Lab/FSS/NHD/USGS_Streamgages-NHD_Locations_Shape")
NHD <- st_read("USGS_Streamgages-NHD_Locations.shp")
  # Format SiteID column to match time series data and feed into our code later
NHD$SITE_NO <- paste0("USGS-", NHD$SITE_NO)
colnames(NHD)[3] <- "SiteID"
  # Subset NHD for our sites
NHD <- subset(NHD$SiteID %in% USGS$SiteID)

setwd("/Users/laurenbolotin/Desktop/Blaszczak Lab/GB CO WQ Data/USGS Data Retrieval from Phil")
USGS_metadata <- readRDS("GBCO_SC_sites.rds")



## Prepare a data frame where we will put the ComID's
USGS$SiteID <- factor(USGS$SiteID)
USGS_sites <- levels(USGS$SiteID)%>%
  as.data.frame()
USGS_sites$COMID <- "" # leave blank for now, this is what we are going to populate
colnames(USGS_sites)[1] <- "SiteID"
head(USGS_sites)

