library(tidyverse)
library(lubridate)

# For Western US (WUS) data:
#---------------------
# Read in SC dv Data ####
#---------------------
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
SC <- readRDS("WUS_all_USGS_SC_dv_data.rds")

## Format dataframe:
  # Get rid of unnecessary columns
  SC <- select(SC, -c("agency_cd"))
  # Rename columns
  colnames(SC) <- c("SiteID", "Date", "Specific.Conductance", "dqi")
  # Convert columns to appropriate classes
  sapply(SC, class)
  SC$SiteID <- as.factor(as.character(SC$SiteID))
  SC$Date <- ymd(SC$Date)
  SC$dqi <- as.factor(as.character(SC$dqi))
  SC$Specific.Conductance <- as.numeric(as.character(SC$Specific.Conductance))

## Get some info about the data:
# Confirm that all dqi == A (approved for publication)
levels(SC$dqi)
# Count number of sites with eligible data for us to include in analysis
levels(SC$SiteID) # 655 sites

## Save the formatted data:
saveRDS(SC, "WUS_USGS_SC_dv_dqi.rds")
write.csv(SC, "WUS_USGS_SC_dv_dqi.csv")
  
  


