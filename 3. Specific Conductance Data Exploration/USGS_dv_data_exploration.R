library(tidyverse)
library(lubridate)
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
# For Western US (WUS) data:
#---------------------
# Read in SC dv Data ####
#---------------------
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
  
  
# For GBCO data:
# #---------------------
# # Read in SC dv Data ####
# #---------------------
# SC <- readRDS("USGS_SC_dv_data.rds")
# colnames(SC)
# SC <- select(SC, -c("agency_cd"))
# colnames(SC) <- c("SiteID", "DateTime", "Specific.Conductance", "dqi")
# sapply(SC, class)
# SC$dqi <- as.factor(SC$dqi)
# levels(SC$dqi)
# # [1] "A"     "A [4]" "A e"   "P"     "P [4]" "P Dis" "P Dry" "P Eqp" "P Ice"
# # table(SC$dqi)
# # x   freq
# # A          365715   ~ 98% Approved for publication -- Processing and review completed.
# # A [4]      236      ??Discharge less than indicated value which is Minimum Recordable Discharge at this site
# # A e        307      Value has been edited or estimated by USGS personnel and is write protected
# # P          6832     Provisional data subject to revision.
# # P [4]      84       ??Discharge less than indicated value which is Minimum Recordable Discharge at this site
# # P Dis      36       Data-collection discontinued
# # P Dry      12       Dry
# # P Eqp      33       Equipment malfunction
# # P Ice      3        Ice affected
# # Many values for these dqi's are NA -- filter these out:
# SC <- SC[!is.na(SC$Specific.Conductance),]
# SC$dqi <- factor(SC$dqi)
# levels(SC$dqi)
# # "A"     "A [4]" "A e"   "P"     "P [4]"
# count(SC$dqi)
# # x   freq
# # 1   A        365715 ~ 98%
# # 2   A [4]    236
# # 3   A e      307
# # 4   P        6832
# # 5   P [4]    84
# 
# ## get rid of all data with a dqi other than "A" (approved for publication)
# SC <- subset(SC, dqi == "A")
# sapply(SC, class)
# SC$SiteID <- as.factor(SC$SiteID)
# levels(SC$SiteID)
# # 132 dv sites
# # save the data quality filtered data
# saveRDS(SC, "USGS_SC_dv_dqi.rds")

