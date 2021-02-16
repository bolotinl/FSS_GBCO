########### Lauren Bolotin - bolotinljb@gmail.com ############################################################
# Adapted from a script by Dr. Phil Savoy
# Download flow and SC data using the dataRetrieval package
##############################################################################################################

library(dataRetrieval)
library(tidyverse)
library(data.table)
library(beepr)

# Bring in site info for sites we want to download data for (HUC 10-18/Western US)
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
disch_huc_sites <- readRDS("USGS_disch_sites.rds") # all lotic disch sites in the western US
SC_huc_sites <- readRDS("USGS_SC_sites.rds") # all lotic SC sites in the western US
both_huc_sites <- readRDS("USGS_disch_SC_sites.rds") # all lotic sites with both disch and SC in the western US

both_huc_sites$Site_ID <- factor(both_huc_sites$Site_ID)
levels(both_huc_sites$Site_ID) # 828 WUS sites with both SC and Q
# For the Western US, ONLY use sites with BOTH discharge and SC

#parameter codes: 00060 -discharge ---- 00095 & 0094 -specific conductance
#pull data using dataretrieval package

#------------------------------
# Download daily value (dv) discharge data ####
#------------------------------
# Subset, since we already downloaded data for HUC regions 10-18 we don't want to do it again
both_huc_sites$Site_ID <- as.numeric(as.character(both_huc_sites$Site_ID))
both_huc_sites$Site_ID <- ifelse(both_huc_sites$Site_ID < 1e7,
                                 yes = paste("0", both_huc_sites$Site_ID, sep=""),
                                 no = paste(both_huc_sites$Site_ID))
siteNumber <- both_huc_sites %>% pull(Site_ID) 
siteNumber <- unique(siteNumber) # 828

parameterCd <- "00060"
startDate <- ""  
endDate <- "" 

# if statCd is not specified, it defaults to 00003 (mean) this is ok for daily data
# Download the data in chunks, save the df's to files, then remove the df's to save space and speed things up before moving on

discharge <- readNWISdv(siteNumber[1:125], 
                        parameterCd, startDate, endDate)
discharge1 <- readNWISdv(siteNumber[126:250], 
                         parameterCd, startDate, endDate)
discharge2 <- readNWISdv(siteNumber[251:375], 
                         parameterCd, startDate, endDate)
discharge3 <- readNWISdv(siteNumber[375:500], 
                         parameterCd, startDate, endDate)

beep() # notifies you with a sound that the downloads have finished running


saveRDS(discharge, "WUS_USGS_disch_data.rds")
saveRDS(discharge1, "WUS_USGS_disch1_data.rds")
saveRDS(discharge2, "WUS_USGS_disch2_data.rds")
saveRDS(discharge3, "WUS_USGS_disch3_data.rds")

rm(discharge, discharge1, discharge2, discharge3)

discharge4 <- readNWISdv(siteNumber[501:625], 
                         parameterCd, startDate, endDate)
discharge5 <- readNWISdv(siteNumber[626:750], 
                         parameterCd, startDate, endDate)
discharge6 <- readNWISdv(siteNumber[751:828], 
                         parameterCd, startDate, endDate)

beep() 

saveRDS(discharge4, "WUS_USGS_disch4_data.rds")
saveRDS(discharge5, "WUS_USGS_disch5_data.rds")
saveRDS(discharge6, "WUS_USGS_disch6_data.rds")

rm(discharge4, discharge5, discharge6)

#------------------------------
# Download daily value (dv) SC data ####
#------------------------------
# There are two parameter codes for SC, start with 00095
parameterCd <- c("00095")
siteNumber <- both_huc_sites$Site_ID[which(both_huc_sites$data_type_cd == "dv")]
siteNumber <- unique(siteNumber)
SCdv <- readNWISdv(siteNumber[1:125], parameterCd, 
                   startDate, endDate)
SCdv1 <- readNWISdv(siteNumber[126:350], parameterCd, 
                    startDate, endDate)
SCdv2 <- readNWISdv(siteNumber[351:700], parameterCd, 
                    startDate, endDate)
SCdv3 <- readNWISdv(siteNumber[701:797], parameterCd, 
                    startDate, endDate)

beep()

saveRDS(SCdv, "WUS_USGS_SC_dv_data.rds") # For WUS 00095 data
saveRDS(SCdv1, "WUS_USGS_SC_dv1_data.rds")
saveRDS(SCdv2, "WUS_USGS_SC_dv2_data.rds")
saveRDS(SCdv3, "WUS_USGS_SC_dv3_data.rds")

rm(SCdv, SCdv1, SCdv2, SCdv3)

#----------------------------
# See how many sites have the parameter code 00094 for SC instead of 00095
#----------------------------
both_huc_94 <- subset(both_huc_sites, both_huc_sites$parm_cd == "94") # 62 sites, and none of them are daily data, so skip


# Code for combining data files into one for the western US is now in compile_data_files.R

