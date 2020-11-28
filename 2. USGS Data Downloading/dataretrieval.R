########### Lauren Bolotin - bolotinljb@gmail.com ############################################################
# Adapted from a script by Dr. Phil Savoy
# Download flow and SC data using the dataRetrieval package
##############################################################################################################

library(dataRetrieval)
library(tidyverse)
library(data.table)
library(beepr)

setwd("/Volumes/Blaszczak Lab/FSS/All Data")
disch_huc_sites <- readRDS("USGS_disch_sites.rds") # all lotic disch sites in the western US
SC_huc_sites <- readRDS("USGS_SC_sites.rds") # all lotic SC sites in the western US
both_huc_sites <- readRDS("USGS_disch_SC_sites.rds") # all lotic sites with both disch and SC in the western US

setdiff(SC_huc_sites$Site_ID, disch_huc_sites$Site_ID) # 252 sites with SC and not Q. Ignore these for now. 
levels(both_huc_sites$Site_ID) # 828 sites with both SC and Q
# For now, for the entire Western US, lets ONLY use sites with BOTH discharge and SC

#parameter codes: 00060 -discharge ---- 00095 & 0094 -specific conductance
#pull data using dataretrieval package

#------------------------------
# Download daily value (dv) discharge data ####
#------------------------------
both_huc_sites$Site_ID <- as.numeric(as.character(both_huc_sites$Site_ID))
both_huc_sites$Site_ID <- ifelse(both_huc_sites$Site_ID < 1e7,
                                  yes = paste("0", both_huc_sites$Site_ID, sep=""),
                                  no = paste(both_huc_sites$Site_ID))
# both_huc_sites$Site_ID <- as.character(both_huc_sites$Site_ID)
siteNumber <- both_huc_sites %>% pull(Site_ID) 
siteNumber <- unique(siteNumber) # 828

parameterCd <- "00060"
startDate <- ""  
endDate <- "" 
# if statCd is not specified, it defaults to 00003 (mean) this is ok
# I downloaded the data in chunks
discharge <- readNWISdv(siteNumber[1:125], 
                        parameterCd, startDate, endDate)
discharge1 <- readNWISdv(siteNumber[126:250], 
                        parameterCd, startDate, endDate)
discharge2 <- readNWISdv(siteNumber[251:375], 
                        parameterCd, startDate, endDate)
discharge3 <- readNWISdv(siteNumber[375:500], 
                        parameterCd, startDate, endDate)
rm(discharge, discharge1, discharge2, discharge3)

discharge4 <- readNWISdv(siteNumber[501:625], 
                         parameterCd, startDate, endDate)
discharge5 <- readNWISdv(siteNumber[626:750], 
                         parameterCd, startDate, endDate)
discharge6 <- readNWISdv(siteNumber[751:828], 
                         parameterCd, startDate, endDate)
rm(discharge4, discharge5, discharge6)

beep() # Notifies us when the download is done with a sound

# Save to file ## Need to redownload GBCO to original filenames. Delete this comment once this has been done. 
saveRDS(discharge, "WUS_USGS_disch_data.rds")
saveRDS(discharge1, "WUS_USGS_disch1_data.rds")
saveRDS(discharge2, "WUS_USGS_disch2_data.rds")
saveRDS(discharge3, "WUS_USGS_disch3_data.rds")
saveRDS(discharge4, "WUS_USGS_disch4_data.rds")
saveRDS(discharge5, "WUS_USGS_disch5_data.rds")
saveRDS(discharge6, "WUS_USGS_disch6_data.rds")

#------------------------------
# Download water quality (qw) SC data (point measurements) ####
#------------------------------
# Will need to do for 00095 and then 00094
parameterCd <- c("00095")
# both_huc_sites$Site_ID <- ifelse(both_huc_sites$Site_ID < 1e7,
#                                yes = paste("0", both_huc_sites$Site_ID, sep=""),
#                                no = both_huc_sites$Site_ID)

siteNumber <- both_huc_sites$Site_ID[which(both_huc_sites$data_type_cd == "qw")]
siteNumber <- unique(siteNumber)
# there are 53 sites with qw SC data
SCqw <- readNWISqw(siteNumber[1:205], parameterCd, 
                   startDate, endDate)
SCqw1 <- readNWISqw(siteNumber[206:413], parameterCd, 
                   startDate, endDate)
# saveRDS(SCqw, "USGS_SC_qw_data.rds") # For GBCO data
saveRDS(SCqw, "WUS_USGS_SC_qw_data.rds") # For WUS data
saveRDS(SCqw1, "WUS_USGS_SC_qw1_data.rds") # For WUS data
rm(SCqw, SCqw1)
#------------------------------
# Download daily value (dv) SC data ####
#------------------------------
siteNumber <- both_huc_sites$Site_ID[which(both_huc_sites$data_type_cd == "dv")]
siteNumber <- unique(siteNumber)
SCdv <- readNWISdv(siteNumber[1:125], parameterCd, 
                   startDate, endDate)
SCdv1 <- readNWISdv(siteNumber[126:350], parameterCd, 
                    startDate, endDate)
SCdv2 <- readNWISdv(siteNumber[351:700], parameterCd, 
                    startDate, endDate)
SCdv3 <- readNWISdv(siteNumber[701:793], parameterCd, 
                    startDate, endDate)
beep(10)
# saveRDS(SCdv, "USGS_SC_dv_data.rds") # For GBCO data

saveRDS(SCdv, "WUS_USGS_SC_dv_data.rds") # For WUS 00095 data
saveRDS(SCdv1, "WUS_USGS_SC_dv1_data.rds")
saveRDS(SCdv2, "WUS_USGS_SC_dv2_data.rds")
saveRDS(SCdv3, "WUS_USGS_SC_dv3_data.rds")

rm(SCdv, SCdv1, SCdv2, SCdv3)
#------------------------------
# Download unit value (uv) SC data ####
#------------------------------
siteNumber <- both_huc_sites$Site_ID[which(SC_huc_sites$data_type_cd == "uv")]
siteNumber <- unique(siteNumber)
SCuv1 <- readNWISuv(siteNumber[1:75], parameterCd, 
                   startDate, endDate) 
SCuv2 <- readNWISuv(siteNumber[76:125], parameterCd, 
                    startDate, endDate)
SCuv3 <- readNWISuv(siteNumber[126:200], parameterCd, 
                    startDate, endDate)
SCuv4 <- readNWISuv(siteNumber[201:248], parameterCd, 
                    startDate, endDate)
beep()
#setwd("/Volumes/Blaszczak Lab/FSS/All Data")
# saveRDS(SCuv1, "USGS_SC_uv1_data.rds")
# saveRDS(SCuv2, "USGS_SC_uv2_data.rds")
# saveRDS(SCuv3, "USGS_SC_uv3_data.rds")
# saveRDS(SCuv4, "USGS_SC_uv4_data.rds")


#----------------------------
# Combine data files that were downloaded in chunks #####
#----------------------------
# clear environment after each one to save space
# SC_uv
allSCuv <- list(SCuv1, SCuv2, SCuv3, SCuv4)
allSCuv <- rbindlist(allSCuv)
saveRDS(allSCuv, "USGS_SC_uv_data.rds")
rm(allSCuv, SCuv, SCuv1, SCuv2, SCuv3, SCuv4)
# disch
disch1 <- readRDS("USGS_disch_data.rds")
saveRDS(disch1, "USGS_disch_data1.rds") # resave this with a different name so we can keep all original downloads as well as the combined file we are aobut to make
disch2 <- readRDS("USGS_disch_data2.rds")
disch3 <- readRDS("USGS_disch_data3.rds")
disch4 <- readRDS("USGS_disch_data4.rds")
disch5 <- readRDS("USGS_disch_data5.rds")
disch6 <- readRDS("USGS_disch_data6.rds")
disch7 <- readRDS("USGS_disch_data7.rds")
# dataframes have different columns in some cases
alldisch <- bind_rows(disch1, disch2)
alldisch <- bind_rows(alldisch, disch3)
alldisch <- bind_rows(alldisch, disch4)
alldisch <- bind_rows(alldisch, disch5)
alldisch <- bind_rows(alldisch, disch6)
alldisch <- bind_rows(alldisch, disch7)
saveRDS(alldisch, "USGS_disch_data.rds")
