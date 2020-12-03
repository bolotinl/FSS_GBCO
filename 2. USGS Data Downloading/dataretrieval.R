########### Lauren Bolotin - bolotinljb@gmail.com ############################################################
# Adapted from a script by Dr. Phil Savoy
# Download flow and SC data using the dataRetrieval package
##############################################################################################################

library(dataRetrieval)
library(tidyverse)
library(data.table)
library(beepr)

setwd("/Volumes/Blaszczak Lab/FSS/All Data")
disch_huc_sites <- readRDS("WUS_USGS_disch_sites.rds") # all lotic disch sites in the western US
SC_huc_sites <- readRDS("WUS_USGS_SC_sites.rds") # all lotic SC sites in the western US
both_huc_sites <- readRDS("WUS_USGS_disch_SC_sites.rds") # all lotic sites with both disch and SC in the western US

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
siteNumber <- both_huc_sites %>% pull(Site_ID) 
siteNumber <- unique(siteNumber) # 828

parameterCd <- "00060"
startDate <- ""  
endDate <- "" 
# if statCd is not specified, it defaults to 00003 (mean) this is ok
# I downloaded the data in chunks, saved the df's to files, then removed them to save space and speed things up before moving on
discharge <- readNWISdv(siteNumber[1:125], 
                        parameterCd, startDate, endDate)
discharge1 <- readNWISdv(siteNumber[126:250], 
                        parameterCd, startDate, endDate)
discharge2 <- readNWISdv(siteNumber[251:375], 
                        parameterCd, startDate, endDate)
discharge3 <- readNWISdv(siteNumber[375:500], 
                        parameterCd, startDate, endDate)
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
rm(discharge4, discharge5, discharge6)

saveRDS(discharge4, "WUS_USGS_disch4_data.rds")
saveRDS(discharge5, "WUS_USGS_disch5_data.rds")
saveRDS(discharge6, "WUS_USGS_disch6_data.rds")

#------------------------------
# Download water quality (qw) SC data (point measurements) ####
#------------------------------
# Will need to do this for 00095 and then 00094
parameterCd <- c("00095")
siteNumber <- both_huc_sites$Site_ID[which(both_huc_sites$data_type_cd == "qw")]
siteNumber <- unique(siteNumber)
# there are 53 sites with qw SC data

SCqw <- readNWISqw(siteNumber[1:205], parameterCd, 
                   startDate, endDate)
SCqw1 <- readNWISqw(siteNumber[206:413], parameterCd, 
                   startDate, endDate)
SCqw2 <- readNWISqw(siteNumber[414:600], parameterCd, 
                    startDate, endDate)
SCqw3 <- readNWISqw(siteNumber[601:785], parameterCd, 
                    startDate, endDate)

# saveRDS(SCqw, "USGS_SC_qw_data.rds") # For GBCO data
saveRDS(SCqw, "WUS_USGS_SC_qw_data.rds") # For WUS data
saveRDS(SCqw1, "WUS_USGS_SC_qw1_data.rds") # For WUS data
saveRDS(SCqw2, "WUS_USGS_SC_qw2_data.rds") # For WUS data
saveRDS(SCqw3, "WUS_USGS_SC_qw3_data.rds") # For WUS data

rm(SCqw, SCqw1, SCqw2, SCqw3)
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
SCdv3 <- readNWISdv(siteNumber[701:797], parameterCd, 
                    startDate, endDate)
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
SCuv1 <- readNWISuv(siteNumber[1:30], parameterCd, 
                   startDate, endDate) 
SCuv1.5 <- readNWISuv(siteNumber[31:50], parameterCd, 
                    startDate, endDate) 
SCuv2 <- readNWISuv(siteNumber[51:100], parameterCd, 
                    startDate, endDate)
SCuv3 <- readNWISuv(siteNumber[100:150], parameterCd, 
                    startDate, endDate)
SCuv3.25 <- readNWISuv(siteNumber[151:175], parameterCd,
                    startDate, endDate) 
SCuv3.5 <- readNWISuv(siteNumber[175:200], parameterCd,
                       startDate, endDate)
saveRDS(SCuv1, "WUS_USGS_SC_uv1_data.rds")
saveRDS(SCuv1.5, "WUS_USGS_SC_uv1.5_data.rds")
saveRDS(SCuv2, "WUS_USGS_SC_uv2_data.rds")
saveRDS(SCuv3, "WUS_USGS_SC_uv3_data.rds")
saveRDS(SCuv3.25, "WUS_USGS_SC_uv3.25_data.rds")
saveRDS(SCuv3.5, "WUS_USGS_SC_uv3.5_data.rds")

rm(SCuv1, SCuv2, SCuv3, SCuv1.5, SCuv3.25, SCuv3.5) # save some space, speed things up

SCuv4 <- readNWISuv(siteNumber[201:248], parameterCd, 
                    startDate, endDate)
SCuv5 <- readNWISuv(siteNumber[249:300], parameterCd, 
                    startDate, endDate)
SCuv6 <- readNWISuv(siteNumber[301:325], parameterCd, 
                    startDate, endDate)
SCuv6.5 <- readNWISuv(siteNumber[326:350], parameterCd, 
                      startDate, endDate)
SCuv7 <- readNWISuv(siteNumber[351:400], parameterCd, 
                    startDate, endDate)
SCuv8 <- readNWISuv(siteNumber[401:406], parameterCd, 
                    startDate, endDate)

saveRDS(SCuv4, "WUS_USGS_SC_uv4_data.rds")
saveRDS(SCuv5, "WUS_USGS_SC_uv5_data.rds")
saveRDS(SCuv6, "WUS_USGS_SC_uv6_data.rds")
saveRDS(SCuv6.5, "WUS_USGS_SC_uv6.5_data.rds")
saveRDS(SCuv7, "WUS_USGS_SC_uv7_data.rds")
saveRDS(SCuv8, "WUS_USGS_SC_uv8_data.rds")

rm(SCuv4, SCuv5, SCuv6, SCuv6.5,SCuv7, SCuv8)
#setwd("/Volumes/Blaszczak Lab/FSS/All Data") # For GBCO data
# saveRDS(SCuv1, "USGS_SC_uv1_data.rds")
# saveRDS(SCuv2, "USGS_SC_uv2_data.rds")
# saveRDS(SCuv3, "USGS_SC_uv3_data.rds")
# saveRDS(SCuv4, "USGS_SC_uv4_data.rds")

#----------------------------
# See how many sites have the parameter code 00094 for SC instead of 00095, download them
#----------------------------
both_huc_94 <- subset(both_huc_sites, both_huc_sites$parm_cd == "94") # 62, and they're all qw sites
parameterCd <- c("00094")
siteNumber <- unique(both_huc_94$Site_ID)
SCqw94 <- readNWISqw(siteNumber, parameterCd, 
                   startDate, endDate)
saveRDS(SCqw94, "WUS_USGS_SC_qw94_data.rds")
rm(SCqw94)

# Code for combining data files into one for the western US is now in compile_data_files.R


# For GBCO data:
# disch1 <- readRDS("USGS_disch_data.rds")
# saveRDS(disch1, "USGS_disch_data1.rds") # resave this with a different name so we can keep all original downloads as well as the combined file we are aobut to make
# disch2 <- readRDS("USGS_disch_data2.rds")
# disch3 <- readRDS("USGS_disch_data3.rds")
# disch4 <- readRDS("USGS_disch_data4.rds")
# disch5 <- readRDS("USGS_disch_data5.rds")
# disch6 <- readRDS("USGS_disch_data6.rds")
# disch7 <- readRDS("USGS_disch_data7.rds")
# # dataframes have different columns in some cases
# alldisch <- bind_rows(disch1, disch2)
# alldisch <- bind_rows(alldisch, disch3)
# alldisch <- bind_rows(alldisch, disch4)
# alldisch <- bind_rows(alldisch, disch5)
# alldisch <- bind_rows(alldisch, disch6)
# alldisch <- bind_rows(alldisch, disch7)
# saveRDS(alldisch, "USGS_disch_data.rds")
