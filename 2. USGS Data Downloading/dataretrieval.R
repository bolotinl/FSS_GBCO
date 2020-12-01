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

#----------------------------
# Combine data files that were downloaded in chunks #####
#----------------------------
# Clear the environment out a bit:
rm(both_huc_94, disch_huc_sites, SC_huc_sites, endDate, parameterCd, siteNumber, startDate)

# SCdv ####################################################
#---------
SCdv <- readRDS("WUS_USGS_SC_dv_data.rds") # 5 columns, normal
# filter for dqi == A (96% of data)
SCdv <- filter(SCdv, SCdv$X_00095_00003_cd == "A")
#---------
SCdv1 <- readRDS("WUS_USGS_SC_dv1_data.rds") # 7 columns, 483940 rows
names(SCdv1)
# split into two dfs, one that has data under the odd columns, and one that doesn't
SCdv1_sub <- filter(SCdv1, !is.na(SCdv1$X_Discontinued.June.2014_00095_00003))
SCdv1 <- filter(SCdv1, is.na(SCdv1$X_Discontinued.June.2014_00095_00003))
# get rid of columns that are entirely NA in each df
SCdv1 <- Filter(function(x)!all(is.na(x)), SCdv1)
SCdv1_sub <- Filter(function(x)!all(is.na(x)), SCdv1_sub)
# fix column names for the odd df to be the same as the other df
names(SCdv1)
names(SCdv1_sub)
colnames(SCdv1_sub) <- colnames(SCdv1)
# bind the two df's together, should be the same length as the original SCdv1 df
SCdv1 <- rbind(SCdv1, SCdv1_sub)
rm(SCdv1_sub)
# filter for dqi == A (98% of data)
SCdv1 <- filter(SCdv1, SCdv1$X_00095_00003_cd == "A")
#---------
SCdv2 <- readRDS("WUS_USGS_SC_dv2_data.rds") # 27 columns, 842520 rows
# split into multiple dfs, ones that have data under the odd columns, and one that doesn't
SCdv2_sub1 <- filter(SCdv2, !is.na(SCdv2$X_Discontinued.June.2014_00095_00003))
SCdv2_sub2 <- filter(SCdv2, !is.na(SCdv2$X_.Discontinued.Jan..10..2008._00095_00003))
SCdv2_sub3 <- filter(SCdv2, !is.na(SCdv2$X_Discontinued.May.19..2009...Discontinued.May.19..2009._00095_00003))
SCdv2_sub4 <- filter(SCdv2, !is.na(SCdv2$X_.EXO2._00095_00003))
SCdv2_sub5 <- filter(SCdv2, !is.na(SCdv2$X_Discontinued.Mar..11..2015_00095_00003))
SCdv2_sub6 <- filter(SCdv2, !is.na(SCdv2$X_Discontinued.Nov..13..2015_00095_00003))
SCdv2_sub7 <- filter(SCdv2, !is.na(SCdv2$X_Discontinued.Nov..12..2015_00095_00003))
SCdv2_sub8 <- filter(SCdv2, !is.na(SCdv2$X_Discontinued.Oct..22..2015...Discontinued.Oct..22..2015._00095_00003))
SCdv2_sub9 <- filter(SCdv2, !is.na(SCdv2$X_Upstream.of.Barrier_00095_00003))
SCdv2_sub10 <- filter(SCdv2, !is.na(SCdv2$X_Downstream.of.Barrier_00095_00003))
SCdv2_sub11 <- filter(SCdv2, !is.na(SCdv2$X_..2.._00095_00003))
SCdv2 <- filter(SCdv2, !is.na(SCdv2$X_00095_00003))
# get rid of columns that are entirely NA in each df
SCdv2 <- Filter(function(x)!all(is.na(x)), SCdv2) ## 7 columns ?!?!? ###
SCdv2_sub1 <- Filter(function(x)!all(is.na(x)), SCdv2_sub1)
SCdv2_sub2 <- Filter(function(x)!all(is.na(x)), SCdv2_sub2)
SCdv2_sub3 <- Filter(function(x)!all(is.na(x)), SCdv2_sub3)
SCdv2_sub4 <- Filter(function(x)!all(is.na(x)), SCdv2_sub4)
SCdv2_sub5 <- Filter(function(x)!all(is.na(x)), SCdv2_sub5) ## 7 columns ?!?!? ###, has some in common with SCdv2
SCdv2_sub6 <- Filter(function(x)!all(is.na(x)), SCdv2_sub6)
SCdv2_sub7 <- Filter(function(x)!all(is.na(x)), SCdv2_sub7)
SCdv2_sub8 <- Filter(function(x)!all(is.na(x)), SCdv2_sub8)
SCdv2_sub9 <- Filter(function(x)!all(is.na(x)), SCdv2_sub9) ## 7 columns ?!?!? ###
SCdv2_sub10 <- Filter(function(x)!all(is.na(x)), SCdv2_sub10) ## 7 columns ?!?!? ###, sub 9 and 10 have some in common
SCdv2_sub11 <- Filter(function(x)!all(is.na(x)), SCdv2_sub11)
# fix column names for the odd dfs to be the same as a normal df (for the ones with 5 columns)
colnames(SCdv2_sub1) <- colnames(SCdv1)
colnames(SCdv2_sub2) <- colnames(SCdv1)
colnames(SCdv2_sub3) <- colnames(SCdv1)
colnames(SCdv2_sub4) <- colnames(SCdv1)
colnames(SCdv2_sub6) <- colnames(SCdv1)
colnames(SCdv2_sub7) <- colnames(SCdv1)
colnames(SCdv2_sub8) <- colnames(SCdv1)
colnames(SCdv2_sub11) <- colnames(SCdv1)
# combine the ones that now have 5 columns and proper column names, then delete them to clean up environment
SCdv2_sub <- list(SCdv2_sub1, SCdv2_sub2, SCdv2_sub3, SCdv2_sub4, SCdv2_sub6, SCdv2_sub7, SCdv2_sub8, SCdv2_sub11)
SCdv2_sub <- rbindlist(SCdv2_sub)
rm(SCdv2_sub1, SCdv2_sub2, SCdv2_sub3, SCdv2_sub4, SCdv2_sub6, SCdv2_sub7, SCdv2_sub8, SCdv2_sub11)
# start solving problems
  # starting with SCdv2: I think they had two sensors installed for a while before decommissioning one
  names(SCdv2)
  # where there is data for the 'normal' columns only, use that data. 
  # where there isn't data for the normal columns, use data from the odd columns.
  # where there is data for both the normal and odd columns, average between the two.
    # but...we need to take data quality indicators into account.
    # filter df's for data quality indicators FIRST
    SCdv2$X_00095_00003[SCdv2$X_00095_00003_cd != 'A'] <- NA
    SCdv2$X_00095_00003_cd[is.na(SCdv2$X_00095_00003)] <- NA
    SCdv2$X_Discontinued.Mar..11..2015_00095_00003[SCdv2$X_Discontinued.Mar..11..2015_00095_00003_cd != 'A'] <- NA
    SCdv2$X_Discontinued.Mar..11..2015_00095_00003_cd[is.na(SCdv2$X_Discontinued.Mar..11..2015_00095_00003)] <- NA
  SCdv2_adj <- SCdv2
  SCdv2_adj <- SCdv2_adj %>% # creates column that has the average, or NA if one value was NA
    mutate(avg_SC = ((SCdv2_adj$X_00095_00003+SCdv2_adj$X_Discontinued.Mar..11..2015_00095_00003)/2))
  SCdv2_adj$avg_SC <- ifelse(is.na(SCdv2_adj$avg_SC) & is.na(SCdv2_adj$X_00095_00003 & !is.na(SCdv2_adj$X_Discontinued.Mar..11..2015_00095_00003)), paste(SCdv2_adj$X_Discontinued.Mar..11..2015_00095_00003), SCdv2_adj$avg_SC) 
  SCdv2_adj$avg_SC <- ifelse(is.na(SCdv2_adj$avg_SC) & !is.na(SCdv2_adj$X_00095_00003 & is.na(SCdv2_adj$X_Discontinued.Mar..11..2015_00095_00003)), paste(SCdv2_adj$X_00095_00003), SCdv2_adj$avg_SC)
  # now overwrite the first column with this new column's data
  SCdv2_adj$X_00095_00003 <- SCdv2_adj$avg_SC
  SCdv2_adj <- select(SCdv2_adj, -c("avg_SC", "X_Discontinued.Mar..11..2015_00095_00003", "X_Discontinued.Mar..11..2015_00095_00003_cd"))
  names(SCdv2_adj)
  SCdv2_adj <- SCdv2_adj[complete.cases(SCdv2_adj),]
  SCdv2 <- SCdv2_adj
  rm(SCdv2_adj)
  
  # now on to SCdv2_sub5
  SCdv2_sub5$X_00095_00003[SCdv2_sub5$X_00095_00003_cd != 'A'] <- NA
  SCdv2_sub5$X_00095_00003_cd[is.na(SCdv2_sub5$X_00095_00003)] <- NA
  SCdv2_sub5$X_Discontinued.Mar..11..2015_00095_00003[SCdv2_sub5$X_Discontinued.Mar..11..2015_00095_00003_cd != 'A'] <- NA
  SCdv2_sub5$X_Discontinued.Mar..11..2015_00095_00003_cd[is.na(SCdv2_sub5$X_Discontinued.Mar..11..2015_00095_00003)] <- NA
  SCdv2_sub5_adj <- SCdv2_sub5
  SCdv2_sub5_adj <- SCdv2_sub5_adj %>% # creates column that has the average, or NA if one value was NA
    mutate(avg_SC = ((SCdv2_sub5_adj$X_00095_00003+SCdv2_sub5_adj$X_Discontinued.Mar..11..2015_00095_00003)/2))
  SCdv2_sub5_adj$avg_SC <- ifelse(is.na(SCdv2_sub5_adj$avg_SC) & is.na(SCdv2_sub5_adj$X_00095_00003 & !is.na(SCdv2_sub5_adj$X_Discontinued.Mar..11..2015_00095_00003)), paste(SCdv2_sub5_adj$X_Discontinued.Mar..11..2015_00095_00003), SCdv2_sub5_adj$avg_SC) 
  SCdv2_sub5_adj$avg_SC <- ifelse(is.na(SCdv2_sub5_adj$avg_SC) & !is.na(SCdv2_sub5_adj$X_00095_00003 & is.na(SCdv2_sub5_adj$X_Discontinued.Mar..11..2015_00095_00003)), paste(SCdv2_sub5_adj$X_00095_00003), SCdv2_sub5_adj$avg_SC)
  # now overwrite the first column with this new column's data
  SCdv2_sub5_adj$X_00095_00003 <- SCdv2_sub5_adj$avg_SC
  SCdv2_sub5_adj <- select(SCdv2_sub5_adj, -c("avg_SC", "X_Discontinued.Mar..11..2015_00095_00003", "X_Discontinued.Mar..11..2015_00095_00003_cd"))
  SCdv2_sub5_adj$X_00095_00003_cd <- "A"
  names(SCdv2_sub5_adj)
  SCdv2_sub5_adj <- SCdv2_sub5_adj[complete.cases(SCdv2_sub5_adj),]
  SCdv2_sub5 <- SCdv2_sub5_adj
  rm(SCdv2_sub5_adj)
  
  # now on to SCdv2_sub9
  class(SCdv2_sub9$site_no)
  SCdv2_sub9$site_no <- as.factor(as.character(SCdv2_sub9$site_no))
  levels(SCdv2_sub9$site_no)
  # there are some really big discrepancies between what looks like two different sensors at this site
  # since I can't explain the situation here, I am going to remove data from this site
  rm(SCdv2_sub9)
  
  # now on to SCdv2_sub10 (likely similar to SCdv2_sub9)
  class(SCdv2_sub10$site_no)
  SCdv2_sub10$site_no <- as.factor(as.character(SCdv2_sub10$site_no))
  levels(SCdv2_sub10$site_no)
  # it's the same site as the one above, so remove
  rm(SCdv2_sub10)
  
  # the last thing to do is rbind SCdv2 and SCdv2_sub5 to SCdv2_sub
  SCdv2_sub <- rbind(SCdv2_sub, SCdv2, SCdv2_sub5)
  SCdv2 <- SCdv2_sub
  rm(SCdv2_sub, SCdv2_sub5)
  # filter for dqi == A (99% of data)
  SCdv2 <- filter(SCdv2, SCdv2$X_00095_00003_cd == "A")
#--------
SCdv3 <- readRDS("WUS_USGS_SC_dv3_data.rds") # 11 columns, 146012 rows
SCdv3 <- unique(SCdv3)
names(SCdv3)
# split into two dfs, one that has data under the odd columns, and one that doesn't
SCdv3_sub1 <- filter(SCdv3, !is.na(SCdv3$X_.original._00095_00003))
SCdv3_sub2 <- filter(SCdv3, !is.na(SCdv3$X_from.DCP_00095_00003))
SCdv3_sub3 <- filter(SCdv3, !is.na(SCdv3$X_..2.._00095_00003))
SCdv3 <- filter(SCdv3, !is.na(SCdv3$X_00095_00003))
# get rid of columns that are entirely NA in each df
SCdv3 <- Filter(function(x)!all(is.na(x)), SCdv3)
SCdv3_sub1 <- Filter(function(x)!all(is.na(x)), SCdv3_sub1)
SCdv3_sub2 <- Filter(function(x)!all(is.na(x)), SCdv3_sub2)
SCdv3_sub3 <- Filter(function(x)!all(is.na(x)), SCdv3_sub3)
# fix column names for the odd df's to be the same as the other df
names(SCdv3)
colnames(SCdv3_sub1) <- colnames(SCdv3)
colnames(SCdv3_sub2) <- colnames(SCdv3)
colnames(SCdv3_sub3) <- colnames(SCdv3)
# bind the two df's together, should be the same length as the original SCdv1 df
SCdv3_sub <- list(SCdv3, SCdv3_sub1, SCdv3_sub2, SCdv3_sub3)
SCdv3_sub <- rbindlist(SCdv3_sub)
SCdv3 <- SCdv3_sub
rm(SCdv3_sub, SCdv3_sub1, SCdv3_sub2, SCdv3_sub3)
# filter for dqi == A (97% of data)
SCdv3 <- filter(SCdv3, SCdv3$X_00095_00003_cd == "A")
#--------
# Combine all SCdv together
#--------
allSCdv <- list(SCdv, SCdv1, SCdv2, SCdv3)
allSCdv <- rbindlist(allSCdv)

#--------
# Save data:
saveRDS(allSCdv, "WUS_all_USGS_SC_dv_data.rds")
rm(allSCdv, SCdv, SCdv1, SCdv2, SCdv3)
# For GBCO data:
# SC_uv ####
# allSCuv <- list(SCuv1, SCuv2, SCuv3, SCuv4)
# allSCuv <- rbindlist(allSCuv)
# saveRDS(allSCuv, "USGS_SC_uv_data.rds")
# rm(allSCuv, SCuv, SCuv1, SCuv2, SCuv3, SCuv4)

# DISCHdv #################################################
#--------
disch1 <- readRDS("WUS_USGS_disch1_data.rds")
names(disch1) # 5 columns, normal
# filter for dqi == A (96% of data)
disch1 <- filter(disch1, disch1$X_00060_00003_cd == "A")

#--------
disch2 <- readRDS("WUS_USGS_disch2_data.rds")
names(disch2)
# filter for dqi == A (94% of data)
disch2 <- filter(disch2, disch2$X_00060_00003_cd == "A")
#-------
disch3 <- readRDS("WUS_USGS_disch3_data.rds") # 7 columns, 1959108 rows
names(disch3)
# split into two dfs, one that has data under the odd columns, and one that doesn't
disch3_sub1 <- filter(disch3, !is.na(disch3$X_below.hatchery...plus.flow.from.hatchery._00060_00003))

class(disch3_sub1$site_no)
disch3_sub1$site_no <- as.factor(as.character(disch3_sub1$site_no))
levels(disch3_sub1$site_no)
# just one site, remove it
disch3 <- filter(disch3, disch3$site_no != "06903900")
rm(disch3_sub1)
# get rid of columns that are entirely NA in each df
disch3 <- Filter(function(x)!all(is.na(x)), disch3)
# filter for dqi == A (94% of data)
disch3 <- filter(disch3, disch3$X_00060_00003_cd == "A")

#--------
disch4 <- readRDS("WUS_USGS_disch4_data.rds") # 5 columns, 2190644 rows
names(disch4)
# filter for dqi == A (94% of data)
disch4 <- filter(disch4, disch4$X_00060_00003_cd == "A")

#--------
disch5b <- readRDS("WUS_USGS_disch5_data.rds") # 5 columns, 2316378 rows
names(disch5)
# filter for dqi == A (97% of data)
disch5 <- filter(disch5, disch5$X_00060_00003_cd == "A")

#-------
disch6 <- readRDS("WUS_USGS_disch6_data.rds") # 5 columns, 1048447 rows
names(disch6)
# filter for dqi == A (93% of data)
disch6 <- filter(disch6, disch6$X_00060_00003_cd == "A")

alldisch <- list(disch1, disch2, disch3, disch4, disch5, disch6)
alldisch <- rbindlist(alldisch)

#--------
# Save data:
saveRDS(alldisch, "WUS_all_USGS_disch_dv_data.rds")
rm(alldisch, disch1, disch2, disch3, disch4, disch5, disch6)

# See how far we get with JUST daily value data for the Western US for now, although the other data has been downloaded





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
