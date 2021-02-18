#----------------------------
# Combine and format data files that were downloaded in chunks #####
#----------------------------
library(dataRetrieval)
library(tidyverse)
library(data.table)
library(beepr)

# The general process: 
  # Read data file in
  # Filter for data that is "Approved for Publication" with a data quality indicator (dqi) = "A"
  # Resolve inconsistencies on a case by case basis (i.e. multiple columns with data, two sensors at one location, etc.)

setwd("/Volumes/Blaszczak Lab/FSS/All Data")

# SCdv (Daily Specific Conductance Data) ####################################################
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

# combine all the ones that now have 5 columns and proper column names, then delete them to clean up environment
SCdv2_sub <- list(SCdv2_sub1, SCdv2_sub2, SCdv2_sub3, SCdv2_sub4, SCdv2_sub6, SCdv2_sub7, SCdv2_sub8, SCdv2_sub11)
SCdv2_sub <- rbindlist(SCdv2_sub)
rm(SCdv2_sub1, SCdv2_sub2, SCdv2_sub3, SCdv2_sub4, SCdv2_sub6, SCdv2_sub7, SCdv2_sub8, SCdv2_sub11)

# start solving problems for df's with 7 columns

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
SCdv2_adj <- SCdv2_adj %>% # creates column that has the average value of the two sensors, or NA if one value was NA
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

# now on to SCdv2_sub5:
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

# now on to SCdv2_sub9:
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
# Combine all formatted SCdv data files together
#--------
allSCdv <- list(SCdv, SCdv1, SCdv2, SCdv3)
allSCdv <- rbindlist(allSCdv)

#--------
# Save data:

saveRDS(allSCdv, "WUS_all_USGS_SC_dv_data.rds")

# allSCdv <- readRDS("WUS_all_USGS_SC_dv_data.rds")
# class(allSCdv$site_no)
# allSCdv$site_no <- as.factor(as.character(allSCdv$site_no))
# levels(allSCdv$site_no)
rm(SCdv, SCdv1, SCdv2, SCdv3)

# DISCHdv (Daily Discharge Data) #################################################
#--------
disch <- readRDS("WUS_USGS_disch_data.rds")
names(disch)

# filter for dqi == A (97% of data) 
disch <- filter(disch, disch$X_00060_00003_cd == "A")

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

disch5 <- readRDS("WUS_USGS_disch5_data.rds") # 5 columns, 2316378 rows
names(disch5)

# filter for dqi == A (97% of data)
disch5 <- filter(disch5, disch5$X_00060_00003_cd == "A")

#-------

disch6 <- readRDS("WUS_USGS_disch6_data.rds") # 5 columns, 1048447 rows
names(disch6)

# filter for dqi == A (93% of data)
disch6 <- filter(disch6, disch6$X_00060_00003_cd == "A")

# bind all daily discharge data together:
alldisch <- list(disch, disch1, disch2, disch3, disch4, disch5, disch6)
alldisch <- rbindlist(alldisch)

#--------
# Save data:
saveRDS(alldisch, "WUS_all_USGS_disch_dv_data.rds")

# alldisch <- readRDS("WUS_all_USGS_disch_dv_data.rds")
# class(alldisch$site_no)
# alldisch$site_no <- as.factor(as.character(alldisch$site_no))
# levels(alldisch$site_no) # 676

rm(disch1, disch2, disch3, disch4, disch5, disch6)

# See what data we are missing from our final data files that were input into the dataRetrieval function to make sure we are not missing any data we could be using
alldisch <- readRDS("WUS_all_USGS_disch_dv_data.rds")
allSCdv <- readRDS("WUS_all_USGS_SC_dv_data.rds")
both_huc_sites <- readRDS("WUS_USGS_disch_SC_sites.rds")

both_huc_sites$Site_ID <- as.numeric(as.character(both_huc_sites$Site_ID))
both_huc_sites$Site_ID <- ifelse(both_huc_sites$Site_ID < 1e7,
                                 yes = paste("0", both_huc_sites$Site_ID, sep=""),
                                 no = paste(both_huc_sites$Site_ID))
both_huc_sites <- subset(both_huc_sites, both_huc_sites$data_type_cd == "dv")
both_huc_sites$Site_ID <- as.factor(as.character(both_huc_sites$Site_ID))

both_huc_sites_list <- unique(both_huc_sites$Site_ID)
alldisch_sites_list <- unique(alldisch$site_no)
allSCdv_sites_list <- unique(allSCdv$site_no)

missing_disch <- setdiff(both_huc_sites_list, alldisch_sites_list)
missing_SC <- setdiff(both_huc_sites_list, allSCdv_sites_list)

# Try a few of the missing sites
siteNumber <- "13046680" # Input SiteID's from missing_SC here 
parameterCd <- "00095"
startDate <- ""  
endDate <- "" 

SC <- readNWISdv(siteNumber, 
                        parameterCd, startDate, endDate)
parameterCd <- "00060"
Q <- readNWISdv(siteNumber, 
                 parameterCd, startDate, endDate)
# None of the sites actually provide daily SC data, so we have gotten all the data we need