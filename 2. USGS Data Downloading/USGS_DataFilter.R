# Data is for all of US
# Need to filter for all basins west of the main stem of the Mississipi River (HUC 14, 15, 16)

setwd("/Volumes/Blaszczak Lab/FSS/All Data")

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(dplyr)

# Read in data
disch_sensor <- read.csv("USGS_all_disch_lotic_sensor.csv")
SC_sensor <- read.csv("USGS_all_SC_lotic_sensor.csv")
disch_SC_sensor <- read.csv("USGS_all_disch_SC_lotic_sensor.csv")

# Need to filter for HUC 10-18
disch_huc_sites <- disch_sensor %>% 
  filter(huc_cd >= 10000000 ) %>% 
  select(Site_ID,station_nm,Lat,Lon,huc_cd,parm_cd,begin_date,end_date,count_nu,site_tp_cd,data_type_cd,access_cd)
  disch_huc_sites$Site_ID <- factor(disch_huc_sites$Site_ID)
  levels(disch_huc_sites$Site_ID) # 12,960

SC_huc_sites <- SC_sensor %>% 
  filter(huc_cd >= 10000000) %>% 
  select(Site_ID,station_nm,Lat,Lon,huc_cd,parm_cd,begin_date,end_date,count_nu,site_tp_cd,data_type_cd,access_cd)
  SC_huc_sites$Site_ID <- factor(SC_huc_sites$Site_ID)  
  levels(SC_huc_sites$Site_ID) # 1,106
  
disch_SC_huc_sites <- disch_SC_sensor %>%
  filter(huc_cd >= 10000000) %>%
  select(Site_ID,station_nm,Lat,Lon,huc_cd,parm_cd,begin_date,end_date,count_nu,site_tp_cd,data_type_cd,access_cd)
  disch_SC_huc_sites$Site_ID <- factor(disch_SC_huc_sites$Site_ID)
  levels(disch_SC_huc_sites$Site_ID) # 828

## Save dataframes
saveRDS(disch_huc_sites, "USGS_disch_sites.rds") # List of HUC 10-18 sites with discharge
saveRDS(SC_huc_sites, "USGS_SC_sites.rds") # List of HUC 10-18 sites with SC
saveRDS(disch_SC_huc_sites, "USGS_disch_SC_sites.rds") # List of HUC 10-18 sites with discharge AND SC

## another method to get list of HUC 14, 15, 16 sites with BOTH discharge and SC:
# overlap_huc_sites <- intersect(disch_huc_sites$Site_ID, SC_huc_sites$Site_ID)
# overlap_huc_sites <- SC_huc_sites[which(SC_huc_sites$Site_ID %in% overlap_huc_sites),]
# class(overlap_huc_sites$Site_ID)
# overlap_huc_sites$Site_ID <- factor(overlap_huc_sites$Site_ID)
# levels(overlap_huc_sites$Site_ID)
# saveRDS(overlap_huc_sites, "GBCO_dischSC_sites.rds") # 182 # List of HUC 14, 15, 16 sites with BOTH discharge and SC
