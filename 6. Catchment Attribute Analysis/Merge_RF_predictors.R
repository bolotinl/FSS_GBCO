# Create df to feed into random forest model with all potential catchment and climate attributes

library(tidyverse)
library(naniar)
library(reshape2)
library(foreign)

# CREATE ATTRIBUTE DFs####
# Start by using almost all catchment attributes and see how the random forest model works
## Get data on cluster membership
setwd("/Volumes/Blaszczak Lab/FSS/FSS_clustering/Cluster Plots and Results")

# Results for clustering conducted on flow-normalized specific conductance data
clust <- readRDS("cluster_results_2cl_w77_SCQ.rds")
# Results for clustering conducted on NON flow-normalized specific conductance data
clust_SC <- readRDS("cluster_results_2cl_w77_SC.rds")

colnames(clust_SC) <- c("SiteID","Cluster_SC")
colnames(clust) <- c("SiteID", "Cluster_SCQ")

## Get the catchment attribute data
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
geol <- readRDS("WUS_UNM_USGS_Geologic_Attributes.rds")
hydro <- readRDS("WUS_UNM_USGS_Hydrologic_Modification_Attributes.rds")
land <- readRDS("WUS_UNM_USGS_Land_Use_Attributes.rds")
phys <- readRDS("WUS_UNM_USGS_Physiographic_Attributes.rds")
soil <- readRDS("WUS_UNM_USGS_Soil_Attributes.rds")
clim <- readRDS("WUS_UNM_USGS_Water_Bal_Climate_Attributes.rds")
atms <- readRDS("WUS_UNM_USGS_Atmospheric_Dep.rds")

# Subset attribute data for stuff we actually want to use (for example, get rid of "No Data" columns, clearly redundant or irrelevant columns)
# We only need ONE of the attribute data frames to include COMID and SiteID
names(geol)
geol <- geol %>%
  select(-c("OLSON_NoData", "BR_NoData", "BR_Water"))

names(hydro)
# See if NDAMS, NORM_STORAGE, MAJOR, and NID_STORAGE change much over time across all locations so we can either choose one year of NID data, or apply different years to different sites

# One site has no catchment attribute data and will mess with our plots, so remove it
explore_hydro <- hydro
explore_hydro <- subset(explore_hydro, explore_hydro$SiteID != "USGS-10312210")
explore_hydro$SiteID <- factor(explore_hydro$SiteID)

# NDAMS (count)
hydro_ndams <- explore_hydro%>%
  select(c("SiteID", "COMID", "NDAMS1990", "NDAMS2000", "NDAMS2010", "NDAMS2013")) %>%
  melt(id.vars = c("SiteID", "COMID"))
hydro_ndams$variable <- substr(hydro_ndams$variable, 6,9)
hydro_ndams$variable <- as.numeric(as.character(hydro_ndams$variable))
# Plot to see change over time
ggplot()+
  geom_line(data = hydro_ndams, mapping = aes(x = variable, y = value, color = SiteID))+
  theme(legend.position = "none")+
  labs(x = "Year", y = "Accumulated Number of Dams Built on or Before Date", title = "NDAMS")+ # Definitely some unrealistic values for 2 sites, so limit the y-axis and we will set these to NA later
  ylim(0, 1600)
  
# NID_STORAGE (acre-feet) 
hydro_nid <- explore_hydro%>%
  select(c("SiteID", "COMID", "NID_STORAGE1990", "NID_STORAGE2000", "NID_STORAGE2010", "NID_STORAGE2013"))%>%
  melt(id.vars = c("SiteID", "COMID"))
hydro_nid$variable <- substr(hydro_nid$variable, 12,15)
hydro_nid$variable <- as.numeric(as.character(hydro_nid$variable))
# Plot to see change over time
ggplot()+
  geom_line(data = hydro_nid, mapping = aes(x = variable, y = value, color = SiteID))+
  theme(legend.position = "none")+
  labs(x = "Year")
# This one does not seem relevant, so we won't use it in the final data frame

# NORM_STORAGE (acre-feet)
hydro_norm <- explore_hydro%>%
  select(c("SiteID", "COMID", "NORM_STORAGE1990", "NORM_STORAGE2000", "NORM_STORAGE2010", "NORM_STORAGE2013"))%>%
  melt(id.vars = c("SiteID", "COMID"))
hydro_norm$variable <- substr(hydro_norm$variable, 13,16)
hydro_norm$variable <- as.numeric(as.character(hydro_norm$variable))
# Plot to see change over time
ggplot()+
  geom_line(data = hydro_norm, mapping = aes(x = variable, y = value, color = SiteID))+
  theme(legend.position = "none")+
  labs(x = "Year", y = "Accumulated Normal Dam Storage (acre-feet)", title = "NORM_STORAGE")+
  ylim(0,2500000)
# Only ~2 sites with significant change around 2000

# MAJOR (count)
hydro_major <- explore_hydro%>%
  select(c("SiteID", "COMID", "MAJOR1990", "MAJOR2000", "MAJOR2010", "MAJOR2013"))%>%
  melt(id.vars = c("SiteID", "COMID"))
hydro_major$variable <- substr(hydro_major$variable, 6,9)
hydro_major$variable <- as.numeric(as.character(hydro_major$variable))
# Plot to see change over time
ggplot()+
  geom_line(data = hydro_major, mapping = aes(x = variable, y = value, color = SiteID))+
  theme(legend.position = "none")+
  labs(x = "Year", y = "Accumulated Number of Major Dams Upstream", title = "MAJOR")+
  ylim(0,300)
# No significant changes except for two sites (appear to be the same 2 sites with unreasonable values for NDAMS as well)

# Things don't change significantly over time overall, so let's keep using 2013 for our final data frame
hydro <- hydro %>%
  select(c("SiteID", "NDAMS2013","NORM_STORAGE2013", "MAJOR2013")) 
rm(explore_hydro, hydro_major, hydro_ndams, hydro_nid, hydro_norm)

names(land)
land <- land %>%
  select(-c("NLCD_NoData", "MIRAD_NoData", "COMID"))

names(phys)
phys <- phys %>%
  select(-c("COMID"))

names(soil)
soil <- soil %>%
  select(-c("Texture_NoData", "pH_NoData", "Salinity_NoData", "COMID"))

names(atms)
atms <- atms %>% 
  select(-c("COMID"))

names(clim)
# We have two different sources from which we got data on the percent of precipitation that falls as snow
# One is the percentage and one is the actual amount of snow in mm. Use the percentage, averaged from 1905-2002
clim <- clim %>%
  select(-c("WBM_NoData", "BFI_NoData", "PRSNOW_NoData", "WBM_PRSNOW", "COMID")) # new version to be used with correct attribute for percent of precipitation as snow

# Concatenate attributes, site, and cluster info into a data frame
dat <- merge(clust, clust_SC, by = "SiteID")
dat <- merge(dat, clim, by = "SiteID")
dat <- merge(dat, geol, by = "SiteID")
dat <- merge(dat, hydro, by = "SiteID")
dat <- merge(dat, land, by = "SiteID")
dat <- merge(dat, phys, by = "SiteID")
dat <- merge(dat, soil, by = "SiteID")
dat <- merge(dat, atms, by = "SiteID")

rm(clim, clust, geol, hydro, land, phys, soil, atms)

names(dat)

# One site has no attribute data so it will need to be removed before the random forest analysis
dat <- subset(dat, dat$SiteID != "USGS-10312210")
dat$SiteID <- factor(dat$SiteID)

## Join HUC 2 region numbers
dat <- readRDS("attribute_df_cluster_results.rds")
huc <- readRDS("WUS_USGS_disch_SC_sites.rds")
huc <- huc %>%
  select(c(Site_ID, huc_cd))

huc$Site_ID <- as.numeric(as.character(huc$Site_ID))
huc$Site_ID <- ifelse(huc$Site_ID < 10000000, paste0("0", huc$Site_ID), paste(huc$Site_ID))
huc$Site_ID <- paste0("USGS-", huc$Site_ID)
huc$huc_cd <- substr(huc$huc_cd, 1,2)
huc <- huc %>% rename(SiteID = "Site_ID")
huc <- huc %>% 
  filter(SiteID %in% dat$SiteID) %>%
  unique()


dat <- merge(dat, huc, by = "SiteID", all.y = FALSE, all.x = TRUE)

# Manually input HUC 2 region for the two Rio Grande sites whose data is not from the USGS
# Region 13 is the Rio Grande Region
dat$huc_cd[1:2] <- 13

# Add Stream Order
nhd_vaa <- read.dbf("/Volumes/Blaszczak Lab/FSS/NHD/PlusFlowlineVAA.dbf")
nhd_vaa <- nhd_vaa %>%
  select(c(ComID, StreamOrde))

nhd_vaa <- nhd_vaa %>% rename(COMID = "ComID")

dat <- merge(dat, nhd_vaa, by = "COMID", all.x = TRUE, all.y = FALSE)
# Not available for all

# Save 
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(dat, "attribute_df_cluster_results.rds")


# Look at outliers in catchment attributes ####


# Climate and WB
boxplot(dat$WBM_AET, dat$WBM_PET, dat$WBM_PRCP, dat$WBM_RUNOFF, dat$WBM_SM_STRG, dat$WBM_TAVG, dat$BFI_pct, dat$PRSNOW)
boxplot(dat$WBM_PRCP, dat$WBM_RUNOFF)
  # Precip and runoff have the most outliers

quantile(dat$WBM_PRCP, 0.25)
quantile(dat$WBM_PRCP, 0.25)*1.5

quantile(dat$WBM_PRCP, 0.75)
quantile(dat$WBM_PRCP, 0.75)*1.5
out_max <- dat$WBM_PRCP[dat$WBM_PRCP > quantile(dat$WBM_PRCP, 0.75)*1.5]

# Geol
boxplot(dat$OLSON_K2O, dat$OLSON_CompressStrength, dat$OLSON_Fe203, dat$OLSON_CaO, dat$OLSON_Fe203, dat$OLSON_HydroConductivity, dat$OLSON_MgO, dat$OLSON_P2O5, dat$OLSON_S, dat$OLSON_SiO2)

# Hydrologic Modification
boxplot(dat$NDAMS2013, dat$NORM_STORAGE2013, dat$MAJOR2013)
out_max <- dat$NDAMS2013[dat$NDAMS2013 > quantile(dat$NDAMS2013, 0.75)*1.5]
out_max <- dat$NORM_STORAGE2013[dat$NORM_STORAGE2013 > quantile(dat$NORM_STORAGE2013, 0.75)*1.5]
out_max <- dat$MAJOR2013[dat$MAJOR2013 > quantile(dat$MAJOR2013, 0.75)*1.5]

# Land Use
boxplot(dat$OpenWater_pct, dat$PerennialIceSnow_pct, dat$DevelopedOpenSpace_pct, dat$DevelopedHiIntensity_pct, dat$DevelopedLowIntensity_pct, dat$DevelopedMedIntensity_pct)
out_max <- dat$OpenWater_pct[dat$OpenWater_pct > quantile(dat$OpenWater_pct, 0.75)*1.5]

# Physiographic Attributes
boxplot(dat$Basin_Area, dat$Stream_Slope, dat$Basin_Slope, dat$Elevation_Max, dat$Elevation_Mean, dat$Elevation_Min, dat$Flowline_Length)

# Soils
boxplot(dat$Silt_avg, dat$Sand_avg, dat$Clay_avg, dat$pH, dat$Salinity, dat$Avg_Bulk_Density, dat$Thickness, dat$Avg_Bulk_Density, dat$Permeability, dat$OM_Content)

# Atmospheric Deposition
boxplot(dat$NADP_CA, dat$NADP_MG, dat$NADP_SO4)
