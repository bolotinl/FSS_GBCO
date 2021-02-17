# Create df to feed into random forest model

library(dplyr)
library(naniar)
library(reshape2)

# CREATE ATTRIBUTE DFs####
# Start by using almost all catchment attributes and see how the random forest model works
# Get data on cluster membership
setwd("/Volumes/Blaszczak Lab/FSS/FSS_clustering/Cluster Plots and Results")
clust <- readRDS("cluster_results.rds") # results for 2 clusters with flow normalized data (SCQ)
# clust <- readRDS("cluster_results_2cl_SC.rds") # results for 2 clusters with non-flow normalized data (SC)


# Get the catchment attribute data
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
geol <- readRDS("WUS_UNM_USGS_Geologic_Attributes.rds")
hydro <- readRDS("WUS_UNM_USGS_Hydrologic_Modification_Attributes.rds")
land <- readRDS("WUS_UNM_USGS_Land_Use_Attributes.rds")
phys <- readRDS("WUS_UNM_USGS_Physiographic_Attributes.rds")
soil <- readRDS("WUS_UNM_USGS_Soil_Attributes.rds")
clim <- readRDS("WUS_UNM_USGS_Water_Bal_Climate_Attributes.rds")
atms <- readRDS("WUS_UNM_USGS_Atmospheric_Dep.rds")

# Subset attribute data for stuff we actually want to use (get rid of "No Data" columns, redundant or irrelevant columns)
# We only need ONE of the attribute data frames to include COMID and SiteID
names(geol)
geol <- geol %>%
  select(-c("OLSON_NoData", "BR_NoData", "BR_Water"))

names(hydro)
# See if NDAMS, NORM_STORAGE, MAJOR, and NID_STORAGE change much over time across all locations

# One site has no data and will mess with our plots, so remove it
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
  labs(x = "Year", y = "Accumulated Number of Dams Built on or Before Date", title = "NDAMS")

# NID_STORAGE (acre-feet) Definition: Accumulated maximum dam storage defined as the total storage space in a reservoir below the maximum attainable water surface elevation, including any surcharge storage
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
# This one does not seem relevant

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
  labs(x = "Year", y = "Accumulated Normal Dam Storage (acre-feet)", title = "NORM_STORAGE")

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
  labs(x = "Year", y = "Accumulated Number of Major Dams Upstream", title = "MAJOR")

# Things don't change significantly over time, so let's keep 2013
hydro <- hydro %>%
  select(c("SiteID", "NDAMS2013", "NID_STORAGE2013", "NORM_STORAGE2013", "MAJOR2013")) 

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
# Try one and then try the other
clim <- clim %>%
  select(-c("WBM_NoData", "BFI_NoData", "PRSNOW_NoData", "PRSNOW", "COMID"))
# clim <- clim %>%
#   select(-c("WBM_NoData", "BFI_NoData", "PRSNOW_NoData", "WBM_PRSNOW"))

# Concatenate attributes, site, and cluster info into a data frame
dat <- merge(clust, clim, by = "SiteID")
dat <- merge(dat, geol, by = "SiteID")
dat <- merge(dat, hydro, by = "SiteID")
dat <- merge(dat, land, by = "SiteID")
dat <- merge(dat, phys, by = "SiteID")
dat <- merge(dat, soil, by = "SiteID")
dat <- merge(dat, atms, by = "SiteID")

rm(clim, clust, geol, hydro, land, phys, soil, atms)

names(dat)

# I know that if data is unavailable, the value defaults to -9999
# One site has no attribute data so it will need to be removed from the clusters
dat <- subset(dat, dat$SiteID != "USGS-10312210")
dat$SiteID <- factor(dat$SiteID)


# Save a data frame that will help us keep track of the SiteID, Cluster, COMID
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(dat, "attribute_df.rds") # without atmospheric deposition
saveRDS(dat, "attribute_df_add_nadp.rds") 

# setwd("/Volumes/Blaszczak Lab/FSS/All Data")
# saveRDS(dat, "attribute_df_4cl.rds") # data frame for attributes and 4 clusters (SCQ)

# FEATURE SELECTION ####
# Play with feature selection
names(dat)
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
# dat <- readRDS("attribute_df.rds")
dat <- readRDS("attribute_df_add_nadp.rds")

dat <- dat %>%
  select(-c("SiteID", "COMID"))
sapply(dat, class)

# Group land use classes: 
# Developed
# Agriculture
# Undeveloped
# Open Water
# dat <- dat %>%
#   mutate(Developed_pct = DevelopedOpenSpace_pct + DevelopedHiIntensity_pct + DevelopedLowIntensity_pct + DevelopedMedIntensity_pct,
#          Agriculture_pct = PastureHay_pct + CultivatedCrops_pct,
#          Undeveloped_pct = PerennialIceSnow_pct + BarrenLand_pct + DeciduousForest_pct + EvergreenForest_pct + MixedForest_pct + ShrubScrub_pct + GrasslandHerbaceous_pct + WoodyWetlands_pct + EmergentHerbWetlands_pct)
# dat <- dat %>%
#   select(-c("DevelopedOpenSpace_pct", "DevelopedHiIntensity_pct", "DevelopedLowIntensity_pct", "DevelopedMedIntensity_pct", "PastureHay_pct", "CultivatedCrops_pct", "PerennialIceSnow_pct", "BarrenLand_pct", "DeciduousForest_pct", "EvergreenForest_pct", "MixedForest_pct", "ShrubScrub_pct", "GrasslandHerbaceous_pct", "WoodyWetlands_pct", "EmergentHerbWetlands_pct"))


# Look at correlations between variables
check_cor <- 
  cor(dat[,-1], use = "pairwise.complete.obs", method = "pearson")

cor_high <-
  check_cor %>% 
  reshape2::melt() %>% 
  subset(Var1 != Var2 & is.finite(value)) %>% 
  subset(abs(value) > 0.9) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(Var1, Var2)

# # This provides two rows per highly correlated pair of predictors
# getwd()
# saveRDS(check_cor, "corr_matrix_all_attributes.rds")
# saveRDS(cor_high, "corr_high_all_attributes.rds")

cor_high$Var1 <- factor(cor_high$Var1)
(cor_remove <- levels(cor_high$Var1))
dat <- dat %>%
  select(-c("NID_STORAGE2013", "MAJOR2013", "Flowline_Length"))

# Look at correlations again
check_cor <- 
  cor(dat[,-1], use = "pairwise.complete.obs", method = "pearson")

cor_high <-
  check_cor %>% 
  reshape2::melt() %>% 
  subset(Var1 != Var2 & is.finite(value)) %>% 
  subset(abs(value) > 0.9) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(Var1, Var2)

saveRDS(dat, "attribute_tune_df.rds")



# # Get rid of some other things that aren't used in other analyses (Olson 2019) or that are definitely redundant and/or definitely correlated
dat <- dat %>%
  select(-c("Stream_Slope", "MIRAD_Irrig_Ag_Land_pct", "BR_Gneiss" ,"BR_Granitic", "BR_Ultramafic",
            "BR_Quarternary","BR_Sedimentary","BR_Volcanic", "BR_Anorthositic",
            "BR_Intermediate"))

saveRDS(dat, "attribute_tune_df.rds")

# 
# saveRDS(dat, "attribute_tune_df.rds") # NOT CURRENT

# # Screw around with removing some other stuff and see what happens
# # Get rid of some other things that aren't used in other analyses (Olson 2019) or that are definitely redundant and/or definitely correlated
# dat <- dat %>%
#   select(-c("Stream_Slope", "Flowline_Length", "MIRAD_Irrig_Ag_Land_pct", "BR_Gneiss" ,"BR_Granitic", "BR_Ultramafic",          
#             "BR_Quarternary","BR_Sedimentary","BR_Volcanic", "BR_Anorthositic",        
#             "BR_Intermediate", "NDAMS2013", "NID_STORAGE2013", "NORM_STORAGE2013", "MAJOR2013", "Salinity", "pH", "Thickness", "Elevation_Min", "Elevation_Max"))
# 
# saveRDS(dat, "attribute_tune_df.rds") # NOT CURRENT
