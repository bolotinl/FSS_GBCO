# Create df to feed into random forest model

library(dplyr)
library(naniar)

# Get data on cluster membership
setwd("/Volumes/Blaszczak Lab/FSS/FSS_clustering")
clust <- readRDS("cluster_results.rds")

# Get the catchment attribute data
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
geol <- readRDS("WUS_UNM_USGS_Geologic_Attributes.rds")
hydro <- readRDS("WUS_UNM_USGS_Hydrologic_Modification_Attributes.rds")
land <- readRDS("WUS_UNM_USGS_Land_Use_Attributes.rds")
phys <- readRDS("WUS_UNM_USGS_Physiographic_Attributes.rds")
soil <- readRDS("WUS_UNM_USGS_Soil_Attributes.rds")
clim <- readRDS("WUS_UNM_USGS_Water_Bal_Climate_Attributes.rds")

# Subset attribute data for stuff we actually want to use
# We only need one of the attribute data frames to include COMID
names(geol)
geol <- geol %>%
  select(-c("OLSON_NoData", "BR_NoData", "BR_Water"))

names(hydro)
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

rm(clim, clust, geol, hydro, land, phys, soil)

names(dat)

# I know that if data is unavailable, the value defaults to -9999
# One site has no attribute data so it will need to be removed from the clusters
# For now we will set the data to NA
# dat <- dat %>% replace_with_na_all(condition = ~.x == -9999) # USGS-10312210

# Actually, remove
dat <- dat[-c(dat$SiteID == "USGS-10312210"), ]

# Save a data frame that will help us keep track of the SiteID, Cluster, COMID
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(dat, "attribute_df.rds")

names(dat)
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
dat <- readRDS("attribute_df.rds")

dat <- dat %>%
  select(-c("SiteID", "COMID"))
sapply(dat, class)

# Group land use classes into: 
# Developed
# Agriculture
# Undeveloped
# Open Water
dat <- dat %>%
  mutate(Developed_pct = DevelopedOpenSpace_pct + DevelopedHiIntensity_pct + DevelopedLowIntensity_pct + DevelopedMedIntensity_pct,
         Agriculture_pct = PastureHay_pct + CultivatedCrops_pct,
         Undeveloped_pct = PerennialIceSnow_pct + BarrenLand_pct + DeciduousForest_pct + EvergreenForest_pct + MixedForest_pct + ShrubScrub_pct + GrasslandHerbaceous_pct + WoodyWetlands_pct + EmergentHerbWetlands_pct)
dat <- dat %>%
  select(-c("DevelopedOpenSpace_pct", "DevelopedHiIntensity_pct", "DevelopedLowIntensity_pct", "DevelopedMedIntensity_pct", "PastureHay_pct", "CultivatedCrops_pct", "PerennialIceSnow_pct", "BarrenLand_pct", "DeciduousForest_pct", "EvergreenForest_pct", "MixedForest_pct", "ShrubScrub_pct", "GrasslandHerbaceous_pct", "WoodyWetlands_pct", "EmergentHerbWetlands_pct"))

# Look at correlations between variables
check_cor <- 
  cor(dat[,-1], use = "everything", method = "pearson")

cor_high <-
  check_cor %>% 
  reshape2::melt() %>% 
  subset(Var1 != Var2 & is.finite(value)) %>% 
  subset(abs(value) > 0.9) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(Var1, Var2)

# Get rid of some other things that aren't used in other analyses (Olson 2019) or that are definitely redundant and/or definitely correlated
dat <- dat %>%
  select(-c("Stream_Slope", "Flowline_Length", "MIRAD_Irrig_Ag_Land_pct", "BR_Gneiss" ,"BR_Granitic", "BR_Ultramafic",          
            "BR_Quarternary","BR_Sedimentary","BR_Volcanic", "BR_Anorthositic",        
            "BR_Intermediate", "NDAMS2013", "NID_STORAGE2013", "NORM_STORAGE2013", "MAJOR2013"))


saveRDS(dat, "attribute_tune_df.rds")

