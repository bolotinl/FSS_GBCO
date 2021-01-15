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
#---------------------------------------------------------
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
#------------------------------------------------------------------
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
saveRDS(dat, "attribute_df.rds")

# Now remove the SiteID and COMID so it isn't used by the RF model
dat <- dat %>%
  select(-c("SiteID", "COMID"))

# Turn cluster into a factor
sapply(dat, class)
dat$Cluster <- as.factor(dat$Cluster)

# Random Forest # following this tutorial: https://www.listendata.com/2014/11/random-forest-with-r.html#What-is-Random-Forest-
#------------------------------------------------

# Run the RF model
library(randomForest)
set.seed(71)
rf <-randomForest(Cluster~.,data=dat, ntree=500) 
print(rf)

# Select mtry value with minimum out of bag(OOB) error.
mtry <- tuneRF(dat[-1],dat$Cluster, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

# It's really weird that three mtry values would have the same OOB, but let's keep it at 7 for now

importance(rf)
varImpPlot(rf)

pred1=predict(rf,type = "prob")
library(ROCR)
perf = prediction(pred1[,2], dat$Cluster)
# 1. Area under curve
auc = performance(perf, "auc")
auc
# 2. True Positive and Negative Rate
pred3 = performance(perf, "tpr","fpr")
# 3. Plot the ROC curve
plot(pred3,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
