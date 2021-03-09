# Feature Selection for Random Forest Model ####

library(tidyverse)

## Bring in catchment attribute data
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
dat <- readRDS("attribute_df_cluster_results.rds")
names(dat)

## Remove any columns we won't be using as predictors or response variables in the random forest
dat <- dat %>%
  select(-c(SiteID, COMID, StreamOrde, huc_cd)) 
sapply(dat, class)

## Look at correlations > 0.9 between variables ####
check_cor <- 
  cor(dat[,-c(1:2)], use = "pairwise.complete.obs", method = "pearson")

cor_high <-
  check_cor %>% 
  reshape2::melt() %>% 
  subset(Var1 != Var2 & is.finite(value)) %>% 
  subset(abs(value) > 0.9) %>% # Start with > 0.9
  dplyr::distinct() %>% 
  dplyr::arrange(Var1, Var2)

# this provides two rows per highly correlated pair of predictors
getwd()
saveRDS(check_cor, "corr_matrix_all_attributes.rds")
saveRDS(cor_high, "corr_high_all_attributes.rds")

cor_high$Var1 <- factor(cor_high$Var1)
(cor_remove <- levels(cor_high$Var1)) # Isolate the individual attributes that are highly correlated with at least one other attribute

## Remove attributes according to decisions based on the correlation matrix and hypotheses for attribute impacts on stream salinity
names(dat)
dat <- dat %>%
  select(-c(# PHYSIOGRAPHIC: 
            "Flowline_Length", # Keep basin area instead
            "Basin_Slope", # Keep stream slope instead
            #"Elevation_Mean", # Test model with min, min and max, and just mean
            "Elevation_Min",
            "Elevation_Max",
            # CLIMATE and WATER BALANCE:
            "WBM_PET", # Correlated with temperature, and we already have AET
            "WBM_RUNOFF", # We want to keep precip
            # GEOLOGY:
            starts_with("BR_"), # Too broad/too big of differences within eac class for predictions of water quality dynamics (Olson & Hawkins 2012)
            # SOILS: 
            "Permeability", # Keep sand, silt, clay fractions instead
            "Avg_Bulk_Density", # Keep OM_content instead
            # ATMOSPHERIC DEPOSITION:
            "NADP_SO4", # Keep NADP_CA and NADP_MG
            # HYDRO MODIFICATION: Try only NDAMS, see how it changes when you swap it with any of these others, since they are all highly correlated
            "MAJOR2013", 
            "NORM_STORAGE2013"))

## Save new dataframe to use in random forest
saveRDS(dat, "attribute_tune_df.rds")

## Look at correlations > 0.8 between variables ####
dat <- readRDS("attribute_tune_df.rds")
check_cor <- 
  cor(dat[,-c(1:2)], use = "pairwise.complete.obs", method = "pearson")

cor_high <-
  check_cor %>% 
  reshape2::melt() %>% 
  subset(Var1 != Var2 & is.finite(value)) %>% 
  subset(abs(value) > 0.8) %>% # Changed threshold to 0.7
  dplyr::distinct() %>% 
  dplyr::arrange(Var1, Var2)

# Group Land Use Classes into: ####

# Developed
# Agriculture
# Undeveloped
# Open Water

dat <- dat %>%
  mutate(Developed_pct = DevelopedOpenSpace_pct + DevelopedHiIntensity_pct + DevelopedLowIntensity_pct + DevelopedMedIntensity_pct,
         Agriculture_pct = CultivatedCrops_pct,
         Undeveloped_pct = PastureHay_pct + PerennialIceSnow_pct + BarrenLand_pct + DeciduousForest_pct + EvergreenForest_pct + MixedForest_pct + ShrubScrub_pct + GrasslandHerbaceous_pct,
         OpenWater2_pct = OpenWater_pct + WoodyWetlands_pct + EmergentHerbWetlands_pct)
dat <- dat %>%
  select(-c("DevelopedOpenSpace_pct", "DevelopedHiIntensity_pct", "DevelopedLowIntensity_pct", "DevelopedMedIntensity_pct", "PastureHay_pct", "CultivatedCrops_pct", "PerennialIceSnow_pct", "BarrenLand_pct", "DeciduousForest_pct", "EvergreenForest_pct", "MixedForest_pct", "ShrubScrub_pct", "GrasslandHerbaceous_pct", "WoodyWetlands_pct", "EmergentHerbWetlands_pct", "OpenWater_pct"))

saveRDS(dat, "attribute_tune_df_1.rds")

####
# dat <- readRDS("attribute_tune_df.rds")
# dat <- dat %>%
#   select(-c(Stream_Slope, OLSON_SiO2, Thickness))
# 
# saveRDS(dat, "attribute_tune_df_2.rds")

## Remove some other variables we don't have hypotheses for
dat <- readRDS("attribute_tune_df_1.rds")
dat <- dat %>%
  select(-c(Stream_Slope, OLSON_SiO2, Thickness))
saveRDS(dat, "attribute_tune_df_1.5.rds")

## Go further
dat <- readRDS("attribute_tune_df_1.5.rds")

check_cor <- 
  cor(dat[,-c(1:2)], use = "pairwise.complete.obs", method = "pearson")

cor_high <-
  check_cor %>% 
  reshape2::melt() %>% 
  subset(Var1 != Var2 & is.finite(value)) %>% 
  subset(abs(value) > 0.8) %>% # Changed threshold to 0.7
  dplyr::distinct() %>% 
  dplyr::arrange(Var1, Var2)




dat <- dat %>%
  select(-c(Agriculture_pct, WBM_TAVG, MIRAD_Irrig_Ag_Land_pct, OLSON_MgO))
saveRDS(dat, "attribute_tune_df_1.75.rds")


dat <- readRDS("attribute_tune_df_2.rds")
dat <- dat %>%
  select(-c(WBM_SM_STRG, OM_Content))
saveRDS(dat, "attribute_tune_df_3.rds")

# Add road density ####
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
dat <- readRDS("attribute_df_cluster_results.rds")
roads_list <- list.files(path = "/Volumes/Blaszczak Lab/StreamCat Dataset", pattern = "RoadDensity")
roads_list

setwd("/Volumes/Blaszczak Lab/StreamCat Dataset")
roads <- lapply(roads_list, read.csv)
roads <- bind_rows(roads)
roads <- roads %>%
  select(c(COMID, RdDensWs))
dat <- merge(dat, roads, by = "COMID", all.x = TRUE)

setwd("/Volumes/Blaszczak Lab/FSS/All Data")
dat_sub_by <- readRDS("attribute_tune_df_2.rds")

dat_sub_by$RdDensWS <- dat$RdDensWs
saveRDS(dat_sub_by, "attribute_tune_df_4.rds")

# Add population density 2010 ####
pop <- read.csv("/Volumes/Blaszczak Lab/FSS/NHD Attributes/Population Infrastructure/POPDENS10_CONUS.csv")
pop <- pop %>% select(c(COMID, ACC_POPDENS10))
dat <- readRDS("attribute_df_cluster_results.rds")

dat <- merge(dat, pop, by = "COMID", all.x = TRUE)

dat_sub_by <- readRDS("attribute_tune_df_2.rds")
dat_sub_by$PopDens2010 <- dat$ACC_POPDENS10
saveRDS(dat_sub_by, "attribute_tune_df_5.rds")













# library(Boruta)
# library(caret)
# library(mlbench)
# library(randomForest)
# data("Sonar")
# str(Sonar)
# 
# set.seed(111)
# boruta <- Boruta(Class ~., 
#                  data = Sonar, 
#                  doTrace = 2, # Allows you to see what's going on in the console
#                  maxRuns = 500)
# print(boruta)
# plot(boruta, 
#      las = 2, # Makes axis lables vertical so you can see them all
#      cex.axis = 0,7) # Changes font size on x axis
# # Blue boxplots are shadow attributes
# # Green boxplots are confirmed important
# # Red boxplots confirmed to be not important
# # Yellow boxplots inconclusive importance
# plotImpHistory(boruta)
# 
# # Tentative Fix
# # come up with a quick decision for tentative attributes
# bor <- TentativeRoughFix(boruta)
# bor # Now all attributes are classified as important or not important
# attStats(boruta)
# # normHits example, if 80% for a variable, 80% of the time, this variable was more important than the shadow attribute
# 
# # Then go through RF model
# # Try normal model with all variables
# 
# # Work on removing variables confirmed not important by boruta
# getNonRejectedFormula(boruta)
# # 41 attributes (confirmed, plus tentative with quick fix)
# # paste formula into new RF model
# rf41 <- randomForest(Class ~ V1 + V2 + V4 + V5 + V8 + V9 + V10 + V11 + V12 + V13 + 
#                        V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + 
#                        V26 + V27 + V28 + V30 + V31 + V32 + V35 + V36 + V37 + V39 + 
#                        V43 + V44 + V45 + V46 + V47 + V48 + V49 + V51 + V52 + V54,
#                      data = train)
# # SLIGHTLY less accuracy after chopping ~1/3 attributes
# 
# getConfirmedFormula(boruta)
# rf33 <- randomForest(Class ~ V1 + V4 + V5 + V9 + V10 + V11 + V12 + V13 + V15 + V16 + 
#                        V17 + V18 + V19 + V20 + V21 + V22 + V23 + V26 + V27 + V28 + 
#                        V31 + V35 + V36 + V37 + V43 + V44 + V45 + V46 + V47 + V48 + 
#                        V49 + V51 + V52,
#                      data = train)
# # SIMILAR to original accuracy, but with ~1/2 the attributes