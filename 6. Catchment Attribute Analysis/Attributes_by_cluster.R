library(tidyverse)
library(reshape2)

# Bring in data
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
dat <- readRDS("attribute_df_cluster_results.rds")

# Remove unnecessary columns
dat <- dat %>%
  select(-c(Cluster_SC, COMID, SiteID))

sapply(dat, class)

# Group land use classes as we do for our random forest analysis
dat <- dat %>%
  mutate(Developed_pct = DevelopedOpenSpace_pct + DevelopedHiIntensity_pct + DevelopedLowIntensity_pct + DevelopedMedIntensity_pct,
         Agriculture_pct = CultivatedCrops_pct,
         Undeveloped_pct = PastureHay_pct + PerennialIceSnow_pct + BarrenLand_pct + DeciduousForest_pct + EvergreenForest_pct + MixedForest_pct + ShrubScrub_pct + GrasslandHerbaceous_pct,
         OpenWater2_pct = OpenWater_pct + WoodyWetlands_pct + EmergentHerbWetlands_pct)
dat <- dat %>%
  select(-c("DevelopedOpenSpace_pct", "DevelopedHiIntensity_pct", "DevelopedLowIntensity_pct", "DevelopedMedIntensity_pct", "PastureHay_pct", "CultivatedCrops_pct", "PerennialIceSnow_pct", "BarrenLand_pct", "DeciduousForest_pct", "EvergreenForest_pct", "MixedForest_pct", "ShrubScrub_pct", "GrasslandHerbaceous_pct", "WoodyWetlands_pct", "EmergentHerbWetlands_pct", "OpenWater_pct"))

dat$Cluster_SCQ <- ifelse(dat$Cluster_SCQ == 1, paste("Cluster 1"), paste("Cluster 2")) 

dat_means <- dat %>%
  group_by(Cluster_SCQ) %>%
  summarise_all(.funs = c("mean" = mean))

dat_means <- melt(dat_means)

options(scipen = 999)
mean_1 <- dat_means %>%
  filter(Cluster_SCQ == "Cluster 1") %>%
  select(-c(Cluster_SCQ)) %>%
  rename(Cluster_1_Mean = "value") 

mean_1$variable <- str_remove(mean_1$variable, "_mean")

mean_2 <- dat_means %>%
  filter(Cluster_SCQ == "Cluster 2") %>%
  select(-c(Cluster_SCQ)) %>%
  rename(Cluster_2_Mean = "value")

mean_2$variable <- str_remove(mean_2$variable, "_mean")

dat_means <- merge(mean_1, mean_2, by = "variable")
rm(mean_1, mean_2)

dat_means[,c(-1)] <- round(dat_means[,c(-1)], digits = 4) 
#####
dat_sd <- dat %>%
  group_by(Cluster_SCQ) %>%
  summarise_all(.funs = c("sd" = sd))

dat_sd <- melt(dat_sd)

options(scipen = 999)
sd_1 <- dat_sd %>%
  filter(Cluster_SCQ == "Cluster 1") %>%
  select(-c(Cluster_SCQ)) %>%
  rename(Cluster_1_sd = "value") 

sd_1$variable <- str_remove(sd_1$variable, "_sd")

sd_2 <- dat_sd %>%
  filter(Cluster_SCQ == "Cluster 2") %>%
  select(-c(Cluster_SCQ)) %>%
  rename(Cluster_2_sd = "value")

sd_2$variable <- str_remove(sd_2$variable, "_sd")

dat_sd <- merge(sd_1, sd_2, by = "variable")
rm(sd_1, sd_2)
dat_sd[,c(-1)] <- round(dat_sd[,c(-1)], digits = 4) 

dat <- merge(dat_means, dat_sd, by = "variable")
dat$Cluster_1 <- paste(dat$Cluster_1_Mean, "+/-", dat$Cluster_1_sd)
dat$Cluster_2 <- paste(dat$Cluster_2_Mean, "+/-", dat$Cluster_2_sd)

names(dat)
dat <- dat %>%
  select(c(variable, Cluster_1, Cluster_2))
