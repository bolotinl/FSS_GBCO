## IQR ##

library(tidyverse)
library(viridis)
library(hrbrthemes)
library(zoo)
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
dat <- readRDS("/Volumes/Blaszczak Lab/FSS/FSS_clustering/output/normalized.rds")
clust <- readRDS("/Volumes/Blaszczak Lab/FSS/FSS_clustering/cluster_results_4cl.rds")

dat <- bind_rows(dat, .id = "SiteID")
dat$SiteID <- factor(dat$SiteID)
dat <- merge(dat, clust, by = "SiteID")
rm(clust)

dat <- dat %>%
  mutate(rolling_avg_7day = rollmean(SpC_filled, 7, fill = NA, align = c("center")))

doy_max <- select(dat, c("SiteID", "doy", "rolling_avg_7day"))
options(scipen = 999) # keep things out of scientific notation

# This tells us what doy had the highest rolling 7 day average gap filled SpC
doy_max <- doy_max %>%
  group_by(SiteID) %>%
  slice(which.max(as.numeric(as.character(rolling_avg_7day))))

doy_max <- doy_max %>%
  rename(doy_max = doy)

dat <- merge(dat, doy_max, by = "SiteID")

dat %>%
  ggplot( aes(x=Cluster, y=doy_max, fill=Cluster)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Basic boxplot") +
  xlab("")
