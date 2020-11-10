########## Lauren Bolotin - bolotinljb@gmail.com #############################################
## See how many of our specific conductance sites (USGS) do not also have discharge measured
##############################################################################################
# As of 11/9 it is 6 sites, not to mention that others may have discharge but not for the whole
  # period of record or the period that we are interested in

library(tidyverse)
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
# Bring in inventory of SC sites
SC <- readRDS("USGS_SC_sites.rds")
# Bring in inventory of Q sites
Q <- readRDS("USGS_disch_sites.rds")
# Bring in out actual dataset
dat <- readRDS("all_SC_data.rds")
# Subset for USGS sites (these are the only ones that will also have discharge at them already)
dat <- subset(dat, dat$Source == "USGS")
dat_sites <- unique(dat$SiteID) %>%
  as.data.frame()

# Format Q inventory
head(Q)
Q$Site_ID <- as.numeric(as.character(Q$Site_ID))
Q$Site_ID <- ifelse(Q$Site_ID < 10000000, paste0("0", Q$Site_ID), paste0(Q$Site_ID))
Q$Site_ID <- paste0("USGS-", Q$Site_ID)
colnames(Q)[1] <- "SiteID"
# Subset Q inventory so you can find out how many SC sites have Q
Qsites <- select(Q, c("SiteID", "huc_cd", "parm_cd"))
# How many SC sites have Q?
Qsites <- subset(Qsites, Qsites$SiteID %in% dat_sites$.)
Qsites <- unique(Qsites)
# How many SC sites do not have Q?
noQ <- setdiff(dat_sites$., Qsites$SiteID)
print(noQ)
# Do the SC sites without Q include years of interest? Is the data high frequency enough that we will even be using it?
sub <- subset(dat, dat$SiteID %in% noQ) # includes sites with high frequency, recent data that we probably want to use
