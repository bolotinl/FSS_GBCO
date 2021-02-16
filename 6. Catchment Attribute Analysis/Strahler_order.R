library(tidyverse)
library(nhdplusTools)

# Bring in SiteID's and their associated ComID's
setwd("/Volumes/Blaszczak Lab/FSS/All Data/")
dat <- readRDS("WUS_UNM_USGS_ComID.rds")

dat$toID <- dat$COMID
dat$toID <- as.numeric(as.character(dat$toID))
dat$toID <- dat$toID+1
dat <- dat %>%
  select(-c("SiteID"))

# dat <- unique(dat)

colnames(dat) <- c("ID", "toID")
dat$ID <- as.numeric(as.character(dat$ID))
dat$toID <- as.numeric(as.character(dat$toID))

(order <- get_streamorder(dat))


