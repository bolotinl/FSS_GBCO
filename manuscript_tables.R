# Create tables for manuscript
library(tidyverse)

# site_info_table.xlsx ####
setwd("/Volumes/Blaszczak Lab/FSS/All Data/")
dat1 <- readRDS("WUS_UNM_USGS_disch_SC_sites.rds")
dat2 <- readRDS("USGS_disch_SC_sites.rds")
dat2$Site_ID <- as.numeric(as.character(dat2$Site_ID))
dat2$Site_ID <- ifelse(dat2$Site_ID < 1e7,
                                 yes = paste("0", dat2$Site_ID, sep=""),
                                 no = paste(dat2$Site_ID))
dat2$Site_ID <- paste0("USGS-", dat2$Site_ID)
dat <- dat2 %>%
  filter(Site_ID %in% dat1$SiteID)
names(dat)
dat <- dat %>%
  select(-c("parm_cd", "begin_date", "end_date", "count_nu", "data_type_cd", "access_cd"))
dat$Site_ID <- factor(dat$Site_ID)
dat <- unique(dat)

dat1 <- dat1 %>%
  filter(str_detect(SiteID, "UNM"))
dat1$site_tp_cd <- "ST"
dat1$station_nm <- c("RIO GRANDE NR COCHITI, NM", "RIO GRANDE NR LYDEN, NM")
dat1$huc_cd <- c("???????", "??????")

names(dat)
names(dat1)
dat1 <- dat1 %>%
  select(c("SiteID", "station_nm", "Lat", "Lon", "huc_cd", "site_tp_cd"))
colnames(dat1) <- names(dat)

dat <- rbind(dat, dat1)
write.csv(dat, "site_info_table.csv")
rm(list = ls())

