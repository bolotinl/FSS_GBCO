############################################################
###### Merge USGS SC Data into a dataframe with USGS Q Data
############################################################
x <- c("tidyverse", "data.table", "lubridate")
lapply(x, require, character.only = TRUE)
rm(x)
setwd("/Volumes/Blaszczak Lab/FSS/All Data")

## Bring in data:
SC <- readRDS("WUS_USGS_SC_dv_dqi.rds")
Q <-  readRDS("WUS_USGS_disch_sub_sites_by_SC_dqi.rds")

## Format dataframes:
SC$SiteDate <- paste(SC$SiteID, SC$Date)
names(SC)
names(Q)
Q <- select(Q, c("SiteID", "Date", "Q_cfs", "Q_cms", "SiteDate"))
SC <- select(SC, c("SiteID", "Date", "Specific.Conductance", "SiteDate"))
colnames(SC)[3] <- "SpC"

## Merge SC and Q data
dat <- merge(SC, Q, by = c("SiteID", "Date", "SiteDate"))
rm(SC, Q)
colnames(dat)
dat$SiteID <- paste("USGS-", dat$SiteID, sep = "")

## Flow normalize (divide SC by Q):
dat$SpC_Qcms <- dat$SpC / dat$Q_cms
## If SpC = 0 AND Q = 0, make SpC_Qcms = 0
dat$SpC_Qcms <- ifelse(dat$SpC == 0 & dat$Q_cms == 0, paste(0), dat$SpC_Qcms)

## If SpC != 0 BUT Q = 0, make SpC_Qcms = NA
dat$SpC_Qcms <- ifelse(dat$SpC != 0 & dat$Q_cms == 0, NA, paste(dat$SpC_Qcms))
dat$SpC_Qcms <- as.numeric(as.character(dat$SpC_Qcms))

table(dat$SiteID)

sapply(dat, class)
ggplot(subset(dat, dat$SiteID == "USGS-14139800"))+
  geom_line(mapping = aes(Date, SpC_Qcms))

## Save data file:
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(dat, "WUS_all_USGS_SC_Q_data.rds")
write.csv(dat, "WUS_all_USGS_SC_Q_data.csv")
rm(dat)

# Add in Rio Grande data from UNM:
wus <- readRDS("WUS_all_USGS_SC_Q_data.rds")
unm <- readRDS("UNM_all_SC_Q_data.rds")
names(wus)
names(unm)
unm$SiteDate <- paste(unm$SiteID, unm$DateTime, sep = " ")
names(unm)
unm <- select(unm, c("SiteID", "DateTime", "SiteDate", "SpC", "Q_cfs", "Q_cms", "SpC_Qcms"))
colnames(unm)[2] <- "Date"
dat <- rbind(wus, unm)

saveRDS(dat, "WUS_UNM_all_USGS_SC_Q_data.rds")
write.csv(dat, "WUS_UNM_all_USGS_SC_Q_data.csv")

