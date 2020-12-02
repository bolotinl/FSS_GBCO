# We may no longer need this script....we already merged the data in USGS_Q_exploration

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

## Flow normalize:
dat$SpC_Qcms <- dat$SpC / dat$Q_cms
## If SpC = 0 AND Q = 0, make SpC_Qcms = 0
dat$SpC_Qcms <- ifelse(dat$SpC == 0 & dat$Q_cms == 0, paste(0), dat$SpC_Qcms)

## SpC != 0 BUT Q = 0, make SpC_Qcms = NA
dat$SpC_Qcms <- ifelse(dat$SpC != 0 & dat$Q_cms == 0, NA, paste(dat$SpC_Qcms))
dat$SpC_Qcms <- as.numeric(as.character(dat$SpC_Qcms))

table(dat$SiteID)
USGS-14139800

sapply(dat, class)
ggplot(subset(dat, dat$SiteID == "USGS-14139800"))+
  geom_line(mapping = aes(Date, SpC_Qcms))

## Save data file:
saveRDS(dat, "WUS_all_USGS_SC_Q_data.rds")



# For GBCO Data:
## Bring in data
# setwd("/Volumes/Blaszczak Lab/FSS/All Data")
# SC <- readRDS("all_SC_data.rds")
# # Q <- readRDS("USGS_disch_SubBySC_dqi.rds") # 527,187
# Q <- readRDS("USGS_disch_dqi.rds") # 558,554 # continue with original Q data for now
# 
# colnames(SC)
# colnames(Q)
# Q$SiteDate <- paste0(Q$SiteID, " ", Q$Date)
# 
# Q <- select(Q, c("SiteID", "Date", "Q_cfs", "Q_cms","SiteDate"))
# Q$Source <- "USGS"
# 
# dat <- merge(SC, Q, by = c("SiteID", "Date", "SiteDate"))
# colnames(dat)
# colnames(dat) <- c("SiteID", "Date", "SiteDate", "SpC", "Source_SpC", "Q_cfs", "Q_cms", "Source_Q")
# 
# dat$Spc_Qcms <- dat$SpC / dat$Q_cms
# dat$SiteID <- factor(dat$SiteID)
# 
# ## If SpC = 0 AND Q = 0, make SpC_Qcms = 0
# dat$Spc_Qcms <- ifelse(dat$SpC == 0 & dat$Q_cms == 0, paste(0), paste(dat$Spc_Qcms))
# 
# ## SpC != 0 BUT Q = 0, make SpC_Qcms = NA
# dat$Spc_Qcms <- ifelse(dat$SpC != 0 & dat$Q_cms == 0, NA, paste(dat$Spc_Qcms))
# 
# 
# table(dat$SiteID)
# 
# ggplot(subset(dat, dat$SiteID == "USGS-09041400"))+
#   geom_line(mapping = aes(Date, Spc_Qcms))
# 
# getwd()
# 
# 
# saveRDS(dat, "all_SC_Q_data.rds")

# # Get rid of Inf, NA, and NaN's before using this data any more
# dat <- readRDS("all_SC_Q_data.rds")
# # NaN's
# is.nan.data.frame <- function(x)
#   do.call(cbind, lapply(x, is.nan))
# 
# dat[is.nan(dat)] <- NA
# # Inf, -Inf's
# dat$Spc_Qcms <- ifelse(dat$Spc_Qcms == "-Inf", paste(0), paste(dat$Spc_Qcms))
# dat$Spc_Qcms <- ifelse(dat$Spc_Qcms == "Inf", paste(0), paste(dat$Spc_Qcms))
# 
# # NA
# dat$Spc_Qcms <- ifelse(dat$SpC == 0 | dat$Q_cms == 0, paste(0), paste(dat$Spc_Qcms))
# saveRDS(dat, "all_SC_Q_data_v2.rds")
# 
# dat_complete <- dat[complete.cases(dat),]
# dat_complete$SiteYear <- paste0(dat_complete$SiteID, " ", year(dat_complete$Date))
# 
# sy_count <- table(dat_complete$SiteYear) %>%
#   as.data.frame()
# 
# sy_count <- sy_count %>%
#   filter(Freq >= 340)
# 
# sub_for_cont <- dat_complete %>%
#   filter(SiteYear %in% sy_count$Var1)
# 
# saveRDS(sub_for_cont, "continuous_SC_Q_data.rds")
# sub_for_cont$SiteID <- factor(sub_for_cont$SiteID)
# levels(sub_for_cont$SiteID) #85
# 
# rm(list=ls())


