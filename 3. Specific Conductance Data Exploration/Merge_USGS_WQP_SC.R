library(dplyr)
library(plyr)
library(ggplot2)
library(lubridate)
library(data.table)
########################### Merge all daily and point USGS data and WQP data 
# Format and merge USGS SC uv, dv, and qw data ####
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
# daily data
USGS_dv <- readRDS("USGS_SC_dv_dqi.rds")
# point data
USGS_qw <- readRDS("USGS_SC_qw_dqi.rds")
# unit value (daily) data
USGS_uv <- readRDS("USGS_SC_uv_daily_dqi.rds")

# Get all df's into the same format, standardize column names
dfs <- list(USGS_dv, USGS_qw, USGS_uv)
lapply(dfs, colnames)
rm(dfs)
USGS_dv <- select(USGS_dv, c("SiteID", "DateTime", "Specific.Conductance"))
colnames(USGS_dv) <- c("SiteID", "Date", "SpC")
USGS_qw <- select(USGS_qw, c("SiteID", "Date", "mean"))
colnames(USGS_qw) <- c("SiteID", "Date", "SpC")
USGS_uv <- select(USGS_uv, c("SiteID", "Date", "Specific.Conductance"))
colnames(USGS_uv) <- c("SiteID", "Date", "SpC")


USGS_dv$SiteDate <- paste(USGS_dv$SiteID, USGS_dv$Date, sep = " ")
USGS_uv$SiteDate <- paste(USGS_uv$SiteID, USGS_uv$Date, sep = " ")
USGS_qw$SiteDate <- paste(USGS_qw$SiteID, USGS_qw$Date, sep = " ")

sapply(USGS_dv, class)
sapply(USGS_qw, class)
sapply(USGS_uv, class)

# confirm we don't have overlops of site-date's with the daily value data and the unit value data we aggregated into daily data
dv_check <- USGS_dv
uv_check <- USGS_uv
qw_check <- USGS_qw

overlap <- intersect(dv_check$SiteDate, uv_check$SiteDate) # empty (we made sure of this earlier)
overlap1 <- intersect(dv_check$SiteDate, qw_check$SiteDate) # empty
overlap2 <- intersect(uv_check$SiteDate, qw_check$SiteDate) # empty

class(dv_check)
class(qw_check)
qw_check <- as.data.frame(qw_check)
class(uv_check)
uv_check <- as.data.frame(uv_check)
USGS_uv <- as.data.frame(USGS_uv)
USGS_qw <- as.data.frame(USGS_qw)
colnames(USGS_uv)
colnames(USGS_dv)
colnames(USGS_qw)

USGS <- do.call("rbind", list(USGS_qw, USGS_dv, USGS_uv))
duplicated <- USGS[duplicated(USGS$SiteDate),] # none!this is good and confirms we got rid of all overlapping data
unique(USGS$SiteDate)

rm(overlap, overlap1, overlap2, uv_check, dv_check, qw_check,  duplicated)

# Standardize SiteID format between USGS and WQP data
colnames(USGS)
USGS$SiteID <- paste0("USGS-", USGS$SiteID)
class(USGS$SiteID)
USGS$SiteID <- factor(USGS$SiteID)
levels(USGS$SiteID) # there are 159 sites with SC just from USGS 
# Redo SiteDate since we added "USGS-" to each USGS SiteID
USGS <- select(USGS, -c("SiteDate"))
USGS$SiteDate <- paste(USGS$SiteID, USGS$Date, sep = " ") 
saveRDS(USGS, "USGS_SC.rds")

# Now onto the WQP data
# Format WQP SC data and merge it with USGS SC data ####
WQ <- read.csv("WQP_TS_final_sites.csv")

# just grab SC data
colnames(WQ)
WQ <- select(WQ, c("SiteID", "DateTime", "Specific.conductance"))
WQ <- WQ[complete.cases(WQ),] # get rid of NA values for SC
class(WQ$SiteID)
WQ$SiteID <- factor(WQ$SiteID)
levels(WQ$SiteID) # 25,623 sites for WQP SC data

colnames(WQ)[3] <- c("SpC")
WQ$SpC <- round(WQ$SpC, digits = 0)

# get this data into the same format as the USGS data
WQ$Date <- date(WQ$DateTime)

WQ$SiteDate <- paste(WQ$SiteID, WQ$Date, sep = " ")

WQ$SiteDate <- as.factor(WQ$SiteDate)
levels(WQ$SiteDate) # 424,664 (out of a 457,474 row df)
unique(WQ$SiteDate) # 424,664
duplicated <- WQ[duplicated(WQ$SiteDate),] # 32,810 (reminder that this does not include the first occurrence of each SiteDate)
# Since we have duplicates, average across them to get =< 1 value/day

WQ <- WQ %>%
  group_by(SiteID, Date, SiteDate) %>%
  summarise_at(.vars = "SpC", .funs = c("SpC"=mean))
WQ$SpC <- round(WQ$SpC, digits = 0)

class(WQ)
WQ <- as.data.frame(WQ)
class(WQ$SiteID)
WQ$SiteID <- factor(WQ$SiteID)
levels(WQ$SiteID) # Still 25,623
duplicated <- WQ[duplicated(WQ$SiteDate),] # none! good

colnames(USGS)
colnames(WQ)

WQ <- select(WQ, c("SiteID", "Date", "SpC", "SiteDate"))
saveRDS(WQ, "WQP_SC.rds")


# Merge USGS and WQP SC data ####
# The USGS and WQP SC data are now in the same format and can be combined
class(USGS)
class(WQ)
USGS <- as.data.frame(USGS)
USGS$Source <- "USGS"
class(USGS$SiteID)
class(WQ$SiteID)
USGS$SiteID <- factor(USGS$SiteID)
WQ$SiteID <- factor(WQ$SiteID)
levels(USGS$SiteID) # 159
levels(WQ$SiteID) # 25,623
WQ$Source <- "WQP"

overlap <- intersect(WQ$SiteDate, USGS$SiteDate) # 27, 588 obs.
# Remove the duplicates from WQP before binding it with the USGS data
WQ <- WQ[-which(WQ$SiteDate %in% overlap),]

SC <- rbind(USGS, WQ)

SC$SiteID <- factor(SC$SiteID)
levels(SC$SiteID) # Final number of sites from all sources is 25,624

duplicated <- SC[duplicated(SC$SiteDate),] # none! good

# Output this new, combined data
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
saveRDS(SC, "all_SC_data.rds")
