# USGS Q Data Exploration

# Goal:  Filter sites with discharge data for those that also have specific conductance data


# Bring in all USGS discharge data
# Discharge data is daily average discharge
x <- c("tidyverse", "lubridate")
lapply(x, require, character.only = TRUE)
rm(x)

setwd("/Volumes/Blaszczak Lab/FSS/All Data")

# Bring in SC data (daily value data): 
SC <- readRDS("WUS_USGS_SC_dv_dqi.rds")
colnames(SC)

# Bring in Q data (daily value data):
Q <- readRDS("WUS_all_USGS_disch_dv_data.rds")

# Format df
colnames(Q)
Q$agency_cd <- as.factor(Q$agency_cd)
levels(Q$agency_cd)
Q <- select(Q, -c("agency_cd"))
colnames(Q) <- c("SiteID", "Date", "Q", "dqi")

# Prepare to filter Q sites by SC sites
Q$SiteDate <- paste(Q$SiteID, Q$Date, sep = " ")
SC$SiteDate <- paste(SC$SiteID, SC$Date, sep = " ")
Q$SiteDate <- as.factor(as.character(Q$SiteDate))
SC$SiteDate <- as.factor(as.character(SC$SiteDate))
Qsub <- filter(Q, SiteDate %in% SC$SiteDate)

# Add a column for Q in cubic meters per second (currently it is in cubic feet per second)
colnames(Qsub)[3] <- "Q_cfs" 
Qsub$Q_cms <- Qsub$Q_cfs * 0.028316846592

# Save data file:
saveRDS(Qsub, "WUS_USGS_disch_sub_sites_by_SC_dqi.rds")
write.csv(Qsub, "WUS_USGS_disch_sub_sites_by_SC_dqi.csv")

