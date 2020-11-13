# USGS Q Data EDA
# Bring in all USGS discharge data for HUC 14, 15, & 16
# Discharge data is daily average discharge

x <- c("tidyverse", "lubridate")
lapply(x, require, character.only = TRUE)
rm(x)

setwd("/Volumes/Blaszczak Lab/FSS/All Data")

# Bring in and use SC data to filter Q data for our sites of interest (and reduce the size of the dataframe so it doesn't slow down processing)
SC <- readRDS("all_SC_data.rds") 
head(SC)
head(Q)

# Bring in all daily discharge data for HUC 14, 15, 16
Q <- readRDS("USGS_disch_data.rds")
colnames(Q) # we need to keep all of these at first
# Format dataframe
colnames(Q)[2] <- "SiteID"
Q$SiteID <- paste0("USGS-", Q$SiteID)
Q$SiteDate <- paste0(Q$SiteID, " ", Q$Date)

#Q <- select(Q, c("site_no", "Date", 4, 5))
#colnames(Q) <- c("SiteID", 'Date',"Q", "dqi")



# Q$Date <- ymd(Q$Date)
# Q$Year <- year(Q$Date)
# Q$SiteDate <- paste0(Q$SiteID, " ", Q$Date)
Q$SiteDate <- as.factor(Q$SiteDate)

# Filter for site-dates where we have SC Data
Qsub <- Q[which(Q$SiteDate %in% SC$SiteDate),]
rm(Q) # get rid of this ASAP to speed things up
#dup <- Qsub[duplicated(Qsub$SiteDate),]
# Qsub <- Qsub[unique(Qsub$SiteDate),]
# Qsub <- unique(Qsub) # 562,899 -> stayed the same
#rm(dup)

# Get rid of unnecessary columns
names(Qsub)
Qsub <- select(Qsub, -c("X_.Data.from.10.1.1992.Forward._00060_00003", "X_.Data.from.10.1.1992.Forward._00060_00003_cd")) # These columns are empty
Qsub$X_00060_00003[which(Qsub$X_.Data.prior.to.10.1.1992._00060_00003 == 1040)] <- 1040 # These columns only have one value, so move it to another column
Qsub <- select(Qsub, -c("X_.Data.prior.to.10.1.1992._00060_00003", "X_.Data.prior.to.10.1.1992._00060_00003_cd")) # Then delete the columns
names(Qsub)
Qsub2 <- Qsub
Qsub2$X_00060_00003 <- ifelse(is.na(Qsub2$X_00060_00003), paste(Qsub2$X_..2.._00060_00003), paste(Qsub2$X_00060_00003))
Qsub2$X_00060_00003_cd <- ifelse(is.na(Qsub2$X_00060_00003_cd), paste(Qsub2$X_..2.._00060_00003_cd), paste(Qsub2$X_00060_00003_cd))
names(Qsub2)
Qsub2 <- select(Qsub2, -c("X_..2.._00060_00003", "X_..2.._00060_00003_cd"))
setdiff(Qsub2$X_..3.._00060_00003, Qsub2$X_00060_00003)
Qsub2 <- select(Qsub2, -c("X_..3.._00060_00003", "X_..3.._00060_00003_cd"))
names(Qsub2)
Qsub2 <- select(Qsub2, -c("agency_cd"))
colnames(Qsub2) <- c("SiteID", 'Date',"Q", "dqi","SiteDate")

# Filter for data that is approved for publication (dqi = A)
Qsub2$dqi <- as.factor(Qsub2$dqi)
levels(Qsub2$dqi)
Qsub2 <- subset(Qsub2, dqi == "A") # (~94%) 562,836 -> 527,187

Qsub2$SiteID <- factor(Qsub2$SiteID) # get rid of unused levels
levels(Qsub2$SiteID) # 1,392 sites with Q data and SC data

diff <- setdiff(SC$SiteID, Qsub2$SiteID)
diff <- as.data.frame(diff)

# Add a column for Q in cubic meters per second
colnames(Qsub2)[3] <- "Q_cfs"
sapply(Qsub2, class)
Qsub2$Q_cfs <- as.numeric(as.character(Qsub2$Q_cfs))

Qsub2$Q_cms <- Qsub2$Q_cfs * 0.028316846592

# Save data file
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
# saveRDS(Q, "USGS_disch_dqi.rds")
saveRDS(Qsub2, "USGS_disch_SubBySC_dqi.rds")
