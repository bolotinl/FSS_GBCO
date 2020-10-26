# We have formatted WQP time series data and metadata
# While formatting the metadata, we filtered sites for flowing waters
# We need to filter the time series data to include only these sites
# **So far, we have only done this for specific conductance, and will need to re-do it for flow and other water quality parameters if we decide to use them

# Set working directory and bring in data:
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
TS <- read.csv("WQP_TS.csv")
class(TS$SiteID)
TS$SiteID <- factor(TS$SiteID)
levels(TS$SiteID) # 64,130

setwd("/Volumes/Blaszczak Lab/FSS/All Data")
meta <- read.csv("WQP_formatted_metadata_WQ.csv") 
names(TS)
names(meta)

# Subset TS data accordingly for acceptable site types: 
TS_sub <- subset(TS, TS$SiteID %in% meta$SiteID)
TS_sub$SiteID <- factor(TS_sub$SiteID)
levels(TS_sub$SiteID) # 28,805

# Output a csv:
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
write.csv(TS_sub, "WQP_TS_final_sites.csv")
