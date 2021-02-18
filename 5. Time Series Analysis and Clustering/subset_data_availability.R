# Identify data availability
# Subset data for availability requirements BEFORE averaging across years
#################################################################################
# Bring in packages
rm(list=ls())
x <- c("tidyverse", "data.table", "lubridate", "zoo", "maps", "beepr")
lapply(x, require, character.only = TRUE)
rm(x)

# Set ggplot theme
theme_set(theme(legend.position = "none", panel.background = element_blank(), 
                axis.line = element_line(colour = "black")))

## Bring in data:
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
dat <- readRDS("WUS_UNM_all_USGS_SC_Q_data.rds")

dat$SiteID <- factor(dat$SiteID)
levels(dat$SiteID) # 652 sites

## Format data:
dat$Date <- ymd(dat$Date)
# create year column
dat$Year <- year(dat$Date) 
dat$Year <- as.factor(dat$Year)
# create column for day of year (doy)
dat$doy <- strftime(dat$Date, format = "%j")
dat$doy <- as.numeric(as.character(dat$doy))
# remove and add necessary columns
dat <- select(dat, -c("SiteDate"))
dat <- mutate(dat, SiteYear = paste(SiteID, Year, sep = " "))

# reorder columns
names(dat)
dat <- select(dat, c("SiteID", "Date", "SiteYear","Year", "doy", "SpC", "Q_cms","SpC_Qcms"))
dat <- dat[complete.cases(dat)]
head(dat)

# Quantify/Subset by annual coverage ###################################################
# Count how many days of the year each site-year has
dat_count <- plyr::count(dat, vars = "SiteYear")
# Subset for site-years with at least 60% (219 days) of data
dat_count <- filter(dat_count, freq >= 219)
sub <- subset(dat, dat$SiteYear %in% dat_count$SiteYear)

# Quantify gap sizes ###################################################################
# set up df for results of gap sizes by site-year
gap_summary <- levels(as.factor(sub$SiteYear)) %>%
  as.data.frame()
colnames(gap_summary) <- "SiteYear"
gap_summary$max_gap <- NA 

# create temporary df for the function we're about to create
doys <-  seq(1,365) %>%
  as.data.frame()
colnames(doys) <- "doys"

# create function for finding gap size
quantify_gap <- function(num){
  site <- gap_summary$SiteYear[num]
  ex_site <- filter(dat, SiteYear == print(paste(site)))
  check_gaps <- merge(doys, ex_site, by.x = "doys", by.y = "doy", all.x = TRUE) # This is where the code is breaking because this isn't the averaged year data
  check_gaps$logical <- check_gaps$SpC_Qcms
  res <- rle(is.na(check_gaps$SpC_Qcms))
  check_gaps$gaps <- rep(res$values*res$lengths,res$lengths)
  max <- max(check_gaps$gaps)
  gap_summary$max_gap[which(gap_summary$SiteYear == gap_summary$SiteYear[num])] <<- max
}

# Create list and apply function to it: 
input_list <- seq(nrow(gap_summary))
lapply(input_list, quantify_gap)
beep()
# Get rid of things we no longer need
rm(doys, dat_count, input_list, quantify_gap)

# Subset by gap size ##################################################################
gap_summary <- filter(gap_summary, max_gap <= 4)
sub <- filter(sub, SiteYear %in% gap_summary$SiteYear)
saveRDS(sub, "WUS_UNM_USGS_SC_Q_availability_subset.rds")
write.csv(sub, "WUS_UNM_USGS_SC_Q_availability_subset.csv")

setwd("/Volumes/Blaszczak Lab/FSS/All Data")
sub <- readRDS("WUS_UNM_USGS_SC_Q_availability_subset.rds")
rm(gap_summary)
class(sub$SiteID)
sub$SiteID <- factor(sub$SiteID)
levels(sub$SiteID) # 246 sites
# NOTE: at this point, sub is a df with data only from site-years that had 219/365 days
# and gaps of four days or smaller

# Plot data availability
dat_availability <- select(sub, c("SiteID", "Year"))
dat_availability <- unique(dat_availability)

# Load packages for fancy figures
library(wesanderson)
library(hrbrthemes)
library(gcookbook)

# HISTOGRAM nSites
pal <- wes_palette("Darjeeling1", 1, type = "discrete")

p <- ggplot(dat_availability, aes(x = as.numeric(as.character(Year)), y = ..count..))+
  geom_histogram(mapping = aes(x = as.numeric(as.character(Year))), binwidth = 1, color = 'white', fill = wes_palette("Zissou1", 1, "discrete"))+
  labs(x = "Year", y = "Number of Sites", title = "Data Availability: # of Sites per Year with Data", subtitle = 'n = 244 sites')+
  theme_ipsum()+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5, face = "bold"))+
  theme(axis.title.x = element_text(hjust = 0.5, size = 14, face = "bold"))+
  theme(axis.title.y = element_text(hjust = 0.5, size = 14, face = "bold"))+
  theme(axis.text = element_text(size = 18, face = "bold"))
    

print(p)


# HISTOGRAM nYears
sub2 <- sub %>%
  group_by(SiteID, Year) %>%
  summarise_at(.vars = "SpC", .funs = c("mean" = mean))
sub2 <- select(sub2, -c("mean"))
sub3 <- sub2 %>%
  group_by(SiteID) %>%
  dplyr::count()

p <- ggplot(sub3, aes(x = n, y = ..count..))+
  geom_histogram(mapping = aes(x = n), binwidth = 1, fill = wes_palette("Zissou1", 1, "discrete"), colour = 'white')+
  labs(x = "Number of Years of Data", y = "Number of Sites", title = "Data Availability: # of Years of Data per Site", subtitle = "n = 244 sites")+
  theme_ipsum()+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5, face = "bold"))+
  theme(axis.title.x = element_text(hjust = 0.5, size = 14, face = "bold"))+
  theme(axis.title.y = element_text(hjust = 0.5, size = 14, face = "bold"))+
  theme(axis.text = element_text(size = 18, face = "bold"))
print(p)

# Map sites to see spatial distribution ###############################################
# Read in location data for USGS sites
meta <- readRDS("WUS_USGS_disch_SC_sites.rds")
# Subset for important info
names(meta)
meta <- select(meta, c("Site_ID", "Lat", "Lon"))

# Format columns
meta$Site_ID <- as.numeric(as.character(meta$Site_ID))
meta$Site_ID <- ifelse(meta$Site_ID < 10000000, paste0("0", meta$Site_ID), paste0(meta$Site_ID))
meta$Site_ID <- paste0("USGS-", meta$Site_ID)
colnames(meta)[1] <- "SiteID"

# Add in UNM data (coordinates for Cochiti and Lyden)
id <- c("UNM-cochiti", "UNM-lyden")
lat <- c(35.40207, 36.84583)
long <- c(-106.18230, -105.59528)

unm_meta <- cbind(id, lat)
unm_meta <- cbind(unm_meta, long)
unm_meta <- as.data.frame(unm_meta)
colnames(unm_meta) <- c("SiteID", "Lat", "Lon")
meta <- rbind(meta, unm_meta)
rm(id, lat, long, unm_meta)

# Subset for sites of interest
meta <- subset(meta, meta$SiteID %in% sub$SiteID)
meta <- unique(meta)
sapply(meta, class)
meta$Lat <- as.numeric(as.character(meta$Lat))
meta$Lon <- as.numeric(as.character(meta$Lon))

# Save new metadata file
getwd()
saveRDS(meta, "WUS_UNM_USGS_disch_SC_sites.rds")
write.csv(meta, "WUS_UNM_USGS_disch_SC_sites.csv")

# NOTE: The next 6 lines are to color the map points by which cluster our sites end up in. This required completing the clustering and coming back to this script. 
# Bring in the cluster results (2 clusters)
clust <- readRDS("/Volumes/Blaszczak Lab/FSS/FSS_clustering/Cluster Plots and Results/cluster_results_2cl_SCQ.rds")
head(clust)
head(meta)
meta <- merge(meta, clust, by = "SiteID")
class(meta$Cluster)
meta$Cluster <- factor(as.character(meta$Cluster))

## Plot the Map
# Set up the US map in the background:
some.states <- c('california', 'nevada', 'utah', 'arizona', 'colorado', 'new mexico', 'texas', 'oregon', 'washington', 'montana', 'idaho', 'north dakota', 'south dakota', 'wyoming', 'kansas', 'nebraska', 'oklahoma', 'missouri', 'arkansas', 'iowa', 'minnesota', 'louisiana')
some.states.map <- map_data("state", region = some.states)

# Set up the actual plotting of the points on top of the map:
# I left some commented out lines in case I decide to add those bits of formatting back in down the line
ggplot(some.states.map)+
  geom_polygon(mapping = aes(x = long, y = lat, group = group), fill = "grey85", color = "white")+
  geom_point(meta, mapping = aes(x = Lon, y = Lat, color = Cluster), size = 1)+
  theme(panel.background = element_blank(), legend.position = 'none', 
        #axis.title = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5), axis.line = element_blank())+
  # These lines are for formatting the title and subtitle if we want to include them back in:
  # labs(title = "Concurrent USGS SC and Q Sites")+
  #labs(subtitle = "n = 246)
  theme_ipsum()+
  # These lines are for formatting of axes if we want to include them back in:
  # theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5, face = "bold"))+
  # theme(axis.title.x = element_text(hjust = 0.5, size = 14, face = "bold"))+
  # theme(axis.title.y = element_text(hjust = 0.5, size = 14, face = "bold"))+
  # theme(axis.text = element_text(size = 18, face = "bold"))+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  theme(axis.title.y = element_blank(), axis.text.y = element_blank())+
  ylab("Latitude")+
  xlab("Longitude")+
  theme(panel.grid.minor  = element_line(color = "white"), panel.grid.major = element_line(color = "white"))+
  scale_x_continuous(position = "top")+
  scale_color_manual(values = c("#3B9AB2", "#E1AF00"), guide = guide_legend(title.position = "top", title.hjust = 0.5))+ # Manually set colors from the wesanderson "Zissou1" palette so we can choose two colors that are somewhat intuitive (i.e. the default was red and blue which doesn't seem like an appropriate representation of what the colors represent in this map)
  theme(legend.position = c(0.15, 0.15), legend.text = element_text(size = 14, face = "bold"), legend.title = element_text(size = 14, face = "bold"), 
        legend.margin = margin(0,0,0,0))+
  theme(plot.margin = margin(20,0,0,0))

# wesanderson palette Zissou 1: values = c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00")
  