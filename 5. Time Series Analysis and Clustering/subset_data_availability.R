#################### Lauren Bolotin - bolotinljb@gmail.com ######################
# Identify data availability
# Subset data for availability requirements BEFORE averaging doy's across years
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
# Flow corrected:
dat <- readRDS("WUS_all_USGS_SC_Q_data.rds")
dat$SiteID <- factor(dat$SiteID)
levels(dat$SiteID) # 645

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
dat <- select(dat, c("SiteID", "Date", "SiteYear","Year", "doy","SpC_Qcms"))
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
saveRDS(sub, "WUS_USGS_SC_Q_availability_subset.rds")
rm(gap_summary)
class(sub$SiteID)
sub$SiteID <- factor(sub$SiteID)
levels(sub$SiteID) # 244
# NOTE: at this point, sub is a df with data only from site-years that had 219/365 days
# and gaps of four days or smaller

# Plot data availability
dat_availability <- select(sub, c("SiteID", "Year"))
dat_availability <- unique(dat_availability)
p <- ggplot(dat_availability)+
  geom_histogram(mapping = aes(x = as.numeric(as.character(Year)), fill = SiteID), binwidth = 1, color = "white")+
  labs(x = "Year", y = "n(Sites)", title = "Data Availability")
print(p)

p <- ggplot(sub)+
  geom_tile(mapping = aes(x = Year, y = SiteID))
print(p)





# Put data availability into a table
dat_availability_table <- plyr::count(dat_availability, vars = "Year")
dat_availability_table <- dat_availability_table[with(dat_availability_table, order(-freq)),]
head(dat_availability_table)

# Subset for 2016, 2017, 2018, 2019
sub$Year <- as.numeric(as.character(sub$Year)) 
sub2 <- filter(sub, Year > 2016)
sub2$SiteID <- factor(sub2$SiteID)
# See how many sites have all three of these years that meet the criteria (coverage & gaps)
dat_availability <- select(sub2, c("SiteID", "Year"))
dat_availability <- unique(dat_availability)

p <- ggplot(dat_availability)+
  geom_histogram(mapping = aes(x = as.numeric(as.character(Year)), fill = SiteID), binwidth = 1, color = "white")+
  labs(x = "Year", y = "n(Sites)", title = "Data Availability")
print(p)
# Subset for only sites that have all three years
sub2017 <- subset(sub2, sub2$Year == 2017)
sub2018 <- subset(sub2, sub2$Year == 2018)
sub2019 <- subset(sub2, sub2$Year == 2019)

sub1718 <- intersect(sub2017$SiteID, sub2018$SiteID)
sub1819 <- intersect(sub2018$SiteID, sub2019$SiteID)
sub171819 <- intersect(sub1718, sub1819)

sub3 <- subset(sub2, sub2$SiteID %in% sub171819)
sub3$SiteID <- factor(sub3$SiteID)
levels(sub3$SiteID)
#NOTE: only 8 sites have all three years
rm(sub2017, sub2018, sub2019, sub1718, sub1819, sub171819, dat_availability, dat_availability_table)

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
# Subset for sites of interest
meta <- subset(meta, meta$SiteID %in% sub$SiteID)
meta <- unique(meta)
# Plot
some.states <- c('california', 'nevada', 'utah', 'arizona', 'colorado', 'new mexico', 'texas', 'oregon', 'washington', 'montana', 'idaho', 'north dakota', 'south dakota', 'wyoming', 'kansas', 'nebraska', 'oklahoma', 'missouri', 'arkansas', 'iowa', 'minnesota', 'louisiana')
some.states.map <- map_data("state", region = some.states)
ggplot(some.states.map)+
  geom_polygon(mapping = aes(x = long, y = lat, group = group), fill = "grey", color = "white")+
  geom_point(meta, mapping = aes(x = Lon, y = Lat, color = "blue"))
# NOTE: yikes........maybe we can keep other years, flow normalize, and acknowledge
# that some years may be over-represented??















# For GBCO data:
dat <- readRDS("all_SC_data.rds")

# Add some important details/formatting #################################################
  # turn date into POSIXct format
dat$Date <- as.POSIXct(as.character(dat$Date), format = "%Y-%m-%d")
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
dat <- select(dat, c("SiteID", "Date", "SiteYear","Year", "doy","SpC", "Source"))
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
  check_gaps$logical <- check_gaps$SpC
  res <- rle(is.na(check_gaps$SpC))
  check_gaps$gaps <- rep(res$values*res$lengths,res$lengths)
  max <- max(check_gaps$gaps)
  gap_summary$max_gap[which(gap_summary$SiteYear == gap_summary$SiteYear[num])] <<- max
}

  # Create list and apply function to it: 
input_list <- seq(nrow(gap_summary))
lapply(input_list, quantify_gap)

  # Get rid of things we no longer need
rm(doys, dat_count, input_list, quantify_gap)

# Subset by gap size ##################################################################
gap_summary <- filter(gap_summary, max_gap <= 3)
sub <- filter(sub, SiteYear %in% gap_summary$SiteYear)
rm(gap_summary)
# NOTE: at this point, sub is a df with data only from site-years that had 219/365 days
      # and gaps of three days or smaller

  # Plot data availability
dat_availability <- select(sub, c("SiteID", "Year"))
dat_availability <- unique(dat_availability)
p <- ggplot(dat_availability)+
  geom_histogram(mapping = aes(x = as.numeric(as.character(Year)), fill = SiteID), binwidth = 1, color = "white")+
  labs(x = "Year", y = "n(Sites)", title = "Data Availability")
print(p)

  # Put data availability into a table
dat_availability_table <- plyr::count(dat_availability, vars = "Year")
dat_availability_table <- dat_availability_table[with(dat_availability_table, order(-freq)),]
head(dat_availability_table)

  # Subset for 2017, 2018, 2019
sub$Year <- as.numeric(as.character(sub$Year)) 
sub2 <- filter(sub, Year > 2016)
sub2$SiteID <- factor(sub2$SiteID)
  # See how many sites have all three of these years that meet the criteria (coverage & gaps)
dat_availability <- select(sub2, c("SiteID", "Year"))
dat_availability <- unique(dat_availability)

p <- ggplot(dat_availability)+
  geom_histogram(mapping = aes(x = as.numeric(as.character(Year)), fill = SiteID), binwidth = 1, color = "white")+
  labs(x = "Year", y = "n(Sites)", title = "Data Availability")
print(p)
  # Subset for only sites that have all three years
sub2017 <- subset(sub2, sub2$Year == 2017)
sub2018 <- subset(sub2, sub2$Year == 2018)
sub2019 <- subset(sub2, sub2$Year == 2019)

sub1718 <- intersect(sub2017$SiteID, sub2018$SiteID)
sub1819 <- intersect(sub2018$SiteID, sub2019$SiteID)
sub171819 <- intersect(sub1718, sub1819)

sub3 <- subset(sub2, sub2$SiteID %in% sub171819)
sub3$SiteID <- factor(sub3$SiteID)
levels(sub3$SiteID)
  #NOTE: only 9 sites have all three years
rm(sub2017, sub2018, sub2019, sub1718, sub1819, sub171819, dat_availability, dat_availability_table)

# Map sites to see spatial distribution ###############################################
  # Read in location data for USGS sites
meta <- readRDS("GBCO_SC_sites.rds")
  # Subset for important info
names(meta)
meta <- select(meta, c("Site_ID", "Lat", "Lon"))
  # Format columns
meta <- subset(meta, meta$Site_ID)
meta$Site_ID <- ifelse(meta$Site_ID < 10000000, paste0("0", meta$Site_ID), paste0(meta$Site_ID))
meta$Site_ID <- paste0("USGS-", meta$Site_ID)
colnames(meta)[1] <- "SiteID"
  # Subset for sites of interest
meta <- subset(meta, meta$SiteID %in% sub3$SiteID)
meta <- unique(meta)
  # Plot
some.states <- c('california', 'nevada', 'utah', 'arizona', 'colorado', 'new mexico')
some.states.map <- map_data("state", region = some.states)
ggplot(some.states.map)+
  geom_polygon(mapping = aes(x = long, y = lat, group = group), fill = "grey", color = "white")+
  geom_point(meta, mapping = aes(x = Lon, y = Lat, color = "blue"))
  # NOTE: yikes........maybe we can keep other years, flow normalize, and acknowledge
        # that some years may be over-represented??
  

  