# Create annual time series representative of each site across years 
# This script: specific conductance data only

## Bring in packages
rm(list=ls())
x <- c("tidyverse", "data.table", "lubridate", "zoo")
lapply(x, require, character.only = TRUE)
rm(x)

## Set the theme for our ggplots
theme_set(theme(legend.position = "none",panel.background = element_blank(), 
                axis.line = element_line(colour = "black")))

## Bring in all specific conductance data (not flow-normalized)
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
dat <- readRDS("WUS_UNM_USGS_SC_Q_availability_subset.rds")

## Format dataframe and add some important details
sapply(dat, class)
dat$Date <- as.POSIXct(as.character(dat$Date), format = "%Y-%m-%d")
dat$Year <- year(dat$Date) 
dat$Year <- as.factor(dat$Year)
# Create column with the Julian Date (day of year or "doy")
dat$doy <- strftime(dat$Date, format = "%j")
dat$doy <- as.numeric(as.character(dat$doy))
dat <- select(dat, -c("SiteDate"))
dat <- mutate(dat, SiteYear = paste(SiteID, Year, sep = " "))
dat$SiteID <- factor(dat$SiteID) # 246 sites

## Create Representative Annual Time Series - MEAN ############################################################################################################
## Make a copy of dat (since we will be using other approaches for this in addition to MEAN (i.e. Median, Upper Quartile, etc.))
avg <- dat
## Average across all values for each day of the year (doy) to get one representative time series BEFORE subsetting (as opposed to after, which reduced available data)
avg <- avg %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "SpC", .funs = c("mean" = mean))

## Check that this worked 
check <- subset(dat, dat$SiteID == "USGS-06036905" & dat$doy == 1)
check_avg <- mean(check$SpC)
mean(c(509, 572))
rm(check, check_avg) # all good

## See how many doy's of data each site has
avg_count <- table(avg$SiteID) %>%
  as.data.frame()

sapply(avg, class)
colnames(avg) <- c("SiteID", "doy", "mean_SpC")

## Save the averaged data to a dataframe, this is what we will use for clustering
saveRDS(avg, "WUS_UNM_SC_avg.rds")
write.csv(avg, "WUS_UNM_SC_avg.csv")

## Create Representative Annual Time Series - UPPER QUANTILE ############################################################################################################
## Create function for creating upper quantile dataframe similar to the 'avg' dataframe
quant75 <- function(x){
  x <- quantile(x, .75)
}

## Copy dat to up_quart
up_quart <- dat

## Across all SC values for a specific doy at a site, take the 75th percentile 
up_quart <- up_quart %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "SpC", .funs = c("upper_quart" = quant75))

## Check that this worked
check <- subset(dat, dat$SiteID == "USGS-06894000" & dat$doy == 1)
check_quant <- quantile(check$SpC, .75)
# # all good, check one more time

## Create Representative Annual Time Series - MEDIAN ############################################################################################################
## Copy dat to med
med <- dat
## Across all SC values for a specific doy, take the median
med <- med %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "SpC", .funs = c("median" = median))

check <- subset(dat, dat$SiteID == "USGS-06894000" & dat$doy == 1)
check_med <- median(check$SpC)
rm(check, check_med)

## Create Representative Annual Time Series - LOWER QUANTILE ############################################################################################################
## Create function for creating lower quantile dataframe similar to the 'avg' dataframe
quant25 <- function(x){
  x <- quantile(x, .25)
}

low_quart <- dat
low_quart <- low_quart %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "SpC", .funs = c("lower_quart" = quant25))

check <- subset(dat, dat$SiteID == "USGS-06894000" & dat$doy == 1)
check_quant <- quantile(check$SpC, .25)

## Plot all data with all quantiles/mean overlain #################################################################################################################
  # Just quantiles
    ggplot(subset(dat, dat$SiteID == "USGS-10133800"))+
  labs(y = "SpC")+
  geom_line(subset(low_quart, up_quart$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = lower_quart), color = "black")+
  geom_line(subset(med, avg$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = median), color = "gray41")+
  geom_line(subset(up_quart, up_quart$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = upper_quart), color = "grey66")

  # All data + quantiles + mean
  dat$Year <- as.numeric(as.character(dat$Year))
  dat$Year <- as.factor(as.character(dat$Year))
  p1 <- ggplot(subset(dat, dat$SiteID == "USGS-10133800"))+
  geom_line(mapping = aes(x = doy, y = SpC, color = Year))+
  geom_line(subset(low_quart, up_quart$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = lower_quart), color = "black")+
  geom_line(subset(med, avg$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = median), color = "black")+
  geom_line(subset(up_quart, up_quart$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = upper_quart), color = "black")+
  geom_line(subset(avg, avg$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = mean_SpC), color = "red")
  print(p1)

## Export any other data you want to use for clustering (since we only exported mean data above)
  saveRDS(low_quart, "WUS_UNM_SC_low_quart.rds")
  write.csv(low_quart, "WUS_UNM_SC_low_quart.csv")
  
  saveRDS(up_quart, "WUS_UNM_SC_up_quart.rds")
  write.csv(up_quart, "WUS_UNM_SC_up_quart.csv")
  
  saveRDS(med, "WUS_UNM_SC_med.rds")
  write.csv(med, "WUS_UNM_SC_med.csv")
  
# Iterate through all sites to make the plot of all data + quantiles + mean #####################################################################################
### Code for creating PDFs of plots in R: 
  ## For one site:
  # pdf("rplot.pdf") 
  # # 2. Create a plot
  # plot(x = my_data$wt, y = my_data$mpg,
  #      pch = 16, frame = FALSE,
  #      xlab = "wt", ylab = "mpg", col = "#2E9FDF")
  # # Close the pdf file
  # dev.off()

# Set the working directory to where we want to save the plots
setwd("/Volumes/Blaszczak Lab/FSS/Figures/SingleTSPlots")
# x <- "USGS-09034500" # assign to x to test out the function below
# Create a site list that the plotting function will run through to create one plot per site
avg$SiteID <- factor(avg$SiteID)
sites <- levels(avg$SiteID)
## Function for multiple sites (and all available data)
  plotSpC <- function(x){
    pdf(paste0("WUS_UNM_", x, "_singleTS.pdf"))
    p <- ggplot(subset(dat, dat$SiteID == x))+
      theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"))+
      geom_line(mapping = aes(x = doy, y = SpC, color = Year))+
      geom_line(subset(low_quart, up_quart$SiteID == x), mapping = aes(x = doy, y = lower_quart), color = "black")+
      geom_line(subset(med, avg$SiteID == x), mapping = aes(x = doy, y = median), color = "black")+
      geom_line(subset(up_quart, up_quart$SiteID == x), mapping = aes(x = doy, y = upper_quart), color = "black")+
      geom_line(subset(avg, avg$SiteID == x), mapping = aes(x = doy, y = mean_SpC), color = "red")
    print(p)
    dev.off()
  }

  # plotSpC(x) # For testing the function on one site
  lapply(sites, plotSpC)
  
  