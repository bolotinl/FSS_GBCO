# Create representative annual time series for each site
# This script: Flow-normalized SC data

rm(list=ls())
x <- c("tidyverse", "data.table", "lubridate", "zoo")
lapply(x, require, character.only = TRUE)
rm(x)

## Set the theme for our ggplots
theme_set(theme(legend.position = "none",panel.background = element_blank(), 
                axis.line = element_line(colour = "black")))

## Bring in all specific conductance data (flow-normalized)
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
dat <- readRDS("WUS_UNM_USGS_SC_Q_availability_subset.rds")

## Add some important details and format dataframe
dat$Date <- as.POSIXct(as.character(dat$Date), format = "%Y-%m-%d")
dat$Year <- year(dat$Date) 
dat$Year <- as.factor(dat$Year)
# Assign Julian Date (day of year or doy) to each observation
dat$doy <- strftime(dat$Date, format = "%j")
dat$doy <- as.numeric(as.character(dat$doy))
dat <- mutate(dat, SiteYear = paste(SiteID, Year, sep = " "))
dat$SpC_Qcms <- as.numeric(as.character(dat$SpC_Qcms))
dat$SiteID <- factor(dat$SiteID) # Make sure we get rid of factor levels for SiteIDs we aren't using

## Create Representative Annual Time Series - MEAN ############################################################################################################
## Mean
avg <- dat
avg$SpC_Qcms <- as.numeric(as.character(avg$SpC_Qcms))
avg <- avg %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "SpC_Qcms", .funs = c("mean" = mean))

## Check that this worked 
check <- subset(dat, dat$SiteID == "USGS-06036905" & dat$doy == 1)
check_avg <- mean(check$SpC_Qcms)
rm(check, check_avg) # all good

## Upper Quantile
quant75 <- function(x){
  x <- quantile(x, .75, na.rm = TRUE)
}

up_quart <- dat
up_quart <- up_quart %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "SpC_Qcms", .funs = c("upper_quart" = quant75))

## Median (Middle Quantile)
med <- dat
med <- med %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "SpC_Qcms", .funs = c("median" = median))

## Lower Quantile
quant25 <- function(x){
  x <- quantile(x, .25, na.rm = TRUE)
}

low_quart <- dat
low_quart <- low_quart %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "SpC_Qcms", .funs = c("lower_quart" = quant25))

# Example plot
p2 <- ggplot(subset(dat, dat$SiteID == "USGS-10133800"))+
  geom_line(mapping = aes(x = doy, y = SpC_Qcms, color = Year))+
  geom_line(subset(low_quart, up_quart$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = lower_quart), color = "black")+
  geom_line(subset(med, avg$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = median), color = "black")+
  geom_line(subset(up_quart, up_quart$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = upper_quart), color = "black")+
  geom_line(subset(avg, avg$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = mean), color = "red")
print(p2)

# Save dataframes with the averaged time series we created
saveRDS(low_quart, "WUS_UNM_SC_Q_low_quart.rds")
write.csv(low_quart, "WUS_UNM_SC_Q_low_quart.csv")

saveRDS(avg, "WUS_UNM_SC_Q_avg.rds") # This is what we will use for the final clustering
write.csv(avg, "WUS_UNM_SC_Q_avg.csv")

saveRDS(up_quart, "WUS_UNM_SC_Q_up_quart.rds")
write.csv(up_quart, "WUS_UNM_SC_Q_up_quart.csv")

saveRDS(med, "WUS_UNM_SC_Q_med.rds")
write.csv(med, "WUS_UNM_SC_Q_med.csv")

## Function to create TS plots for all sites
setwd("/Volumes/Blaszczak Lab/FSS/Figures/SingleTSPlots")
plotSpC_Qcms <- function(x){
  pdf(paste0("WUS_UNM_", x, "_singleTS_flowcorrected.pdf"))
  p <- ggplot(subset(dat, dat$SiteID == x))+
    theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    geom_line(mapping = aes(x = doy, y = SpC_Qcms, color = Year))+
    geom_line(subset(low_quart, up_quart$SiteID == x), mapping = aes(x = doy, y = lower_quart), color = "black")+
    geom_line(subset(med, avg$SiteID == x), mapping = aes(x = doy, y = median), color = "black")+
    geom_line(subset(up_quart, up_quart$SiteID == x), mapping = aes(x = doy, y = upper_quart), color = "black")+
    geom_line(subset(avg, avg$SiteID == x), mapping = aes(x = doy, y = mean), color = "red")
  print(p)
  dev.off()
}

# plotSpC(x)
dat$SiteID <- factor(dat$SiteID)
sites <- levels(dat$SiteID)
lapply(sites, plotSpC_Qcms) # already ran and saved PDFs
