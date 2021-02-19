# Create annual time series representative of each site across years 
# This script: discharge data only

## Bring in packages
rm(list=ls())
x <- c("tidyverse", "data.table", "lubridate", "zoo")
lapply(x, require, character.only = TRUE)
rm(x)

## Set the theme for our ggplots
theme_set(theme(legend.position = "none",panel.background = element_blank(), 
                axis.line = element_line(colour = "black")))

setwd("/Volumes/Blaszczak Lab/FSS/All Data")
dat <- readRDS("WUS_UNM_USGS_SC_Q_availability_subset.rds")


## Format dataframe and add important details
dat$Date <- as.POSIXct(as.character(dat$Date), format = "%Y-%m-%d")
dat$Year <- year(dat$Date) 
dat$Year <- as.factor(dat$Year)
# Create column with the Julian Date (day of year or "doy")
dat$doy <- strftime(dat$Date, format = "%j")
dat$doy <- as.numeric(as.character(dat$doy))
dat <- mutate(dat, SiteYear = paste(SiteID, Year, sep = " "))
# Get rid of unused factor levels for SiteID
dat$SiteID <- factor(dat$SiteID) # 246 sites

## MEAN #########
avg <- dat
avg <- avg %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "Q_cms", .funs = c("mean" = mean))

## Upper Quantile
quant75 <- function(x){
  x <- quantile(x, .75)
}

up_quart <- dat
up_quart <- up_quart %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "Q_cms", .funs = c("upper_quart" = quant75))

## Median (Middle Quantile)
med <- dat
med <- med %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "Q_cms", .funs = c("median" = median))

## Lower Quantile
quant25 <- function(x){
  x <- quantile(x, .25)
}

low_quart <- dat
low_quart <- low_quart %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "Q_cms", .funs = c("lower_quart" = quant25))

p2 <- ggplot(subset(dat, dat$SiteID == "USGS-10133800"))+
  geom_line(mapping = aes(x = doy, y = Q_cms, color = Year))+
  geom_line(subset(low_quart, up_quart$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = lower_quart), color = "black")+
  geom_line(subset(med, avg$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = median), color = "black")+
  geom_line(subset(up_quart, up_quart$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = upper_quart), color = "black")+
  geom_line(subset(avg, avg$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = mean), color = "red")
print(p2)


## Rerun Code to Make PDF's of All Plots of All Data + Quantile + Mean
setwd("/Volumes/Blaszczak Lab/FSS/Figures/SingleTSPlots")
plotSpC <- function(x){
  pdf(paste0("WUS_UNM_", x, "_singleTS_flow_only.pdf"))
  p <- ggplot(subset(dat, dat$SiteID == x))+
    theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    geom_line(mapping = aes(x = doy, y = Q_cms, color = Year))+
    geom_line(subset(low_quart, up_quart$SiteID == x), mapping = aes(x = doy, y = lower_quart), color = "black")+
    geom_line(subset(med, avg$SiteID == x), mapping = aes(x = doy, y = median), color = "black")+
    geom_line(subset(up_quart, up_quart$SiteID == x), mapping = aes(x = doy, y = upper_quart), color = "black")+
    geom_line(subset(avg, avg$SiteID == x), mapping = aes(x = doy, y = mean), color = "red")
  print(p)
  dev.off()
}

dat$SiteID <- factor(dat$SiteID)
sites <- levels(dat$SiteID)

## Test for one site
# x <- "USGS-09014050"
# plotSpC(x)
# rm(x)

## Apply to all sites
lapply(sites, plotSpC) # already ran and saved PDFs

## Save data files
saveRDS(low_quart, "WUS_UNM_Q_low_quart.rds")
write.csv(low_quart, "WUS_UNM_Q_low_quart.csv")

saveRDS(avg, "WUS_UNM_Q_avg.rds")
write.csv(avg, "WUS_UNM_Q_avg.csv")

saveRDS(up_quart, "WUS_UNM_Q_up_quart.rds")
write.csv(up_quart, "WUS_UNM_Q_up_quart.csv")

saveRDS(med, "WUS_UNM_Q_med.rds")
write.csv(med, "WUS_UNM_Q_med.csv")
