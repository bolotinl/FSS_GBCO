rm(list=ls())
x <- c("tidyverse", "data.table", "lubridate", "zoo")
lapply(x, require, character.only = TRUE)
rm(x)

## Set the theme for our ggplots
theme_set(theme(legend.position = "none",panel.background = element_blank(), 
                axis.line = element_line(colour = "black")))

setwd("/Volumes/Blaszczak Lab/FSS/All Data")
# dat <- readRDS("all_SC_Q_data.rds")
# sites <- readRDS("TS_avg_site_selection.rds")
# sites$SiteID <- as.factor(sites$SiteID)
# sites <- levels(sites$SiteID)
# dat <- subset(dat, dat$SiteID %in% sites) # 407,672
# dat <- readRDS("WUS_USGS_SC_Q_availability_subset.rds")
dat <- readRDS("WUS_UNM_USGS_SC_Q_availability_subset.rds")


## Add some important details
dat$Date <- as.POSIXct(as.character(dat$Date), format = "%Y-%m-%d")
dat$Year <- year(dat$Date) 
dat$Year <- as.factor(dat$Year)
dat$doy <- strftime(dat$Date, format = "%j")
dat$doy <- as.numeric(as.character(dat$doy))
dat <- select(dat, -c("SiteDate"))
dat <- mutate(dat, SiteYear = paste(SiteID, Year, sep = " "))

dat$SpC_Qcms <- as.numeric(as.character(dat$SpC_Qcms))

# is.nan.data.frame <- function(x)
#   do.call(cbind, lapply(x, is.nan))
# 
# dat[is.nan(dat)] <- NA
## Rerun Quantile and Mean Code for Filtered Dataset ####
## Mean
avg <- dat
avg$SpC_Qcms <- as.numeric(as.character(avg$SpC_Qcms))
avg <- avg %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "SpC_Qcms", .funs = c("mean" = mean))

## Check that this worked 
# check <- subset(dat, dat$SiteID == "USGS-09014050" & dat$doy == 1)
# check_avg <- mean(check$SpC)
# rm(check, check_avg) # all good

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

p2 <- ggplot(subset(dat, dat$SiteID == "USGS-10133800"))+
  geom_line(mapping = aes(x = doy, y = SpC_Qcms, color = Year))+
  geom_line(subset(low_quart, up_quart$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = lower_quart), color = "black")+
  geom_line(subset(med, avg$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = median), color = "black")+
  geom_line(subset(up_quart, up_quart$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = upper_quart), color = "black")+
  geom_line(subset(avg, avg$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = mean), color = "red")
print(p2)
# saveRDS(low_quart, "SC_Q_low_quart.rds")
# saveRDS(avg, "SC_Q_avg.rds")
# saveRDS(up_quart, "SC_Q_up_quart.rds")
# saveRDS(med, "SC_Q_med.rds")
# saveRDS(low_quart, "WUS_SC_Q_low_quart.rds")
# saveRDS(avg, "WUS_SC_Q_avg.rds")
# saveRDS(up_quart, "WUS_SC_Q_up_quart.rds")
# saveRDS(med, "WUS_SC_Q_med.rds")
saveRDS(low_quart, "WUS_UNM_SC_Q_low_quart.rds")
saveRDS(avg, "WUS_UNM_SC_Q_avg.rds")
saveRDS(up_quart, "WUS_UNM_SC_Q_up_quart.rds")
saveRDS(med, "WUS_UNM_SC_Q_med.rds")

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
