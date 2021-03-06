#################### Lauren Bolotin - bolotinljb@gmail.com ######################
# Visualize what multiple years of data at one sight looks like plotted on top of each other
# Create "averaged" (or quantile) time series of specific conductance for each site of interest
#################################################################################
### NOTE: currently, this script dismisses the subset_data_availability.R script
### this script has it's own section for subsetting data, but it does it AFTER
### averaging across years and creating representative time series whereas the other 
### script subsets data first and the output would be assigned directly to avg in this script
## Bring in necessary packages
rm(list=ls())
x <- c("tidyverse", "data.table", "lubridate", "zoo")
lapply(x, require, character.only = TRUE)
rm(x)

## Set the theme for our ggplots
theme_set(theme(legend.position = "none",panel.background = element_blank(), 
                axis.line = element_line(colour = "black")))

## Bring in all specific conductance data
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
# dat <- readRDS("all_SC_Q_data.rds")
dat <- readRDS("all_SC_Q_data_v2.rds")
sapply(dat, class)

## Add some important details
dat$Date <- as.POSIXct(as.character(dat$Date), format = "%Y-%m-%d")
dat$Year <- year(dat$Date) 
dat$Year <- as.factor(dat$Year)
dat$doy <- strftime(dat$Date, format = "%j")
dat$doy <- as.numeric(as.character(dat$doy))
dat <- select(dat, -c("SiteDate"))
dat <- mutate(dat, SiteYear = paste(SiteID, Year, sep = " "))

## NOT FLOW CORRECTED ####################
## Example of Mean Time Series #####
## Make a copy of dat
#avg <- dat
avg <- dat
## Average across all values for each day of the year (doy) to get one representative time series
avg <- avg %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "SpC", .funs = c("mean" = mean))

## Check that this worked 
# check <- subset(dat, dat$SiteID == "USGS-09014050" & dat$doy == 1)
# check_avg <- mean(check$SpC)
# rm(check, check_avg) # all good

# See how many doy's of data each site has
avg_count <- table(avg$SiteID) %>%
  as.data.frame()

## Begin by subsetting by sites that have >= 60% of the 365 day year
avg_count <- filter(avg_count, Freq >= 219)

## Subset the averaged data by this new site selection
avg <- filter(avg, SiteID %in% avg_count$Var1)
avg$SiteID <- factor(avg$SiteID)
levels(avg$SiteID) # 263 sites when we increase the minimum required days  and average before subsetting, as opposed to 85 when we required >= 350 days and averaged across doy's after subsetting

## Take a look at the gaps to get an idea of how big they might be
## To see this, we want to create this df:
gap_summary <- levels(avg$SiteID) %>%
  as.data.frame()
colnames(gap_summary) <- "SiteID"
gap_summary$max_gap <- NA

# create temporary df for calculating gap size
doys <-  seq(1,365) %>%
  as.data.frame()
colnames(doys) <- "doys"

quantify_gap <- function(num){
site <- gap_summary$SiteID[num]
ex_site <- filter(avg, SiteID == print(paste(site)))
check_gaps <- merge(doys, ex_site, by.x = "doys", by.y = "doy", all.x = TRUE)
check_gaps$logical <- check_gaps$mean
res <- rle(is.na(check_gaps$mean))
check_gaps$gaps <- rep(res$values*res$lengths,res$lengths)
max <- max(check_gaps$gaps)
gap_summary$max_gap[which(gap_summary$SiteID == gap_summary$SiteID[num])] <<- max
}

## Test on one site:
# quantify_gap(8)

## Create list and apply function to it:
input_list <- seq(nrow(gap_summary))
lapply(input_list, quantify_gap)

## For now, subset the data by the sites that have >= 219 days of data and =< 3 day gaps
gap_summary <- filter(gap_summary, max_gap <= 3)
avg <- filter(avg, SiteID %in% gap_summary$SiteID)

# Try plotting data:
ggplot(subset(dat, dat$SiteID == "USGS-09085150"))+
  geom_line(mapping = aes(x = doy, y = SpC, color = Year))+
  geom_line(subset(avg, avg$SiteID == "USGS-09085150"), mapping = aes(x = doy, y = mean), color = "black")


## Create a heatmap to show the years covered at each site
# subset all_SC_data.rds for the sites used to make averaged TS
sub <- subset(dat, dat$SiteID %in% avg$SiteID)

sapply(sub, class)
sub$Year <- as.numeric(as.character(sub$Year))

ggplot(sub)+
  geom_tile(mapping = aes(x = Year, y = SiteID))

ggplot(sub)+
  geom_histogram(mapping = aes(x = Year), binwidth = 1, color = "white")

sub_plot <- dplyr::select(sub, c("SiteID", "Year"))
sub_plot <- unique(sub_plot)
site_count <- plyr::count(df = sub_plot, vars = "Year")

ggplot(sub_plot)+
  geom_col(mapping = aes(x = Year, y = nlevels(SiteID)))+
  xlim(1975, 2020)



## Format the avg dataframe for input to the clustering code
## Look to this file for guidance
Example_avgTS_PSavoy <- readRDS("/Volumes/Blaszczak Lab/FSS/All Data/Example_avgTS_PSavoy.rds")
sapply(Example_avgTS_PSavoy, class)

sapply(avg, class)
colnames(avg) <- c("SiteID", "doy", "mean_SpC")

## Save the averaged data to a dataframe
#saveRDS(avg, "SC_avg.rds")
saveRDS(avg, "SC_subset_avg.rds")

#####

## Example Plots of Upper Quantile Time Series ####
quant75 <- function(x){
  x <- quantile(x, .75)
}

up_quart <- dat
up_quart <- up_quart %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "SpC", .funs = c("upper_quart" = quant75))

# check <- subset(dat, dat$SiteID == "USGS-09014050" & dat$doy == 1)
# check_quant <- quantile(check$SpC, .75)
# # all good, check one more time
# 
# check <- subset(dat, dat$SiteID == "USGS-09306500" & dat$doy == 121)
# check_quant <- quantile(check$SpC, .75)
# # all good

ggplot(subset(dat, dat$SiteID == "USGS-10133800"))+
  geom_line(mapping = aes(x = doy, y = SpC, color = Year))+
  geom_line(subset(up_quart, up_quart$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = upper_quart), color = "black")

## Example Plots of Median (Middle Quartile) Time Series ####
med <- dat
med <- med %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "SpC", .funs = c("median" = median))

# check <- subset(dat, dat$SiteID == "USGS-09014050" & dat$doy == 1)
# check_med <- median(check$SpC)
# rm(check, check_med)

ggplot(subset(dat, dat$SiteID == "USGS-10133800"))+
  geom_line(mapping = aes(x = doy, y = SpC, color = Year))+
  geom_line(subset(med, avg$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = median), color = "black")

## Example Plots of Lower Quartile Time Series ####
quant25 <- function(x){
  x <- quantile(x, .25)
}

low_quart <- dat
low_quart <- low_quart %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "SpC", .funs = c("lower_quart" = quant25))

# check <- subset(dat, dat$SiteID == "USGS-09014050" & dat$doy == 1)
# check_quant <- quantile(check$SpC, .25)
# # all good, check one more time
# 
# check <- subset(dat, dat$SiteID == "USGS-09306500" & dat$doy == 121)
# check_quant <- quantile(check$SpC, .25)
# # all good

ggplot(subset(dat, dat$SiteID == "USGS-10133800"))+
  geom_line(mapping = aes(x = doy, y = SpC, color = Year))+
  geom_line(subset(low_quart, up_quart$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = lower_quart), color = "black")

## Plot all data with all quantiles overlain ####
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
  geom_line(subset(avg, avg$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = mean), color = "red")

saveRDS(low_quart, "SC_low_quart.rds")
saveRDS(up_quart, "SC_up_quart.rds")
saveRDS(med, "SC_med.rds")

# Iterate through all sites to make the plot of all data + quantiles + mean
### Code for creating PDFs of plots in R: 
## For one site:
# pdf("rplot.pdf") 
# # 2. Create a plot
# plot(x = my_data$wt, y = my_data$mpg,
#      pch = 16, frame = FALSE,
#      xlab = "wt", ylab = "mpg", col = "#2E9FDF")
# # Close the pdf file
# dev.off()

# setwd("/Volumes/Blaszczak Lab/FSS/Figures/SingleTSPlots")
# x <- "USGS-09034500"
# sites <- levels(dat$SiteID) 

## Function for multiple sites (and all available data)
# plotSpC <- function(x){
#   pdf(paste0(x, "_singleTS.pdf"))
#   p <- ggplot(subset(dat, dat$SiteID == x))+
#     theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"))+
#     geom_line(mapping = aes(x = doy, y = SpC, color = Year))+
#     geom_line(subset(low_quart, up_quart$SiteID == x), mapping = aes(x = doy, y = lower_quart), color = "black")+
#     geom_line(subset(med, avg$SiteID == x), mapping = aes(x = doy, y = median), color = "black")+
#     geom_line(subset(up_quart, up_quart$SiteID == x), mapping = aes(x = doy, y = upper_quart), color = "black")+
#     geom_line(subset(avg, avg$SiteID == x), mapping = aes(x = doy, y = mean), color = "red")
#   print(p)
#   dev.off()
# }

# plotSpC(x)
# lapply(sites, plotSpC)

# Bring in list of sites with continuous SC data
dat$SiteID <- factor(dat$SiteID)
levels(dat$SiteID) # 85 sites

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

dat[is.nan(dat)] <- NA

## THIS IS FLOW CORRECTED ###############################
#dat$Spc_Qcms <- ifelse(dat$Q_cfs == 0.00, paste("0"), paste(dat$Spc_Qcms))
dat$Spc_Qcms <- as.numeric(as.character(dat$Spc_Qcms))

## Rerun Quantile and Mean Code for Filtered Dataset ####
## Mean
avg <- dat
avg$Spc_Qcms <- as.numeric(as.character(avg$Spc_Qcms))
avg <- avg %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "Spc_Qcms", .funs = c("mean" = mean))

## Check that this worked 
# check <- subset(dat, dat$SiteID == "USGS-09014050" & dat$doy == 1)
# check_avg <- mean(check$SpC)
# rm(check, check_avg) # all good

# See how many doy's of data each site has
avg_count <- table(avg$SiteID) %>%
  as.data.frame()

## Begin by subsetting by sites that have >= 60% of the 365 day year
avg_count <- filter(avg_count, Freq >= 219)

## Subset the averaged data by this new site selection
avg <- filter(avg, SiteID %in% avg_count$Var1)
avg$SiteID <- factor(avg$SiteID)
levels(avg$SiteID) # 207 sites when we increase the minimum required days  and average before subsetting, as opposed to 85 when we required >= 350 days and averaged across doy's after subsetting

## Take a look at the gaps to get an idea of how big they might be
## To see this, we want to create this df:
gap_summary <- levels(avg$SiteID) %>%
  as.data.frame()
colnames(gap_summary) <- "SiteID"
gap_summary$max_gap <- NA

# create temporary df for calculating gap size
doys <-  seq(1,365) %>%
  as.data.frame()
colnames(doys) <- "doys"

quantify_gap <- function(num){
  site <- gap_summary$SiteID[num]
  ex_site <- filter(avg, SiteID == print(paste(site)))
  check_gaps <- merge(doys, ex_site, by.x = "doys", by.y = "doy", all.x = TRUE)
  check_gaps$logical <- check_gaps$mean
  res <- rle(is.na(check_gaps$mean))
  check_gaps$gaps <- rep(res$values*res$lengths,res$lengths)
  max <- max(check_gaps$gaps)
  gap_summary$max_gap[which(gap_summary$SiteID == gap_summary$SiteID[num])] <<- max
}

## Test on one site:
# quantify_gap(8)

## Create list and apply function to it:
input_list <- seq(nrow(gap_summary))
lapply(input_list, quantify_gap)

## For now, subset the data by the sites that have >= 219 days of data and =< 3 day gaps
gap_summary <- filter(gap_summary, max_gap <= 3)
avg <- filter(avg, SiteID %in% gap_summary$SiteID)

# Try plotting data:
ggplot(subset(dat, dat$SiteID == "USGS-09014050"))+
  geom_line(mapping = aes(x = doy, y = Spc_Qcms, color = Year))+
  geom_line(subset(avg, avg$SiteID == "USGS-09014050"), mapping = aes(x = doy, y = mean), color = "black")


## Create a heatmap to show the years covered at each site
# subset all_SC_data.rds for the sites used to make averaged TS
sub <- subset(dat, dat$SiteID %in% avg$SiteID)

sapply(sub, class)
sub$Year <- as.numeric(as.character(sub$Year))

ggplot(sub)+
  geom_tile(mapping = aes(x = Year, y = SiteID))

ggplot(sub)+
  geom_histogram(mapping = aes(x = Year), binwidth = 1, color = "white")

sub_plot <- dplyr::select(sub, c("SiteID", "Year"))
sub_plot <- unique(sub_plot)
site_count <- plyr::count(df = sub_plot, vars = "Year")

ggplot(sub_plot)+
  geom_col(mapping = aes(x = Year, y = nlevels(SiteID)))+
  xlim(1975, 2020)

Example_avgTS_PSavoy <- readRDS("/Volumes/Blaszczak Lab/FSS/All Data/Example_avgTS_PSavoy.rds")
sapply(Example_avgTS_PSavoy, class)

sapply(avg, class)
colnames(avg) <- c("SiteID", "doy", "mean_SpC")

## Save the averaged data to a dataframe
#saveRDS(avg, "SC_avg.rds")
saveRDS(avg, "SC_Q_avg.rds")





## Upper Quantile
up_quart <- dat
up_quart <- up_quart %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "Spc_Qcms", .funs = c("upper_quart" = quant75))

## Median (Middle Quatile)
med <- dat
med <- med %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "Spc_Qcms", .funs = c("median" = median))

## Lower Quantile
low_quart <- dat
low_quart <- low_quart %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "Spc_Qcms", .funs = c("lower_quart" = quant25))

p2 <- ggplot(subset(dat, dat$SiteID == "USGS-10133800"))+
  geom_line(mapping = aes(x = doy, y = Spc_Qcms, color = Year))+
  geom_line(subset(low_quart, up_quart$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = lower_quart), color = "black")+
  geom_line(subset(med, avg$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = median), color = "black")+
  geom_line(subset(up_quart, up_quart$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = upper_quart), color = "black")+
  geom_line(subset(avg, avg$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = mean), color = "red")
print(p2)
# saveRDS(low_quart, "SC_Q_low_quart.rds")
# saveRDS(avg, "SC_Q_avg.rds")
# saveRDS(up_quart, "SC_Q_up_quart.rds")
# saveRDS(med, "SC_Q_med.rds")
saveRDS(avg, "SC_Q_avg_v2.rds")


## Rerun Code to Make PDF's of All Plots of All Data + Quantile + Mean
dat$SiteID <- factor(dat$SiteID)
sites <- levels(dat$SiteID)

setwd("/Volumes/Blaszczak Lab/FSS/Figures/SingleTSPlots")
plotSpC <- function(x){
  pdf(paste0(x, "_singleTS_flowcorrected.pdf"))
  p <- ggplot(subset(dat, dat$SiteID == x))+
    theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    geom_line(mapping = aes(x = doy, y = Spc_Qcms, color = Year))+
    geom_line(subset(low_quart, up_quart$SiteID == x), mapping = aes(x = doy, y = lower_quart), color = "black")+
    geom_line(subset(med, avg$SiteID == x), mapping = aes(x = doy, y = median), color = "black")+
    geom_line(subset(up_quart, up_quart$SiteID == x), mapping = aes(x = doy, y = upper_quart), color = "black")+
    geom_line(subset(avg, avg$SiteID == x), mapping = aes(x = doy, y = mean), color = "red")
  print(p)
  dev.off()
}

# plotSpC(x)

lapply(sites, plotSpC) # already ran and saved PDFs

# Some plots only show one line, this is because those sites only have 1 year of continuous dat(a
dat$Year <- factor(sub_cont$Year)

## REDO THIS: #####
# See how many sites any given year has SC data for
# the highest %age of sites we have in one year of continuous data is __% :( 
# make barplot

# ggplot(count_sy, aes(x = vars, y = n))+
#   geom_bar(stat = "identity", fill = "turquoise3")+
#   theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(colour = "black"))+
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 40))+
#   labs(x = "Year", y = "Available Sites", title = "# of Continuous Sites per Year")

## Do the same thing for JUST discharge data: ############
##########################################################
avg <- dat
avg <- avg %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "Q_cms", .funs = c("mean" = mean))

## Upper Quantile
up_quart <- dat
up_quart <- up_quart %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "Q_cms", .funs = c("upper_quart" = quant75))

## Median (Middle Quatile)
med <- dat
med <- med %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "Q_cms", .funs = c("median" = median))

## Lower Quantile
low_quart <- dat
low_quart <- low_quart %>%
  group_by(SiteID, doy) %>%
  summarise_at(.vars = "Q_cms", .funs = c("lower_quart" = quant25))

## Rerun Code to Make PDF's of All Plots of All Data + Quantile + Mean
setwd("/Volumes/Blaszczak Lab/FSS/Figures/SingleTSPlots")
plotSpC <- function(x){
  pdf(paste0(x, "_singleTS_flow_only.pdf"))
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

# plotSpC(x)

lapply(sites, plotSpC) # already ran and saved PDFs
  
p3 <- ggplot(subset(dat, dat$SiteID == "USGS-10133800"))+
  geom_line(mapping = aes(x = doy, y = Q_cms, color = Year))+
  geom_line(subset(low_quart, up_quart$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = lower_quart), color = "black")+
  geom_line(subset(med, avg$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = median), color = "black")+
  geom_line(subset(up_quart, up_quart$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = upper_quart), color = "black")+
  geom_line(subset(avg, avg$SiteID == "USGS-10133800"), mapping = aes(x = doy, y = mean), color = "red")

saveRDS(low_quart, "Q_low_quart.rds")
saveRDS(avg, "Q_avg.rds")
saveRDS(up_quart, "Q_up_quart.rds")
saveRDS(med, "Q_med.rds")
