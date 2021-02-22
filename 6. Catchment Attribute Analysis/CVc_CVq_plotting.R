# Bring in 3 df's:
# 1) the averaged SC time series
# 2) the averaged Q time series
# 3) the averaged SC/Q time series (This is the data BEFORE it got normalized and gap filled, but it is the data that ultimately went into the clustering)

library(tidyverse)

setwd("/Volumes/Blaszczak Lab/FSS/All Data")
SC <- readRDS("WUS_UNM_SC_avg.rds")
Q <- readRDS("WUS_UNM_Q_avg.rds") # Lauren, you still need to create this file. You never ran the script for this with the UNM data
SC_Q <- readRDS("WUS_UNM_SC_Q_avg.rds")

# Change column names to be distinct
SC <- SC %>% rename(mean_SC = mean_SpC)
Q <- Q %>% rename(mean_Q = mean)
SC_Q <- SC_Q %>% rename(mean_SC_Q = mean)

dat <- merge(SC, Q)
dat <- merge(dat, SC_Q)

rm(SC_Q, SC, Q)
