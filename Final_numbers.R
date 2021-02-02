dat <- readRDS("/Volumes/Blaszczak Lab/FSS/All Data/WUS_all_USGS_SC_Q_data.rds")
head(dat)
dat$SiteID <- factor(dat$SiteID)
levels(dat$SiteID)

# 645 sites
# plus two sites from Rio Grande

dat <- readRDS("/Volumes/Blaszczak Lab/FSS/All Data/WUS_UNM_USGS_SC_Q_availability_subset.rds")
head(dat)
dat$SiteID <- factor(dat$SiteID)
levels(dat$SiteID)
# 246 sites
dat$SiteYear <- factor(dat$SiteYear)
levels(dat$SiteYear)

# then, when you pair the NHD catchment attribute data, there is one site that doesn't have any data
# the site is USGS-10312210
nyear <- subset(dat, dat$SiteID == "USGS-10312210")
nyear$SiteYear <- factor(nyear$SiteYear)
levels(nyear$SiteYear)
# 3 site-years