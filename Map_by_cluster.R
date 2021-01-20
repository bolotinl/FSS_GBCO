library(tidyverse)

# Bring in cluster membership
setwd("/Volumes/Blaszczak Lab/FSS/FSS_clustering")
clust <- readRDS("cluster_results.rds")

# Bring in metadata
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
unm_meta <- readRDS("WUS_UNM_USGS_disch_SC_sites.rds")

# Bind together
names(clust)
names(unm_meta)
dat <- merge(unm_meta, clust, by = "SiteID")

# Map, colored by cluster membership
some.states <- c('california', 'nevada', 'utah', 'arizona', 'colorado', 'new mexico', 'texas', 'oregon', 'washington', 'montana', 'idaho', 'north dakota', 'south dakota', 'wyoming', 'kansas', 'nebraska', 'oklahoma', 'missouri', 'arkansas', 'iowa', 'minnesota', 'louisiana')
some.states.map <- map_data("state", region = some.states)
ggplot(some.states.map)+
  geom_polygon(mapping = aes(x = long, y = lat, group = group), fill = "grey", color = "white")+
  geom_point(dat, mapping = aes(x = Lon, y = Lat, color = Cluster),  size = 1)+
  theme(panel.background = element_blank(), legend.position = 'none', 
        axis.title = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5), axis.line = element_blank())+
  labs(title = "Concurrent USGS SC and Q Sites", subtitle = "n = 246")
