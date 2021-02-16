#===============================================================================
#Script for filtering data queries
#===============================================================================
detach("package:here", unload = TRUE) 
    # If package:here has already been called in an R session,
    # 1) detach it using this line
    # 2) set your working directory to the correct location
    # 3) re-call the package from your library
setwd("/Volumes/Blaszczak Lab/FSS/All Data") # working directory containing concurrent_overlap.R, disch_query.rds, and SC_query.rds
library("here") # WORKING DIRECTORY MUST BE CORRECT BEFORE CALLING THIS PACKAGE AND USING IT
library("rgdal") #For CRS
library("data.table")
library("dplyr") #For filtering

#Source functions for finding overlap
source(here("concurrent_overlap.R"))

#Read in the data queries
 disch_query <- readRDS(here("disch_query.rds")) # All discharge sites in the US
 SC_query <- readRDS(here("SC_query.rds")) # All SC sites in the US

#-------------------------------------------------
#Filter for all lotic NWIS sites with specific conductivity sensors (subdaily or daily)  
#-------------------------------------------------
  SC_lotic_sensor <- SC_query %>%
    filter(site_tp_cd %in% c("ST", "ST-CA", "ST-DCH", "ST-TS", "SP")) %>%
    filter(data_type_cd == "dv" | data_type_cd == "uv") %>%
    filter(!is.na(Lat)) %>%
    distinct(Site_ID, Lat, Lon) # Flowing waters with SC
    
 
    SC_lotic_sensor <- SC_query %>%
      filter(Site_ID %in% SC_lotic_sensor$Site_ID)
    class(SC_lotic_sensor$Site_ID)
    SC_lotic_sensor$Site_ID <- factor(SC_lotic_sensor$Site_ID)
    levels(SC_lotic_sensor$Site_ID) # 2811
  
disch_lotic_sensor <- disch_query %>%
   filter(site_tp_cd %in% c("ST", "ST-CA", "ST-DCH", "ST-TS", "SP")) %>%
   filter(data_type_cd == "dv" | data_type_cd == "uv") %>%
   filter(!is.na(Lat)) %>%
   distinct(Site_ID, Lat, Lon) # Flowing waters with Q
  
  disch_lotic_sensor <- disch_query %>%
    filter(Site_ID %in% disch_lotic_sensor$Site_ID)
  class(disch_lotic_sensor$Site_ID)
  disch_lotic_sensor$Site_ID <- factor(disch_lotic_sensor$Site_ID)
  levels(disch_lotic_sensor$Site_ID) # 23425
  
  
  
  # Write 2 csv's: 1 for all lotic sites with SC and 1 for all lotic sites with Q
  write.csv(SC_lotic_sensor, "USGS_all_SC_lotic_sensor.csv") # as opposed to SC_only which is sites that have SC and not disch
  write.csv(disch_lotic_sensor,"USGS_all_disch_lotic_sensor.csv") # as opposed to disch_only which is sites that have disch and not SC
  
#-------------------------------------------------
#Filter for all lotic NWIS sites with specific conductivity and discharge sensors (subdaily or daily)  
#-------------------------------------------------
  #Since the specific conductivity query contains two different parameter codes ("00094", "00095")
  #We have to do a little trick to get the overlap function to work by giving them all the same parameter code
    SC_query2 <- SC_query
    SC_query2$parm_cd <- "00000"

  #Find concurrent overlap of data
    both_sensor <- overlap_fun(
      param_queries = list(disch_query, SC_query2),
      site_types = c("ST", "ST-CA", "ST-DCH", "ST-TS", "SP"),
      data_types = c("uv", "dv"),
      min_obs = 1
    )  # there are 2002 sites with flowing water in the COUNTRY with both SC and Discharge sensors

    # As it is, the output dataframe for both_sensor is just the SiteID and the Lat Long. Let's include other columns.
    SC_both <- subset(SC_lotic_sensor, SC_lotic_sensor$Site_ID %in% both_sensor$Site_ID)
    disch_both <- subset(disch_lotic_sensor, disch_lotic_sensor$Site_ID %in% both_sensor$Site_ID)
    both_sensor <- rbind(SC_both, disch_both)
    class(both_sensor$Site_ID)
    both_sensor$Site_ID <- factor(both_sensor$Site_ID)
    levels(both_sensor$Site_ID) # 2002 still, so we're good and we've added important metadata
    write.csv(both_sensor, "USGS_all_disch_SC_lotic_sensor.csv") # Flowing waters with Q and SC

