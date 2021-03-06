## WQP meta data compiler

lapply(c("plyr","dplyr","ggplot2","cowplot",
         "lubridate","tidyverse", "readxl", "dataRetrieval"), require, character.only=T)

## Set wd and bring in metadata files
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
files <- list.files(path = getwd(), pattern = "station-ions")
file <- files[1]
HUC1415 <- fread(file)
HUC1415$HUC <- substr(HUC1415$HUCEightDigitCode, 1, 2)
HUC1415$file <- paste(file)
file <- files[2]
HUC16 <- fread(file)
HUC16$HUC <- "16"
HUC16$file <- paste(file)
## Merage all metadata together
meta <- rbind(HUC1415, HUC16, deparse.level = 1)

head(meta)
names(meta)


## Subset columns
sub <- meta[,c("MonitoringLocationIdentifier","MonitoringLocationName","MonitoringLocationTypeName",
               "HUCEightDigitCode", "DrainageAreaMeasure/MeasureValue","LatitudeMeasure", "LongitudeMeasure",
               "HorizontalCoordinateReferenceSystemDatumName","VerticalMeasure/MeasureValue", "CountryCode", "StateCode",
               "HUC","file")]
rm(HUC1415, HUC16, meta, file, files)

## Rename columns
colnames(sub) <- c("SiteID","SiteID_LongName","WaterBodyType","HUCEightDigitCode","WatershedArea_sqkm","Latitude","Longitude",
                   "Coord_Units","Altitude_feet","Country","State", "HUC","file")

## Coerce state codes to state abbreviations
sub$State <- factor(as.character(sub$State)) # temporarily convert to a factor so we can see the levels
levels(sub$State)
sub$State <- as.numeric(as.character(sub$State)) # turn back from factor to numeric so we can convert WQP state code to common state code
## codes found at: https://www.mcc.co.mercer.pa.us/dps/state_fips_code_listing.htm
class(sub$State)
sub$State2 <- sub$State
sub$State2[which(sub$State == 4)] <- "AZ"
sub$State2[which(sub$State == 16)] <- "ID"
sub$State2[which(sub$State == 32)] <- "NV"
sub$State2[which(sub$State == 35)] <- "NM"
sub$State2[which(sub$State == 41)] <- "OR"
sub$State2[which(sub$State == 48)] <- "TX"
sub$State2[which(sub$State == 49)] <- "UT"
sub$State2[which(sub$State == 56)] <- "WY"
sub$State2[which(sub$State == 6)] <- "CA"
sub$State2[which(sub$State == 8)] <- "CO"
sub$State2[which(sub$State == 85)] <- NA # what could this be?
## convert to a factor and check levels for the final time
sub$State2 <- factor(as.character(sub$State2))
levels(sub$State2)

## get rid of State column that has the old state code
sub <- subset(sub, select = -c(State))
## name the State2 column as State
sub <- rename (sub, State = State2)

head(sub)
tail(sub)

## Filter to only include flowing waters
WBT <- sub %>% group_by(WaterBodyType) %>% count()
levels(as.factor(sub$WaterBodyType))

# USGS site types included in data: "ST" (stream), "ST-CA"(canal), "ST-DCH"(ditch), "ST-TS"(tidal stream), "SP" (spring)
acceptable_sitetypes <- c("River/Stream","River/Stream Perennial",
                         "Spring","Stream","Canal Transport","Stream: Ditch",
                         "Channelized Stream","Canal Drainage","Canal Irrigation",
                         "Stream: Canal","River/Stream Intermittent","Stream: Tidal stream",
                         "Riverine Impoundment","Storm Sewer","River/Stream Ephemeral")

sub2 <- sub[which(sub$WaterBodyType %in% acceptable_sitetypes),]
View(sub2 %>% group_by(WaterBodyType) %>% count())

names(sub2)
class(sub2$SiteID)
sub2$SiteID <- factor(sub2$SiteID)
levels(sub2$SiteID)
rm(sub, WBT, acceptable_sitetypes)

## Export
setwd("/Volumes/Blaszczak Lab/FSS/All Data")
write.csv(sub2, "WQP_formatted_metadata_WQ.csv")





