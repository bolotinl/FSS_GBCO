## Evaluation of catchment covariates

## Load packages
lapply(c("plyr","dplyr","ggplot2","cowplot","lubridate","boot",
         "tidyverse", "ggExtra", "data.table", "caret"), require, character.only=T) #any FALSE need to be installed
theme_set(theme_bw())

## Import
cm <- readRDS("corr_matrix_all_attributes.rds")
colnames(cm)

## Examine highly correlated relationships
## Pull out columns with highly corr values
cm_high <- findCorrelation(cm, cutoff = 0.9)
View(cm[,cm_high])

## Start with examining each category separately to identify redundancies

############################
## Climate & Water Balance
############################
Climate_WB <- c("WBM_AET","WBM_PET","WBM_PRCP","WBM_RUNOFF",
             "WBM_PRSNOW","WBM_SM_STRG","WBM_TAVG","BFI_pct")
View(cm[Climate_WB, Climate_WB])
## Remove PET because it is highly correlated temperature and we already have AET
## Remove WBM_RUNOFF because we want to keep snow

##############
## Geologic
##############
Geo <- c("BR_Gneiss","BR_Granitic", "BR_Ultramafic", "BR_Quarternary",
         "BR_Sedimentary","BR_Volcanic", "BR_Anorthositic", "BR_Intermediate",
         "OLSON_K2O","OLSON_CaO","OLSON_Fe203", "OLSON_MgO", "OLSON_P2O5",
         "OLSON_S", "OLSON_SiO2", "OLSON_CompressStrength","OLSON_HydroConductivity")

View(cm[Geo, Geo])
## Although the BR attributes don't seem to be strongly correlated with each other,
## we need to think of a justification of which ones we include and why (remember all of these
## are technically predictor variables so they all need to stem from a hypothesis)

## Same goes for the OLSON attributes -- I would get rid of CompressStrength unless I'm missing something
## The different soil attributes can stay (OLSON_CaO, etc.) but let's make sure to justify it in the text

######################
## Hydro Modifications
#####################
hydro_mod <- c("NDAMS2013","NID_STORAGE2013","NORM_STORAGE2013","MAJOR2013")
View(cm[hydro_mod, hydro_mod])

## OK - most of these are strongly correlated with each other
## Let's try sticking with NDAMS and test the model to see whether NID_STORAGE or NORM_STORAGE is better
## We want to probably keep the correlation below 0.8 if we can (ideal would be <0.7) so you might even just
## end up choosing one of these

######################
## Land Use
#####################
LU <- c("OpenWater_pct","PerennialIceSnow_pct","DevelopedOpenSpace_pct","DevelopedLowIntensity_pct",
"DevelopedMedIntensity_pct","DevelopedHiIntensity_pct","BarrenLand_pct",           
"DeciduousForest_pct","EvergreenForest_pct","MixedForest_pct",          
"ShrubScrub_pct","GrasslandHerbaceous_pct","PastureHay_pct",           
"CultivatedCrops_pct","WoodyWetlands_pct","EmergentHerbWetlands_pct","MIRAD_Irrig_Ag_Land_pct")

View(cm[LU,LU])

## LU categories can aggregated here to reduce correlations (e.g., add up all of the developed categories)
## Keep the irrigated land separate from any aggregation becaues we're definitely interested in that

############################################
## Physiographic Attributes
###########################################
Physiograph <- c("Basin_Area","Stream_Slope","Basin_Slope","Elevation_Mean","Elevation_Min",
                 "Elevation_Max","Flowline_Length","Silt_avg","Clay_avg","Sand_avg","pH",
                 "Salinity","Avg_Bulk_Density","Thickness","Permeability","OM_Content",
                 "NADP_CA","NADP_MG","NADP_SO4")
View(cm[Physiograph,Physiograph])

## Let's remove Flowline_Length (keep Basin_Area)
## Keep Stream_Slope over Basin_Slope (remove Basin_Slope)
## Test the model between keeping elevation min versus elevation mean (I'm inclined to keep the min and max and leave out the mean)
## You could get rid of permeability in favor of keeping Silt, Clay, and Sand averages
## I would keep OM_Content over Bulk Density...
## Keep NADP_CA over NADP_SO4






