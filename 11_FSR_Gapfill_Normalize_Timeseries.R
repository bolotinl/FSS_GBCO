#===============================================================================
## Script for gap filling and z-normalizing time series
## Code author: P. Savoy
## Adjustments made by: L. Bolotin
#===============================================================================

## Set working directory to the folder containing this script "2. Clustering"
setwd() 

gapfill_normalize <- function(){
#-------------------------------------------------
#Define a function to gapfill and z-normalize the timeseries
#-------------------------------------------------
  gapfill_znorm <- function(Site){
    #-------------------------------------------------
    #Formatting the data
    #-------------------------------------------------
      #Make a complete set of dates ignoring leap years
        full_dates <- setNames(
          data.frame(unique(Site[, "SiteID"]), c(1:365), stringsAsFactors = FALSE), 
          c("SiteID", "doy")
        )
        
      #Merge the average timeseries to the full dates  
        ts_full <- merge(
          full_dates,
          Site[, c("doy", "mean_SpC")],
          by = "doy",
          all.x = TRUE
        )

    #-------------------------------------------------
    #Gap-filling and z normalization of the data
    #-------------------------------------------------
      #Predict missing values 
      #(AT PRESENT GAPS ARE VERY SMALL, SO JUST LINEARLY INTERPOLATE)
      #########################################################################################################################  
        predicted <- approx(
          x = ts_full[, "doy"],
          y = ts_full[, "mean_SpC"],
          xout = ts_full[, "doy"],
          rule = 2
        )        

      #Gap-filling with the linearly interpolated data
        ts_full$SpC_filled <- ts_full[, "mean_SpC"]
        ts_full[is.na(ts_full[, "mean_SpC"]), "SpC_filled"] <- predicted$y[is.na(ts_full[, "mean_SpC"])]
     
      #Defining a function for z normalization
        znorm <- function(timeseries, var){
          ts.mean <- mean(timeseries[, var], na.rm = TRUE)
          ts.dev <- sd(timeseries[, var], na.rm = TRUE)
          (timeseries[, var] - ts.mean)/ts.dev
        }
        
      #Z normalize the average year
        ts_full$SpC_norm <- znorm(ts_full, "SpC_filled")   
        
    #-------------------------------------------------
    #Get the final output    
    #-------------------------------------------------    
      #Reorder the columns to get the final output
        final <- ts_full[, c("SiteID", "doy", "mean_SpC", "SpC_filled", "SpC_norm")]
        
      #Changing "mean_SpC" column name for consistent formatting
        colnames(final)[which(names(final) == "mean_SpC")] <- "SpC_mean"
        
    return(final)    
        
  } #End gapfill_znorm function

#-------------------------------------------------
#Gap-filling and z-normalizing the timeseries  
#-------------------------------------------------
  #Read in the compiled set of timeseries
   ## VARIABLE: input data ##########################################
  compiled_ts <- readRDS("../All Data/FSR_SC_Q_avg.rds")
  colnames(compiled_ts)[3] <- "mean_SpC"
    compiled_ts <- subset(compiled_ts, compiled_ts$doy < 366)
  #Replacing SiteID since it was a factor
    class(compiled_ts)
    compiled_ts <- as.data.frame(compiled_ts)
    compiled_ts$SiteID <- as.character(compiled_ts$SiteID)
    
  #Split the data by site
    site_split <- split(compiled_ts, compiled_ts$SiteID)  
    
  #Apply the function to all timeseries
    normalized <- lapply(site_split, FUN = gapfill_znorm)
    
  #Save the output
      # As .rds file:
    saveRDS(normalized, "../All Data/FSR_normalized_timeseries.rds")
      # As .csv file:
    write.csv(normalized, "../All Data/FSR_normalized_timeseries.csv")
    
} #End gapfill_normalize wrapper 

gapfill_normalize()

