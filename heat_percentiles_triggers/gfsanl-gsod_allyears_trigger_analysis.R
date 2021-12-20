################################################################################
# gfsanl-gsod_allyears_trigger_analysis.R
# Code to perform trigger analysis for six locations in Pakistan at three different
# intra-seasonal periods for trigger of heatwave criteria using different percentile
# thresholds. As input, uses already created table of data from Ross Maidment.
# Author: Alex Saunders
# Date created: 03/12/2021
# Date modified: 05/12/2021
################################################################################

################################################################################
# Install packages and set up working environment
################################################################################

rm(list=ls())

packages = c("lubridate","dplyr")

for(package in packages){
  print(paste0('** Loading package: ',package))
  if(!require(package,character.only = TRUE)) {
    print("*** Package not found... installing")
    install.packages(package)
    if(!require(package,character.only = TRUE)) {
      print("*** ERROR: Package not found")
    }else{
      print('*** Package installed')
    }
  }
  library(package,character.only = TRUE)
  print(paste0('*** Package loaded: ', package))
}
print('* END: Loading packages')

# set input path
input_path <- "C://Users/alexa/Documents/02_work/02_start/02_deliv/01_pk_heat/06_r/01_input/"
output_path <- gsub("01_input","02_output",input_path)

################################################################################
# Read and setup input data
################################################################################

# read csv containing gsod and gfs variables from April 2004 to end 2020
data_allyears <- read.csv(paste0(input_path,"gfsanl-gsod_allyears.csv"))

# read in lookup tables from csv files
tbl_stations <- read.csv(paste0(input_path,"tbl_stations.csv"))
tbl_periods <- read.csv(paste0(input_path,"tbl_periods.csv"))
tbl_percentiles <-read.csv(paste0(input_path,"tbl_percentiles.csv"))
tbl_rp_thresholds_hi <- read.csv(paste0(input_path,"tbl_rp_thresholds_hi.csv"))
tbl_missingyears_gsod <- read.csv(paste0(input_path,"tbl_missingyears_gsod.csv"))


################################################################################
# Set up match column strings and names of variables we are interested in
################################################################################

# define variables for which we want to compute the trigger rate, t and hi in C
matchcols_vars <- c("temp_cel","hi_cel")
variables <- grep(paste(matchcols_vars, collapse = "|"), colnames(data_allyears), value = T)

# define match column strings for use later
matchcols_trigs <- c("excday_","trigday_","trig_")


################################################################################
# Loop through locations and intraseasonal periods and calculate no of triggers
# at different percentile thresholds from 90%ile to 99%ile
################################################################################

# start loop through locations (i)

for (i in 1:length(tbl_stations$stationid)) {
  stationid <- tbl_stations$stationid[i]
  loc <- tbl_stations$location[i]
  
  # subset allyears data to the location
  data_allyears_loc <- data_allyears[which(data_allyears$station_id == stationid), ]
  
  # for gsod variables, missing years will be ignored, read in missing years
  ignore_years <- tbl_missingyears_gsod$missingyear[which(tbl_missingyears_gsod$loc == loc)]
  
  # start loop through intra-seasonal periods for the location (j)
  
  for (j in 1:length(tbl_periods$period)) {
    period <- tbl_periods$period[j]
    startm <- tbl_periods$startmonth[j]
    endm <- tbl_periods$endmonth[j]
    period_name <- paste0(month.abb[startm],"-",month.abb[endm-1])
    
    data_allyears_loc_period <- data_allyears_loc[which(lubridate::month(data_allyears_loc$date) >= startm 
                                                        & lubridate::month(data_allyears_loc$date) < endm), ]
    
    # calculate exceedance days, trigger days and triggers (i.e. trigger days accounting
    # for breaks) for gfs and gsod, t and hi
    
    # start loop through variables (k)
    for (k in 1:length(variables)) {
      variable <- variables[k]
      data_allyears_loc_period_var <- data_allyears_loc_period[, c("date", variable)]
      
      # if gsod, ignore missing years (set to na)
      if (grepl("gsod", variable) == TRUE & length(ignore_years) > 0) {
        data_allyears_loc_period_var[which(lubridate::year(data_allyears_loc_period_var$date) %in% ignore_years), variable] <- NA
      }
      
      # compute the percentiles for the given variable
      percentiles <- cbind(tbl_percentiles, quantile(data_allyears_loc_period_var[, variable], tbl_percentiles$percentile, na.rm = T))
      colnames(percentiles) <- c("percentile","flag","val")
      
      # compute exceedance days, lookup variable value compared to percentiles of the 
      # variable and assign a flag from tbl_percentiles
      
      # assign percentile which is met or exceeded "excday"
      for (l in 1:length(percentiles$percentile)) {
        data_allyears_loc_period_var$excday[which(data_allyears_loc_period_var[, variable] >= percentiles$val[l])] <- percentiles$percentile[l]
        
        # if gsod, ignore missing years (ie can be no triggers, set to zero)
        if (grepl("gsod", variable) == TRUE & length(ignore_years) > 0) {
          data_allyears_loc_period_var[which(lubridate::year(data_allyears_loc_period_var$date) %in% ignore_years), "excday"] <- 0
        }
        
      }
      
      # compute trigger days, i.e. 2 consecutive days above threshold
      for (a in 1:length(data_allyears_loc_period_var$excday)) {
        data_allyears_loc_period_var$trigday[a] <- min(data_allyears_loc_period_var$excday[(a-1):a])
        
        # compute triggers, i.e. 2 consecutive days but with at least one day break before
        # another trigger can be registered
        if (a > 1 && data_allyears_loc_period_var$trigday[a] > data_allyears_loc_period_var$trigday[a-1]) {
          data_allyears_loc_period_var$trig[a] <- data_allyears_loc_period_var$trigday[a]
        }  else {
          data_allyears_loc_period_var$trig[a] <- 0
        }
        
      }
      
      # rename the excday, trigday and trig with the variable name
      data_allyears_loc_period_var <- data_allyears_loc_period_var[,3:5]
      colnames(data_allyears_loc_period_var) <- paste0(colnames(data_allyears_loc_period_var),"_",variable)
      
      # append columns back to all data for the given location and period (12 new cols 
      # in total)
      if (k == 1) {
        data_allyears_loc_period_triggers <- cbind(data_allyears_loc_period, data_allyears_loc_period_var)
      } else {
        data_allyears_loc_period_triggers <- cbind(data_allyears_loc_period_triggers, data_allyears_loc_period_var)
      }
      
      
      # end loop through variables
    }
    
    # create summary of trigger days for the given location and period
    
    # start with interim summary using aggregate
    summary_variables <- grep(paste(matchcols_trigs, collapse = "|"),colnames(data_allyears_loc_period_triggers), value = T)
    # 12 columns in total
    
    # create count for each: year, month and percentile combination, for each variable column
    for (m in 1:length(summary_variables)) {
      summary_variable <- summary_variables[m]
      data_summary_loc_period_var <- aggregate(data_allyears_loc_period_triggers[, summary_variable], 
                                               list(year = lubridate::year(data_allyears_loc_period_triggers$date),
                                                    month = lubridate::month(data_allyears_loc_period_triggers$date),
                                                    percentile = data_allyears_loc_period_triggers[, summary_variable]), 
                                               FUN = length)
      colnames(data_summary_loc_period_var) <- c("year","month","percentile",summary_variable)
      
      # append aggregate summary for all variables
      if (m == 1) {
        data_summary_loc_period_interim <- data_summary_loc_period_var
      }  else {
        data_summary_loc_period_interim <- merge(data_summary_loc_period_interim, data_summary_loc_period_var, by = c("year","month","percentile"), all = T)
      }
      
    }
    
    # set NAs to zero and keep only rows which have count of triggers above 90%ile or higher
    data_summary_loc_period_interim[is.na(data_summary_loc_period_interim)] <- 0
    data_summary_loc_period_interim <- data_summary_loc_period_interim[which(data_summary_loc_period_interim$percentile > 0),]
    
    
    # append "fill" rows so that all years, months, percentiles have a row (fill variable 
    # columns with zero where nothing)
    all_yrs <- unique(lubridate::year(data_allyears_loc_period_triggers$date))
    all_mos <- unique(lubridate::month(data_allyears_loc_period_triggers$date))
    all_pcts <- unique(percentiles$percentile[-1])
    all_yrmopct <- data.frame(matrix(NA, nrow = length(all_yrs)*length(all_mos)*length(all_pcts), ncol = 3))
    colnames(all_yrmopct) <- c("year","month","percentile")
    i<-0
    for (y in 1:length(all_yrs)) {
      yr <- all_yrs[y]
      for (m in 1:length(all_mos)) {
        mo <- all_mos[m]
        for (p in 1:length(all_pcts)) {
          i <- i + 1
          pct <- all_pcts[p]
          all_yrmopct$year[i] <- yr
          all_yrmopct$month[i] <- mo
          all_yrmopct$percentile[i] <- pct
          
        }
      }
    }
    
    allyrmopct_data_summary_loc_period_interim <- unique(data_summary_loc_period_interim[,c("year","month","percentile")])
    allyrmopct_missing <- setdiff(all_yrmopct, allyrmopct_data_summary_loc_period_interim)
    allyrmopct_missing[,4:ncol(data_summary_loc_period_interim)] <- 0
    colnames(allyrmopct_missing) <- colnames(data_summary_loc_period_interim)
    data_summary_loc_period_interim <- rbind(data_summary_loc_period_interim, allyrmopct_missing)
    
    
    # current count is based on exact match for the percentile, but we need the 
    # cumulative count for all percentiles higher than or equal to
    # i.e. exceeding 95%ile also means you have exceeded 90% and 92%iles
    
    # create a copy of the interim summary table, with same columns and rows
    data_summary_loc_period <- data_summary_loc_period_interim
    
    # update the values as the count of triggers equal or above the given percentile
    for (c in 1:length(summary_variables)) {
      summary_variable <- summary_variables[c]
      for (d in 1:nrow(data_summary_loc_period)) {
        data_summary_loc_period[d, summary_variable] <- sum(data_summary_loc_period[which(data_summary_loc_period$year == data_summary_loc_period[d, "year"]
                                                                                          & data_summary_loc_period$month == data_summary_loc_period[d, "month"]
                                                                                          & data_summary_loc_period$percentile >= data_summary_loc_period[d, "percentile"]), 
                                                                                    summary_variable])
      }
    }
    
    
    # add location and intra-seasonal period, reorder columns and rows and write to csv
    data_summary_loc_period$loc <- loc
    data_summary_loc_period$period <- period_name 
    data_summary_loc_period <- data_summary_loc_period[,c(ncol(data_summary_loc_period)-1,ncol(data_summary_loc_period),1:(ncol(data_summary_loc_period)-2))]
    data_summary_loc_period <- data_summary_loc_period[order(data_summary_loc_period$year, data_summary_loc_period$month, data_summary_loc_period$percentile), ]
    write.csv(data_summary_loc_period, paste0(output_path,"gfs-gsod_triggers_by_yr-mo-pct_",loc,"_",period_name,".csv"), row.names = F)
    
    # aggregate again to create summary by year and percentile, only for triggers
    # (not for exceedance days or trigger days)
    summary_variables <- grep("trig_",colnames(data_summary_loc_period),value = T)
    for (m in 1:length(summary_variables)) {
      summary_variable <- summary_variables[m]
      data_summary_loc_period_trig_var <- aggregate(data_summary_loc_period[, summary_variable], 
                                                    list(data_summary_loc_period$loc,
                                                         data_summary_loc_period$period,
                                                         data_summary_loc_period$year,
                                                         data_summary_loc_period$percentile), 
                                                    FUN = sum)
      colnames(data_summary_loc_period_trig_var) <- c("loc","period","year","percentile",summary_variable)
      
      # append aggregate summary for all variables
      if (m == 1) {
        data_summary_loc_period_trig <- data_summary_loc_period_trig_var[order(data_summary_loc_period_trig_var$loc,data_summary_loc_period_trig_var$period,data_summary_loc_period_trig_var$year,data_summary_loc_period_trig_var$percentile), ]
      }  else {
        data_summary_loc_period_trig <- merge(data_summary_loc_period_trig, data_summary_loc_period_trig_var, by = c("loc","period","year","percentile"), all = T)
      }
      
    }
    
    # write to csv summary by year and percentile only for triggers
    write.csv(data_summary_loc_period_trig, paste0(output_path,"gfs-gsod_triggers_by_yr-pct_",loc,"_",period_name,".csv"), row.names = F)
    
    # aggregate for a third time to create summary by percentile only, only for triggers
    for (m in 1:length(summary_variables)) {
      summary_variable <- summary_variables[m]
      data_summary_loc_period_trig_var <- aggregate(data_summary_loc_period[, summary_variable], 
                                                    list(data_summary_loc_period$loc,
                                                         data_summary_loc_period$period,
                                                         data_summary_loc_period$percentile), 
                                                    FUN = sum)
      colnames(data_summary_loc_period_trig_var) <- c("loc","period","percentile",summary_variable)
      
      # append aggregate summary for all variables
      if (m == 1) {
        data_summary_loc_period_trig <- data_summary_loc_period_trig_var[order(data_summary_loc_period_trig_var$loc,data_summary_loc_period_trig_var$period,data_summary_loc_period_trig_var$percentile), ]
      }  else {
        data_summary_loc_period_trig <- merge(data_summary_loc_period_trig, data_summary_loc_period_trig_var, by = c("loc","period","percentile"), all = T)
      }
      
    }
    
    # write to csv summary by percentile only for triggers
    write.csv(data_summary_loc_period_trig, paste0(output_path,"gfs-gsod_triggers_by_pct_",loc,"_",period_name,".csv"),row.names = F)
    
    
    
    # end loop through intra-seasonal periods for the location       
  }
  
  # end loop through locations 
}


################################################################################
# End of the data processing
################################################################################


################################################################################
# End of script
################################################################################








