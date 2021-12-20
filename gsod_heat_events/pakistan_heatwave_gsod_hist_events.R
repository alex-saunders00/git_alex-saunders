################################################################################
# pakistan_heatwave_gsod_hist_events.R
# Code to extract gsod meteorological variables at locations in Pakistan for known
# historical events. Locations and times of events are known, although in some cases
# locations are coarse and hence the maximum across many stations is sought out.
# Makes use of the package GSODR by Adam H Sparks
# Author: Alex Saunders
# Date created: 02/12/2021
# Date modified: 02/12/2021
################################################################################

################################################################################
# Install packages and set up working environment
################################################################################

rm(list=ls())

packages = c("GSODR")

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


# set fixed a and b parameters for calculation of RH
a <- 17.625
b <- 243.04


################################################################################
# Check GSOD stations for Pakistan
################################################################################
#update_station_list()
paste0(.libPaths(), "/GSODR/extdata")[1]
load(system.file("extdata", "isd_history.rda", package = "GSODR"))

# create data.frame for Pakistan only
gsod_stations <- subset(isd_history, COUNTRY_NAME == "PAKISTAN")
gsod_stations

# subset based on known location name
subset(gsod_stations, grepl("JINNAH", NAME))


################################################################################
# Import list of stations and time periods for extraction
################################################################################

# load csv file of station names / id and start and end dates required
times <- read.csv(paste0(input_path,"times.csv"))
times <- times[!is.na(times$start.year),]
times$uid <- seq.int(nrow(times))

locs <- read.csv(paste0(input_path,"locs.csv"))
locs


################################################################################
# PART 1: Extract GSOD meteorological data
################################################################################

# extract based on known stationid - first make consistent between two datasets
nchar(locs$stationid)
nchar(gsod_stations$STNID)
locs$STNID <- paste0(substr(locs$stationid,1,6),"-",substr(locs$stationid,7,11))
locscheck <- merge(locs, gsod_stations, by = "STNID", all.x = T, all.y = F)
locscheck[is.na(locscheck$NAME),]
# RAIFIQUI (PAFB), station 41524599999 (Punjab province) data does not exist or 
# not available

# remove from list of stations to avoid issues
locs <- locs[which(locs$STNID != locscheck[is.na(locscheck$NAME),"STNID"]),]


# pass to extract_gsod a list of stations for the year of event, perform separately 
# for each uid

for (i in 1:length(times$uid)) {

  uid <- times$uid[i]
  times_event <- times[which(times$uid == uid),]
  stations_event <- locs$STNID[which(locs$loc == times_event$loc)]
  gsod_event <- get_GSOD(years = times_event$start.year, station = stations_event)
  
  # subset to only the period of interest and write out as csv
  gsod_event <- gsod_event[which(as.Date(gsod_event$YEARMODA, format = "%d/%m/%Y") >= as.Date(times_event$start.date, format = "%d/%m/%Y")
                                 & as.Date(gsod_event$YEARMODA, format = "%d/%m/%Y") <= as.Date(times_event$end.date, format = "%d/%m/%Y")),]
  
  # add RH, calculate using Magnus formula
  gsod_event$RH <- round(100 * (exp((a * gsod_event$DEWP) / (b + gsod_event$DEWP))
                                               / exp((a * gsod_event$TEMP) / (b + gsod_event$TEMP))),1)
  
  # add HI
  gsod_event$t_fah <- (gsod_event$TEMP * (9/5)) + 32
  
  gsod_event$hi_fah <- round (
    -42.379+(2.04901523*gsod_event$t_fah)+(10.14333127*gsod_event$RH)
    -(0.22475541*gsod_event$t_fah*gsod_event$RH)-(0.00683783*gsod_event$t_fah*gsod_event$t_fah)
    -(0.05481717*gsod_event$RH*gsod_event$RH)+(0.00122874*gsod_event$t_fah*gsod_event$t_fah*gsod_event$RH)
    +(0.00085282*gsod_event$t_fah*gsod_event$RH*gsod_event$RH)-(0.00000199*gsod_event$t_fah*gsod_event$t_fah*gsod_event$RH*gsod_event$RH)
    , 1)
  
  gsod_event$hi <- round (
    (gsod_event$hi_fah - 32) * (5/9)
    , 1)
  
  write.csv(gsod_event, paste0(output_path,"gsod_event_",uid,"_",times_event$loc,"_",as.Date(times_event$start.date, format = "%d/%m/%Y"),"_",as.Date(times_event$end.date, format = "%d/%m/%Y"),".csv"), row.names = F)
  
  # from all the stations over all days, find the max HI day
  gsod_event_day_himax <- gsod_event[order(gsod_event$hi, decreasing = T),]
  gsod_event_day_himax <- gsod_event_day_himax[1,]
  
  # extract useful data for the max T day and the max RH day (i.e. location, min/max temp, dewpt)
  gsod_event_day_himax_stats <- gsod_event_day_himax[,c("STNID","NAME","YEARMODA","TEMP","DEWP","MAX","MIN","RH","hi")]
  
  # tidy up statistics
  gsod_event_day_himax_stats$STNID <- gsub("-","",gsod_event_day_himax_stats$STNID)
  colnames(gsod_event_day_himax_stats) <- c("stationid","stationname","date","t","tdewp","tmax","tmin","rh","hi")
  gsod_event_day_himax_stats$uid <- uid
  
  # append to himax days for all events
  if (i == 1){
    gsod_events_day_himax_stats <- gsod_event_day_himax_stats
  } else {
    gsod_events_day_himax_stats <- rbind(gsod_events_day_himax_stats, gsod_event_day_himax_stats)
  }
  
  # from all the stations over all days, compute the max T and mean T (daily T)
  gsod_event_period_stats <- as.data.frame(uid)
  gsod_event_period_stats$tmax_period <- max(gsod_event$TEMP)
  gsod_event_period_stats$tmean_period <- round(mean(gsod_event$TEMP), 1)
  
  
  # append to event period for all events
  if (i == 1){
    gsod_events_period_stats <- gsod_event_period_stats
  } else {
    gsod_events_period_stats <- rbind(gsod_events_period_stats, gsod_event_period_stats)
  }
  
}


# create summary stats file for all events
gsod_events_summary <- merge(times, gsod_events_day_himax_stats, by = "uid")
gsod_events_summary <- merge(gsod_events_summary, gsod_events_period_stats, by = "uid")


# write out summary stats file
write.csv(gsod_events_summary, paste0(output_path,"gsod_events_summary_himaxday.csv"), row.names = F)


################################################################################
# PART 2: calculate the percentile for the HI max for each event
################################################################################


# read in the outputs from PART 1
input_path <- paste0(output_path,"gsod_historical_events/")
input_file <- "gsod_events_summary_himaxday.csv"
gsod_events_summary_himaxday <- read.csv(paste0(input_path,input_file))

# set start and end date for full record to be consistent with prior GFS analysis
start.year <- 2004
end.year <- 2020
years <- start.year:end.year

# set start and end months consistent with Apr-Jul monitoring period
start.month <- 4
end.month <- 7

# standardise format of stationid
gsod_events_summary_himaxday$STNID <- paste0(substr(gsod_events_summary_himaxday$stationid,1,6),"-",substr(gsod_events_summary_himaxday$stationid,7,11))

# take copy of the input data for adding the percentiles as new columns
gsod_events_summary_himaxday_percentiles <- gsod_events_summary_himaxday
gsod_events_summary_himaxday_percentiles$t_perc <- 0
gsod_events_summary_himaxday_percentiles$tdewp_perc <- 0
gsod_events_summary_himaxday_percentiles$tmax_perc <- 0
gsod_events_summary_himaxday_percentiles$tmin_perc <- 0
gsod_events_summary_himaxday_percentiles$rh_perc <- 0
gsod_events_summary_himaxday_percentiles$hi_perc <- 0

# start loop through each row (i.e. station for a given event)
for (i in 1:nrow(gsod_events_summary_himaxday)) {
  
  uid <- gsod_events_summary_himaxday$uid[i]
  stationid <- gsod_events_summary_himaxday$stationid[i]
  loc <- gsod_events_summary_himaxday$loc[i]
  gsod_data_station <- get_GSOD(years = years, station = gsod_events_summary_himaxday$STNID[i])

  # subset to only the months of interest
  gsod_data_station <- gsod_data_station[which(gsod_data_station$MONTH >= start.month
                                 & gsod_data_station$MONTH <= end.month),]
  
  
  # add RH, calculate using Magnus formula
  gsod_data_station$RH <- round(100 * (exp((a * gsod_data_station$DEWP) / (b + gsod_data_station$DEWP))
                                / exp((a * gsod_data_station$TEMP) / (b + gsod_data_station$TEMP))),1)
  
  # add HI
  gsod_data_station$t_fah <- (gsod_data_station$TEMP * (9/5)) + 32
  
  gsod_data_station$hi_fah <- round (
    -42.379+(2.04901523*gsod_data_station$t_fah)+(10.14333127*gsod_data_station$RH)
    -(0.22475541*gsod_data_station$t_fah*gsod_data_station$RH)-(0.00683783*gsod_data_station$t_fah*gsod_data_station$t_fah)
    -(0.05481717*gsod_data_station$RH*gsod_data_station$RH)+(0.00122874*gsod_data_station$t_fah*gsod_data_station$t_fah*gsod_data_station$RH)
    +(0.00085282*gsod_data_station$t_fah*gsod_data_station$RH*gsod_data_station$RH)-(0.00000199*gsod_data_station$t_fah*gsod_data_station$t_fah*gsod_data_station$RH*gsod_data_station$RH)
    , 1)
  
  gsod_data_station$hi <- round (
    (gsod_data_station$hi_fah - 32) * (5/9)
    , 1)
  
  # write 2004-2020 data for the station out as csv
  write.csv(gsod_data_station, paste0(output_path,"gsod_data_station_",uid,"_",loc,"_",stationid,"_2004-2020_Apr-Jul.csv"), row.names = F)
  
  
  # calculate the percentile for hi, t, tmax, tmin, rh from the himax day during the event
  # uses the type 1 quantile function of ecdf
  # add the results to the dataframe
  gsod_events_summary_himaxday_percentiles$t_perc[i]      <- ecdf(gsod_data_station$TEMP)(gsod_events_summary_himaxday$t[i])
  gsod_events_summary_himaxday_percentiles$tdewp_perc[i]  <- ecdf(gsod_data_station$DEWP)(gsod_events_summary_himaxday$tdewp[i])
  gsod_events_summary_himaxday_percentiles$tmax_perc[i]   <- ecdf(gsod_data_station$MAX)(gsod_events_summary_himaxday$tmax[i])
  gsod_events_summary_himaxday_percentiles$tmin_perc[i]   <- ecdf(gsod_data_station$MIN)(gsod_events_summary_himaxday$tmin[i])
  gsod_events_summary_himaxday_percentiles$rh_perc[i]     <- ecdf(gsod_data_station$RH)(gsod_events_summary_himaxday$rh[i])
  gsod_events_summary_himaxday_percentiles$hi_perc[i]     <- ecdf(gsod_data_station$hi)(gsod_events_summary_himaxday$hi[i])

  
# end loop through stations
}


# write out to csv
write.csv(gsod_events_summary_himaxday_percentiles, paste0(output_path,"gsod_events_summary_himaxday_percentiles_2004-2020_Apr-Jul.csv"), row.names = F)















