################################################################################
# extract_gsod_latestday.R
# Code to routinely extract gsod meteorological variables and calculate heatwave 
# metrics for selected locations in Pakistan.
# extract_gsod_latestday.R makes use of the package "GSODR" by Adam H Sparks to 
# extract the latest GSOD data and write out as a csv file for selected stations.
# Author: Alex Saunders
# Date created: 17/12/2021
# Date modified: 17/12/2021
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
output_path <- paste0(gsub("01_input","02_output",input_path),"gsod_routine/")


# set fixed a and b parameters for calculation of relative humidity
a <- 17.625
b <- 243.04


################################################################################
# Get all GSOD stations for Pakistan
################################################################################

# update the station list based on the latest data from NCEI - not required
#update_station_list()
load(system.file("extdata", "isd_history.rda", package = "GSODR"))

# create data.frame of stations for Pakistan only
gsod_stations_pk <- subset(isd_history, COUNTRY_NAME == "PAKISTAN")


################################################################################
# Import list of target stations and check they exist in GSOD
################################################################################

# load csv file of station names / id required
# currently interested in six stations, more can be added by updating input csv
locs <- read.csv(paste0(input_path,"gsod_routine_locs.csv"))

# make stationid format consistent
locs$STNID <- paste0(substr(locs$stationid,1,6),"-",substr(locs$stationid,7,11))

# check target stations exist in gsod data, provide a warning if not
locs_check <- merge(locs, gsod_stations_pk, by = "STNID", all.x = T, all.y = F)
locs_na <- locs_check[is.na(locs_check$NAME),]
locs_ok <- locs_check[!is.na(locs_check$NAME),]

if (nrow(locs_na) > 0) {
  print("warning: one or more stations are not recognised in GSOD")
  print(locs_na[,c("loc","stationid")])
  print("locations to be extracted:")
  print(locs_ok)
} else {
  print("locations to be extracted:")
  print(locs_ok)
}


################################################################################
# Extract GSOD meteorological data up to most recent day
################################################################################

# pass to extract_gsod a list of stations and the current year
# accounts for latency by subtracting 5 days, i.e. on 01/01/2022 we still need
# data for 27/12/2021
lagdays <- 5
latestyear <- as.integer(format(Sys.Date()-lagdays, "%Y"))
latestday  <- Sys.Date()-lagdays
gsod_data <- get_GSOD(years = latestyear, station = locs_ok$STNID)

# order by date with most recent first
gsod_data <- gsod_data[order(gsod_data$YEARMODA, decreasing = T)]


# add heatwave variables

# add RH, calculated using Magnus formula
gsod_data$RH <- round(100 * (exp((a * gsod_data$DEWP) / (b + gsod_data$DEWP))
                              / exp((a * gsod_data$TEMP) / (b + gsod_data$TEMP))),1)

# add HI, calculated using NOAA formula
gsod_data$t_fah <- (gsod_data$TEMP * (9/5)) + 32

gsod_data$hi_fah <- round (
  -42.379+(2.04901523*gsod_data$t_fah)+(10.14333127*gsod_data$RH)
  -(0.22475541*gsod_data$t_fah*gsod_data$RH)-(0.00683783*gsod_data$t_fah*gsod_data$t_fah)
  -(0.05481717*gsod_data$RH*gsod_data$RH)+(0.00122874*gsod_data$t_fah*gsod_data$t_fah*gsod_data$RH)
  +(0.00085282*gsod_data$t_fah*gsod_data$RH*gsod_data$RH)-(0.00000199*gsod_data$t_fah*gsod_data$t_fah*gsod_data$RH*gsod_data$RH)
  , 1)

gsod_data$hi <- round (
  (gsod_data$hi_fah - 32) * (5/9)
  , 1)

# drop unwanted variables and tidy up column names
keep <- c("STNID","NAME","CTRY","LATITUDE","LONGITUDE","ELEVATION","BEGIN","END","YEARMODA",
          "YEAR","MONTH","DAY","YDAY","TEMP","MAX","MIN","DEWP","RH","hi") #"t_fah","hi_fah"
gsod_data <- gsod_data[, ..keep]
colnames(gsod_data) <- c("STNID","NAME","CTRY","LATITUDE","LONGITUDE","ELEVATION","BEGIN",
                         "END","YEARMODA","YEAR","MONTH","DAY","YDAY",
                         "t_cel","tmax_cel","tmin_cel","td_cel","rh","hi_cel")

# update END to be correct (in case it contains an older date from the station
# list database)
for (STNID in locs_ok$STNID) {
  enddate <- as.integer(gsub("-","",max(gsod_data$YEARMODA[which(gsod_data$STNID == STNID)])))
  gsod_data[which(gsod_data$STNID == STNID), "END"] <- enddate
}


# subset to the latest day only
gsod_data_latestday <- gsod_data[which(gsod_data$YEARMODA == latestday),]

# check that data exists for latest day for all stations and fill with NAs
# where the data is missing
gsod_data_latestday <- merge(locs, gsod_data_latestday, by = "STNID", all.x = T, all.y = F)

if (any(is.na(gsod_data_latestday$t_cel))) {

  gsod_data_latestday[, c("YEARMODA","YEAR","MONTH","DAY","YDAY")] <- unique(gsod_data_latestday[!is.na(gsod_data_latestday$t_cel),c("YEARMODA","YEAR","MONTH","DAY","YDAY")])
  gsod_data_latestday <- gsod_data_latestday[, c("STNID","loc","stationid","NAME.x","CTRY.x","LAT","LON","ELEVATION","BEGIN.x",
                                                 "END.x","YEARMODA","YEAR","MONTH","DAY","YDAY",
                                                 "t_cel","tmax_cel","tmin_cel","td_cel","rh","hi_cel")]
  colnames(gsod_data_latestday) <- c("STNID","loc","stationid","NAME","CTRY","LATITUDE","LONGITUDE","ELEVATION","BEGIN",
                                     "END","YEARMODA","YEAR","MONTH","DAY","YDAY",
                                     "t_cel","tmax_cel","tmin_cel","td_cel","rh","hi_cel")
  
  # update attributes for stationid (elevation and enddate) which are missing
  for (STNID in locs_ok$STNID) {
    gsod_data_latestday[which(gsod_data_latestday$STNID == STNID), "END"] <- unique(gsod_data$END[which(gsod_data$STNID == STNID)])
    gsod_data_latestday[which(gsod_data_latestday$STNID == STNID), "ELEVATION"] <- unique(gsod_data$ELEVATION[which(gsod_data$STNID == STNID)])
  }

} else {
  colnames(gsod_data_latestday) <- c("STNID","loc","stationid","NAME","CTRY","LATITUDE","LONGITUDE","ELEVATION","BEGIN",
                                     "END","YEARMODA","YEAR","MONTH","DAY","YDAY",
                                     "t_cel","tmax_cel","tmin_cel","td_cel","rh","hi_cel")
}

# add flag for missing day of data
gsod_data_latestday$missingday <- 0
gsod_data_latestday$missingday[is.na(gsod_data_latestday$t_cel)] <- 1

# write out the data for the latest day
write.csv(gsod_data_latestday,paste0(output_path,"gsod_data_latestday_",Sys.Date(),".csv"), row.names = F)


################################################################################
# END
################################################################################





