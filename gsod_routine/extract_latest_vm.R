################################################################################
# extract_latest_vm.R
# Code to routinely extract gsod meteorological variables and calculate heatwave 
# metrics for selected locations in Pakistan.
# extract_latest_vm.R makes use of the package "GSODR" by Adam H Sparks to 
# extract the latest GSOD data and write out as a csv file for selected stations.
# It also makes use of "googledrive" to upload the files to a shared Google
# Drive location.
# Author: Alex Saunders
# Date created: 17/12/2021
# Date modified: 21/12/2021
################################################################################


################################################################################
# Install packages and set up working environment
################################################################################

rm(list=ls())
print(paste0("*** started routine gsod extraction at ",Sys.time(), " ***"))

packages = c("GSODR","lubridate","googledrive")
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
input_path <- "/home/start/"
output_path <- input_path


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
  print(locs_ok[,c("loc","stationid","NAME")])
} else {
  print("locations to be extracted:")
  print(locs_ok[,c("loc","stationid","NAME")])
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
print(paste0("attempting to download data for start of ", latestyear, " to ", latestday))
gsod_data <- get_GSOD(years = latestyear, station = locs_ok$STNID)

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
  +(0.00085282*gsod_data$t_fah*gsod_data$RH*gsod_data$RH)
  -(0.00000199*gsod_data$t_fah*gsod_data$t_fah*gsod_data$RH*gsod_data$RH)
  , 1)

gsod_data$hi <- round (
  (gsod_data$hi_fah - 32) * (5/9)
  , 1)

# drop unwanted variables and tidy up column names
gsod_data <- merge(locs_ok[,c("STNID","loc","stationid")], gsod_data)
keep <- c("loc","stationid","NAME","YEARMODA",
          "TEMP","MAX","MIN","DEWP","RH","hi")
gsod_data <- gsod_data[, keep]
colnames(gsod_data) <- c("location","station_id","station_name","date",
                         "t_cel","tmax_cel","tmin_cel","td_cel","rh","hi_cel")


# check for each station that data exists for all days and fill with NAs where 
# data does not exist
days <- seq(as.Date(paste0(latestyear,"-01-01")), as.Date(latestday), "days")
rows_total <- length(days) * length(locs_check$STNID)
gsod_data_fill <- as.data.frame(matrix(0, ncol = ncol(gsod_data), nrow = rows_total - nrow(gsod_data)))
colnames(gsod_data_fill) <- colnames(gsod_data)
class(gsod_data_fill$date) <- "Date"
i <- 0

for (l in 1:length(locs_check$STNID)) {
  loc <- locs_ok$loc[l]
  
  for (d in 1:length(days)) {
    day <- days[d]
    
    if (nrow(gsod_data[which(gsod_data$loc == loc 
                             & gsod_data$date == day),]) < 1) {
      i <- i+1
      gsod_data_fill$location[i] <- locs_ok$loc[l]
      gsod_data_fill$station_id[i] <- locs_ok$stationid[l]
      gsod_data_fill$station_name[i] <- locs_ok$NAME[l]
      gsod_data_fill$date[i] <- day
      gsod_data_fill[i, c("t_cel","tmax_cel","tmin_cel","td_cel","rh","hi_cel")] <- NA
      
    }
  }
}

# append the days with missing data back to the gsod data and order by date
gsod_data <- rbind(gsod_data, gsod_data_fill)
gsod_data <- gsod_data[order(gsod_data$date, decreasing = T),]

# add flag for missing day of data and last updated column
gsod_data$missing <- 0
gsod_data$missing[is.na(gsod_data$t_cel)] <- 1
gsod_data$last_updated <- Sys.time()


################################################################################
# Write outputs locally
################################################################################

# write out the data for all days until latest day - overwrites the previous days data
write.csv(gsod_data, paste0(output_path,"start_gsod_year_to_date.csv"), row.names = F)

# subset to the latest day and write out
gsod_data_latestday <- gsod_data[which(gsod_data$date == latestday),]
write.csv(gsod_data_latestday, paste0(output_path,"start_gsod_latest.csv"), row.names = F)

# logging
print(paste0("latest day data was available for ", length(gsod_data_latestday$missing[which(gsod_data_latestday$missing == 0)])," out of total ",length(gsod_data_latestday$missing)," stations"))
print(paste0("year to date and latest day data were saved to ", output_path))


################################################################################
# Write outputs to Google Drive
################################################################################

# upload files to the google drive
drive_path <- "gsod_routine"
drive_upload(paste0(output_path,"start_gsod_year_to_date.csv"), path = drive_path, name = "start_gsod_year_to_date", type = "spreadsheet", overwrite = T) #type = "text/csv"
drive_upload(paste0(output_path,"start_gsod_latest.csv"), path = drive_path, name = "start_gsod_latest", type = "spreadsheet", overwrite = T)
print(paste0("latest day data was uploaded to Google Drive path ", drive_path, " at ", Sys.time()))

# allow access to the files for anyone with the link to the drive location
drive_share_anyone("start_gsod_year_to_date")
drive_share_anyone("start_gsod_latest")
print(paste0("latest day data access permissions granted"))
print(paste0("***** finished routine gsod extraction at ",Sys.time(), " *****"))

rm(list=ls())

################################################################################
# END
################################################################################
