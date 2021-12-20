################################################################################
# run_daily.R
# Code to routinely extract gsod meteorological variables and calculate heatwave 
# metrics for selected locations in Pakistan.
# run_daily.R makes use of the package "taskscheduleR" to create a scheduled task
# which runs the GSOD extraction at the same time every day.
# Author: Alex Saunders
# Date created: 17/12/2021
# Date modified: 17/12/2021
################################################################################


################################################################################
# Install packages and set up working environment
################################################################################

rm(list=ls())

packages = c("taskscheduleR")

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

# set script path
script_path <- "C://Users/alexa/Documents/GitHub/git_alex-saunders/gsod_routine/"


################################################################################
# Schedule the script to run automatically every day
################################################################################

# create the scheduled task to run daily at the specified time
starttime <- "16:00"
taskscheduler_create(taskname = "extract_gsod_latest_daily", rscript = paste0(script_path,"extract_latest.R"), 
                     schedule = "DAILY", starttime = starttime)

# list currently scheduled tasks
#tasks <- taskscheduler_ls()
#tasks[grep("gsod",tasks$TaskName),]

# delete test tasks
#taskscheduler_delete("extract_gsod_latest")


################################################################################
# END
################################################################################





