################################################################################
# set_up_run_daily_vm.R
# This script only needs to be run once, or again if the automated run schedule
# need to be updated. 
# set_up_run_daily_vm.R makes use of package "cronR" to establish an 
# automated task to be run every day at the same time.
# Here the interactive GUI Rshiny is used to make the scheduling even simpler,
# but the code for cron_add() works too.
# It calls the script "extract_latest_vm.R" which then does all the work.
# Author: Alex Saunders
# Date created: 21/12/2021
# Date modified: 22/12/2021
################################################################################


################################################################################
# Install packages and set up working environment
################################################################################

rm(list=ls())

packages = c("cronR","miniUI","shiny","shinyFiles")

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
script_path <- "/home/start/"
list.files(script_path, full.names = T)

################################################################################
# Schedule the script to run automatically every day
################################################################################

# create the scheduled task to run daily at the specified time
cmd <- cron_rscript(paste0(script_path,"/extract_latest_vm.R"),
                    log_append = F, log_timestamp = F)

cron_add(cmd, frequency = "daily", at = "06:00",
         id = "gsod_routine_0600", 
         description = "extract latest gsod data at 06AM daily run on vm",
         ask = F)

# list or remove existing cron jobs
#cron_ls()
#cron_rm(id = "gsod_routine_midday", ask = F) # use the id

# create the scheduled task to export the log file at the specified time
cmd <- cron_rscript(paste0(script_path,"/export_log_gsod_routine.R"),
                    log_append = F, log_timestamp = F)

cron_add(cmd, frequency = "daily", at = "06:10",
         id = "gsod_routine_logfile_0610", 
         description = "export log file at 06AM daily run on vm",
         ask = F)

################################################################################
# END
################################################################################


