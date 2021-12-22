################################################################################
# export_log_gsod_routine_vm.R
# Code to export the log of the scheduled daily task to the same Google Drive
# location as the output file, to make it easier for users to access the log file
# in case of issues.
# Author: Alex Saunders
# Date created: 22/12/2021
# Date modified: 22/12/2021
################################################################################

input_path <- "/home/start/"
output_path <- input_path

packages = c("googledrive")
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

# upload log file to the google drive
drive_path <- "gsod_routine"
drive_upload(paste0(output_path,"extract_latest_vm.log"), path = drive_path, name = "log", type = "spreadsheet", overwrite = T) #type = "text/csv"
print(paste0("log file was uploaded to Google Drive path ", drive_path, " at ", Sys.time()))


################################################################################
# END
################################################################################

