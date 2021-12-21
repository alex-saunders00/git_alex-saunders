################################################################################
# check_log_gsod_routine_vm.R
# Code to check the log of the scheduled daily task to extract gsod data for
# Pakistan.
# Author: Alex Saunders
# Date created: 21/12/2021
# Date modified: 21/12/2021
################################################################################

rm(list=ls())

# show files in working directory
input_path <- "/home/start/"
list.files()

# log file created time
created <- file.info("extract_latest_vm.log")$ctime
created

# log file contents
log <- read.csv("extract_latest_vm.log", header = F)
log
