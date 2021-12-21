################################################################################
# get_input_from_drive_gsod_routine_locs.R
# This script only needs to be run once, or again if the input locations
# need to be updated. Reads in the inputs from a csv saved at Google Drive and
# makes a copy locally.
# get_input_from_drive_gsod_routine_locs.R makes use of package "googledrive"
# to access and read the file at the Google Drive
# Author: Alex Saunders
# Date created: 21/12/2021
# Date modified: 21/12/2021
################################################################################

# install googledrive package
install.packages("googledrive")
library("googledrive")


# get working directory
getwd()
list.files()


# search for the gsod_routine_locs.csv file within the Drive - this contains 
# the input locations and stations for which to extract gsod data
# drive_find will the first time ask for authentication to the Google Drive
# account, creates an authorization code which is pasted here to give access
# to the Google Drive
drive_find(pattern = "gsod_routine_locs")


# having setup the authentication and found that the gsod_routine_locs.csv file
# exists, download the file to local
drive_download("gsod_routine_locs.csv", overwrite = T)
## File downloaded:
##   . gsod_routine_locs.csv <id: 1BylNLX3sZoeAnzm_4cuEgYWRiYrB5QIB>
##   Saved locally as:
##   . gsod_routine_locs.csv
# exists at "/home/start/gsod_routine_locs.csv"



