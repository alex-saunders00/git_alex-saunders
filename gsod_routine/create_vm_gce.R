projectid = "vast-alcove-335416"
zone = "us-central1-b"
account_key = "C://Users/alexa/Documents/GCE/start-gsod-routine/vast-alcove-335416-d119ca08ad01.json"

Sys.setenv(GCE_AUTH_FILE = account_key,
           GCE_DEFAULT_PROJECT_ID = projectid,
           GCE_DEFAULT_ZONE = zone
)

install.packages("googleComputeEngineR")
library(googleComputeEngineR)

# output:
# Setting scopes to https://www.googleapis.com/auth/cloud-platform
# Successfully auto-authenticated via C://Users/alexa/Documents/GCE/start-gsod-routine/vast-alcove-335416-d119ca08ad01.json
# Set default project ID to 'vast-alcove-335416'
# Set default zone to 'us-central1'

#gce_global_project(project = projectid)
#gce_global_zone(zone = zone)


# create the VM with rstudio template
vm <- gce_vm(template = "rstudio", 
             name = "vm-start-rstudio",
             predefined_type = "e2-micro", 
             username = "start",
             password = "startnetwork-carf")

# output:
# 2021-12-17 19:22:54> Creating template VM
# 2021-12-17 19:22:58> Checking operation...PENDING
# 2021-12-17 19:23:08> Operation running...
# 2021-12-17 19:23:19> Operation running...
# 2021-12-17 19:23:39> Operation complete in 29 secs
# 2021-12-17 19:23:40> ## VM Template: 'rstudio' running at http://146.148.96.184
#   2021-12-17 19:23:40> On first boot, wait a few minutes for docker container to install before logging in.
# ==Google Compute Engine Instance==
#   
#   Name:                vm-start-rstudio
# Created:             2021-12-17 11:22:57
# Machine Type:        e2-micro
# Status:              RUNNING
# Zone:                us-central1-b
# External IP:         146.148.96.184
# Disks: 
#   deviceName       type       mode boot autoDelete
# 1 vm-start-rstudio-boot-disk PERSISTENT READ_WRITE TRUE       TRUE
# 
# Metadata:  
#   key             value
# 2               template           rstudio
# 3 google-logging-enabled              true
# 4           rstudio_user             start
# 5             rstudio_pw startnetwork-carf
# 6      gcer_docker_image  rocker/tidyverse
# 2021-12-17 19:23:40> Creating http firewall rule
# 2021-12-17 19:23:41> Operation running...
# 2021-12-17 19:23:47> Operation complete in 3 secs
# 2021-12-17 19:23:47> Creating https firewall rule
# 2021-12-17 19:23:48> Operation running...
# 2021-12-17 19:23:55> Operation complete in 4 secs
# 2021-12-17 19:23:55> vm-start-rstudio VM running


# can now access the vm via the url: http://146.148.96.184

