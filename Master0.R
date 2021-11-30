#####################################
### Master file calling R-scripts ###
###    to run the programm        ###
#####################################

# The folder of the project
W = "C:/works/diploma"
WR = paste(W,"results",sep = "/")
WP = paste(W,"programs",sep = "/")
WG = paste(W,"graphs",sep = "/")
WD = paste(W,"data",sep = "/")
WRD = paste(W,"raw_data",sep = "/")

library(haven)
library(gdata)
library(tidyverse)
library(dplyr)
library(lubridate)

# Sub master calling R scripts to build the Longitudinal format of the data.  
#setwd(WP)
#source("masterLongWithoutMaxdate.R")

# reduce missingness of conditions and  Multimorbidity 
setwd(WD)
total=read_dta("total.dta")
setwd(WP)
source("ReduceMissingness.R")

# check the missingness
setwd(WP)
source("missingness.R")
# Prepare the data for analysis
setwd(WP)
source("PrepareForAnalysis.R")
table(total$multimorbidity,useNA = "always")

# Clear workspace
rm(list = as.character(ls()[!(ls() %in% c("W","WR","WP","WG","WD","WRD","total","total2","hospsToInclude","exportResList"))]) )
