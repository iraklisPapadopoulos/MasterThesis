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

# Year of the analysis
year=2000

#minimum age in study
minAge=18

# Maximum date for the analyses
endDate = as.Date(paste0(year,"-12-31"))

# minimum date for the analyses
startDate = as.Date(paste0(year,"-01-01"))

#exclude dates before that date in getMaxDate R programm
exBefore= as.Date("1980-1-1")

# Time gap for the definition of lost to follow up (in days)
gap = 1.5*365.25

# BMI boundaries
supBMI=40
infBMI=14

# eGFR limit to find renal disease
egfrLimit=60

# Libraries
library(haven)
library(gdata)
library(tidyverse)
library(dplyr)
library(lubridate)
library(transplantr)

# Get the maxdate before 31/12/year
#setwd(WP)
#source("getMaxDate.R")

# Apply the eligibility criteria for final tblbas
setwd(WP)
source("elig1.R")
dim(tblbas)

# Apply the eligibility criteria to find Multimorbidities
setwd(WP)
source("eligMM.R")
dim(tblbas)
table(finaltbl$multimor)
table(finaltbl$mm,useNA = "always")

# Clear workspace
rm(list = as.character(ls()[!(ls() %in% c("W","WR","WP","WG","WD","WRD","hospsToInclude","exportResList"))]) )

# Year of the analysis
year=2014

# Maximum date for the analyses
endDate = as.Date(paste0(year,"-12-31"))

# Time gap for the definition of lost to follow up (in days)
gap = 1.5*365.25

# Get the maxdate before 31/12/year
setwd(WP)
source("getMaxDate.R")

# Apply the eligibility criteria for final tblbas
setwd(WP)
source("elig1.R")

# Apply the eligibility criteria to find Multimorbidities
setwd(WP)
source("eligMM.R")

table(finaltbl$mm)

###################
### other year  ###
###################