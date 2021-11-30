#####################################
### Master file calling R-scripts ###
### to run the simulations        ###
#####################################
# The folder of the project
W = "C:/works/diploma"
WR = paste(W,"results",sep = "/")
WP = paste(W,"programs",sep = "/")
WG = paste(W,"graphs",sep = "/")
WD = paste(W,"data",sep = "/")
WRD = paste(W,"raw_data",sep = "/")
################
####  2000  ####
################
# Year of the analysis
year=2000
#minimum age in study
minAge=18
# Maximum date for the analyses
endDate = as.Date(paste0(year,"-12-31"))
#exclude dates before that date in getMaxDate R programm
exBefore= as.Date("1980-1-1")
# Time gap for the definition of lost to follow up (in days)
gap = 1.5*365.25
# BMI boundaries
supBMI=40
infBMI=14
# Libraries
library(haven)
library(gdata)
library(tidyverse)
library(dplyr)
library(lubridate)
library(transplantr)
# Get the maxdate before 31/12/year
setwd(WP)
source("getMaxDate.R")
# Apply the eligibility criteria for final tblbas
setwd(WP)
source("elig1.R")
dim(tblbas)
# Apply the eligibility criteria to find Multimorbidities
setwd(WP)
source("eligMM.R")
dim(tblbas)
table(finaltbl$mm)
finaltbl$CalendarYear=year
total=finaltbl
# Clear workspace
rm(list = as.character(ls()[!(ls() %in% c("W","WR","WP","WG","WD","WRD","total","hospsToInclude","exportResList"))]) )
################
####  2001  ####
################
# Year of the analysis
year=2001
#minimum age in study
minAge=18
# Maximum date for the analyses
endDate = as.Date(paste0(year,"-12-31"))
#exclude dates before that date in getMaxDate R programm
exBefore= as.Date("1980-1-1")
# Time gap for the definition of lost to follow up (in days)
gap = 1.5*365.25
# BMI boundaries
supBMI=40
infBMI=14
# Libraries
library(haven)
library(gdata)
library(tidyverse)
library(dplyr)
library(lubridate)
library(transplantr)
# Get the maxdate before 31/12/year
setwd(WP)
source("getMaxDate.R")
# Apply the eligibility criteria for final tblbas
setwd(WP)
source("elig1.R")
dim(tblbas)
# Apply the eligibility criteria to find Multimorbidities
setwd(WP)
source("eligMM.R")
dim(tblbas)
table(finaltbl$mm)
finaltbl$CalendarYear=year
total=rbind.data.frame(total,finaltbl)
# Clear workspace
rm(list = as.character(ls()[!(ls() %in% c("W","WR","WP","WG","WD","WRD","total","hospsToInclude","exportResList"))]) )
################
####  2002  ####
################
# Year of the analysis
year=2002
#minimum age in study
minAge=18
# Maximum date for the analyses
endDate = as.Date(paste0(year,"-12-31"))
#exclude dates before that date in getMaxDate R programm
exBefore= as.Date("1980-1-1")
# Time gap for the definition of lost to follow up (in days)
gap = 1.5*365.25
# BMI boundaries
supBMI=40
infBMI=14
# Libraries
library(haven)
library(gdata)
library(tidyverse)
library(dplyr)
library(lubridate)
# Get the maxdate before 31/12/year
setwd(WP)
source("getMaxDate.R")
# Apply the eligibility criteria for final tblbas
setwd(WP)
source("elig1.R")
dim(tblbas)
# Apply the eligibility criteria to find Multimorbidities
setwd(WP)
source("eligMM.R")
dim(tblbas)
table(finaltbl$mm)
finaltbl$CalendarYear=year
total=rbind.data.frame(total,finaltbl)
# Clear workspace
rm(list = as.character(ls()[!(ls() %in% c("W","WR","WP","WG","WD","WRD","total","hospsToInclude","exportResList"))]) )
################
####  2003  ####
################
# Year of the analysis
year=2003
#minimum age in study
minAge=18
# Maximum date for the analyses
endDate = as.Date(paste0(year,"-12-31"))
#exclude dates before that date in getMaxDate R programm
exBefore= as.Date("1980-1-1")
# Time gap for the definition of lost to follow up (in days)
gap = 1.5*365.25
# BMI boundaries
supBMI=40
infBMI=14
# Libraries
library(haven)
library(gdata)
library(tidyverse)
library(dplyr)
library(lubridate)
# Get the maxdate before 31/12/year
setwd(WP)
source("getMaxDate.R")
# Apply the eligibility criteria for final tblbas
setwd(WP)
source("elig1.R")
dim(tblbas)
# Apply the eligibility criteria to find Multimorbidities
setwd(WP)
source("eligMM.R")
dim(tblbas)
table(finaltbl$mm)
finaltbl$CalendarYear=year
total=rbind.data.frame(total,finaltbl)
# Clear workspace
rm(list = as.character(ls()[!(ls() %in% c("W","WR","WP","WG","WD","WRD","total","hospsToInclude","exportResList"))]) )
################
####  2004  ####
################
# Year of the analysis
year=2004
#minimum age in study
minAge=18
# Maximum date for the analyses
endDate = as.Date(paste0(year,"-12-31"))
#exclude dates before that date in getMaxDate R programm
exBefore= as.Date("1980-1-1")
# Time gap for the definition of lost to follow up (in days)
gap = 1.5*365.25
# BMI boundaries
supBMI=40
infBMI=14
# Libraries
library(haven)
library(gdata)
library(tidyverse)
library(dplyr)
library(lubridate)
# Get the maxdate before 31/12/year
setwd(WP)
source("getMaxDate.R")
# Apply the eligibility criteria for final tblbas
setwd(WP)
source("elig1.R")
dim(tblbas)
# Apply the eligibility criteria to find Multimorbidities
setwd(WP)
source("eligMM.R")
dim(tblbas)
table(finaltbl$mm)
finaltbl$CalendarYear=year
total=rbind.data.frame(total,finaltbl)
# Clear workspace
rm(list = as.character(ls()[!(ls() %in% c("W","WR","WP","WG","WD","WRD","total","hospsToInclude","exportResList"))]) )
################
####  2005  ####
################
# Year of the analysis
year=2005
#minimum age in study
minAge=18
# Maximum date for the analyses
endDate = as.Date(paste0(year,"-12-31"))
#exclude dates before that date in getMaxDate R programm
exBefore= as.Date("1980-1-1")
# Time gap for the definition of lost to follow up (in days)
gap = 1.5*365.25
# BMI boundaries
supBMI=40
infBMI=14
# Libraries
library(haven)
library(gdata)
library(tidyverse)
library(dplyr)
library(lubridate)
# Get the maxdate before 31/12/year
setwd(WP)
source("getMaxDate.R")
# Apply the eligibility criteria for final tblbas
setwd(WP)
source("elig1.R")
dim(tblbas)
# Apply the eligibility criteria to find Multimorbidities
setwd(WP)
source("eligMM.R")
dim(tblbas)
table(finaltbl$mm)
finaltbl$CalendarYear=year
total=rbind.data.frame(total,finaltbl)
# Clear workspace
rm(list = as.character(ls()[!(ls() %in% c("W","WR","WP","WG","WD","WRD","total","hospsToInclude","exportResList"))]) )
################
####  2006  ####
################
# Year of the analysis
year=2006
#minimum age in study
minAge=18
# Maximum date for the analyses
endDate = as.Date(paste0(year,"-12-31"))
#exclude dates before that date in getMaxDate R programm
exBefore= as.Date("1980-1-1")
# Time gap for the definition of lost to follow up (in days)
gap = 1.5*365.25
# BMI boundaries
supBMI=40
infBMI=14
# Libraries
library(haven)
library(gdata)
library(tidyverse)
library(dplyr)
library(lubridate)
# Get the maxdate before 31/12/year
setwd(WP)
source("getMaxDate.R")
# Apply the eligibility criteria for final tblbas
setwd(WP)
source("elig1.R")
dim(tblbas)
# Apply the eligibility criteria to find Multimorbidities
setwd(WP)
source("eligMM.R")
dim(tblbas)
table(finaltbl$mm)
finaltbl$CalendarYear=year
total=rbind.data.frame(total,finaltbl)
# Clear workspace
rm(list = as.character(ls()[!(ls() %in% c("W","WR","WP","WG","WD","WRD","total","hospsToInclude","exportResList"))]) )
################
####  2007  ####
################
# Year of the analysis
year=2007
#minimum age in study
minAge=18
# Maximum date for the analyses
endDate = as.Date(paste0(year,"-12-31"))
#exclude dates before that date in getMaxDate R programm
exBefore= as.Date("1980-1-1")
# Time gap for the definition of lost to follow up (in days)
gap = 1.5*365.25
# BMI boundaries
supBMI=40
infBMI=14
# Libraries
library(haven)
library(gdata)
library(tidyverse)
library(dplyr)
library(lubridate)
# Get the maxdate before 31/12/year
setwd(WP)
source("getMaxDate.R")
# Apply the eligibility criteria for final tblbas
setwd(WP)
source("elig1.R")
dim(tblbas)
# Apply the eligibility criteria to find Multimorbidities
setwd(WP)
source("eligMM.R")
dim(tblbas)
table(finaltbl$mm)
finaltbl$CalendarYear=year
total=rbind.data.frame(total,finaltbl)
# Clear workspace
rm(list = as.character(ls()[!(ls() %in% c("W","WR","WP","WG","WD","WRD","total","hospsToInclude","exportResList"))]) )
################
####  2008  ####
################
# Year of the analysis
year=2008
#minimum age in study
minAge=18
# Maximum date for the analyses
endDate = as.Date(paste0(year,"-12-31"))
#exclude dates before that date in getMaxDate R programm
exBefore= as.Date("1980-1-1")
# Time gap for the definition of lost to follow up (in days)
gap = 1.5*365.25
# BMI boundaries
supBMI=40
infBMI=14
# Libraries
library(haven)
library(gdata)
library(tidyverse)
library(dplyr)
library(lubridate)
# Get the maxdate before 31/12/year
setwd(WP)
source("getMaxDate.R")
# Apply the eligibility criteria for final tblbas
setwd(WP)
source("elig1.R")
dim(tblbas)

# Apply the eligibility criteria to find Multimorbidities
setwd(WP)
source("eligMM.R")
dim(tblbas)
table(finaltbl$mm)
finaltbl$CalendarYear=year
total=rbind.data.frame(total,finaltbl)
# Clear workspace
rm(list = as.character(ls()[!(ls() %in% c("W","WR","WP","WG","WD","WRD","total","hospsToInclude","exportResList"))]) )
################
####  2009  ####
################
# Year of the analysis
year=2009
#minimum age in study
minAge=18
# Maximum date for the analyses
endDate = as.Date(paste0(year,"-12-31"))
#exclude dates before that date in getMaxDate R programm
exBefore= as.Date("1980-1-1")
# Time gap for the definition of lost to follow up (in days)
gap = 1.5*365.25
# BMI boundaries
supBMI=40
infBMI=14
# Libraries
library(haven)
library(gdata)
library(tidyverse)
library(dplyr)
library(lubridate)
# Get the maxdate before 31/12/year
setwd(WP)
source("getMaxDate.R")
# Apply the eligibility criteria for final tblbas
setwd(WP)
source("elig1.R")
dim(tblbas)
# Apply the eligibility criteria to find Multimorbidities
setwd(WP)
source("eligMM.R")
dim(tblbas)
table(finaltbl$mm)
finaltbl$CalendarYear=year
total=rbind.data.frame(total,finaltbl)
# Clear workspace
rm(list = as.character(ls()[!(ls() %in% c("W","WR","WP","WG","WD","WRD","total","hospsToInclude","exportResList"))]) )
################
####  2010  ####
################
# Year of the analysis
year=2010
#minimum age in study
minAge=18
# Maximum date for the analyses
endDate = as.Date(paste0(year,"-12-31"))
#exclude dates before that date in getMaxDate R programm
exBefore= as.Date("1980-1-1")
# Time gap for the definition of lost to follow up (in days)
gap = 1.5*365.25
# BMI boundaries
supBMI=40
infBMI=14
# Libraries
library(haven)
library(gdata)
library(tidyverse)
library(dplyr)
library(lubridate)
# Get the maxdate before 31/12/year
setwd(WP)
source("getMaxDate.R")
# Apply the eligibility criteria for final tblbas
setwd(WP)
source("elig1.R")
dim(tblbas)
# Apply the eligibility criteria to find Multimorbidities
setwd(WP)
source("eligMM.R")
dim(tblbas)
table(finaltbl$mm)
finaltbl$CalendarYear=year
total=rbind.data.frame(total,finaltbl)
# Clear workspace
rm(list = as.character(ls()[!(ls() %in% c("W","WR","WP","WG","WD","WRD","total","hospsToInclude","exportResList"))]) )
################
####  2011  ####
################
# Year of the analysis
year=2011
#minimum age in study
minAge=18
# Maximum date for the analyses
endDate = as.Date(paste0(year,"-12-31"))
#exclude dates before that date in getMaxDate R programm
exBefore= as.Date("1980-1-1")
# Time gap for the definition of lost to follow up (in days)
gap = 1.5*365.25
# BMI boundaries
supBMI=40
infBMI=14
# Libraries
library(haven)
library(gdata)
library(tidyverse)
library(dplyr)
library(lubridate)
# Get the maxdate before 31/12/year
setwd(WP)
source("getMaxDate.R")
# Apply the eligibility criteria for final tblbas
setwd(WP)
source("elig1.R")
dim(tblbas)
# Apply the eligibility criteria to find Multimorbidities
setwd(WP)
source("eligMM.R")
dim(tblbas)
table(finaltbl$mm)
finaltbl$CalendarYear=year
total=rbind.data.frame(total,finaltbl)
# Clear workspace
rm(list = as.character(ls()[!(ls() %in% c("W","WR","WP","WG","WD","WRD","total","hospsToInclude","exportResList"))]) )
################
####  2012  ####
################
# Year of the analysis
year=2012
#minimum age in study
minAge=18
# Maximum date for the analyses
endDate = as.Date(paste0(year,"-12-31"))
#exclude dates before that date in getMaxDate R programm
exBefore= as.Date("1980-1-1")
# Time gap for the definition of lost to follow up (in days)
gap = 1.5*365.25
# BMI boundaries
supBMI=40
infBMI=14
# Libraries
library(haven)
library(gdata)
library(tidyverse)
library(dplyr)
library(lubridate)
# Get the maxdate before 31/12/year
setwd(WP)
source("getMaxDate.R")
# Apply the eligibility criteria for final tblbas
setwd(WP)
source("elig1.R")
dim(tblbas)
# Apply the eligibility criteria to find Multimorbidities
setwd(WP)
source("eligMM.R")
dim(tblbas)
table(finaltbl$mm)
finaltbl$CalendarYear=year
total=rbind.data.frame(total,finaltbl)
# Clear workspace
rm(list = as.character(ls()[!(ls() %in% c("W","WR","WP","WG","WD","WRD","total","hospsToInclude","exportResList"))]) )
################
####  2013  ####
################
# Year of the analysis
year=2013
#minimum age in study
minAge=18
# Maximum date for the analyses
endDate = as.Date(paste0(year,"-12-31"))
#exclude dates before that date in getMaxDate R programm
exBefore= as.Date("1980-1-1")
# Time gap for the definition of lost to follow up (in days)
gap = 1.5*365.25
# BMI boundaries
supBMI=40
infBMI=14
# Libraries
library(haven)
library(gdata)
library(tidyverse)
library(dplyr)
library(lubridate)
# Get the maxdate before 31/12/year
setwd(WP)
source("getMaxDate.R")
# Apply the eligibility criteria for final tblbas
setwd(WP)
source("elig1.R")
dim(tblbas)
# Apply the eligibility criteria to find Multimorbidities
setwd(WP)
source("eligMM.R")
dim(tblbas)
table(finaltbl$mm)
finaltbl$CalendarYear=year
total=rbind.data.frame(total,finaltbl)
# Clear workspace
rm(list = as.character(ls()[!(ls() %in% c("W","WR","WP","WG","WD","WRD","total","hospsToInclude","exportResList"))]) )
################
####  2014  ####
################
# Year of the analysis
year=2014
#minimum age in study
minAge=18
# Maximum date for the analyses
endDate = as.Date(paste0(year,"-12-31"))
#exclude dates before that date in getMaxDate R programm
exBefore= as.Date("1980-1-1")
# Time gap for the definition of lost to follow up (in days)
gap = 1.5*365.25
# BMI boundaries
supBMI=40
infBMI=14
# Libraries
library(haven)
library(gdata)
library(tidyverse)
library(dplyr)
library(lubridate)
# Get the maxdate before 31/12/year
setwd(WP)
source("getMaxDate.R")
# Apply the eligibility criteria for final tblbas
setwd(WP)
source("elig1.R")
dim(tblbas)
# Apply the eligibility criteria to find Multimorbidities
setwd(WP)
source("eligMM.R")
dim(tblbas)
table(finaltbl$mm)
finaltbl$CalendarYear=year
total=rbind.data.frame(total,finaltbl)
# Clear workspace
rm(list = as.character(ls()[!(ls() %in% c("W","WR","WP","WG","WD","WRD","total","hospsToInclude","exportResList"))]) )
################
####  2015  ####
################
# Year of the analysis
year=2015
#minimum age in study
minAge=18
# Maximum date for the analyses
endDate = as.Date(paste0(year,"-12-31"))
#exclude dates before that date in getMaxDate R programm
exBefore= as.Date("1980-1-1")
# Time gap for the definition of lost to follow up (in days)
gap = 1.5*365.25
# BMI boundaries
supBMI=40
infBMI=14
# Libraries
library(haven)
library(gdata)
library(tidyverse)
library(dplyr)
library(lubridate)
# Get the maxdate before 31/12/year
setwd(WP)
source("getMaxDate.R")
# Apply the eligibility criteria for final tblbas
setwd(WP)
source("elig1.R")
dim(tblbas)
# Apply the eligibility criteria to find Multimorbidities
setwd(WP)
source("eligMM.R")
dim(tblbas)
table(finaltbl$mm)
finaltbl$CalendarYear=year
total=rbind.data.frame(total,finaltbl)
# Clear workspace
rm(list = as.character(ls()[!(ls() %in% c("W","WR","WP","WG","WD","WRD","total","hospsToInclude","exportResList"))]) )
################
####  2016  ####
################
# Year of the analysis
year=2016
#minimum age in study
minAge=18
# Maximum date for the analyses
endDate = as.Date(paste0(year,"-12-31"))
#exclude dates before that date in getMaxDate R programm
exBefore= as.Date("1980-1-1")
# Time gap for the definition of lost to follow up (in days)
gap = 15*365.25
# BMI boundaries
supBMI=40
infBMI=14
# Libraries
library(haven)
library(gdata)
library(tidyverse)
library(dplyr)
library(lubridate)
# Get the maxdate before 31/12/year
setwd(WP)
source("getMaxDate.R")
# Apply the eligibility criteria for final tblbas
setwd(WP)
source("elig1.R")
dim(tblbas)
# Apply the eligibility criteria to find Multimorbidities
setwd(WP)
source("eligMM.R")
dim(tblbas)
table(finaltbl$mm)
finaltbl$CalendarYear=year
total=rbind.data.frame(total,finaltbl)
# Clear workspace
rm(list = as.character(ls()[!(ls() %in% c("W","WR","WP","WG","WD","WRD","total","hospsToInclude","exportResList"))]) )
################
####  2017  ####
################
# Year of the analysis
year=2017
#minimum age in study
minAge=18
# Maximum date for the analyses
endDate = as.Date(paste0(year,"-12-31"))
#exclude dates before that date in getMaxDate R programm
exBefore= as.Date("1980-1-1")
# Time gap for the definition of lost to follow up (in days)
gap = 1.5*365.25
# BMI boundaries
supBMI=40
infBMI=14
# Libraries
library(haven)
library(gdata)
library(tidyverse)
library(dplyr)
library(lubridate)
# Get the maxdate before 31/12/year
setwd(WP)
source("getMaxDate.R")
# Apply the eligibility criteria for final tblbas
setwd(WP)
source("elig1.R")
dim(tblbas)
# Apply the eligibility criteria to find Multimorbidities
setwd(WP)
source("eligMM.R")
dim(tblbas)
table(finaltbl$mm)
finaltbl$CalendarYear=year
total=rbind.data.frame(total,finaltbl)
# Clear workspace
rm(list = as.character(ls()[!(ls() %in% c("W","WR","WP","WG","WD","WRD","total","hospsToInclude","exportResList"))]) )
################
####  2018  ####
################
# Year of the analysis
year=2018
#minimum age in study
minAge=18
# Maximum date for the analyses
endDate = as.Date(paste0(year,"-12-31"))
#exclude dates before that date in getMaxDate R programm
exBefore= as.Date("1980-1-1")
# Time gap for the definition of lost to follow up (in days)
gap = 1.5*365.25
# BMI boundaries
supBMI=40
infBMI=14
# Libraries
library(haven)
library(gdata)
library(tidyverse)
library(dplyr)
library(lubridate)
# Get the maxdate before 31/12/year
setwd(WP)
source("getMaxDate.R")
# Apply the eligibility criteria for final tblbas
setwd(WP)
source("elig1.R")
dim(tblbas)
# Apply the eligibility criteria to find Multimorbidities
setwd(WP)
source("eligMM.R")
dim(tblbas)
table(finaltbl$mm)
finaltbl$CalendarYear=year
total=rbind.data.frame(total,finaltbl)
# Clear workspace
rm(list = as.character(ls()[!(ls() %in% c("W","WR","WP","WG","WD","WRD","total","hospsToInclude","exportResList"))]) )
total=total[complete.cases(total$mm),]
total2=total
total2$PATIENT=as.character(total2$PATIENT)
setwd(WD)
write_dta(total, "total.dta")
write.csv(total,file="total.csv",row.names = F)