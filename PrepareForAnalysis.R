#####################################################
### Prepare data for GEE Longitudinal analysis    ###
#####################################################
#setwd(WD)
#total=read_dta("total.dta")
#totalf=read_dta("totalFinal.dta")
setwd(WRD)
tblvis=read_dta("tblvis.dta")
tbllab=read_dta("tbllab.dta")
tbllab_cd4=read_dta("tbllab_cd4.dta")
tblbas=read_dta("tblbas.dta")
total=total[!is.na(total$HIV_POS_D),]

##########################
## Time fixed Variables ##
##########################

# Generate smoking status (smoke 0-1-9)

dim(total)
total$smoke=NULL
total$SMOKING_Y=NULL
exSmoke=which(tblvis$SMOKING_Y==2 | tblvis$SMOKING_Y==9)
tblvis=tblvis[-exSmoke,]
tblvis=tblvis[!duplicated(tblvis$PATIENT),]

total=left_join(total,tblvis[,c("PATIENT","SMOKING_Y")],by="PATIENT")
#total=merge(total,tblvis[,c("PATIENT","SMOKING_Y")],by="PATIENT",all.x = T)
total$SMOKING_Y[is.na(total$SMOKING_Y)]<- 9

total$smoke= NA
total$smoke[total$SMOKING_Y ==0] = "No"
total$smoke[total$SMOKING_Y == 1] = "Yes"
total$smoke[total$SMOKING_Y == 9] = "Unknown"

total$smoke = factor(total$smoke,levels = c("No","Yes","Unknown"))
table(total$smoke,useNA = "always")

# Generate mode of infection (mode MSM-MSW-IDU)

total$mode = NULL

total=total[!(total$MODE==4|total$MODE==5 |total$MODE==8 |total$MODE==9 | is.na(total$MODE)),]
total$mode = NA
total$mode[total$MODE %in% c(1,3)] = "MSM"
total$mode[total$MODE == 2] = "IDU"
total$mode[total$MODE %in% c(6,7)] = "MSW"
total$mode[is.na(total$mode)] = "Other/Unknown"
total$mode = factor(total$mode,levels = c("MSM","IDU","MSW", "Other/Unknown"))
#total$mode = factor(total$mode,levels = c("MSM","IDU","MSW"))
table(total$mode,useNA = "always")

# Gender(gender 0 Male-1 Female)

total$gender = NA
total$gender[total$GENDER == 1] = "Male"
total$gender[total$GENDER == 2] = "Female"
total$gender = factor(total$gender,levels = c("Female","Male"))
table(total$gender,useNA = "always")

# Generate education level (primary-secondary-university-unknown)
total$edulvl = NA
total$edulvl[total$EDU_LVL %in% c(0,1)] = "primary"
total$edulvl[total$EDU_LVL %in% c(2,3)] = "secondary"
total$edulvl[total$EDU_LVL ==4] = "university"
total$edulvl[is.na(total$EDU_LVL) | total$EDU_LVL %in% c(8,9) ] = "Other/Unknown"
total$edulvl = factor(total$edulvl,levels = c("primary","secondary","university","Other/Unknown"))

# Country or region of birth (region_group europe-asia-africa-america-unkown)

total$ORIGIN[total$ORIGIN==""] = "001"
total$ORIGIN = as.numeric(total$ORIGIN)
total$region_group = NA
total$region_group[total$ORIGIN %in% c(300,150,151,154,039,155,8,56,100,112,196,246,250,276,372,380,440,498,616,620,642,688,724,804,826)] = "Europe"
total$region_group[total$ORIGIN %in% c(2,14,17,15,18,11,12,24,40,120,178,231,232,288,404,434,466,504,566,686,710,716,788,800,818,834)] = "Africa"
total$region_group[total$ORIGIN %in% c(142,143,30,34,35,145,4,50,51,142,143,145,268,360,364,368,398,422,528,586,608,760,764,792,860)] = "Asia"
#total$region_group[total$ORIGIN %in% c(32,170,192,484,840,19)] = "America"
total$region_group[is.na(total$region_group) | total$ORIGIN==001] = "Unknown"
#total$region_group = factor(total$region_group,levels = c("Africa","Europe","Asia","America","Unknown"))
total$region_group = factor(total$region_group,levels = c("Africa","Europe","Asia","Unknown"))
table(total$region_group)


#total$region = NA
#total$region[total$ORIGIN %in% c(300,150,151,154,039,155,8,56,100,112,196,246,250,276,372,380,440,498,616,620,642,688,724,804,826)] = "Europe"
#total$region[!(total$ORIGIN %in% c(300,150,151,154,039,155,8,56,100,112,196,246,250,276,372,380,440,498,616,620,642,688,724,804,826))] = "other"
#total$region = factor(total$region,levels = c("Europe","other"))
#table(total$region)

# Ethnicity of patient/race (race white-black-asian-middleEast-unknown)

total$ORIGIN = as.numeric(total$ORIGIN)
total$race = NA
total$race[total$ETHNIC %in% c(0,10)] = "White"
#total$race[total$ETHNIC %in% c(20,21,24)] = "Black"
#total$race[total$ETHNIC %in% c(40,43,45)] = "Asian"
#total$race[total$ETHNIC %in% c(72,73,74)] = "Middle East"
#total$race[total$ETHNIC %in% c(99)] = "Unknown"
total$race[total$ETHNIC %in% c(20,21,24,40,43,45,72,73,74,99)] = "Black-other"
#total$race = factor(total$race,levels = c("White","Black","Asian","Middle East","Unknown"))
total$race = factor(total$race,levels = c("White","Black-other"))

table(total$race)


# Aids (No 0 -1 Yes)
total$aids = NA
total$aids[total$AIDS_Y == 0] = "No"
total$aids[total$AIDS_Y == 1] = "Yes"
total$aids = factor(total$aids,levels = c("No","Yes"))
table(total$aids,useNA = "always")

#
#total$multimor <- rowSums(total[, c('dia','hyp','cancer','card','deslip','ren','liv')], na.rm = TRUE)

# Multimorbidity (No 0 - 1 Yes)
total$multimorbidity = NA
total$multimorbidity[total$mm == 0] = "No"
total$multimorbidity[total$mm == 1] = "Yes"
total$multimorbidity = factor(total$multimorbidity,levels = c("No","Yes"))
table(total$multimorbidity,useNA = "always")

## age categorical
total$catage = NA
total$catage[total$age>=18 & total$age<40] = "[18,40)"
total$catage[total$age>=40 & total$age<50] = "[40,50)"
total$catage[total$age>=50 & total$age<60] = "[50,60)"
total$catage[total$age>=60] = "[60,+)"
total$catage=as.factor(total$catage)
table(total$catage,useNA = "always")


###########
### BMI ###
###########
total$bmi=NULL
total$BMI=NULL
tblbmi=read_dta("tblvis.dta")
tblbmi=tblbmi[,c("PATIENT","WEIGH","HEIGH")]
tblbmi=tblbmi[!is.na(tblbmi$WEIGH) ,]
tblbmi=tblbmi[ !is.na(tblbmi$HEIGH),]
tblbmi=tblbmi[!duplicated(tblbmi$PATIENT),]
tblbmi <- tblbmi[ tblbmi$PATIENT %in% total$PATIENT,]
tblbmi$bmi=tblbmi$WEIGH/(tblbmi$HEIGH/100)^2
total=merge(total,tblbmi[,c("PATIENT","bmi")], by="PATIENT",all = T)
remove(tblbmi)

# bmi categorical 
total$BMI = NA
total$BMI[total$bmi <20] = "Underweight"
total$BMI[total$bmi >=20 & total$bmi <25] = "Normal"
total$BMI[total$bmi >=25 & total$bmi <30] = "Overweight"
total$BMI[total$bmi >30] = "Obese"
total$BMI = factor(total$BMI,levels = c("Underweight","Normal","Overweight","Obese"))
table(total$BMI,useNA = "always")
# CD4 cells at ART initiation
# CD4 within 6 months before to 3 months after the initiation date was used ?????
# CD4 at ART ,<200, 200–349, 350–499, >500, missing

# BMI μία και καλή? ή κάθε χρονιά να αλλάζει?

###########################
## Time Varing Variables ##
###########################

# Age each year (Age) 
#check

# Age as categorical 
#check
# Age categorical 10-year margin 
#total$catage10 = NA
#total$catage10[total$age>=18 & total$age<30] = "[18,30)"
#total$catage10[total$age>=30 & total$age<40] = "[30,40)"
#total$catage10[total$age>=40 & total$age<50] = "[40,50)"
#otal$catage10[total$age>=50 & total$age<60] = "[50,60)"
#total$catage10[total$age>=60] = "[60,+)"
#total$catage10=as.factor(total$catage10)
#table(total$catage10,useNA = "always")

# Age categorical like wong
#total$catagew = NA
#total$catagew[total$age>=18 & total$age<40] = "[18,40)"
#total$catagew[total$age>=40 & total$age<50] = "[40,49)"
#total$catagew[total$age>=50 & total$age<60] = "[50,59)"
#total$catagew[total$age>=60] = "[60,+)"
#total$catagew=as.factor(total$catagew)
#able(total$catagew,useNA = "always")


# cART type (0 no-ART,1 no cART,2 PI-based,3 NNRTI,4 INSTI)
total$cART = NA
total$cART[total$haart == 0] = "noART"
total$cART[total$haart == 1] = "nocART"
total$cART[total$haart == 2] = "PI-based"
total$cART[total$haart == 3] = "NNRTI"
total$cART[total$haart == 4] = "INSTIs"
total$cART = factor(total$cART,levels = c("noART","nocART","PI-based","NNRTI","INSTIs"))
table(total$cART,useNA = "always")
# Calendar Year (CalendarYear)
#check

# Annual CD4 cells count, median CD4 within a Calendar Year
#check

## viral suppression
total$VLsuppress = NA
total$VLsuppress[total$LastRNA <=400] = "suppressed"
total$VLsuppress[total$LastRNA >400] = "unsuppressed"


total$VLsuppress = factor(total$VLsuppress,levels = c("suppressed","unsuppressed"))
table(total$VLsuppress,useNA = "always")

# Last CD4 count of each year (CD4_V)
#check

# Highest Viral load in a Calendar Year
#check

# Last Viral Load in a Calendar Year
#check

# time since ART initiation
#check

# AIDS diagnosis (AIDS_Y)
#check

### keep patients in tblvis that exist in total


# Follow up time -- # Last visit Date -Fisrt visit Date

## nullize variables
#total$timeFollow=NULL
setwd(WRD)
tblvis=read_dta("tblvis.dta")
tbltime <- tblvis[ tblvis$PATIENT %in% total$PATIENT,] 

tbltime=tbltime[order(tbltime$PATIENT,tbltime$VIS_D),]
head <- aggregate(tbltime, by=list(tbltime$PATIENT), FUN = function(x) { first = head(x,1) } )
tail <- aggregate(tbltime, by=list(tbltime$PATIENT), FUN = function(x) {  last = tail(x,1) } )
names(head)[4]='minDate'
names(tail)[4]='maxDate'
humanYear=merge(head[,c("PATIENT","minDate")],tail[,c("PATIENT","maxDate")],by="PATIENT",all.x=T)
humanYear$timeFollow=humanYear$maxDate - humanYear$minDate
humanYear <- humanYear[ humanYear$PATIENT %in% total$PATIENT,] 
total=merge(total,humanYear[,c("PATIENT","timeFollow")],by="PATIENT",all.x = T)
total$timeFollow=total$timeFollow/365.25
remove(head)
remove(tail)
remove(humanYear)
remove(tbltime)

# Follow up time -- # Last visit - cART initation Date
#total$TimeSinceART=NULL
#total=total[order(total$PATIENT,total$maxdate),]
#tail <- aggregate(total, by=list(total$PATIENT), FUN = function(x) {  last = tail(x,1) } )
#names(tail)[28]='maxyear'
#ail$TimeSinceART=tail$maxyear-tail$RECART_D
#total=merge(total,tail[,c("PATIENT","TimeSinceART")],by="PATIENT",all = T)

# Follow up time -- # Last visit - enroll in Cohort Date
#total$TimeSinceEnroll=NULL
#ail$TimeSinceEnroll=tail$maxyear-tail$ENROL_D
#total=merge(total,tail[,c("PATIENT","TimeSinceEnroll")],by="PATIENT",all = T)

# Follow up time -- # Censore date -  cART initation Date
#total$TimeARTtoCens=total$CENS_D-total$RECART_D

#remove(tail)

# Total time of receiving ART
total$s=total$maxdate-total$RECART_D
total = total[order(total$PATIENT,total$s),]
total$ord = unlist(tapply(total$PATIENT,total$PATIENT,function(x) 1:length(x)))
total$maxord = unlist(tapply(total$PATIENT,total$PATIENT,function(x) rep(length(x),length(x))))
timeART = total[total$ord==total$maxord,]
timeART$s=as.numeric(timeART$s)
timeART$s=timeART$s/365.25
timeART$ART_Duration=timeART$s
total$s=NULL
total=merge(total,timeART[,c("PATIENT","ART_Duration")], by="PATIENT",all.x = T)
total$ART_Duration[total$ART_Duration<0]=0

# time since art
total$timeSinceART=NULL
total$timeSinceART=total$cARTrecartTime
total$cARTrecartTime=NULL
total$timeSinceART[total$timeSinceART<=0]=0
summary(total$timeSinceART)
#############################
#### CD4 in cART initation###
#############################
setwd(WRD)
tbllab_cd4=read_dta("tbllab_cd4.dta")

### keep Patients in tbllab_cd4 that exist in total 
totallabCD4 <- tbllab_cd4[ tbllab_cd4$PATIENT %in% total$PATIENT,] 
totallabCD4=merge(totallabCD4,tblbas[,c("PATIENT","RECART_D")], by="PATIENT",all.x=T)
#totallabCD4=left_join(totallabCD4,total[,c("PATIENT","RECART_D")], by="PATIENT",all=T)
totallabCD4=totallabCD4[!is.na(totallabCD4$RECART_D) ,]
totallabCD4=totallabCD4[totallabCD4$CD4_U==1 ,]

# look at a gap 6 months before and 3 months after

library(lubridate)
totallabCD4$cd4inART1=as.Date(totallabCD4$RECART_D) %m-% months(6)
totallabCD4$cd4inART2=as.Date(totallabCD4$RECART_D) %m+% months(10)
pat=which(totallabCD4$CD4_D <= totallabCD4$cd4inART2 & totallabCD4$CD4_D >= totallabCD4$cd4inART1)
totallabCD4=totallabCD4[pat,]

# build median cd4 at cART initation 
total$cd4ART=NULL
totallabCD4$cd4ART = unlist(tapply(totallabCD4$CD4_V,totallabCD4$PATIENT,function(x) rep(median(x,na.rm = T),length(x))))
totallabCD4=totallabCD4[!duplicated(totallabCD4$PATIENT),]
total=merge(total,totallabCD4[,c("PATIENT","cd4ART")], by="PATIENT",all.x = T)
#total=left_join(total,totallabCD4[,c("PATIENT","cd4ART")], by="PATIENT",all=T)

remove(totallabCD4)
# ayto an thelo na afairesv meres
#totallabCD4$cd4inART1=ymd(totallabCD4$RECART_D) -20
#totallabCD4$cd4inART2=ymd(totallabCD4$RECART_D) +20


# Cd4 at Art factorial
total$cd4ARTcat=as.numeric(total$cd4ART)
total$cd4ARTcat = NA
total$cd4ARTcat[total$cd4ART<200] = "< 200"
total$cd4ARTcat[total$cd4ART >=200 & total$cd4ART <349 ] = "200-349"
total$cd4ARTcat[total$cd4ART >=350 & total$cd4ART <499 ] = "350-499"
total$cd4ARTcat[total$cd4ART>500] = ">500"

total$cd4ARTcat = factor(total$cd4ARTcat,levels = c("< 200","200-349","350-499",">500"))
table(total$cd4ARTcat,useNA = "always")

#############################
#### CD4 at HIV diagnosis ###
#############################

### keep Patients in tbllab_cd4 that exist in total 

totallabCD4 <- tbllab_cd4[ tbllab_cd4$PATIENT %in% total$PATIENT,] 

totallabCD4=merge(totallabCD4,tblbas[,c("PATIENT","HIV_POS_D")], by="PATIENT",all.x=T)
totallabCD4=totallabCD4[!is.na(totallabCD4$HIV_POS_D) ,]
totallabCD4=totallabCD4[totallabCD4$CD4_U==1 ,]

# look at a gap 6 months before and 3 months after

library(lubridate)
totallabCD4$cd4inART1=as.Date(totallabCD4$HIV_POS_D) %m-% months(3)
totallabCD4$cd4inART2=as.Date(totallabCD4$HIV_POS_D) %m+% months(12)
pat=which(totallabCD4$CD4_D <= totallabCD4$cd4inART2 & totallabCD4$CD4_D >= totallabCD4$cd4inART1 & totallabCD4$CD4_U==1)
totallabCD4=totallabCD4[pat,]
# build median cd4 at cART initation 
total$cd4hiv=NULL
totallabCD4$cd4hiv = unlist(tapply(totallabCD4$CD4_V,totallabCD4$PATIENT,function(x) rep(median(x,na.rm = T),length(x))))
totallabCD4=totallabCD4[!duplicated(totallabCD4$PATIENT),]
total=merge(total,totallabCD4[,c("PATIENT","cd4hiv")], by="PATIENT",all.x = T)
summary(total$cd4hiv)
remove(totallabCD4)
# ayto an thelo na afairesv meres
#totallabCD4$cd4inART1=ymd(totallabCD4$RECART_D) -20
#totallabCD4$cd4inART2=ymd(totallabCD4$RECART_D) +20


# Cd4 at Art factorial
total$cd4hivcat=as.numeric(total$cd4hiv)
total$cd4hivcat = NA
total$cd4hivcat[total$cd4hiv<200] = "< 200"
total$cd4hivcat[total$cd4hiv >=200 & total$cd4hiv <349 ] = "200-349"
total$cd4hivcat[total$cd4hiv >=350 & total$cd4hiv <499 ] = "350-499"
total$cd4hivcat[total$cd4hiv>500] = ">500"

total$cd4hivcat = factor(total$cd4hivcat,levels = c("< 200","200-349","350-499",">500"))
table(total$cd4hivcat,useNA = "always")


######################
## age at diagnosis ##
######################
total$ageAtDiagnosis=(total$HIV_POS_D-total$BIRTH_D)/365.25
total$ageAtDiagnosis=as.numeric(total$ageAtDiagnosis)
#Age categorical at HIV Positive like wong
total$catagehiv = NA
total$catagehiv[total$ageAtDiagnosis>=18 & total$ageAtDiagnosis<40] = "[18,40)"
total$catagehiv[total$ageAtDiagnosis>=40 & total$ageAtDiagnosis<50] = "[40,49)"
total$catagehiv[total$ageAtDiagnosis>=50 & total$ageAtDiagnosis<60] = "[50,59)"
total$catagehiv[total$ageAtDiagnosis>=60] = "[60,+)"
total$catagehiv=as.factor(total$catagehiv)
table(total$catagehiv,useNA = "always")
#######################################################################################

############################
## age at cART initiation ##
############################
total$ageATcART=(total$RECART_D-total$BIRTH_D)/365.25
#Age categorical at ART like wong
total$catageart = NA
total$catageart[total$ageATcART>=18 & total$ageATcART<40] = "[18,40)"
total$catageart[total$ageATcART>=40 & total$ageATcART<50] = "[40,49)"
total$catageart[total$ageATcART>=50 & total$ageATcART<60] = "[50,59)"
total$catageart[total$ageATcART>=60] = "[60,+)"
total$catageart=as.factor(total$catageart)
table(total$catageart,useNA = "always")

#############################

# define total morbidities without NAs
total$morbidities <- rowSums(total[, c('dia','hyp','cancer','card','deslip','ren','liv')], na.rm = TRUE)
total$morbidities[is.na(total$multimorbidity)]=NA
summary(total$morbidities)
# build an easier ID

total=total[order(total$PATIENT),]

total$id <- as.numeric(factor(total$PATIENT, levels=unique(total$PATIENT)))

# delete the NAs in CD4 unit
#total=total[!is.na(total$CD4_U),]
#total$LastRNA[total$LastRNA==-1]=25
#summary(total$LastRNA)
#setwd(WD)
#write_dta(total, "totalFinal.dta")
#write.csv(total,file="totalFinal.csv",row.names = F)
# keep only total
#rm(list = as.character(ls()[!(ls() %in% c("W","WR","WP","WG","WD","WRD","hospsToInclude","exportResList","total"))]) )
