setwd(WRD)
tblbas=read_dta("tblbas.dta")
tblltfu=read_dta("tblltfu.dta")
art_comp=read_dta("art_comp.dta")
tbllab_cd4=read_dta("tbllab_cd4.dta")
tbllab_rna=read_dta("tbllab_rna.dta")
tbldis=read_dta("tbldis.dta")
patients=read_dta("patients.dta")
tbllab=read_dta("tbllab.dta")
tblMED=read_dta("tblMED.dta")
tblCEP=read_dta("tblCEP.dta")
tblCANC=read_dta("tblCANC.dta")
tbllab_BP=read_dta("tbllab_BP.dta")
tblvis=read_dta("tblvis.dta")
tblart=read_dta("tblart.dta")
type_art=read_dta("type_art.dta")

flowchart = data.frame(Desc = NA,N = NA)
flowchart[1,] = c("Total",nrow(tblbas))


#######
## 0 ##
#######

#tblltfu=tblltfu[tblltfu$DROP_D>=startDate | tblltfu$DEATH_D>=startDate,]
#tbllab=tbllab[tbllab$LAB_D>=startDate,]
#tblMED=tblMED[tblMED$MED_SD>=startDate,]
#tblCEP=tblCEP[tblCEP$CEP_D>=startDate,]
#tblCANC=tblCANC[tblCANC$CANC_D>=startDate,]
#tbllab_BP=tbllab_BP[tbllab_BP$BP_D>=startDate,]
#tblvis=tblvis[tblvis$VIS_D>=startDate,]
#tbldis=tbldis[tbldis$DIS_D>=startDate,]
#tbllab_rna=tbllab_rna[tbllab_rna$RNA_D>=startDate,]
#tbllab_cd4=tbllab_cd4[tbllab_cd4$CD4_D>=startDate,]
#art_comp=art_comp[art_comp$START_D>=startDate,]

tblltfu=tblltfu[tblltfu$DROP_D<=endDate | tblltfu$DEATH_D<=endDate,]
tbllab=tbllab[tbllab$LAB_D<=endDate,]
tblMED=tblMED[tblMED$MED_SD<=endDate,]
tblCEP=tblCEP[tblCEP$CEP_D<=endDate,]
tblCANC=tblCANC[tblCANC$CANC_D<=endDate,]
tbllab_BP=tbllab_BP[tbllab_BP$BP_D<=endDate,]
tblvis=tblvis[tblvis$VIS_D<=endDate,]
tbldis=tbldis[tbldis$DIS_D<=endDate,]
tbllab_rna=tbllab_rna[tbllab_rna$RNA_D<=endDate,]
tbllab_cd4=tbllab_cd4[tbllab_cd4$CD4_D<=endDate,]
art_comp=art_comp[art_comp$START_D<=endDate,]
type_art=type_art[type_art$chngdate<=endDate ,]
type_art=type_art[type_art$chngdate>=as.Date('1996-01-01') ,]


#####
# 1 #
#####

# Impute missing diagnosis dates by the first RNA test
# Temp data frame with the first RNA date
temp = as.data.frame(tbllab_rna)
temp = temp[order(temp$PATIENT,temp$RNA_D),]
temp$ord = unlist(tapply(temp$PATIENT,temp$PATIENT,function(x) 1:length(x)))
temp = temp[temp$ord==1,]
temp$FirstRNAdate = temp$RNA_D
temp <- temp[,c("PATIENT","FirstRNAdate")]

tblbas$HIV_POS_D[ which( tblbas$HIV_POS_D < as.Date("1970-1-1") |  tblbas$HIV_POS_D > Sys.Date() )  ] = NA

dim(tblbas)
tblbas = merge(tblbas,temp,by = "PATIENT",all.x = T)
dim(tblbas)

# Replace the missing diagnosis date with the first RNA date
tblbas$HIV_POS_D[is.na(tblbas$HIV_POS_D)] <- tblbas$FirstRNAdate[is.na(tblbas$HIV_POS_D)]

# Exclude those with missing or invalid HIV diagnosis date

# First ART initiation date
temp_ART = as.data.frame(art_comp)
temp_ART = temp_ART[order(temp_ART$PATIENT,temp_ART$START_D),]
temp_ART$ord = unlist(tapply(temp_ART$PATIENT,temp_ART$PATIENT,function(x) 1:length(x)))
temp_ART = temp_ART[temp_ART$ord==1,]


dim(tblbas)
#tblbas=finaltbl
setwd(WD)
maxdate <- read.csv(paste0("maxdate",year,".csv"))
maxdate$PATIENT=as.character(maxdate$PATIENT)

#####
# 2 #
#####

# restrict up to maxdate
maxdate <- maxdate[ maxdate$PATIENT %in% tblbas$PATIENT, ]
maxdate$maxdate = as.Date(maxdate$maxdate)
tblbas = merge(tblbas,maxdate[,c("PATIENT","maxdate")],all.y = T)
#tblbas=tblbas[!is.na(tblbas$maxdate),]

#####
# 3 #
#####

# IdS for those who were diagnosed after 31/12/year
diagPATs = tblbas$PATIENT[ which(tblbas$HIV_POS_D > endDate ) ]
dim(tblbas)
tblbas = tblbas[ !(tblbas$PATIENT %in% diagPATs), ]
dim(tblbas)
#flowchart = rbind(flowchart,c(paste("Diagnosed <",endDate),nrow(tblbas)))

#####
# 4 #
#####

# Ids for those with missing/invalid HIV diagnosis date
exlPats = tblbas$PATIENT[which( tblbas$HIV_POS_D > tblbas$START_D | is.na(tblbas$HIV_POS_D) |  tblbas$HIV_POS_D > tblbas$maxdate   )]

dim(tblbas)
tblbas = tblbas[ !(tblbas$PATIENT %in% exlPats), ]
dim(tblbas)
tblbas$START_D = NULL
#flowchart = rbind(flowchart,c("Missing/Invalid Diagnosis date",nrow(tblbas)))


#####
# 5 #
#####

# Ids for those who died before 31/12/year
tblltfu$DEATH_D[ which(tblltfu$DEATH_D < as.Date("1980-1-1") | tblltfu$DEATH_D > Sys.Date() ) ] = NA
tblltfu$DateDeathKnown_NH[ which(tblltfu$DateDeathKnown_NH < as.Date("1980-1-1") | tblltfu$DateDeathKnown_NH > Sys.Date() ) ] = NA
deadPATs = tblltfu$PATIENT[ which(tblltfu$DEATH_D <= endDate | tblltfu$DateDeathKnown_NH <= endDate) ]

# Exclude patients who died prior to 31/12/year
ExclPATs1 = unique(c(deadPATs))

dim(tblbas)
tblbas = tblbas[ !(tblbas$PATIENT %in% ExclPATs1), ]
dim(tblbas)

#####
# 5b#
#####

# Maximum date prior to 31/12/year
setwd(WD)
maxdate <- read.csv(paste0("maxdate",year,".csv"))
maxdate$PATIENT=as.character(maxdate$PATIENT)
maxdate <- maxdate[ maxdate$PATIENT %in% tblbas$PATIENT, ]

# For those without a maxdate, assign a maxdate 1 day after HIV diagnosis date
dim(maxdate)
maxdate = merge(maxdate,tblbas[,c("PATIENT","HIV_POS_D")],by = "PATIENT",all.y = T)
dim(maxdate)

maxdate$maxdate = as.Date(maxdate$maxdate)
maxdate$maxdate[is.na(maxdate$maxdate)] = maxdate$HIV_POS_D[is.na(maxdate$maxdate)] + 1
maxdate$X = NULL
maxdate$HIV_POS_D = NULL

dim(tblbas)
tblbas$maxdate = NULL
tblbas <- merge(tblbas,maxdate[,c("PATIENT","maxdate")],by = "PATIENT")
dim(tblbas)

##################
# 6, 7, 8, 9, 10 #
##################
#migPats, ageSexMisPats ,unlinked, lost10Pats

# Ids with those who moved to another country before 1/6/year
migPats = tblbas$PATIENT[ which(tblbas$DROP_Y == 1 & tblbas$DROP_RS == 3.1 & tblbas$maxdate < (endDate - 365.25/2) )    ]

# Ids for those with missing Birthdate or missing sex
tblbas$BIRTH_D[ which(tblbas$BIRTH_D<=as.Date("1911-12-31")) ] = NA
tblbas$ageDiag = as.numeric(tblbas$HIV_POS_D - tblbas$BIRTH_D)/365.25
ageSexMisPats = tblbas$PATIENT[ which( is.na(tblbas$BIRTH_D) | !(tblbas$GENDER %in% 1:2) | tblbas$ageDiag <= minAge) ]

# Unlinked patients: no clinic visit after first HIV positive data provided that they have been diagnosed up to 1.5 years before endDate
unlinked = tblbas$PATIENT[ which( (is.na(tblbas$maxdate) | tblbas$maxdate == tblbas$HIV_POS_D) & (endDate - tblbas$HIV_POS_D) > gap )]
dim(tblbas)
tblbas = tblbas[ !(tblbas$PATIENT %in% unlinked), ]
dim(tblbas)
flowchart = rbind(flowchart,c("Linked to care",nrow(tblbas)))

ind = which( is.na(tblbas$maxdate) | tblbas$maxdate <= tblbas$HIV_POS_D)
tblbas$maxdate[ind] <- tblbas$HIV_POS_D[ind] + 1

tblbas$TimeLost = as.numeric(endDate - tblbas$maxdate)
tblbas$lost = 1*(tblbas$TimeLost>gap)

# Ids with those who have been lost for more than 10 years
lost10Pats = tblbas$PATIENT[ which(tblbas$TimeLost>10*365.25)    ]
dim(tblbas)

tblbas = merge(tblbas,tblltfu[,c("PATIENT","DROP_Y","DROP_RS")],by = "PATIENT",all.x = T)
dim(tblbas)

# Exclude patients who 
# (i) moved to another country before 1/6/year, or
# (ii) have been lost for more than 10 years
ExclPATs2 = unique(c(migPats,ageSexMisPats))

dim(tblbas)
tblbas = tblbas[ !(tblbas$PATIENT %in% lost10Pats), ]
dim(tblbas)
#flowchart = rbind(flowchart,c("Lost > 10 years",nrow(tblbas)))

dim(tblbas)
tblbas = tblbas[ !(tblbas$PATIENT %in% ExclPATs2), ]
dim(tblbas)
#flowchart = rbind(flowchart,c(paste0("Missing age/sex or migrated < ","1/7/",year),nrow(tblbas)))

# Keep observations prior to 31/12/year
tbllab_cd4 = tbllab_cd4[tbllab_cd4$CD4_D <= endDate,]
tbllab_rna = tbllab_rna[tbllab_rna$RNA_D <= endDate,]
art_comp = art_comp[art_comp$START_D <= endDate,]
tbllab = tbllab[tbllab$LAB_D <= endDate,]

######
# 12 #
######

# Get of first AIDS diagnosis
dim(tbldis)
tbldis = tbldis[which(tbldis$DIS_D>as.Date("1979-12-31")),] 
dim(tbldis)

dim(tbldis)
tbldis = tbldis[tbldis$DIS_D<=endDate,]
dim(tbldis)

tbldis = tbldis[order(tbldis$PATIENT,tbldis$DIS_D),]
tbldis$ord = unlist(tapply(tbldis$PATIENT,tbldis$PATIENT,function(x){1:length(x)}))
temp_dis = tbldis[tbldis$ord == 1,]

dim(tblbas)
tblbas = merge(tblbas,temp_dis[,c("PATIENT","DIS_D")],by = "PATIENT",all.x = T)
dim(tblbas)

#build age in the end of the year
tblbas$Age=as.numeric(round((endDate-as.Date(tblbas$BIRTH_D))/365.25,3))

#age now
#tblbas$ageNow=as.numeric(round((Sys.Date()-as.Date(tblbas$BIRTH_D))/365.25,3))

## keep those with cART and HIV positive after 01-01-1996

#art=tblbas$PATIENT[which(tblbas$RECART_D>endDate)]
#tblbas=tblbas[!(tblbas$PATIENT %in% art),]
#tblbas=tblbas[tblbas$RECART_Y==1 & tblbas$HIV_POS_D>=as.Date('1996-01-01'),]

tblbas=tblbas[tblbas$RECART_D>=as.Date('1996-01-01'),]
tblbas=tblbas[!is.na(tblbas$PATIENT),]
#flowchart = rbind(flowchart,c(paste0("keep only tose with cART after 1996",year),nrow(tblbas)))

# From all tables, keep PAT in tblbas
patients <- patients[ patients$PATIENT %in% tblbas$PATIENT, ]
art_comp <- art_comp[ art_comp$PATIENT %in% tblbas$PATIENT, ]
tblltfu <- tblltfu[ tblltfu$PATIENT %in% tblbas$PATIENT, ]
tbllab_cd4 <- tbllab_cd4[ tbllab_cd4$PATIENT %in% tblbas$PATIENT, ]
tbllab_rna <- tbllab_rna[ tbllab_rna$PATIENT %in% tblbas$PATIENT, ]
tbllab <- tbllab[ tbllab$PATIENT %in% tblbas$PATIENT, ]
maxdate <- maxdate[ maxdate$PATIENT %in% tblbas$PATIENT,] 
tblMED <- tblMED[ tblMED$PATIENT %in% tblbas$PATIENT,] 
tblCEP <- tblCEP[ tblCEP$PATIENT %in% tblbas$PATIENT,] 
tblCANC <- tblCANC[ tblCANC$PATIENT %in% tblbas$PATIENT,] 
tbllab_BP <- tbllab_BP[ tbllab_BP$PATIENT %in% tblbas$PATIENT,] 
tblvis <- tblvis[ tblvis$PATIENT %in% tblbas$PATIENT,] 




