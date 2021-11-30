########################
### Import data in R ###
########################

# Get the names of all AMACS tables
setwd(WRD)
tableNames <- gsub(".dta","",dir())
tableNames <- tableNames[tableNames != "tblmode_extra"]


for (j in 1:length(tableNames))
{
  temp <- as.data.frame(read_dta(paste0(tableNames[j],".dta")))
  mv(from = "temp", to = tableNames[j])
  print(j)
}

tbllab = tbllab[,c("PATIENT","LAB_D","hosp")]
names(tbllab)[2] = "maxdate"

tbllab_cd4 = tbllab_cd4[,c("PATIENT","CD4_D","hosp")]
names(tbllab_cd4)[2] = "maxdate"

tbllab_rna = tbllab_rna[,c("PATIENT","RNA_D","hosp")]
names(tbllab_rna)[2] = "maxdate"

tblvis = tblvis[,c("PATIENT","VIS_D","hosp")]
names(tblvis)[2] = "maxdate"

tblart = tblart[,c("PATIENT","ART_SD","hosp")]
names(tblart)[2] = "maxdate"

tbldis = tbldis[,c("PATIENT","DIS_D","hosp")]
names(tbldis)[2] = "maxdate"

tbldis_b = tbldis_b[,c("PATIENT","DIS_D","hosp")]
names(tbldis_b)[2] = "maxdate"

# tbllab_viro = tbllab_viro[,c("PATIENT","VS_D","hosp")]
# names(tbllab_viro)[2] = "maxdate"

tblcep = tblcep[,c("PATIENT","CEP_D","hosp")]
names(tblcep)[2] = "maxdate"

tblmed = tblmed[,c("PATIENT","MED_SD","hosp")]
names(tblmed)[2] = "maxdate"
# 
# tblcoinf = tblcoinf[,c("PATIENT","COINF_D","hosp")]
# names(tblcoinf)[2] = "maxdate"
# 
tblcanc = tblcanc[,c("PATIENT","CANC_D","hosp")]
names(tblcanc)[2] = "maxdate"

art_comp = art_comp[,c("PATIENT","START_D","hosp")]
names(art_comp)[2] = "maxdate"

alldates = rbind(tbllab,tbllab_cd4,tbllab_rna,tblvis,tblart,tbldis,tbldis_b,art_comp,tblcep,tblmed,tblcanc)
indExcl = which(is.na(alldates$maxdate) | alldates$maxdate > Sys.Date() | alldates$maxdate < as.Date("1980-1-1") )

dim(alldates)
alldates = alldates[-indExcl,]
dim(alldates)

alldates = alldates[alldates$maxdate < endDate,]
alldates = alldates[order(alldates$PATIENT,alldates$maxdate),]
alldates$ord = unlist(tapply(alldates$PATIENT,alldates$PATIENT,function(x) 1:length(x)))
alldates$maxord = unlist(tapply(alldates$PATIENT,alldates$PATIENT,function(x) rep(length(x),length(x))))
maxdate = alldates[alldates$ord==alldates$maxord,]
maxdate$ord = NULL
maxdate$maxord = NULL

setwd(WD)
write.csv(maxdate,paste0("maxdate",year,".csv"),row.names = F)

rm(list = tableNames)
rm(alldates)
rm(j)
rm(indExcl)
