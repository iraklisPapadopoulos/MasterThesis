##################################
### Apply eligibility criteria ###
###  to find Multimorbidities  ###
##################################

# keep lab values that recorded after the first visit

tbllab=merge(tbllab,tblbas[,c("PATIENT","FRSVIS_D")],by='PATIENT',all.x = T)
tbllab=tbllab[!(tbllab$LAB_D<tbllab$FRSVIS_D),]
tblbas <- tblbas[ tblbas$PATIENT %in% tblvis$PATIENT, ]

#############################
#### DIABETES MELITUS########
#############################

## those who take diabetic meds

dia_MED=tblMED
dia_MED=tblMED[startsWith(tblMED$MED_ID,'A10'),]
dia_MED=dia_MED[!duplicated(dia_MED$PATIENT),]
dia_MED$indMed=1

## those diagnosed with Diabetes melitus

dia_CEP=tblCEP
dia_CEP=tblCEP[startsWith(tblCEP$CEP_ID,'DIA') | startsWith(tblCEP$CEP_SPEC,'E1'),]
dia_CEP=dia_CEP[!duplicated(dia_CEP$PATIENT),]
dia_CEP$indCep=1


#glucoze in blood>126ml/dl and HbA1C>6.5% 

dia_LAB=tbllab
dia_LAB=dia_LAB[dia_LAB$LAB_U==4,]
dia_LAB$ind = NA
dia_LAB$ind[dia_LAB$LAB_ID=='GLUC' & dia_LAB$LAB_V >=126 | dia_LAB$LAB_ID=='A1C' & dia_LAB$LAB_V <=6.5] = 1
dia_LAB$ind[dia_LAB$LAB_ID=='GLUC' & dia_LAB$LAB_V <126 | dia_LAB$LAB_ID=='A1C' & dia_LAB$LAB_V >6.5] = 0
table(dia_LAB$ind,useNA = "always")
dia_LAB=dia_LAB[complete.cases(dia_LAB$PATIENT),]
table(dia_LAB$ind,useNA = "always")
dia_LAB=dia_LAB[order(dia_LAB$PATIENT),]

# prepare the proper indicator

dia_LAB2=dia_LAB
dia_LAB2=dia_LAB2[complete.cases(dia_LAB2$ind),]
dia_LAB2$ind2 = unlist(tapply(dia_LAB2$ind,dia_LAB2$PATIENT,function(x) rep(sum(x),length(x))))
dia_LAB2=dia_LAB2[!duplicated(dia_LAB2$PATIENT),]
dia_LAB=dia_LAB[!duplicated(dia_LAB$PATIENT),]
dia_LAB=merge(dia_LAB,dia_LAB2[,c("PATIENT","ind2")],by='PATIENT',all = T)
table(dia_LAB$ind2,useNA = "always")
dia_LAB$indicator=NA
dia_LAB$indicator[dia_LAB$ind2>=1]=1
dia_LAB$indicator[dia_LAB$ind2==0]=0
table(dia_LAB$indicator,useNA = "always")
dia_LAB=dia_LAB[!duplicated(dia_LAB$PATIENT), ]
table(dia_LAB$indicator)
# Final dataset with patients having diabetes melitus
# dia_mel

dia_mel=merge(dia_MED[,c("PATIENT","indMed")],dia_LAB[,c("PATIENT","indicator")],by='PATIENT',all = T)
dia_mel=merge(dia_mel,dia_CEP[,c("PATIENT","indCep")],by='PATIENT',all = T)
table(dia_mel$indicator,useNA = "always")
dim(dia_mel)

# replace NAs in indicator by values in MED and CEP

dia_mel$indicator[is.na(dia_mel$indicator)] <- dia_mel$indMed[is.na(dia_mel$indicator)]
dia_mel$indicator[is.na(dia_mel$indicator)] <- dia_mel$indCep[is.na(dia_mel$indicator)]
table(dia_mel$indicator,useNA = "always")

dia_mel$dia=dia_mel$indicator
finaltbl=tblbas
finaltbl=merge(finaltbl,dia_mel[,c("PATIENT","dia")],by="PATIENT",all.x = T)

dim(finaltbl)
remove(dia_CEP)
remove(dia_LAB)
remove(dia_LAB2)
remove(dia_MED)
#############################
####   HYPERTANSION  ########
#############################

# those who take hypertension meds 
hyp_MED=tblMED
hyp_MED=data.frame(hyp_MED %>% 
                     filter(MED_ID == 'C-HYP' | MED_ID=='C02' |
                              MED_ID=='C03' | MED_ID=='C04' | 
                              MED_ID=='C07'| MED_ID=='C08' |  MED_ID=='C09'  ))
hyp_MED=hyp_MED[!duplicated(hyp_MED$PATIENT), ]
hyp_MED$indMed=1
hyp_MED$MedorCep=1
#those who diagnosed with Hypertansion

hyp_CEP=tblCEP[startsWith(tblCEP$CEP_ID,'HTN') | tblCEP$CEP_SPEC>='I10' & tblCEP$CEP_SPEC<='I15.9', ]
hyp_CEP=hyp_CEP[!duplicated(hyp_CEP$PATIENT),]
hyp_CEP$indCep=1
hyp_CEP$MedorCep2=1

## those with high blood pressure sys>=140 diA>=90
hyp_BL=tbllab_BP


hyp_BL$ind = NA
hyp_BL$ind[hyp_BL$BP_SYS >140 | hyp_BL$BP_DIA>90] = 1
hyp_BL$ind[hyp_BL$BP_SYS <=140 | hyp_BL$BP_DIA<=90] = 0
#hyp_BL$ind[is.na(hyp_BL$ind)] = 9
table(hyp_BL$ind,useNA = "always")
#hyp_BL$ind = factor(hyp_BL$ind,levels = c("1","0","9"))
table(hyp_BL$ind)
hyp_BL=hyp_BL[complete.cases(hyp_BL$PATIENT),]

#############

hyp_BL2=hyp_BL
hyp_BL2=hyp_BL2[complete.cases(hyp_BL2$ind),]
hyp_BL2$ind2 = unlist(tapply(hyp_BL2$ind,hyp_BL2$PATIENT,function(x) rep(sum(x),length(x))))
hyp_BL2=hyp_BL2[!duplicated(hyp_BL2$PATIENT),]
hyp_BL=hyp_BL[!duplicated(hyp_BL$PATIENT),]
hyp_BL=merge(hyp_BL,hyp_BL2[,c("PATIENT","ind2")],by='PATIENT',all = T)
table(hyp_BL2$ind2,useNA = "always")
hyp_BL$indicator=NA
hyp_BL$indicator[hyp_BL$ind2>=1]=1
hyp_BL$indicator[hyp_BL$ind2==0]=0
table(hyp_BL$indicator,useNA = "always")
hyp_BL=hyp_BL[!duplicated(hyp_BL$PATIENT), ]
table(hyp_BL$indicator,useNA = "always")
hyp_BL$HighPres=hyp_BL$indicator

#hypVis
hypVis=tblvis[,c("PATIENT","IdealBP_NH","VIS_D")]
hypVis=merge(hypVis,maxdate,by='PATIENT',all = T)
hypVis=hypVis[hypVis$VIS_D<=hypVis$maxdate,]
hypVis$indVis=NA
hypVis$indVis[hypVis$IdealBP_NH==1]=0
hypVis=hypVis[!duplicated(hypVis$PATIENT), ]
table(hypVis$indVis,useNA = "always")

# hyper

hyper=merge(hyp_MED[,c("PATIENT","indMed","MedorCep")],hyp_BL[,c("PATIENT","indicator","HighPres")],by='PATIENT',all = T)
hyper=merge(hyper,hyp_CEP[,c("PATIENT","indCep","MedorCep2")],by='PATIENT',all = T)
hyper=merge(hyper,hypVis[,c("PATIENT","indVis")],by='PATIENT',all = T)

table(hyper$indicator,useNA = "always")
dim(hyper)

# replace NAs in indicator by values in MED and CEP

hyper$indicator[is.na(hyper$indicator)] <- hyper$indMed[is.na(hyper$indicator)]
hyper$indicator[is.na(hyper$indicator)] <- hyper$indCep[is.na(hyper$indicator)]
hyper$indicator[is.na(hyper$indicator)] <- hyper$indVis[is.na(hyper$indicator)]

table(hyper$indicator,useNA = "always")

hyper$hyp=hyper$indicator

finaltbl=merge(finaltbl,hyper[,c("PATIENT","hyp","MedorCep","MedorCep2","HighPres")],by="PATIENT",all.x = T)


#############################
####   CANCER+NAMD   ########
#############################
ca=tblCANC
ca$indCa=1
dim(ca)

ca_CEP=tblCEP
ca_CEP$ind=NA
ca_CEP$ind[startsWith(tblCEP$CEP_ID,'NADM')]=1
ca_CEP$ind[!startsWith(tblCEP$CEP_ID,'NADM')]=0
table(ca_CEP$ind,useNA = "always")

#ca_CEP1=tblCEP[startsWith(tblCEP$CEP_ID,'NADM'),]

ca_CEP=merge(ca_CEP[,c("PATIENT","ind")],ca[,c("PATIENT","indCa")],by="PATIENT",all = T)
table(ca_CEP$ind,useNA = "always")
ca_CEP$ind[is.na(ca_CEP$ind)] <- ca_CEP$indCa[is.na(ca_CEP$ind)]
table(ca_CEP$ind,useNA = "always")
ca_CEP=ca_CEP[!is.na(ca_CEP$PATIENT),]
ca_CEP$s=unlist(tapply(ca_CEP$ind,ca_CEP$PATIENT,function(x) rep(sum(x,na.rm = T),length(x))))
ca_CEP$s[ca_CEP$s>=1]=1
ca_CEP$s[ca_CEP$s==0]=0
ca_CEP$ind=ca_CEP$s

ca_CEP=ca_CEP[!duplicated(ca_CEP$PATIENT), ]
#ca_CEP$cancer=1
ca_CEP$cancer=ca_CEP$ind
finaltbl=merge(finaltbl,ca_CEP[,c("PATIENT","cancer")],by="PATIENT",all.x =  T)
table(finaltbl$cancer,useNA = "always")

#############################
####    cardios      ########
#############################
cardio=tblCEP
cardio$ind=NA
cardio$ind[startsWith(tblCEP$CEP_ID,'AMI') | startsWith(tblCEP$CEP_ID,'CHD')
              | startsWith(tblCEP$CEP_SPEC,'DAMI') | startsWith(tblCEP$CEP_ID,'STR') | startsWith(tblCEP$CEP_SPEC,'SINF')]=1
cardio$ind[!(startsWith(tblCEP$CEP_ID,'AMI') | startsWith(tblCEP$CEP_ID,'CHD')
           | startsWith(tblCEP$CEP_SPEC,'DAMI') | startsWith(tblCEP$CEP_ID,'STR') | startsWith(tblCEP$CEP_SPEC,'SINF'))]=0
table(cardio$ind,useNA = "always")
cardio=cardio[!is.na(cardio$PATIENT),]

cardio$s=unlist(tapply(cardio$ind,cardio$PATIENT,function(x) rep(sum(x,na.rm = T),length(x))))
cardio$s[cardio$s>=1]=1
cardio$s[cardio$s==0]=0
cardio$ind=cardio$s

cardio=cardio[!duplicated(cardio$PATIENT), ]
cardio$card=cardio$ind
dim(cardio)
finaltbl=merge(finaltbl,cardio[,c("PATIENT","card")],by="PATIENT",all.x= T)
table(finaltbl$card,useNA = "always")
#############################
####    LDL-CHOL     ########
#############################
chol=data.frame(tbllab)
chol=chol[chol$LAB_U==4,]
#chol=chol[chol$LAB_ID=='CHOL'| chol$LAB_ID=='HDL' | chol$LAB_ID=='TRIG',]
chol$ind = NA
chol$ind[chol$LAB_ID=='CHOL' & chol$LAB_V >=240 | chol$LAB_ID=='HDL' & chol$LAB_V <=35 | chol$LAB_ID=='TRIG' & chol$LAB_V >=200] = 1
chol$ind[chol$LAB_ID=='CHOL' & chol$LAB_V <240 | chol$LAB_ID=='HDL' & chol$LAB_V >35 | chol$LAB_ID=='TRIG' & chol$LAB_V <200] = 0
#chol$ind[is.na(chol$ind)] = 9
#chol$ind = factor(chol$ind,levels = c("1","0","9"))
table(chol$ind,useNA = "always")
chol=chol[complete.cases(chol$PATIENT),]
table(chol$ind,useNA = "always")

# create the right indicator

chol2=chol
chol2=chol2[complete.cases(chol2$ind),]
chol2=chol2[!is.na(chol2$PATIENT),]

chol2$ind2 = unlist(tapply(chol2$ind,chol2$PATIENT,function(x) rep(sum(x),length(x))))
chol2=chol2[!duplicated(chol2$PATIENT),]
chol=chol[!duplicated(chol$PATIENT),]
chol=merge(chol,chol2[,c("PATIENT","ind2")],by='PATIENT',all = T)
table(chol2$ind2,useNA = "always")
chol$indicator=NA
chol$indicator[chol$ind2>=1]=1
chol$indicator[chol$ind2==0]=0
table(chol$indicator,useNA = "always")
chol=chol[!duplicated(chol$PATIENT), ]
table(chol$indicator,useNA = "always")

# deslipedemia

chol_MED=tblMED
chol_MED=tblMED[startsWith(tblMED$MED_ID,'C10'),]
chol_MED=chol_MED[!duplicated(chol_MED$PATIENT), ]
chol_MED$indMed=1
deslipidemia=merge(chol_MED[,c("PATIENT","indMed")],chol[,c("PATIENT","indicator")],by='PATIENT',all = T)
table(deslipidemia$indicator,useNA = "always")
#dia_mel=dia_mel[!duplicated(dia_mel$PATIENT), ]
dim(deslipidemia)

# replace NAs in indicator by values in MED and CEP

deslipidemia$indicator[is.na(deslipidemia$indicator)] <- deslipidemia$indMed[is.na(deslipidemia$indicator)]
table(deslipidemia$indicator,useNA = "always")
deslipidemia$deslip=deslipidemia$indicator
finaltbl=merge(finaltbl,deslipidemia[,c("PATIENT","deslip")],by="PATIENT",all.x = T)
dim(finaltbl)
table(finaltbl$deslip,useNA = "always")

##############################
####   kidney disease     ####
##############################
setwd(WP)
source("eGFRcalculation.R")

finaltbl=merge(finaltbl,egfr[,c("PATIENT","ren")],by="PATIENT",all.x = T)

dim(finaltbl)
table(finaltbl$ren,useNA = "always")

##############################
####  liver disease     ######
##############################
liver_CEP=tblCEP
liver_CEP$ind=NA

liver_CEP$ind[startsWith(tblCEP$CEP_ID,'CLD') | startsWith(tblCEP$CEP_ID,'LIVB') | startsWith(tblCEP$CEP_ID,'LIVD') | startsWith(tblCEP$CEP_ID,'LIVT')]=1
liver_CEP$ind[!(startsWith(tblCEP$CEP_ID,'CLD') | startsWith(tblCEP$CEP_ID,'LIVB') | startsWith(tblCEP$CEP_ID,'LIVD') | startsWith(tblCEP$CEP_ID,'LIVT'))]=0
table(liver_CEP$ind,useNA = "always")
liver_CEP=liver_CEP[!is.na(liver_CEP$PATIENT),]
liver_CEP$s=unlist(tapply(liver_CEP$ind,liver_CEP$PATIENT,function(x) rep(sum(x,na.rm = T),length(x))))
liver_CEP$s[liver_CEP$s>=1]=1
liver_CEP$s[liver_CEP$s==0]=0
liver_CEP$ind=liver_CEP$s
liver_CEP=liver_CEP[!duplicated(liver_CEP$PATIENT), ]

liver_CEP$liv=liver_CEP$ind
finaltbl=merge(finaltbl,liver_CEP[,c("PATIENT","liv")],by="PATIENT",all.x = T)

#HEP HESY να τα βάλω??απο cep id
dim(finaltbl)
table(finaltbl$liv,useNA = "always")

finaltbl=finaltbl[!is.na(finaltbl$BIRTH_D),]
##################################
###### Imputation with Mice ######
##################################

#setwd(WP)
#source("mice.R")
#################################

# Let's build up Multimorbidity variable `mm`
totalMis=finaltbl[,c("PATIENT","dia","hyp","cancer","card","deslip","ren","liv")]

# Number of NAs per row
totalMis$a=rowSums(is.na(totalMis))

# define the sum per row without NAs
totalMis$b <- rowSums(totalMis[, c('dia','hyp','cancer','card','deslip','ren','liv')], na.rm = TRUE)

# generate Multimorbidity

totalMis$mm=NA
totalMis$mm[totalMis$a>=1 & totalMis$b==1]=NA
totalMis$mm[totalMis$a>=2 & totalMis$b<=1]=NA
totalMis$mm[totalMis$a==0  & totalMis$b<=1]=0
totalMis$mm[totalMis$a<=1  & totalMis$b==0]=0
totalMis$mm[totalMis$a>=0 & totalMis$b>=2]=1
table(totalMis$mm,useNA = "always")

# merge mm in finaltbl

finaltbl=merge(finaltbl,totalMis[,c("PATIENT","mm")], by="PATIENT",all.x = T)

#finaltbl=merge(finaltbl,finalData[,c("PATIENT","mm2")], by="PATIENT",all = T)


