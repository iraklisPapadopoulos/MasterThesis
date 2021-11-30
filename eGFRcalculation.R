#tbllab=read_dta("tbllab.dta")
#tblbas=read_dta("tblbas.dta")
#The CKD-EPI formula eGFR is calculated from age, sex and ethnicity as well as serum creatinine.
# eGFR limit to find renal disease
egfrLimit=60

cre=tbllab
cre=cre[cre$LAB_ID=='CRE' ,]
cre=cre[!(cre$LAB_U==99 ),]
#drop=which(cre$LAB_U==5 & cre$LAB_V>=5)
#cre=cre[-drop,]

cre=merge(cre,tblbas[,c("PATIENT","FRSVIS_D")],by='PATIENT',all.x = T)

#cre=cre[!(cre$LAB_D<cre$FRSVIS_D),]

#metatropi ta (6) ìmol-->mmol
#convert ìmol/L --> mmol/L
cre$mili=NA
cre$mili[cre$LAB_U==4]=1
cre$mili[cre$LAB_U==5]=1
cre$mili[cre$LAB_U==1]= 1
cre$mili[cre$LAB_U==6]= 1000
cre=cre[!is.na(cre$PATIENT),]
summary(cre$LAB_V)

cre$LAB_V=cre$LAB_V/cre$mili

# afou ta (6) kai (1) einai se mmol/L tha ta kanw mg/dL
#convert mmol/L --> mg/dL
cre$multi=NA
cre$multi[cre$LAB_U==4]=1
cre$multi[cre$LAB_U==5]=1
cre$multi[cre$LAB_U==1]= 88.4
cre$multi[cre$LAB_U==6]= 88.4
cre=cre[!is.na(cre$PATIENT),]
cre$LAB_V=cre$LAB_V/cre$multi
summary(cre$LAB_V)
cre$LAB_V=round(cre$LAB_V,4)

cre$ord = unlist(tapply(cre$PATIENT,cre$PATIENT,function(x) 1:length(x)))
cre$maxord = unlist(tapply(cre$PATIENT,cre$PATIENT,function(x) rep(length(x),length(x))))
cre=cre[cre$ord==cre$maxord,]

cre=cre[!duplicated(cre$PATIENT), ]

egfr=tblbas
egfr=merge(egfr,cre[,c("PATIENT","LAB_V")],by="PATIENT",all = T)

egfr = egfr[,c("PATIENT","GENDER","ETHNIC","LAB_V","Age")]
names(egfr)[4] = "creatin"
#egfr$creatin[is.na(egfr$creatin)]<-0
egfr$ETHNIC[is.na(egfr$ETHNIC)]<-10

egfr$ETHNIC[egfr$ETHNIC %in% c(20,21,22,23,24)] = "black"
egfr$ETHNIC[egfr$ETHNIC %in% c(0,10,40,99)] = "non-black"

egfr$creatin[egfr$creatin==0]=0.0001
#egfr imputation
egfr$eGFR <- ckd_epi(creat = egfr$creatin, age = egfr$Age, sex = egfr$GENDER, eth = egfr$ETHNIC,units = "US")

#inf in eGFR is 999(personal definition)
#egfr$eGFR[is.infinite(egfr$eGFR)]<-999
#egfr=egfr[egfr$eGFR<=egfrLimit,]
egfr$ren = NA
egfr$ren[egfr$eGFR<=egfrLimit] = 1
egfr$ren[egfr$eGFR>egfrLimit] = 0
table(egfr$ren,useNA = "always")

