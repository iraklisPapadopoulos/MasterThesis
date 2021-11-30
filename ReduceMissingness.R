####################################################
###########      REDUCE MISSINGNESS      ########### 
####################################################

#setwd(WD)
#total=read_dta("total.dta")
library(zoo)
######################
### NAMD-CVD-LIVER ###
######################

#total=total[!is.na(total$CD4_U),]
total$cancer[is.na(total$cancer)]<-0
total$card[is.na(total$card)]<-0
total$liv[is.na(total$liv)]<-0


################
### diabetes ###
################

dia=total[,c("PATIENT","dia","CalendarYear")]

#1
dia=dia[order(dia$CalendarYear,decreasing = TRUE),]
dia=dia[order(dia$PATIENT),]
table(dia$dia,useNA = "always")
dia=dia %>% group_by(PATIENT) %>% mutate(dia2=na.locf(dia, na.rm=FALSE))
table(dia$dia,useNA = "always")
table(dia$dia2,useNA = "always")
dia$dia[is.na(dia$dia) & dia$dia2==0]=0
#dia$dia2[is.na(dia$dia) & dia$dia2==1]=NA
#table(dia$dia2,useNA = "always")
total=total[order(total$PATIENT,total$CalendarYear),]
dia=dia[order(dia$PATIENT,dia$CalendarYear),]

total$dia=dia$dia
table(total$dia,useNA = "always")

#2
#dia=dia[order(dia$CalendarYear),]
#dia=dia[order(dia$PATIENT),]
#table(dia$dia,useNA = "always")
#dia$dia2=NULL
#dia=dia %>% group_by(PATIENT) %>% mutate(dia2=na.locf(dia, na.rm=FALSE))
#table(dia$dia,useNA = "always")
#table(dia$dia2,useNA = "always")
#dia$dia[is.na(dia$dia) & dia$dia2==1]=1
#dia$dia[is.na(dia$dia) & dia$dia2==0]=0
#table(dia$dia,useNA = "always")
#total$dia=dia$dia
#table(total$dia,useNA = "always")

#total$s=unlist(tapply(total$dia,total$PATIENT,function(x) rep(sum(x,na.rm = T),length(x))))
#table(total$dia,useNA = "always")
#total$s[total$s>=1]=NA
#table(total$s,useNA = "always")
#total$dia[is.na(total$dia)]=total$s[is.na(total$dia)]
#table(total$dia,useNA = "always")
#total$s=NULL

#############
### renal ###
#############

ren=total[,c("PATIENT","ren","CalendarYear")]
#ren$s=unlist(tapply(ren$ren,ren$PATIENT,function(x) rep(sum(x,na.rm = T),length(x))))
#ren$s[ren$s>=1]=NA


#total$s=unlist(tapply(total$ren,total$PATIENT,function(x) rep(sum(x,na.rm = T),length(x))))
#table(total$ren,useNA = "always")
#total$s[total$s>=1]=NA
##total$ren[total$s>=1]=1
#table(total$ren,useNA = "always")
#total$ren[is.na(total$ren)]=total$s[is.na(total$ren)]

#table(total$ren,useNA = "always")
#total$s=NULL


#1
ren=ren[order(ren$CalendarYear,decreasing = TRUE),]
ren=ren[order(ren$PATIENT),]
table(ren$ren,useNA = "always")
ren=ren %>% group_by(PATIENT) %>% mutate(ren2=na.locf(ren, na.rm=FALSE))
table(ren$ren,useNA = "always")
table(ren$ren2,useNA = "always")
ren$ren[is.na(ren$ren) & ren$ren2==0]=0


#ren=ren[order(ren$CalendarYear),]
#ren=ren[order(ren$PATIENT),]
#able(ren$ren,useNA = "always")
#ren=ren %>% group_by(PATIENT) %>% mutate(ren3=na.locf(ren2, na.rm=FALSE))
#table(ren$ren,useNA = "always")
#table(ren$ren3,useNA = "always")
#ren$ren[is.na(ren$ren) & ren$ren3==0]=0
#dia$dia2[is.na(dia$dia) & dia$dia2==1]=NA
#table(dia$dia2,useNA = "always")

total=total[order(total$PATIENT,total$CalendarYear),]
ren=ren[order(ren$PATIENT,ren$CalendarYear),]

total$ren=ren$ren
table(total$ren,useNA = "always")

####################
### deslipidemia ###
####################

des=total[,c("PATIENT","deslip","CalendarYear")]

#des$s=unlist(tapply(des$deslip,des$PATIENT,function(x) rep(sum(x,na.rm = T),length(x))))
#table(des$deslip,useNA = "always")
#des$s[des$s>=1]=NA
#des$deslip[is.na(des$deslip) & des$s==0]=0
#table(des$deslip,useNA = "always")

#1

des=des[order(des$CalendarYear,decreasing = TRUE),]
des=des[order(des$PATIENT),]
table(des$deslip,useNA = "always")
des=des %>% group_by(PATIENT) %>% mutate(des2=na.locf(deslip, na.rm=FALSE))
#table(des$deslip,useNA = "always")
#table(des$des2,useNA = "always")
des$deslip[is.na(des$deslip) & des$des2==0]=0
#des$des2[is.na(des$deslip) & des$des2==1]=NA
table(des$des2,useNA = "always")
#total$deslip=des$des2
total=total[order(total$PATIENT,total$CalendarYear),]
des=des[order(des$PATIENT,des$CalendarYear),]

total$deslip=des$deslip
table(total$deslip,useNA = "always")



#2
#des=des[order(des$CalendarYear),]
#des=des[order(des$PATIENT),]
#table(des$deslip,useNA = "always")
#des$des2=NULL
#des=des %>% group_by(PATIENT) %>% mutate(des2=na.locf(deslip, na.rm=FALSE))
#table(des$deslip,useNA = "always")
#table(des$des2,useNA = "always")
#des$deslip[is.na(des$deslip & des$des2==1)]=1
##des$des3[is.na(des$deslip) & des$des2==1]=1
#table(des$des2,useNA = "always")
#total$deslip=des$deslip
#table(total$deslip,useNA = "always")

#3
#total$s=unlist(tapply(total$deslip,total$PATIENT,function(x) rep(sum(x,na.rm = T),length(x))))
#table(total$deslip,useNA = "always")
#total$s[total$s>=1]=NA
#total$ren[total$s>=1]=1
#table(total$deslip,useNA = "always")
#total$deslip[is.na(total$deslip)]=total$s[is.na(total$deslip)]
#table(total$deslip,useNA = "always")


###########
### HTN ###
###########
#total$s=NULL
#total$s=unlist(tapply(total$hyp,total$PATIENT,function(x) rep(sum(x,na.rm = T),length(x))))
#table(total$hyp,useNA = "always")
#total$s[total$s>=1]=NA
#table(total$s,useNA = "always")
#total$hyp[is.na(total$hyp)]=total$s[is.na(total$hyp)]
#table(total$hyp,useNA = "always")
#total$s=NULL

hyp=total[,c("PATIENT","hyp","CalendarYear")]

hyp=hyp[order(hyp$CalendarYear,decreasing = TRUE),]
hyp=hyp[order(hyp$PATIENT),]
table(hyp$hyp,useNA = "always")
hyp=hyp %>% group_by(PATIENT) %>% mutate(hyp2=na.locf(hyp, na.rm=FALSE))
table(hyp$hyp,useNA = "always")
table(hyp$hyp2,useNA = "always")
hyp$hyp[is.na(hyp$hyp) & hyp$hyp2==0]=0
#des$des2[is.na(des$deslip) & des$des2==1]=NA
table(hyp$hyp,useNA = "always")
#total$deslip=des$des2
total=total[order(total$PATIENT,total$CalendarYear),]
hyp=hyp[order(hyp$PATIENT,hyp$CalendarYear),]

total$hyp=hyp$hyp
table(total$hyp,useNA = "always")

#hyp$s=unlist(tapply(hyp$hyp,hyp$PATIENT,function(x) rep(sum(x,na.rm = T),length(x))))

#hyp=total[,c("PATIENT","hyp","CalendarYear","mm","CENTER")]
#hyp=hyp[is.na(hyp$hyp) & hyp$mm==1,]
#hyp=hyp[!is.na(hyp$PATIENT),]
#hyp=hyp[!duplicated(hyp$PATIENT),]
#hyp$ind=0
#total=merge(total,hyp[,c("PATIENT","ind")],by='PATIENT',all.x = T)
#table(total$hyp,useNA = "always")
#total$hyp[is.na(total$hyp)]=total$ind[is.na(total$hyp)]
#table(total$hyp,useNA = "always")


#total$dia[is.na(total$dia)]<-0
#total$hyp[is.na(total$hyp)]<-0
#total$deslip[is.na(total$deslip)]<-0
#total$ren[is.na(total$ren)]<-0

######################
### Multimorbidity ###
######################

# Let's build up Multimorbidity variable `mm`
totalMis=total[,c("PATIENT","CalendarYear","dia","hyp","cancer","card","deslip","ren","liv")]

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
totalMis$mm2=total$mm
totalMis$mm[is.na(totalMis$mm)]=totalMis$mm2[is.na(totalMis$mm)]


totalMis$cond=NA
totalMis$cond[totalMis$a>=0 & totalMis$b>=1]=1
totalMis$cond[totalMis$a==0 & totalMis$b==0]=0
totalMis$cond[totalMis$a<=1  & totalMis$b>=1]=1
totalMis$cond[totalMis$a==0 & totalMis$b>=1]=1
totalMis$cond[totalMis$a>=1  & totalMis$b==0]=NA

table(totalMis$cond,useNA = "always")




#t=totalMis[,c("PATIENT","mm","CalendarYear","mm2")]
# merge mm in finaltbl
#total$mmArxiko=total$mm
#total$cond=NULL
#total$mm=NULL

total=total[order(total$PATIENT,total$CalendarYear),]
totalMis=totalMis[order(totalMis$PATIENT,totalMis$CalendarYear),]
total$asc=seq(1:nrow(total))
totalMis$asc=seq(1:nrow(totalMis))
total$mm=NULL
total=merge(total,totalMis[,c("asc","mm","cond","mm2")],by='asc',all = T)
table(total$mm,useNA = "always")
table(total$cond,useNA = "always")

#table(total$mmArxiko,useNA = "always")
#total=total %>% group_by(PATIENT) %>% mutate(mm3=na.locf(mm, na.rm=FALSE))
#total=total %>% group_by(PATIENT) %>% mutate(cond2=na.locf(cond, na.rm=FALSE))


################################################################################################################
# Clear workspace
rm(list = as.character(ls()[!(ls() %in% c("W","WR","WP","WG","WD","WRD","totalMis","total","total2","hospsToInclude","exportResList"))]) )


