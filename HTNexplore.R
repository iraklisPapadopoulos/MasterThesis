

tapply(x$HighPres,x$mOc,table)
tapply(x$mOc,x$HighPres,table)

a=x[x$CalendarYear==2000,]
tapply(a$HighPres,a$mOc,table)



#### ανα νοσοκομεια


y=x
#y=y[!duplicated(y$PATIENT),]

100*tapply(y$hyp,y$CENTER,function(x) sum(is.na(x)))/tapply(y$hyp,y$CENTER,function(x) length(x))

table(x$HighPres[x$mOc==0],useNA = "always")
617/(617+2031)
table(x$HighPres[x$mOc==1],useNA = "always")
295/(295+137)

tapply(x$HighPres[x$mOc==0],x$CalendarYear,FUN = sum,na.rm=T)

x=x[x$mOc==0 & x$HighPres==1,]
x=x[!duplicated(x$PATIENT),]


tapply(x$PATIENT,x$CalendarYear,length)



y=total[,c("PATIENT","CalendarYear","hyp")]
y=y[y$hyp==1,]
y=y[!duplicated(y$PATIENT),]
tapply(y$PATIENT,y$CalendarYear,length)



tapply(x$PATIENT,x$CalendarYear,length)/tapply(y$PATIENT,y$CalendarYear,length)
