#missings
#MisVal=as.data.frame(colSums(is.na(total)))
#ames(MisVal)[1]="missValues"
#MisVal
# % missingness of diseases
diabetes     =tapply(total$dia,total$CalendarYear        ,function(x) {sum(is.na(x))/length(x)})
hypertension =tapply(total$hyp,total$CalendarYear        ,function(x) {sum(is.na(x))/length(x)})
cancer       =tapply(total$cancer,total$CalendarYear     ,function(x) {sum(is.na(x))/length(x)})
cvd          =tapply(total$card,total$CalendarYear       ,function(x) {sum(is.na(x))/length(x)})
deslipidemia =tapply(total$deslip,total$CalendarYear     ,function(x) {sum(is.na(x))/length(x)})
renal        =tapply(total$ren,total$CalendarYear        ,function(x) {sum(is.na(x))/length(x)})
liver        =tapply(total$liv,total$CalendarYear        ,function(x) {sum(is.na(x))/length(x)})
mm           =tapply(total$mm,total$CalendarYear         ,function(x) {sum(is.na(x))/length(x)})
#mm2          =tapply(t$mm,t$CalendarYear                 ,function(x) {sum(is.na(x))/length(x)})
propMis=data.frame(diabetes,hypertension,cancer,cvd,deslipidemia,renal,liver,mm)
rm(list = as.character(ls()[!(ls() %in% c("W","WR","WP","WG","WD","WRD","total","propMis","propMisp","hospsToInclude","exportResList"))]) )

propMisp=propMis*100
View(propMisp)
