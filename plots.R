#########################
## descriptive analysis##
#########################
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(lcsm)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(grid)
library(gridExtra)
#################################################
# KSMOOTH of Multimorbidity percents over years##
#################################################
e=tapply(total$mm,total$CalendarYear,FUN=sum,na.rm=T)/tapply(total$mm,total$CalendarYear,FUN=length)
e*100
mm=as.data.frame(e)
mm$CalendarYear=rownames(mm)
total=merge(total,mm,by='CalendarYear',all.x = T)
# e is the name of prevalence of mm
setwd(WG)
jpeg('mmCrude.jpg',width=6,height=5,units="in",res=1200)
mmCrude=plot(cbind(total$CalendarYear,total$e
                   ), main = "Multimorbidity through years",xaxt="n",xlab='Calendar Year',ylab='% of Multimorbidity',pch=".")+
axis(1, xaxp=c(2000, 2018, 18), las=2)+
lines(ksmooth(x=total$CalendarYear,y=total$e,kernel="normal",bandwidth=0.9),type="l",lty=1)
dev.off()

#lines(lowess(x=total$CalendarYear,y=total$prev),lty=2)
#legend(15,18,legend=c('lowess','kernal'),lty=c(1,10))
#p <- ggplot(data = total, aes(x = CalendarYear, y = MedianCD4, group = PATIENT),ylim(0,1500))+
#  geom_point(aes(color =CalendarYear ))+geom_smooth(color = "black", size = 0.8, linetype = 2)+
#  coord_cartesian(ylim = c(0, 3000))
#p
#p + coord_cartesian(ylim = c(0, 3000))+ geom_point() 
#totalp=total[!total$cART=="noART",]
#p <- ggplot(data = total, aes(x = ageATcART, y = MedianCD4),ylim(0,1500))+
#  geom_point()+geom_smooth(color = "red", size = 0.8, linetype = 2)+
#  coord_cartesian(xlim = c(17,80),ylim = c(0, 3000))+
#  ggtitle("Plot CD4 measurements against Age at cART initiation") +
 # theme_ipsum() +
#  ylab("Median cd4 of the Year")+
#  xlab("Age at cART initiation")
#p

#################
## Age  for conditions   ##
#################
total=total2
total=total[!is.na(total$cond),]

### by Age group
t1=as.table(tapply(total$cond,list(total$CalendarYear , total$catage),FUN=sum,na.rm=T))
t2=as.table(tapply(total$cond,list(total$CalendarYear , total$catage),FUN=length))   
t=t1*(1/t2)
t
condAge=as.data.frame(t)
colnames(condAge)=c("CalendarYear","Age","percent")
#mmAge=reshape(mmAge, idvar = "Var1", timevar = "Var2", direction = "wide")
#colnames(mmAge)=c("CalendarYear","[18-40)","[40-50)","[50-60)","[60+)")
# Plot
condAge %>%
  ggplot( aes(x=CalendarYear, y=percent*100, group=Age, color=Age)) +
  geom_line() + geom_point()+
  scale_color_discrete()+
  ggtitle("Morbidity percentages over time by Age ") +
 # theme_ipsum() +
  xlab("Calendar Year")+
  ylab("% of having at least 1 morbidity") +
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=14, face="bold",vjust=-0.5,hjust=0.5),
    axis.title.y = element_text(color="black", size=14, face="bold",hjust=0.5)
  ) 
setwd(WG)
ggsave("condAge1111111111111.jpeg", width = 30, height = 20,units = "cm")


#################
## GENDER  for conditions   ##
#################
total=total2
total=total[!is.na(total$cond),]
### by Age group
t1=as.table(tapply(total$cond,list(total$CalendarYear , total$gender),FUN=sum,na.rm=T))
t2=as.table(tapply(total$cond,list(total$CalendarYear , total$gender),FUN=length))   
t=t1*(1/t2)
t
condGender=as.data.frame(t)
colnames(condGender)=c("CalendarYear","gender","percent")
#mmAge=reshape(mmAge, idvar = "Var1", timevar = "Var2", direction = "wide")
#colnames(mmAge)=c("CalendarYear","[18-40)","[40-50)","[50-60)","[60+)")
# Plot
condGender %>%
  ggplot( aes(x=CalendarYear, y=percent*100, group=gender, color=gender)) +
  geom_line() + geom_point()+
  scale_color_discrete()+
  ggtitle("Διαχρονικός επιπολασμός νοσηρότητας ανά φύλο") +
#  theme_ipsum() +
  xlab("Ημερολογιακό έτος")+
  ylab("Επιπολασμός νοσηρότητας (%)") +
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=14, face="bold",vjust=-0.5,hjust=0.5),
    axis.title.y = element_text(color="black", size=14, face="bold",hjust=0.5)
  ) 
setwd(WG)
ggsave("condGender2.jpeg", width = 30, height = 20,units = "cm")




# MULTIMORBIDITY


#################
## Age   ##
#################
total=total[!is.na(total$mm),]
### by Age group
t1=as.table(tapply(total$mm,list(total$CalendarYear , total$catage),FUN=sum,na.rm=T))
t2=as.table(tapply(total$mm,list(total$CalendarYear , total$catage),FUN=length))   
t=t1*(1/t2)
t
mmAge=as.data.frame(t)
colnames(mmAge)=c("CalendarYear","Age","percent")
#mmAge=reshape(mmAge, idvar = "Var1", timevar = "Var2", direction = "wide")
#colnames(mmAge)=c("CalendarYear","[18-40)","[40-50)","[50-60)","[60+)")
# Plot
mmAge %>%
  ggplot( aes(x=CalendarYear, y=percent*100, group=Age, color=Age)) +
  geom_line() + geom_point()+
  scale_color_discrete()+
  ggtitle("Multimorbidity percentages over time by Age ") +
  theme_ipsum() +
  ylab("% of Multimorbidity") +
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=14, face="bold",vjust=-0.5,hjust=0.5),
    axis.title.y = element_text(color="black", size=14, face="bold",hjust=0.5)
  ) 
setwd(WG)
ggsave("mmAge.jpeg", width = 30, height = 20,units = "cm")


##################################################################
# plot mm percenteges over time by Age at HIV diagnosis groups####
##################################################################
#################
## Age at HIV  ##
#################

### by Age group
t1=as.table(tapply(total$mm,list(total$CalendarYear , total$gender),FUN=sum,na.rm=T))
t2=as.table(tapply(total$mm,list(total$CalendarYear , total$gender),FUN=length))   
t=t1*(1/t2)
t
mmAge=as.data.frame(t)
colnames(mmAge)=c("CalendarYear","Age","percent")
#mmAge=reshape(mmAge, idvar = "Var1", timevar = "Var2", direction = "wide")
#colnames(mmAge)=c("CalendarYear","[18-40)","[40-50)","[50-60)","[60+)")
# Plot
mmAge %>%
  ggplot( aes(x=CalendarYear, y=percent, group=Age, color=Age)) +
  geom_line() + geom_point()+
    scale_color_discrete()+
  ggtitle("Multimorbidity percentages over time by Age at HIV diagnosis") +
  theme_ipsum() +
  ylab("% of Multimorbidity") +
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=14, face="bold",vjust=-0.5,hjust=0.5),
    axis.title.y = element_text(color="black", size=14, face="bold",hjust=0.5)
  ) 
setwd(WG)
ggsave("mmAgehiv.jpeg", width = 30, height = 20,units = "cm")

########################################################
# plot mm percenteges over time by Age at ART groups####
########################################################
#################
## Age at cART ##
#################

### by Age group
a1=as.table(tapply(total$mm,list(total$CalendarYear , total$catageart),FUN=sum,na.rm=T))
a2=as.table(tapply(total$mm,list(total$CalendarYear , total$catageart),FUN=length))   
a=a1*(1/a2)
a
mmArt=as.data.frame(a)
colnames(mmArt)=c("CalendarYear","Age","percent")

# Plot
mmArt %>%
  ggplot( aes(x=CalendarYear, y=percent, group=Age, color=Age)) +
  geom_line() + geom_point()+
  scale_color_discrete()+
  ggtitle("Multimorbidity percentages over time by Age at cART initiation") +
  theme_ipsum() +
  ylab("% of Multimorbidity") +
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=14, face="bold",vjust=-0.5,hjust=0.5),
    axis.title.y = element_text(color="black", size=14, face="bold",hjust=0.5)
  ) 
setwd(WG)
ggsave("mmArt.jpeg", width = 30, height = 20,units = "cm")


#################################################
# plot mm percenteges over time by mode groups###
#################################################
##########
## mode ##
##########
### by Age group
m1=as.table(tapply(total$mm,list(total$CalendarYear , total$gender),FUN=sum,na.rm=T))
m2=as.table(tapply(total$mm,list(total$CalendarYear , total$gender),FUN=length))   
m=m1*(1/m2)
m
mmMode=as.data.frame(m)
colnames(mmMode)=c("CalendarYear","mode","percent")
#plot
mmMode %>%
  ggplot( aes(x=CalendarYear, y=percent*100, group=mode, color=mode)) +
  geom_line(size=1) + geom_point()+
  scale_color_discrete()+
  ggtitle("Multimorbidity percentages over time by Mode of infection group") +
#  theme_ipsum() +
  ylab("% of Multimorbidity")+
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=14, face="bold",vjust=-0.5,hjust=0.5),
    axis.title.y = element_text(color="black", size=14, face="bold",hjust=0.5)
  )  
setwd(WG)
ggsave("mmMode.jpeg", width = 30, height = 20,units = "cm")

#################################################
# plot mm percenteges over time by race groups###
#################################################
##########
## race ##
##########
### by Age group
r1=as.table(tapply(total$mm,list(total$CalendarYear , total$race),FUN=sum,na.rm=T))
r2=as.table(tapply(total$mm,list(total$CalendarYear , total$race),FUN=length))   
r=r1*(1/r2)
r
mmRace=as.data.frame(r)
colnames(mmRace)=c("CalendarYear","race","percent")

# Plot
mmRacePlot=mmRace %>%
  ggplot( aes(x=CalendarYear, y=percent, group=race, color=race)) +
  geom_line(size=1) + geom_point()+
  scale_color_discrete()+
  ggtitle("Multimorbidity percentages over time by Race group") +
  theme_ipsum() +
  ylab("% of Multimorbidity") +
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=14, face="bold",vjust=-0.5,hjust=0.5),
    axis.title.y = element_text(color="black", size=14, face="bold",hjust=0.5)
  ) 

setwd(WG)
ggsave("mmRace.jpeg", width = 30, height = 20,units = "cm")



#################################################
# plot mm percenteges over time by edulvl groups###
#################################################
##########
## edulvl ##
##########
### by Age group
e1=as.table(tapply(total$mm,list(total$CalendarYear , total$edulvl),FUN=sum,na.rm=T))
e2=as.table(tapply(total$mm,list(total$CalendarYear , total$edulvl),FUN=length))   
e=e1*(1/e2)
e
mmEdu=as.data.frame(e)
colnames(mmEdu)=c("CalendarYear","edulvl","percent")

# Plot
mmEduPlot=mmEdu %>%
  ggplot( aes(x=CalendarYear, y=percent, group=edulvl, color=edulvl)) +
  geom_line(size=1) + geom_point()+
  scale_color_discrete()+
  ggtitle("Multimorbidity percentages over time by education level group") +
  theme_ipsum() +
  ylab("% of Multimorbidity") +
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=14, face="bold",vjust=-0.5,hjust=0.5),
    axis.title.y = element_text(color="black", size=14, face="bold",hjust=0.5)
  ) 

setwd(WG)
ggsave("mmEdu.jpeg", width = 30, height = 20,units = "cm")



#################################################
# plot mm percenteges over time by smoke groups###
#################################################
##########
## smoke ##
##########
### by Age group
s1=as.table(tapply(total$mm,list(total$CalendarYear , total$smoke),FUN=sum,na.rm=T))
s2=as.table(tapply(total$mm,list(total$CalendarYear , total$smoke),FUN=length))   
s=s1*(1/s2)
s
mmSmoke=as.data.frame(s)
colnames(mmSmoke)=c("CalendarYear","smoke","percent")

# Plot
mmSmokePlot=mmSmoke %>%
  ggplot( aes(x=CalendarYear, y=percent, group=smoke, color=smoke)) +
  geom_line(size=1) + geom_point()+
  scale_color_discrete()+
  ggtitle("Multimorbidity percentages over time by smoking group") +
  theme_ipsum() +
  ylab("% of Multimorbidity") +
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=14, face="bold",vjust=-0.5,hjust=0.5),
    axis.title.y = element_text(color="black", size=14, face="bold",hjust=0.5)
  ) 

setwd(WG)
ggsave("mmSmoke.jpeg", width = 30, height = 20,units = "cm")




#################################################
# plot mm percenteges over time by smoke groups###
#################################################
##########
## smoke ##
##########
### by Age group
s1=as.table(tapply(total$mm,list(total$CalendarYear , total$smoke),FUN=sum,na.rm=T))
s2=as.table(tapply(total$mm,list(total$CalendarYear , total$smoke),FUN=length))   
s=s1*(1/s2)
s
mmSmoke=as.data.frame(s)
colnames(mmSmoke)=c("CalendarYear","smoke","percent")

# Plot
mmSmokePlot=mmSmoke %>%
  ggplot( aes(x=CalendarYear, y=percent, group=smoke, color=smoke)) +
  geom_line(size=1) + geom_point()+
  scale_color_discrete()+
  ggtitle("Multimorbidity percentages over time by smoking group") +
  theme_ipsum() +
  ylab("% of Multimorbidity") +
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=14, face="bold",vjust=-0.5,hjust=0.5),
    axis.title.y = element_text(color="black", size=14, face="bold",hjust=0.5)
  ) 

setwd(WG)
ggsave("mmSmoke.jpeg", width = 30, height = 20,units = "cm")





#################################################
# plot mm percenteges over time by bmi groups###
#################################################
##########
## bmi ##
##########
### by Age group
b1=as.table(tapply(total$mm,list(total$CalendarYear , total$BMI),FUN=sum,na.rm=T))
b2=as.table(tapply(total$mm,list(total$CalendarYear , total$BMI),FUN=length))   
b=b1*(1/b2)
b
mmBmi=as.data.frame(b)
colnames(mmBmi)=c("CalendarYear","bmi","percent")

# Plot
mmBmiPlot=mmBmi %>%
  ggplot( aes(x=CalendarYear, y=percent, group=bmi, color=bmi)) +
   geom_point(size=2)+
  scale_color_discrete()+
  ggtitle("Multimorbidity percentages over time by BMI group") +
  theme_ipsum() +
  ylab("% of Multimorbidity") +
  geom_line(size = 1)+
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=14, face="bold",vjust=-0.5,hjust=0.5),
    axis.title.y = element_text(color="black", size=14, face="bold",hjust=0.5)
  ) 

setwd(WG)
ggsave("mmBmi.jpeg", width = 30, height = 20,units = "cm")




#################################################
# plot mm percenteges over time by cART groups###
#################################################
##########
## cART ##
##########
### by Age group
total=total[!is.na(total$cARTregimen),]
c1=as.table(tapply(total$mm,list(total$CalendarYear , total$cARTregimen),FUN=sum,na.rm=T))
c2=as.table(tapply(total$mm,list(total$CalendarYear , total$cARTregimen),FUN=length))   
c=c1*(1/c2)
c
mmCart=as.data.frame(c)
colnames(mmCart)=c("CalendarYear","cART","percent")

# Plot
mmCartPlot=mmCart %>%
  ggplot( aes(x=CalendarYear, y=percent*100, group=cART, color=cART)) +
   geom_point(size=2)+
  scale_color_discrete()+
  ggtitle("Multimorbidity percentages over time by cART group") +
  theme_ipsum() +
  ylab("% of Multimorbidity") +
  geom_line()+
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=14, face="bold",vjust=-0.5,hjust=0.5),
    axis.title.y = element_text(color="black", size=14, face="bold",hjust=0.5)
  ) 






setwd(WG)
ggsave("mmcART.jpeg", width = 30, height = 20,units = "cm")



#################################################
# plot mm percenteges over time by aids groups###
#################################################
##########
## aids ##
##########
### by Age group
a1=as.table(tapply(total$mm,list(total$CalendarYear , total$aids),FUN=sum,na.rm=T))
a2=as.table(tapply(total$mm,list(total$CalendarYear , total$aids),FUN=length))   
a=a1*(1/a2)
a
mmAids=as.data.frame(a)
colnames(mmAids)=c("CalendarYear","aids","percent")

# Plot
mmAidsPlot=mmAids %>%
  ggplot( aes(x=CalendarYear, y=percent*100, group=aids, color=aids)) +
  geom_line(linetype=1,size = 1)+ geom_point(size=2)+
  scale_color_discrete()+
  ggtitle("Multimorbidity percentages over time by AIDS status") +
  theme_ipsum() +
  ylab("% of Multimorbidity") +
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=14, face="bold",vjust=-0.5,hjust=0.5),
    axis.title.y = element_text(color="black", size=14, face="bold",hjust=0.5)
  ) 

#+  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),        panel.background = element_blank())
  

setwd(WG)
ggsave("mmAids.jpeg", width = 30, height = 20,units = "cm")


#######################
### Prevalence Plot ###
#######################
total=t
total=total[!is.na(total$mm),]
mytable=table(total$morbidities,total$CalendarYear)
mytable2=prop.table(mytable,2)
barplot(mytable)
total2=as.data.frame.matrix(mytable)
total2$morbidities=rownames(total2)
library(reshape2)
total2Long=melt(total2,id.vars = c("morbidities"), value.name="counts")
names(total2Long)[2]="CalendarYear"

e=tapply(total$mm,total$CalendarYear,FUN=sum,na.rm=T)/tapply(total$mm,total$CalendarYear,FUN=length)
e

mm=as.data.frame(e)
mm$CalendarYear=rownames(mm)
colnames(mm)=c("multimorbidity","CalendarYear")
total2Long=merge(total2Long,mm,by='CalendarYear',all.x = T)
total2Long$part=unlist(tapply(total2Long$counts,total2Long$CalendarYear,function(x) {x/sum(x)}))
total2Long=total2Long[!total2Long$morbidities==0,]
total2Long=total2Long[!total2Long$morbidities==1,]
total2Long$multimorbidity=as.numeric(total2Long$multimorbidity)
total2Long$counts=as.numeric(total2Long$counts)
total2Long$morbidities=as.factor(total2Long$morbidities)


ggp <- ggplot(total2Long, aes(x = CalendarYear, y =part*100 , fill = factor(morbidities,levels = c(5,4,3,2)), label =multimorbidity )) +  # Create stacked bar chart
  geom_bar(stat = "identity")
ggp  
ggp<- ggp + geom_bar(stat="identity", position = "stack") +
  ylab("Percent(%) of people with Multimorbidity") + 
  xlab("Year") + 
  labs(fill = "morbidities") 


# Create a table grob
# Extract the legend as a separate grob
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

leg = g_legend(ggp)
total=total[!total$morbidities==0,]
total=total[!total$morbidities==1,]
a=table(total$morbidities,total$CalendarYear)
tab = tableGrob(a, rows=NULL)

tab$widths <- unit(rep(1/ncol(tab), ncol(tab)), "npc")

# Lay out plot, legend, and table grob
grid.arrange(arrangeGrob(nullGrob(), 
                         ggp + #guides(fill=F) + 
                           theme(axis.text.x=element_blank(),
                                 axis.title.x=element_blank(),
                                 axis.ticks.x=element_blank()),
                         widths=c(1,8)), 
             arrangeGrob(arrangeGrob(nullGrob(),leg,heights=c(1,15)),
                         tab, nullGrob(), widths=c(4,20,1)),
             heights=c(4,2))

setwd(WG)
ggsave("multimor.jpeg", width = 30, height = 20,units = "cm")

#####################################
### Prevalence of conditions Plot ###
#####################################
total=t
total=total[!is.na(total$cond),]
mytable=table(total$morbidities,total$CalendarYear)
mytable2=prop.table(mytable,2)
barplot(mytable)
total2=as.data.frame.matrix(mytable)
total2$morbidities=rownames(total2)
library(reshape2)
total2Long=melt(total2,id.vars = c("morbidities"), value.name="counts")
names(total2Long)[2]="CalendarYear"

e=tapply(total$cond,total$CalendarYear,FUN=sum,na.rm=T)/tapply(total$cond,total$CalendarYear,FUN=length)
e

mm=as.data.frame(e)
mm$CalendarYear=rownames(mm)
colnames(mm)=c("multimorbidity","CalendarYear")
total2Long=merge(total2Long,mm,by='CalendarYear',all.x = T)
total2Long$part=unlist(tapply(total2Long$counts,total2Long$CalendarYear,function(x) {x/sum(x)}))
total2Long=total2Long[!total2Long$morbidities==0,]
#total2Long=total2Long[!total2Long$morbidities==1,]
total2Long$multimorbidity=as.numeric(total2Long$multimorbidity)
total2Long$counts=as.numeric(total2Long$counts)
total2Long$morbidities=as.factor(total2Long$morbidities)


ggp <- ggplot(total2Long, aes(x = CalendarYear, y =part*100 , fill = factor(morbidities,levels = c(6,5,4,3,2,1)), label =multimorbidity )) +  # Create stacked bar chart
  geom_bar(stat = "identity")
ggp 
ggp<-ggp + geom_bar(stat="identity", position = "stack") +
  ylab("Percent(%) of people in HIV care with >1 Condition") + 
  xlab("Year") + 
  labs(fill = "morbidities") 





# Create a table grob
# Extract the legend as a separate grob
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

leg = g_legend(ggp)
total=total[!total$morbidities==0,]
a=table(total$morbidities,total$CalendarYear)
tab = tableGrob(a, rows=NULL)

tab$widths <- unit(rep(1/ncol(tab), ncol(tab)), "npc")

# Lay out plot, legend, and table grob
grid.arrange(arrangeGrob(nullGrob(), 
                         ggp + #guides(fill=F) + 
                           theme(axis.text.x=element_blank(),
                                 axis.title.x=element_blank(),
                                 axis.ticks.x=element_blank()),
                         widths=c(1,8)), 
             arrangeGrob(arrangeGrob(nullGrob(),leg,heights=c(1,15)),
                         tab, nullGrob(), widths=c(3,19,1)),
             heights=c(4,2))

setwd(WG)
ggsave("conditions.jpeg", width = 30, height = 20,units = "cm")


#### multimorbidity by Age 2000-2009-2018

backup=total
total=totalSTART
total=total[!is.na(total$mm),]
total=total[total$CalendarYear==2000,]
mytable=table(total$morbidities,total$catage)
mytable2=prop.table(mytable,2)
barplot(mytable)
total2=as.data.frame.matrix(mytable)
total2$morbidities=rownames(total2)
library(reshape2)
total2Long=melt(total2,id.vars = c("morbidities"), value.name="counts")
names(total2Long)[2]="catage"

e=tapply(total$mm,total$catage,FUN=sum,na.rm=T)/tapply(total$mm,total$catage,FUN=length)
e

mm=as.data.frame(e)
mm$catage=rownames(mm)
colnames(mm)=c("multimorbidity","catage")
total2Long=merge(total2Long,mm,by='catage',all.x = T)
total2Long$part=unlist(tapply(total2Long$counts,total2Long$catage,function(x) {x/sum(x)}))
total2Long=total2Long[!total2Long$morbidities==0,]
total2Long=total2Long[!total2Long$morbidities==1,]
total2Long$multimorbidity=as.numeric(total2Long$multimorbidity)
total2Long$counts=as.numeric(total2Long$counts)
total2Long$morbidities=as.factor(total2Long$morbidities)


ggp <- ggplot(total2Long, aes(x = catage, y =part*100 , fill = factor(morbidities,levels = c(5,4,3,2)), label =multimorbidity )) +  # Create stacked bar chart
  geom_bar(stat = "identity")
ggp  
ggp<- ggp + geom_bar(stat="identity", position = "stack") +
  ylab("Percent(%) of people with Multimorbidity") + 
  xlab("Age Categories") + 
  labs(fill = "Morbidities") 
ggp

setwd(WG)
ggsave("mmByAge2000.jpeg", width = 30, height = 20,units = "cm")

############
total=backup
total=total[!is.na(total$mm),]
total=total[total$CalendarYear==2009,]
mytable=table(total$morbidities,total$catage)
mytable2=prop.table(mytable,2)
barplot(mytable)
total2=as.data.frame.matrix(mytable)
total2$morbidities=rownames(total2)
library(reshape2)
total2Long=melt(total2,id.vars = c("morbidities"), value.name="counts")
names(total2Long)[2]="catage"

e=tapply(total$mm,total$catage,FUN=sum,na.rm=T)/tapply(total$mm,total$catage,FUN=length)
e

mm=as.data.frame(e)
mm$catage=rownames(mm)
colnames(mm)=c("multimorbidity","catage")
total2Long=merge(total2Long,mm,by='catage',all.x = T)
total2Long$part=unlist(tapply(total2Long$counts,total2Long$catage,function(x) {x/sum(x)}))
total2Long=total2Long[!total2Long$morbidities==0,]
total2Long=total2Long[!total2Long$morbidities==1,]
total2Long$multimorbidity=as.numeric(total2Long$multimorbidity)
total2Long$counts=as.numeric(total2Long$counts)
total2Long$morbidities=as.factor(total2Long$morbidities)


ggp <- ggplot(total2Long, aes(x = catage, y =part*100 , fill = factor(morbidities,levels = c(5,4,3,2)), label =multimorbidity )) +  # Create stacked bar chart
  geom_bar(stat = "identity")
ggp  
ggp<- ggp + geom_bar(stat="identity", position = "stack") +
  ylab("Percent(%) of people with Multimorbidity") + 
  xlab("Age Categories") + 
  labs(fill = "Morbidities") 
ggp

setwd(WG)
ggsave("mmByAge2009.jpeg", width = 30, height = 20,units = "cm")


############
#total=backup
#total=total[!is.na(total$mm),]
total=total[total$CalendarYear==2018,]
mytable=table(total$morbidities,total$catage)
mytable2=prop.table(mytable,2)
barplot(mytable)
total2=as.data.frame.matrix(mytable)
total2$morbidities=rownames(total2)
library(reshape2)
total2Long=melt(total2,id.vars = c("morbidities"), value.name="counts")
names(total2Long)[2]="catage"

e=tapply(total$mm,total$catage,FUN=sum,na.rm=T)/tapply(total$mm,total$catage,FUN=length)
e

mm=as.data.frame(e)
mm$catage=rownames(mm)
colnames(mm)=c("multimorbidity","catage")
total2Long=merge(total2Long,mm,by='catage',all.x = T)
total2Long$part=unlist(tapply(total2Long$counts,total2Long$catage,function(x) {x/sum(x)}))
total2Long=total2Long[!total2Long$morbidities==0,]
total2Long=total2Long[!total2Long$morbidities==1,]
total2Long$multimorbidity=as.numeric(total2Long$multimorbidity)
total2Long$counts=as.numeric(total2Long$counts)
total2Long$morbidities=as.factor(total2Long$morbidities)


ggp <- ggplot(total2Long, aes(x = catage, y =part*100 , fill = factor(morbidities,levels = c(5,4,3,2)), label =multimorbidity )) +  # Create stacked bar chart
  geom_bar(stat = "identity")
ggp  
ggp<- ggp + geom_bar(stat="identity", position = "stack") +
  ylab("Percent(%) of people with Multimorbidity") + 
  xlab("Age Categories") + 
  labs(fill = "Morbidities") 
ggp

setwd(WG)
ggsave("mmByAge2018.jpeg", width = 30, height = 20,units = "cm")




#total=total2[total2$CalendarYear==2000,]
total=total2[!is.na(total2$mm),]
e=tapply(total$mm,total$CalendarYear,FUN=sum,na.rm=T)/tapply(total$mm,total$CalendarYear,FUN=length)
e
mm=as.data.frame(e*100)
mm$year=rownames(mm)
colnames(mm)=c("mm","CalendarYear")

total=total2[!is.na(total2$deslip),]
mm$deslip=100*unlist(tapply(total$deslip,total$CalendarYear,FUN=sum,na.rm=T)/tapply(total$deslip,total$CalendarYear,FUN=length))

total=total2[!is.na(total2$hyp),]
mm$hyp=100*unlist(tapply(total$hyp,total$CalendarYear,FUN=sum,na.rm=T)/tapply(total$hyp,total$CalendarYear,FUN=length))

total=total2[!is.na(total2$ren),]
mm$ren=100*unlist(tapply(total$ren,total$CalendarYear,FUN=sum,na.rm=T)/tapply(total$ren,total$CalendarYear,FUN=length))

total=total2[!is.na(total2$dia),]
mm$dia=100*unlist(tapply(total$dia,total$CalendarYear,FUN=sum,na.rm=T)/tapply(total$dia,total$CalendarYear,FUN=length))

total=total2[!is.na(total2$card),]
mm$card=100*unlist(tapply(total$card,total$CalendarYear,FUN=sum,na.rm=T)/tapply(total$card,total$CalendarYear,FUN=length))

total=total2[!is.na(total2$liv),]
mm$liv=100*unlist(tapply(total$liv,total$CalendarYear,FUN=sum,na.rm=T)/tapply(total$liv,total$CalendarYear,FUN=length))

total=total2[!is.na(total2$cancer),]
mm$cancer=100*unlist(tapply(total$cancer,total$CalendarYear,FUN=sum,na.rm=T)/tapply(total$cancer,total$CalendarYear,FUN=length))



#### comorbidities

mm$CalendarYear=as.factor(mm$CalendarYear)
mm$CalendarYear=as.numeric(mm$CalendarYear)

mm %>% ggplot() +
  geom_line(aes(x = CalendarYear, y = mm), size = 1.5, color = "black") +
  geom_point(aes(x = CalendarYear, y = mm), 
             shape = 21, size = 2.0, stroke = 1.5, color = "black", fill = "white") +
  
  geom_line(aes(x = CalendarYear, y = deslip), size = 0.8, color = "blue") +
  geom_point(aes(x = CalendarYear, y = deslip), 
             shape = 21, size = 1.5, stroke = 1.5, color = "blue", fill = "white")+
  
  geom_line(aes(x = CalendarYear, y = hyp), size = 0.8, color = "darkorchid") +
  geom_point(aes(x = CalendarYear, y = hyp), 
             shape = 21, size = 1.0, stroke = 1.5, color = "darkorchid", fill = "white") +
  
  geom_line(aes(x = CalendarYear, y = ren), size = 0.8, color = "green") +
  geom_point(aes(x = CalendarYear, y = ren), 
             shape = 21, size = 1.5, stroke = 1.5, color = "green", fill = "white")+
  
  geom_line(aes(x = CalendarYear, y = card), size = 0.8, color = "red") +
  geom_point(aes(x = CalendarYear, y = card), 
             shape = 21, size = 1.0, stroke = 1.5, color = "red", fill = "white") +
  
  geom_line(aes(x = CalendarYear, y = liv), size = 0.8, color = "orange") +
  geom_point(aes(x = CalendarYear, y = liv), 
             shape = 21, size = 1.5, stroke = 1.5, color = "orange", fill = "white")+
  
  geom_line(aes(x = CalendarYear, y = cancer), size = 0.8, color = "deeppink") +
  geom_point(aes(x = CalendarYear, y = cancer), 
             shape = 21, size = 1.0, stroke = 1.5, color = "deeppink", fill = "white") +
  
  geom_line(aes(x = CalendarYear, y = dia), size = 0.8, color = "cyan") +
  geom_point(aes(x = CalendarYear, y = dia), 
             shape = 21, size = 1.5, stroke = 1.5, color = "cyan", fill = "white")+
  ggtitle("Prevalence of Comorbidities through years") +
  xlab("Calendar Year") +
  ylab("Prevalence(%)") +
  theme_ipsum() +
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=14, face="bold",vjust=-0.5,hjust=0.5),
    axis.title.y = element_text(color="black", size=14, face="bold",hjust=0.5)
  ) 
setwd(WG)
ggsave("ComorbiditiesPrevalence.jpeg", width = 30, height = 20,units = "cm")  
