# Script to organize fish call data
# Glass sponge reef acoustics project 

#Glass sponge reef working group 4/1/2022

# Load packages----
source("scripts/install_packages_function.R")

lp("tidyverse")
lp("lubridate")
lp("Rraven")
lp("readxl")

# Bring in functions we've written----
source("scripts/organizemanualchecks.R")
source("scripts/organizeautodetections.R")

# Explore how to load in autodetection data----
# # now bring in files
# 
# lb<-imp_raven("odata/lb_detector",
#               all.data=TRUE)
# 
# # glimpse data
# 
# glimpse(lb)
# 
# table(lb$Class)
# 
# # this is how you call the function
# 
# function.check<-fishcall.bymin.auto(seltable=lb)

###Lions Bay----
# start reviewing manual detections 

lb1<-man.check.organize("LionsBayDectionsCheck_reviewer1.txt")
lb2<-man.check.organize("LionsBayDectionsCheck_reviewer2.txt")
lb3<-man.check.organize("LionsBayDectionsCheck_reviewer3.txt")

#we will just be exploring the first reviewer check
lb1$fish.call.entry.check
lb1[[1]]

lb1$quick.look
lb1[[2]]
lb1$quick.look[1,]
lb1[[2]]$Auto.Class

theme_set(theme_bw()+theme(panel.grid = element_blank()))

ggplot(lb1$by.min.summary)+
  geom_jitter(aes(x=n.man.fishcall,y=n.auto.fishcall))+
  geom_abline(aes(intercept=0,slope=1))
#our fish calls and auto detector calls are not matching

ggplot(lb1$full.data)+
  geom_jitter(aes(x=Confidence,y=Auto.Class,color=agree),alpha=.5)+
  facet_wrap(~Call.type)
# # examine outlier here
# filter(lb1$full.data,Auto.Class=="NN")%>%
#   filter(agree==FALSE)%>%
#   filter(Call.type=="N")

# we fixed the problem in the function - code below not needed anymore
# ##auto detector thinks its a fish when its not for the N section.
# lb1$full.data%>%
#   mutate(Call.type= ifelse(Call.type %in% c("G", "G ","G  "),"G",ifelse(Call.type %in% c("K", "K  "),"K","N")))%>%
#   ggplot()+
#   geom_jitter(aes(x=Confidence,y=Auto.Class,color=agree),alpha=.5)+
#   facet_wrap(~Call.type)
# #for this last 2 plots, theres one false data point where i.  should not be with the auto class NN and the manual N. If you run this line below you see in the call type column a singular N that we think might be the problem and do not know what it is about.....
# lb1$full.data%>%filter(Auto.Class=="NN"& agree=="FALSE")%>%View()
# 


###BellaBella-----
# start reviewing manual detections
bella<-man.check.organize("BellaBellaDectionsCheck.txt")

bella$fish.call.entry.check
bella[[1]]

bella$quick.look
bella[[2]]
bella$quick.look[1,]
bella[[2]]$Auto.Class

theme_set(theme_bw()+theme(panel.grid = element_blank()))



ggplot(bella$full.data)+
  geom_jitter(aes(x=Confidence,y=Auto.Class,color=agree),alpha=.5)

bella$full.data$man.class<-case_when(
  bella$full.data$Auto.Class=="FS" & bella$full.data$agree ~ "FS",
  bella$full.data$Auto.Class=="FS" & !bella$full.data$agree ~"NN",
  bella$full.data$Auto.Class=="NN" & bella$full.data$agree ~ "NN",
  bella$full.data$Auto.Class=="NN" & !bella$full.data$agree ~"FS")


ggplot(bella$full.data)+
  geom_jitter(aes(x=Confidence,y=man.class),alpha=.5)+
  facet_wrap(~Auto.Class)

# adjusting confidence cutoffs doesn't seem like a great approach to improve fit

# Now look at per minute
ggplot(bella$by.min.summary)+
  geom_jitter(aes(x=n.man.fishcall,y=n.auto.fishcall))+
  geom_abline(aes(intercept=0,slope=1))

# bring in SPL

bella.spl<-read_xlsx("odata/Bella Bella_SPL_60sec.xlsx")

bella.spl2<-bella.spl%>%
  mutate(yr=year(Date),
         mnth=month(Date),
         d=day(Date),
         hr=hour(Time),
         min=minute(Time),
         sec=second(Time),
         SPL_DT=ymd_hms(paste(yr,mnth,d,hr,min,sec)),
         inter=row_number())%>%
  select(-DateTime,-Date,-Time)

bella$full.data$inter<-findInterval(bella$full.data$CallDate,bella.spl2$SPL_DT)

bella.spl3<-bella.spl2%>%
  select(-yr,-mnth,-d,-hr,-min,-sec)

bella.full.min<-bella$full.data%>%
  left_join(bella.spl3)%>%
  mutate(Auto=ifelse(Auto.Class=="FS",1,0))%>%
  group_by(inter,`.02-0.1kHz`, `0.1-1kHz`, `1-10kHz`,
           `10-48kHz`,`0.02-48kHz`)%>%
  summarize(n.man.calls=sum(Man.Class),
            n.auto.calls=sum(Auto))%>%
  mutate(auto.man.ratio=(n.auto.calls+1)/(1+n.man.calls))

ggplot(bella.full.min)+
  geom_jitter(aes(x=n.man.calls,y=n.auto.calls,color=`0.02-48kHz`))+
  geom_abline(aes(intercept=0,slope=1))


ggplot(bella.full.min)+
  geom_jitter(aes(y=auto.man.ratio,x=`0.02-48kHz`,color=n.man.calls))+
  scale_color_viridis_c()




# make a confusion matrix
lp("caret")
prediction<-bella$full.data$Auto.Class
prediction<-ifelse(prediction=="","NN",prediction)
truth<-case_when(
  bella$full.data$Auto.Class=="FS" & bella$full.data$agree ~ "FS",
  bella$full.data$Auto.Class=="FS" & !bella$full.data$agree ~"NN",
  bella$full.data$Auto.Class=="NN" & bella$full.data$agree ~ "NN",
  bella$full.data$Auto.Class=="NN" & !bella$full.data$agree ~"FS")

xtab<-table(prediction,truth)

confusionMatrix(xtab, dnn=c("Prediction","Reference"))



###Hecate----