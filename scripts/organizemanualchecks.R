# Script to organize manual detection checks
# Glass sponge reef acoustics project 

#Glass sponge reef working group 4/27/2022

man.check.organize<-function(fname){ # this begins the function that will bring in the file we need and organize it properly
# load packages
source("scripts/install_packages_function.R")

lp("tidyverse")
lp("lubridate")
lp("Rraven")

#we may need to adjust this as we go, but this is the list of what I think
# fish calls may have been called in all the datasets
man.calls<-c("F","f","FS","fs")
call.types.k<-c("K","K  ")
call.types.g<-c("G","G ","G  ")

#bring in the dataset
md<-imp_raven(path="odata",
              file=fname,
              all.data=TRUE)

#start to organize
md2<-md%>%
  separate(`Begin File`,
           # starting from right and using negatives to allow stid lengths to differ
           into=c("stid","yr","mnth","d","hr","min","sec","ext"),
           sep = c(-16,-14,-12,-10,-8,-6,-4))%>%
  select(-ext)%>%
  mutate(FileDate=ymd_hms(paste(yr,mnth,d,hr,min,sec)),# this is the date-time the file started recording
         CallDate=FileDate+`File Offset (s)`,# this is the date-time the fish call actually occurred
         # the following replaces the year,month day, min, sec variables with the fish call specific values
         yr=year(CallDate),
         mnth=month(CallDate),
         d=day(CallDate),
         hr=hour(CallDate),
         min=minute(CallDate),
         sec=second(CallDate),
         # if the automatic detector called it a fish call give it a 1 otherwise 
         # give it a 0
         auto.class=ifelse(Class=="FS",1,0),
         # same but with manual class. We had to do it a bit differently because 
         # there are multiple things the manual fish calls may have been called (that list above)
         # so what this says is if the manual assignment value is in that list of 
         # things that may be a fish call, then it gets a 1 otherwise it gets a 0
         man.class=ifelse(`Fish Call` %in% man.calls,1,0),
         # this creates a variable that is true if the autodetector and the manual
         # detector called it the same thing and false if they didn't
         agree=ifelse(auto.class==man.class,TRUE,FALSE),
         `Call Type`=ifelse(`Call Type` %in% call.types.k,"K",`Call Type`),
         `Call Type`=ifelse(`Call Type` %in% call.types.g,"G",`Call Type`))
#think through all the combinations of things that would indicate
# data entry error on the part of the reviewer

#this is where reviewer called it a fish but assigned a call type of noise
mdp1<-filter(md2,auto.class==0)%>%
  filter(agree==FALSE)%>%
  filter(`Call Type`=="N")

# This is where reviewer called it noise but assigned it to grunt or knock
mdp2<-filter(md2,auto.class==0)%>%
  filter(agree==TRUE)%>%
  filter(`Call Type` %in% c("K","G"))

# This is where reviewer called it noise but assigned it to grunt or knock
mdp3<-filter(md2,auto.class==1)%>%
  filter(agree==FALSE)%>%
  filter(`Call Type` %in% c("K","G"))

#this is where reviewer called it a fish but assigned a call type of noise
mdp4<-filter(md2,auto.class==1)%>%
  filter(agree==TRUE)%>%
  filter(`Call Type`=="N")

# now we can remove problematic rows
md2<-anti_join(md2,mdp1)%>%
  anti_join(mdp2)%>%
  anti_join(mdp3)%>%
  anti_join(mdp4)

#creating a streamlined data set for us to look at later
man.indcall<-md2%>%
  #this function indicates which columns you want to keep
  # if you only want to get rid of a few columns you can use this function too, 
  # you just list the columns you want to get rid of with a - in front
  select(Auto.Class=Class,#when you put a new variable name = old variable name in this function it renames the variable
         Man.Class=man.class,
         agree,
         CallDate,
         Call.type=`Call Type`,
         delta.time=`Delta Time (s)`,
         low.freq=`Low Freq (Hz)`,
         high.freq=`High Freq (Hz)`,
         Confidence)

# create a data set that lets us look at the detectors confidence in its calls when it got it
# right and when it got it wrong
man.sum.indcall<-man.indcall%>%
  group_by(Auto.Class,agree,Call.type)%>%
  summarize(n.calls=n(),mean.confidence=mean(Confidence,na.rm=TRUE),
            sd.confidence=sd(Confidence,na.rm=TRUE),
            min.confidence=min(Confidence,na.rm=TRUE))%>%
  arrange(agree)

#creates a table that quickly summarizes when and for what call types the detector performed
# well (or not)
man.quicklook<-man.indcall%>%
  group_by(Auto.Class,agree)%>%
  summarize(n.calls=n())%>%
  arrange(agree)


# creates the streamlined dataset we created for the larger data set,
# but with the total number of automatically detected calls and with 
# the total number of manually detected calls
by.min<-md2%>%
  group_by(yr,mnth,d,hr,min)%>%
  # get total number of fish calls
  summarize(n.auto.fishcall=sum(auto.class),
            n.man.fishcall=sum(man.class))

#this says we want to return everything we created in the form of a list
# I am also having it return the unique values for the manual call variable in case
# we need to amend the function to include more possible values for fish sounds
return(list(fish.call.entry.check=unique(md$`Fish Call`),quick.look=man.quicklook,individual.call.summary=man.sum.indcall,
            by.min.summary=by.min,full.data=man.indcall))
}
