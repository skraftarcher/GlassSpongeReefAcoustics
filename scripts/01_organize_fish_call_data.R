# Script to organize fish call data
# Glass sponge reef acoustics project 

#Glass sponge reef working group 4/1/2022

# load packages
source("scripts/install_packages_function.R")

lp("tidyverse")
lp("lubridate")
lp("Rraven")

# now bring in files

lb<-imp_raven("odata/lb_detector",
              all.data=TRUE)

# glimpse data

glimpse(lb)

table(lb$Class)

# organizing by minute

fishcall.bymin.auto<-function(seltable){ # this is a function that can be used to 
  # summarize all automatic detector datasets to get the number of fish calls by minute
  
  lb2<-seltable%>% 
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
         sec=second(CallDate))

# now summarize by minute to compare to manual detections

lb.min<-lb2%>%
  #create a variable that is 1 if its a fish call and 0 if it was classified as noise
  mutate(fishcall=ifelse(Class=="FS",1,0))%>%
  # group by unique minutes
  group_by(yr,mnth,d,hr,min)%>%
  # get total number of fish calls
  summarize(nfishcall=sum(fishcall))
  
# this is the data you want the funtion to return
return(lb.min)
} # this closes the function

# this is how you call the function

function.check<-fishcall.bymin.auto(seltable=lb)






