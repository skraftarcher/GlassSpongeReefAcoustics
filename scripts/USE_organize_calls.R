# script to organize data

source("scripts/install_packages_function.R")
lp("tidyverse")
lp("Rraven")
lp("lubridate")

# bring in hydrophone location info
hyd<-read_csv("odata/downloaded_2024-05-16_hydrophone_locations.csv")

# bella bella
# find empty files first
fls<-list.files("odata/bella")

bb<-read.delim(file.path("odata","bella",fls[1]),sep="\t")
for(i in 2:length(fls))bb<-rbind(bb,read.delim(file.path("odata","bella",fls[i]),sep="\t"))

# get date and time of each observation
bb2<-bb%>%
  separate(Begin.File,into = c("bg","yr","mnth","dy","hr","mns","secs"),sep=c(9,11,13,15,17,19,21))%>%
  mutate(file.start.date=ymd_hms(paste(yr,mnth,dy,hr,mns,secs)),
         cal.time=file.start.date+File.Offset..s.)%>%
  select(cal.time,Confidence)%>%
  mutate(yr=year(cal.time),
         mnth=month(cal.time),
         dy=day(cal.time),
         hr=hour(cal.time),
         mns=minute(cal.time))

# hourly summary
bb3<-bb2%>%
  group_by(yr,mnth,dy,hr)%>%
  reframe(calls=n())%>%
  distinct()

# get all hours from min to max time in bb2
bb4<-data.frame(dt=seq(min(bb2$cal.time),max(bb2$cal.time),3600))%>%
  mutate(yr=year(dt),
         mnth=month(dt),
         dy=day(dt),
         hr=hour(dt),
         md=paste(mnth,"-",dy),
         Reef="Bella Bella")%>%
  left_join(hyd)%>%
  select(-dt)%>%
  left_join(bb3)%>%
  mutate(calls=ifelse(is.na(calls),0,calls))

# visualize
ggplot(bb4)+
  geom_tile(aes(y=hr,x=md,fill = calls))
                