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
         mns=minute(cal.time),
         Reef="Bella Bella")%>%
  left_join(hyd)
