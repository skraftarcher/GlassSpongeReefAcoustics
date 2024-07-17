# put all the data sets together
# load packages
source("scripts/install_packages_function.R")
lp("tidyverse")
lp("lubridate")

sites<-read.csv("odata/downloaded_2024-05-17_hydrophone_locations.csv")
sunmoon<-read.csv("wdata/moonandsun.csv")%>%
  mutate(hrs=ymd_hms(hrs),
         yr=year(hrs),
         mnth=month(hrs),
         dy=day(hrs),
         hr=hour(hrs))%>%
  select(-hrs,-hrs.utc)%>%
  distinct(Reef,yr,mnth,dy,hr,.keep_all=TRUE)


#the spl data set for each reef is organized slightly differently so organize each on its own
bella.calls<-read.csv("wdata/bella_fullcalldataset.csv")
bella.spl<-read_xlsx("odata/downloaded_2024-05-17_Bella Bella_SPL_60sec.xlsx")%>%
  mutate(yr=year(Date),
         mnth=month(Date),
         dy=day(Date),
         hr=hour(Time),
         mns=minute(Time))%>%
  select(yr,mnth,dy,hr,mns,
         broad.spl=`0.02-48kHz`,#20Hz-48kHz
         vlow.spl=`.02-0.1kHz`,#20Hz-100Hz
         low.spl=`0.1-1kHz`,#100Hz-1kHz
         mid.spl=`1-10kHz`,#10-10kHz
         high.spl=`10-48kHz`)#10-48kHz
  
bella.weather<-read.csv("wdata/bellabella_weather.csv")%>%
  separate(date.time,into=c("date","time"),sep=" ")%>%
  mutate(time=ifelse(is.na(time),"00:00:00",time))%>%
  separate(date,into=c("yr","mnth","dy"),sep="-",convert = TRUE)%>%
  separate(time,into=c("hr","mns","secs"),sep=":",convert = TRUE)%>%
  select(-mns,-secs)

bella<-left_join(bella.calls,bella.spl)%>%
  left_join(bella.weather)%>%
  left_join(sunmoon[sunmoon$Reef=="Bella Bella",])

lions.calls<-read.csv("wdata/lionsbay_fullcalldataset.csv")

lions.spl<-read_xlsx("odata/downloaded_2024-05-17_Lion's Bay_SPL_60sec.xlsx")%>%
  mutate(yr=year(Date),
         mnth=month(Date),
         dy=day(Date),
         hr=hour(Time),
         mns=minute(Time))%>%
  select(yr,mnth,dy,hr,mns,
         broad.spl=`20Hz-48kHz`,#20Hz-48kHz
         vlow.spl=`20-100Hz`,#20Hz-100Hz
         low.spl=`100-1000Hz`,#100Hz-1kHz
         mid.spl=`1-10kHz`,#10-10kHz
         high.spl=`10-48kHz`)#10-48kHz

lions.weather<-read.csv("wdata/lionsbay_weather.csv")%>%
  separate(date.time,into=c("date","time"),sep=" ")%>%
  mutate(time=ifelse(is.na(time),"00:00:00",time))%>%
  separate(date,into=c("yr","mnth","dy"),sep="-",convert = TRUE)%>%
  separate(time,into=c("hr","mns","secs"),sep=":",convert = TRUE)%>%
  select(-mns,-secs)

lions<-left_join(lions.calls,lions.spl)%>%
  left_join(lions.weather)%>%
  left_join(sunmoon[sunmoon$Reef=="Lions Bay",])


hecate.spl<-read_xlsx("odata/downloaded_2024-05-17_Hecate SPL Comparison.xlsx")%>%
  select(Recorder,
         yr=Year,
         mnth=Month,
         dy=Day,
         hr=Hour,
         mns=Minute,
         broad.spl=SPLFull,
         vlow.spl=SPL20,
         low.spl=SPL100,
         mid.spl=SPL1000,
         high.spl=SPL10000)
