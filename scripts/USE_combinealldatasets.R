# put all the data sets together
# load packages
source("scripts/install_packages_function.R")
lp("tidyverse")
lp("lubridate")
lp("stringr")
lp("readxl")

sunmoon<-read.csv("wdata/moonandsun.csv")%>%
  mutate(hrs=ymd_hms(hrs),
         yr=year(hrs),
         mnth=month(hrs),
         dy=day(hrs),
         hr=hour(hrs))%>%
  select(-hrs,-hrs.utc)%>%
  distinct(Reef,yr,mnth,dy,hr,.keep_all=TRUE)


#the spl data set for each reef is organized slightly differently so organize each on its own
# organize call dataset per minute
bella.calls<-read.csv("wdata/bella_fullcalldataset.csv")%>%
  group_by(yr,mnth,dy,hr,mns)%>%
  summarize(calls=n())

bella.spl<-read_xlsx("odata/bellaspl.xlsx")%>%
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

bella.tides<-bind_rows(read.csv("odata/bella_tide_data1.csv"),read.csv("odata/bella_tide_data2.csv"))%>%
  mutate(date=mdy_hm(Obs_date),
         yr=year(date),
         mnth=month(date),
         dy=day(date),
         hr=hour(date),
         mns=minute(date))%>%
  select(-Obs_date,-date)

bella.boat<-read_xlsx("odata/PAMlabResults_BellaBella__20170821_20171022__AOutput.xlsx")%>%
  select(Time,ship='shipping tonals')%>%
  mutate(yr=year(Time),
         mnth=month(Time),
         dy=day(Time),
         hr=hour(Time),
         mns=minute(Time),
         ship=ifelse(ship==0,0,1))%>%
  select(-Time)

bella<-left_join(bella.spl,bella.calls)%>%
  left_join(bella.weather)%>%
  left_join(sunmoon[sunmoon$Reef=="Bella Bella",])%>%
  left_join(bella.tides)%>%
  left_join(bella.boat)%>%
  mutate(calls=ifelse(is.na(calls),0,calls),
         ID="BellaBella")

lions.calls<-read.csv("wdata/lionsbay_fullcalldataset.csv")%>%
  group_by(yr,mnth,dy,hr,mns)%>%
  summarize(calls=n())

lions.spl<-read_xlsx("odata/lionsbayspl.xlsx")%>%
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

lions.tides<-bind_rows(read.csv("odata/lions_tide_data1.csv"),read.csv("odata/lions_tide_data2.csv"))%>%
  mutate(date=mdy_hm(Obs_date),
         yr=year(date),
         mnth=month(date),
         dy=day(date),
         hr=hour(date),
         mns=minute(date))%>%
  select(-Obs_date,-date)

lions.boat<-read_xlsx("odata/PAMlabResults_LionsBay__20170930_20171130__AOutput 1(in).xlsx")%>%
  select(Time,ship='shipping tonals')%>%
  mutate(yr=year(Time),
         mnth=month(Time),
         dy=day(Time),
         hr=hour(Time),
         mns=minute(Time),
         ship=ifelse(ship==0,0,1))%>%
  select(-Time)

lions<-left_join(lions.spl,lions.calls)%>%
  left_join(lions.weather)%>%
  left_join(sunmoon[sunmoon$Reef=="Lions Bay",])%>%
  left_join(lions.tides)%>%
  left_join(lions.boat)%>%
  mutate(calls=ifelse(is.na(calls),0,calls),
         ID="LionsBay")


hecate.spl<-read_xlsx("odata/hecatespl.xlsx")%>%
  select(Recorder,
         ship=PresShipNoise,
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

# hecate has several recorders - put them all in 

fls<-list.files("wdata",pattern="fullcalldataset")
fls<-fls[grep(fls,pattern="hecate")]

hecate.calls<-data.frame()
for(i in 1:length(fls)){
  t1<-read.csv(file.path("wdata",fls[i]))
  t2<-toupper(str_split_i(fls[i],pattern="_",i=2))
  t1$Recorder<-t2
  hecate.calls<-bind_rows(hecate.calls,t1)
}

hecate.calls<-hecate.calls%>%
  group_by(Recorder,yr,mnth,dy,hr,mns)%>%
  summarize(calls=n())

hecate.weather<-read.csv("wdata/hecate_weather.csv")%>%
  separate(date.time,into=c("date","time"),sep=" ")%>%
  mutate(time=ifelse(is.na(time),"00:00:00",time))%>%
  separate(date,into=c("yr","mnth","dy"),sep="-",convert = TRUE)%>%
  separate(time,into=c("hr","mns","secs"),sep=":",convert = TRUE)%>%
  select(-mns,-secs)

hecate.tides<-read.csv("odata/hecate_tide_data.csv")%>%
  mutate(date=mdy_hm(Obs_date),
         yr=year(date),
         mnth=month(date),
         dy=day(date),
         hr=hour(date),
         mns=minute(date))%>%
  select(-Obs_date,-date)

hecate<-left_join(hecate.spl,hecate.calls)%>%
  left_join(hecate.weather)%>%
  left_join(sunmoon[sunmoon$Reef=="Hecate",])%>%
  left_join(hecate.tides)%>%
  mutate(calls=ifelse(is.na(calls),0,calls))%>%
  rename(ID=Recorder)


# output these datasets
write.csv(bella,"wdata/bella_alldata_perminute.csv",row.names = FALSE)
write.csv(lions,"wdata/lionsbay_alldata_perminute.csv",row.names = FALSE)
write.csv(hecate,"wdata/hecate_alldata_perminute.csv",row.names = FALSE)
write.csv(bind_rows(bella,lions,hecate),"wdata/allreefs_alldata_perminute.csv",row.names = FALSE)
