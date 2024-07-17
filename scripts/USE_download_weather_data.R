#script to pull in weather data
#load packages
if(!require(weathercan))devtools::install_github("ropensci/weathercan")
if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)
if(!require(readxl))install.packages("readxl");library(readxl)


# find weather stations
stations_dl()
stations_search("Bonilla")

#download weather data
#bella bella
bbwthr <- weathercan::weather_dl(station_ids = 49388,# use Bella Bella station
                                 time_disp="UTC",
                                 start = "2017-08-21", 
                                 end = "2017-10-22")

#lions bay
lbwthr <- weathercan::weather_dl(station_ids = 6817,# use howe sound - Pam rocks station
                                 time_disp="UTC",
                                 start = "2017-09-30", 
                                 end = "2017-11-30")

#Hecate
hwthr <- weathercan::weather_dl(station_ids = 8265,# Bonilla Island station
                                 time_disp="UTC",
                                 start = "2017-05-12", 
                                 end = "2017-05-21")

#organize our weather data
#bella bella
bbwthr2<-bbwthr%>%
  select(date.time=time,precip_amt,temp,wind_dir,wind_spd)%>%
  mutate()

write.csv(bbwthr2,"wdata/bellabella_weather.csv",row.names = FALSE)

#lions bay
lbwthr2<-lbwthr%>%
  select(date.time=time,precip_amt,temp,wind_dir,wind_spd)

write.csv(lbwthr2,"wdata/lionsbay_weather.csv",row.names = FALSE)

#hecate
hwthr2<-hwthr%>%
  select(date.time=time,precip_amt,temp,wind_dir,wind_spd)

write.csv(hwthr2,"wdata/hecate_weather.csv",row.names = FALSE)
