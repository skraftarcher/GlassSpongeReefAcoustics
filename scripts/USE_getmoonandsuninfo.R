# script to get lunar phases

if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)
if(!require(readxl))install.packages("readxl");library(readxl)
if(!require(oce))install.packages("oce");library(oce)


# first bring in call data sets

bella<-read.csv("wdata/bella_fullcalldataset.csv")%>%
  mutate(file.start.date=as.POSIXct(file.start.date,tz="America/Los_Angeles"),
         md=ymd(paste(yr,mnth,dy)))

lions<-read.csv("wdata/lionsbay_fullcalldataset.csv")%>%
  mutate(file.start.date=as.POSIXct(file.start.date,tz="America/Los_Angeles"),
         md=ymd(paste(yr,mnth,dy)))


hecate<-bind_rows(read.csv("wdata/hecate_fj3_fullcalldataset.csv"),
                  read.csv("wdata/hecate_dfo3_fullcalldataset.csv"))%>%
  mutate(file.start.date=as.POSIXct(file.start.date,tz="America/Los_Angeles"),
         md=ymd(paste(yr,mnth,dy)))

# bring in location information
sites<-read.csv("odata/downloaded_2024-05-17_hydrophone_locations.csv")

bella.moon<-data.frame(Reef="Bella Bella",
                       hrs=seq(from=min(bella$file.start.date),to=max(bella$file.start.date)+1800,by=3600))%>%
  mutate(hrs.utc=with_tz(hrs,tz="UTC"),
         longitude = -sites$Longitude[sites$Reef=="Bella Bella"],
         latitude = sites$Latitude[sites$Reef=="Bella Bella"],
         illumination = moonAngle(t=hrs.utc, 
                                  longitude=longitude,
                                  latitude=latitude)$illuminatedFraction,
         altitude.moon = moonAngle(t=hrs.utc, 
                                   longitude=longitude,
                                   latitude=latitude)$altitude,
         altitude.sun = sunAngle(t=hrs.utc, 
                                 longitude=-sites$Longitude[sites$Reef=="Bella Bella"],
                                 latitude=sites$Latitude[sites$Reef=="Bella Bella"])$altitude)
                       

lions.moon<-data.frame(Reef="Lions Bay",
                       hrs=seq(from=min(lions$file.start.date),to=max(lions$file.start.date)+1800,by=3600))%>%
  mutate(hrs.utc=with_tz(hrs,tz="UTC"),
         longitude = sites$Longitude[sites$Reef=="Lions Bay"],
         latitude = sites$Latitude[sites$Reef=="Lions Bay"],
         illumination = moonAngle(t=hrs.utc, 
                                  longitude = longitude,
                                  latitude = latitude)$illuminatedFraction,
         altitude.moon = moonAngle(t=hrs.utc, 
                                   longitude = longitude,
                                   latitude = latitude)$altitude,
         altitude.sun = sunAngle(t=hrs.utc,
                                 longitude = sites$Longitude[sites$Reef=="Lions Bay"],
                                 latitude = sites$Latitude[sites$Reef=="Lions Bay"])$altitude)

hecate.moon<-data.frame(Reef="Hecate",
                        hrs=seq(from=min(hecate$file.start.date),to=max(hecate$file.start.date)+1800,by=3600))%>%
  mutate(hrs.utc=with_tz(hrs,tz="UTC"), 
         longitude = mean(sites$Longitude[sites$Reef %in% c("Hecate On Reef","Hecate Off Reef")]),
         latitude = mean(sites$Latitude[sites$Reef %in% c("Hecate On Reef","Hecate Off Reef")]),
         illumination = moonAngle(t=hrs.utc, 
                                  longitude = longitude,
                                  latitude = latitude)$illuminatedFraction,
         altitude.moon = moonAngle(t=hrs.utc, 
                                   longitude = longitude,
                                   latitude = latitude)$altitude,
         altitude.sun = sunAngle(t=hrs.utc, 
                                 longitude = mean(sites$Longitude[sites$Reef %in% c("Hecate On Reef","Hecate Off Reef")]),
                                 latitude = mean(sites$Latitude[sites$Reef %in% c("Hecate On Reef","Hecate Off Reef")]))$altitude)


write.csv(bind_rows(bella.moon,lions.moon,hecate.moon),"wdata/moonandsun.csv",row.names = FALSE)
