# script to organize data

source("scripts/install_packages_function.R")
lp("tidyverse")
lp("Rraven")
lp("readxl")

theme_set(theme_bw()+theme(panel.grid = element_blank(),
                           panel.background = element_rect(fill="grey3"),
                           legend.position = "top"))
# bring in hydrophone location info
hyd<-read_csv("odata/downloaded_2024-05-17_hydrophone_locations.csv")
# bring in SPL data
hecspl<-read_xlsx("odata/hecatespl.xlsx")%>%
  mutate(DateTime=mdy_hm(DateTime))
bellaspl<-read_xlsx("odata/bellaspl.xlsx")
year(bellaspl$DateTime)<-2017
lionsspl<-read_xlsx("odata/lionsbayspl.xlsx")
year(lionsspl$DateTime)<-2017

# bella bella----
# find empty files first
fls<-list.files("odata/bella")

bb<-read.delim(file.path("odata","bella",fls[1]),sep="\t")
for(i in 2:length(fls))bb<-rbind(bb,read.delim(file.path("odata","bella",fls[i]),sep="\t"))

# get date and time of each observation
bb2<-bb%>%
  separate(Begin.File,
           into = c("bg","yr","mnth","dy","hr","mns","secs"),
           sep=c(9,11,13,15,17,19,21),
           remove = FALSE)%>%
  mutate(file.start.date=ymd_hms(paste(yr,mnth,dy,hr,mns,secs)),
         cal.time=file.start.date+File.Offset..s.)%>%
  select(cal.time,Begin.File,File.Offset..s.,file.start.date,Confidence)%>%
  mutate(yr=year(cal.time),
         mnth=month(cal.time),
         dy=day(cal.time),
         hr=hour(cal.time),
         mns=minute(cal.time))%>%
  filter(file.start.date >=min(bellaspl$DateTime))

# save this
write.csv(bb2,"wdata/bella_fullcalldataset.csv",row.names = FALSE)

# hourly summary
bb3<-bb2%>%
  group_by(yr,mnth,dy,hr)%>%
  reframe(calls=n())%>%
  distinct()

# get all hours from min to max time in bb2
bb4<-data.frame(dt=seq(min(bb2$file.start.date),max(bb2$file.start.date)+1800,3600))%>%
  mutate(yr=year(dt),
         mnth=month(dt),
         dy=day(dt),
         hr=hour(dt),
         md=ymd(paste(yr,mnth,dy)),
         Reef="Bella Bella")%>%
  left_join(hyd)%>%
  select(-dt)%>%
  left_join(bb3)%>%
  mutate(calls=ifelse(is.na(calls),0,calls))

# visualize
ggplot(bb4)+
  geom_tile(aes(y=hr,x=md,fill = calls))+
  scale_y_reverse()+
  scale_fill_viridis_c(option="B",name="Calls per hour")+
  ylab("Hour of the day")+
  xlab("")+
  labs(title="Bella Bella")

ggsave("figures/bellabella_fullcalldataset.png", width=7,height=5)

# lions bay     
fls<-list.files("odata/lions")

lb<-read.delim(file.path("odata","lions",fls[1]),sep="\t")
for(i in 2:length(fls))lb<-rbind(lb,read.delim(file.path("odata","lions",fls[i]),sep="\t"))

# get date and time of each observation
lb2<-lb%>%
  separate(Begin.File,
           into = c("bg","yr","mnth","dy","hr","mns","secs"),
           sep=c(9,11,13,15,17,19,21),
           remove = FALSE)%>%
  mutate(file.start.date=ymd_hms(paste(yr,mnth,dy,hr,mns,secs)),
         cal.time=file.start.date+File.Offset..s.)%>%
  select(cal.time,Begin.File,File.Offset..s.,file.start.date,Confidence)%>%
  mutate(yr=year(cal.time),
         mnth=month(cal.time),
         dy=day(cal.time),
         hr=hour(cal.time),
         mns=minute(cal.time))%>%
  filter(file.start.date >=min(lionsspl$DateTime))

write.csv(lb2,"wdata/lionsbay_fullcalldataset.csv",row.names=FALSE)

# hourly summary
lb3<-lb2%>%
  group_by(yr,mnth,dy,hr)%>%
  reframe(calls=n())%>%
  distinct()

# get all hours from min to max time in lb2
lb4<-data.frame(dt=seq(min(lb2$file.start.date),max(lb2$file.start.date)+1800,3600))%>%
  mutate(yr=year(dt),
         mnth=month(dt),
         dy=day(dt),
         hr=hour(dt),
         md=ymd(paste(yr,mnth,dy)),
         ID="LionsBay")%>%
  left_join(hyd)%>%
  select(-dt)%>%
  left_join(lb3)%>%
  mutate(calls=ifelse(is.na(calls),0,calls))

# visualize
ggplot(lb4)+
  geom_tile(aes(y=hr,x=md,fill = calls))+
  scale_y_reverse()+
  scale_fill_viridis_c(option="B",name="Calls per hour")+
  ylab("Hour of the day")+
  xlab("")+
  labs(title="Lions Bay")

ggsave("figures/lionsbay_fulldataset.png", width=7,height=5)


# Hecate     
fls<-list.files("odata/hec_dfo1")

hd1<-read.delim(file.path("odata","hec_dfo1",fls[1]),sep="\t")
for(i in 2:length(fls))hd1<-rbind(hd1,read.delim(file.path("odata","hec_dfo1",fls[i]),sep="\t"))

# get date and time of each observation
dfo1<-hecspl%>%
  filter(Recorder=="DFO1")
dfo2<-hecspl%>%
  filter(Recorder=="DFO2")
dfo3<-hecspl%>%
  filter(Recorder=="DFO3")
dfo4<-hecspl%>%
  filter(Recorder=="DFO4")
dfo5<-hecspl%>%
  filter(Recorder=="DFO5")
fj1<-hecspl%>%
  filter(Recorder=="FJ1")
fj3<-hecspl%>%
  filter(Recorder=="FJ3")

hd12<-hd1%>%
  separate(Begin.File,
           into = c("bg","yr","mnth","dy","hr","mns","secs"),
           sep=c(9,11,13,15,17,19,21),
           remove=FALSE)%>%
  mutate(file.start.date=ymd_hms(paste(yr,mnth,dy,hr,mns,secs)),
         cal.time=file.start.date+File.Offset..s.)%>%
  select(cal.time,Begin.File,File.Offset..s.,file.start.date,Confidence)%>%
  mutate(yr=year(cal.time),
         mnth=month(cal.time),
         dy=day(cal.time),
         hr=hour(cal.time),
         mns=minute(cal.time))%>%
  filter(file.start.date>min(dfo1$DateTime))

write.csv(hd12,"wdata/hecate_dfo1_fullcalldataset.csv",row.names = FALSE)

# hourly summary
hd13<-hd12%>%
  group_by(yr,mnth,dy,hr)%>%
  reframe(calls=n())%>%
  distinct()

# get all hours from min to max time in hd12
hd14<-data.frame(dt=seq(min(hd12$file.start.date),max(hd12$file.start.date)+1800,3600))%>%
  mutate(yr=year(dt),
         mnth=month(dt),
         dy=day(dt),
         hr=hour(dt),
         md=ymd(paste(yr,mnth,dy)),
         ID="DFO1")%>%
  left_join(hyd)%>%
  select(-dt)%>%
  left_join(hd13)%>%
  mutate(calls=ifelse(is.na(calls),0,calls))

# visualize
ggplot(hd14)+
  geom_tile(aes(y=hr,x=md,fill = calls))+
  scale_y_reverse()+
  scale_fill_viridis_c(option="B",name="Calls per hour")+
  ylab("Hour of the day")+
  xlab("")+
  labs(title=unique(hd14$Reef))

ggsave("figures/hecate_offreef1_fulldataset.png", width=3,height=5)


fls<-list.files("odata/hec_dfo2")

hd2<-read.delim(file.path("odata","hec_dfo2",fls[1]),sep="\t")
for(i in 2:length(fls))hd2<-rbind(hd2,read.delim(file.path("odata","hec_dfo2",fls[i]),sep="\t"))

# get date and time of each observation
hd22<-hd2%>%
  separate(Begin.File,
           into = c("bg","yr","mnth","dy","hr","mns","secs"),
           sep=c(9,11,13,15,17,19,21),
           remove=FALSE)%>%
  mutate(file.start.date=ymd_hms(paste(yr,mnth,dy,hr,mns,secs)),
         cal.time=file.start.date+File.Offset..s.)%>%
  select(cal.time,Begin.File,File.Offset..s.,file.start.date,Confidence)%>%
  mutate(yr=year(cal.time),
         mnth=month(cal.time),
         dy=day(cal.time),
         hr=hour(cal.time),
         mns=minute(cal.time))%>%
  filter(file.start.date>min(dfo2$DateTime))

write.csv(hd22,"wdata/hecate_dfo2_fullcalldataset.csv",row.names = FALSE)

# hourly summary
hd23<-hd22%>%
  group_by(yr,mnth,dy,hr)%>%
  reframe(calls=n())%>%
  distinct()

# get all hours from min to max time in hd22
hd24<-data.frame(dt=seq(min(hd22$file.start.date),max(hd22$file.start.date)+1800,3600))%>%
  mutate(yr=year(dt),
         mnth=month(dt),
         dy=day(dt),
         hr=hour(dt),
         md=ymd(paste(yr,mnth,dy)),
         ID="DFO2")%>%
  left_join(hyd)%>%
  select(-dt)%>%
  left_join(hd23)%>%
  mutate(calls=ifelse(is.na(calls),0,calls))

# visualize
ggplot(hd24)+
  geom_tile(aes(y=hr,x=md,fill = calls))+
  scale_y_reverse()+
  scale_fill_viridis_c(option="B",name="Calls per hour")+
  ylab("Hour of the day")+
  xlab("")+
  labs(title=unique(hd24$Reef))

ggsave("figures/hecate_onreef1_fulldataset.png", width=3,height=5)


fls<-list.files("odata/hec_dfo3")

hd3<-read.delim(file.path("odata","hec_dfo3",fls[1]),sep="\t")
for(i in 2:length(fls))hd3<-rbind(hd3,read.delim(file.path("odata","hec_dfo3",fls[i]),sep="\t"))

# get date and time of each observation
hd32<-hd3%>%
  separate(Begin.File,
           into = c("bg","yr","mnth","dy","hr","mns","secs"),
           sep=c(9,11,13,15,17,19,21),
           remove=FALSE)%>%
  mutate(file.start.date=ymd_hms(paste(yr,mnth,dy,hr,mns,secs)),
         cal.time=file.start.date+File.Offset..s.)%>%
  select(cal.time,Begin.File,File.Offset..s.,file.start.date,Confidence)%>%
  mutate(yr=year(cal.time),
         mnth=month(cal.time),
         dy=day(cal.time),
         hr=hour(cal.time),
         mns=minute(cal.time))%>%
  filter(file.start.date>min(dfo3$DateTime))

write.csv(hd32,"wdata/hecate_dfo3_fullcalldataset.csv",row.names = FALSE)

# hourly summary
hd33<-hd32%>%
  group_by(yr,mnth,dy,hr)%>%
  reframe(calls=n())%>%
  distinct()

# get all hours from min to max time in hd32
hd34<-data.frame(dt=seq(min(hd32$file.start.date),max(hd32$file.start.date)+1800,3600))%>%
  mutate(yr=year(dt),
         mnth=month(dt),
         dy=day(dt),
         hr=hour(dt),
         md=ymd(paste(yr,mnth,dy)),
         ID="DFO3")%>%
  left_join(hyd)%>%
  select(-dt)%>%
  left_join(hd33)%>%
  mutate(calls=ifelse(is.na(calls),0,calls))

# visualize
ggplot(hd34)+
  geom_tile(aes(y=hr,x=md,fill = calls))+
  scale_y_reverse()+
  scale_fill_viridis_c(option="B",name="Calls per hour")+
  ylab("Hour of the day")+
  xlab("")+
  labs(title=unique(hd34$Reef))

ggsave("figures/hecate_offreef2_fulldataset.png", width=3,height=5)


fls<-list.files("odata/hec_dfo4")

hd4<-read.delim(file.path("odata","hec_dfo4",fls[1]),sep="\t")
for(i in 2:length(fls))hd4<-rbind(hd4,read.delim(file.path("odata","hec_dfo4",fls[i]),sep="\t"))

# get date and time of each observation
hd42<-hd4%>%
  separate(Begin.File,
           into = c("bg","yr","mnth","dy","hr","mns","secs"),
           sep=c(10,12,14,16,18,20,22),
           remove=FALSE)%>%
  mutate(file.start.date=ymd_hms(paste(yr,mnth,dy,hr,mns,secs)),
         cal.time=file.start.date+File.Offset..s.)%>%
  select(cal.time,Begin.File,File.Offset..s.,file.start.date,Confidence)%>%
  mutate(yr=year(cal.time),
         mnth=month(cal.time),
         dy=day(cal.time),
         hr=hour(cal.time),
         mns=minute(cal.time))%>%
  filter(file.start.date>min(dfo4$DateTime))

write.csv(hd42,"wdata/hecate_dfo4_fullcalldataset.csv",row.names = FALSE)

# hourly summary
hd43<-hd42%>%
  group_by(yr,mnth,dy,hr)%>%
  reframe(calls=n())%>%
  distinct()

# get all hours from min to max time in hd42
hd44<-data.frame(dt=seq(min(hd42$file.start.date),max(hd42$file.start.date)+1800,3600))%>%
  mutate(yr=year(dt),
         mnth=month(dt),
         dy=day(dt),
         hr=hour(dt),
         md=ymd(paste(yr,mnth,dy)),
         ID="DFO4")%>%
  left_join(hyd)%>%
  select(-dt)%>%
  left_join(hd43)%>%
  mutate(calls=ifelse(is.na(calls),0,calls))

# visualize
ggplot(hd44)+
  geom_tile(aes(y=hr,x=md,fill = calls))+
  scale_y_reverse()+
  scale_fill_viridis_c(option="B",name="Calls per hour")+
  ylab("Hour of the day")+
  xlab("")+
  labs(title=unique(hd44$Reef))

ggsave("figures/hecate_onreef2_fulldataset.png", width=3,height=5)

fls<-list.files("odata/hec_dfo5")

hd5<-read.delim(file.path("odata","hec_dfo5",fls[1]),sep="\t")
for(i in 2:length(fls))hd5<-rbind(hd5,read.delim(file.path("odata","hec_dfo5",fls[i]),sep="\t"))

# get date and time of each observation
hd52<-hd5%>%
  separate(Begin.File,
           into = c("bg","yr","mnth","dy","hr","mns","secs"),
           sep=c(9,11,13,15,17,19,21),
           remove=FALSE)%>%
  mutate(file.start.date=ymd_hms(paste(yr,mnth,dy,hr,mns,secs)),
         cal.time=file.start.date+File.Offset..s.)%>%
  select(cal.time,Begin.File,File.Offset..s.,file.start.date,Confidence)%>%
  mutate(yr=year(cal.time),
         mnth=month(cal.time),
         dy=day(cal.time),
         hr=hour(cal.time),
         mns=minute(cal.time))%>%
  filter(file.start.date>min(dfo5$DateTime))

write.csv(hd52,"wdata/hecate_dfo5_fullcalldataset.csv",row.names = FALSE)

# hourly summary
hd53<-hd52%>%
  group_by(yr,mnth,dy,hr)%>%
  reframe(calls=n())%>%
  distinct()

# get all hours from min to max time in hd52
hd54<-data.frame(dt=seq(min(hd52$file.start.date),max(hd52$file.start.date)+1800,3600))%>%
  mutate(yr=year(dt),
         mnth=month(dt),
         dy=day(dt),
         hr=hour(dt),
         md=ymd(paste(yr,mnth,dy)),
         ID="DFO5")%>%
  left_join(hyd)%>%
  select(-dt)%>%
  left_join(hd53)%>%
  mutate(calls=ifelse(is.na(calls),0,calls))

# visualize
ggplot(hd54)+
  geom_tile(aes(y=hr,x=md,fill = calls))+
  scale_y_reverse()+
  scale_fill_viridis_c(option="B",name="Calls per hour")+
  ylab("Hour of the day")+
  xlab("")+
  labs(title=unique(hd54$Reef))

ggsave("figures/hecate_onreef3_fulldataset.png", width=3,height=5)


fls<-list.files("odata/hec_fj1")

hd6<-read.delim(file.path("odata","hec_fj1",fls[1]),sep="\t")
for(i in 2:length(fls))hd6<-rbind(hd6,read.delim(file.path("odata","hec_fj1",fls[i]),sep="\t"))

# get date and time of each observation
hd62<-hd6%>%
  separate(Begin.File,
           into = c("bg","yr","mnth","dy","hr","mns","secs"),
           sep=c(11,13,15,17,19,21,23),
           remove=FALSE)%>%
  mutate(file.start.date=ymd_hms(paste(yr,mnth,dy,hr,mns,secs)),
         cal.time=file.start.date+File.Offset..s.)%>%
  select(cal.time,Begin.File,File.Offset..s.,file.start.date,Confidence)%>%
  mutate(yr=year(cal.time),
         mnth=month(cal.time),
         dy=day(cal.time),
         hr=hour(cal.time),
         mns=minute(cal.time))%>%
  filter(file.start.date>min(fj1$DateTime))

write.csv(hd62,"wdata/hecate_fj1_fullcalldataset.csv",row.names = FALSE)

# hourly summary
hd63<-hd62%>%
  group_by(yr,mnth,dy,hr)%>%
  reframe(calls=n())%>%
  distinct()

# get all hours from min to max time in hd62
hd64<-data.frame(dt=seq(min(hd62$file.start.date),max(hd62$file.start.date)+1800,3600))%>%
  mutate(yr=year(dt),
         mnth=month(dt),
         dy=day(dt),
         hr=hour(dt),
         md=ymd(paste(yr,mnth,dy)),
         ID="FJ1")%>%
  left_join(hyd)%>%
  select(-dt)%>%
  left_join(hd63)%>%
  mutate(calls=ifelse(is.na(calls),0,calls))

# visualize
ggplot(hd64)+
  geom_tile(aes(y=hr,x=md,fill = calls))+
  scale_y_reverse()+
  scale_fill_viridis_c(option="B",name="Calls per hour")+
  ylab("Hour of the day")+
  xlab("")+
  labs(title=unique(hd64$Reef))

ggsave("figures/hecate_onreef4_fulldataset.png", width=3,height=5)


fls<-list.files("odata/hec_fj3")

hd7<-read.delim(file.path("odata","hec_fj3",fls[1]),sep="\t")
for(i in 2:length(fls))hd7<-rbind(hd7,read.delim(file.path("odata","hec_fj3",fls[i]),sep="\t"))

# get date and time of each observation
hd72<-hd7%>%
  separate(Begin.File,
           into = c("bg","yr","mnth","dy","hr","mns","secs"),
           sep=c(11,13,15,17,19,21,23),
           remove=FALSE)%>%
  mutate(file.start.date=ymd_hms(paste(yr,mnth,dy,hr,mns,secs)),
         cal.time=file.start.date+File.Offset..s.)%>%
  select(cal.time,Begin.File,File.Offset..s.,file.start.date,Confidence)%>%
  mutate(yr=year(cal.time),
         mnth=month(cal.time),
         dy=day(cal.time),
         hr=hour(cal.time),
         mns=minute(cal.time))%>%
  filter(file.start.date>min(fj3$DateTime))

write.csv(hd72,"wdata/hecate_fj3_fullcalldataset.csv",row.names = FALSE)

# hourly summary
hd73<-hd72%>%
  group_by(yr,mnth,dy,hr)%>%
  reframe(calls=n())%>%
  distinct()

# get all hours from min to max time in hd72
hd74<-data.frame(dt=seq(min(hd72$file.start.date),max(hd72$file.start.date)+1800,3600))%>%
  mutate(yr=year(dt),
         mnth=month(dt),
         dy=day(dt),
         hr=hour(dt),
         md=ymd(paste(yr,mnth,dy)),
         ID="FJ3")%>%
  left_join(hyd)%>%
  select(-dt)%>%
  left_join(hd73)%>%
  mutate(calls=ifelse(is.na(calls),0,calls))

# visualize
ggplot(hd74)+
  geom_tile(aes(y=hr,x=md,fill = calls))+
  scale_y_reverse()+
  scale_fill_viridis_c(option="B",name="Calls per hour")+
  ylab("Hour of the day")+
  xlab("")+
  labs(title=unique(hd74$Reef))

ggsave("figures/hecate_onreef5_fulldataset.png", width=3,height=5)

# make a boxplot figure

all.reefs<-bind_rows(bb4,lb4,hd14,hd24,hd34,hd44,hd54,hd64,hd74)%>%
  mutate(Reef2=case_when(
    Reef=="Bella Bella"~"Bella Bella",
    Reef=="Lions Bay"~"Lions Bay",
    !Reef %in% c("Bella Bella","Lions Bay")~"Hecate"))
all.reefs$hr<-as.factor(all.reefs$hr)

write.csv(all.reefs,"wdata/allreefs_hourlysummary.csv",row.names = FALSE)


ggplot(data=all.reefs)+
  geom_boxplot(aes(y=calls,x=hr,fill=Reef))+
  theme(panel.background = element_rect(fill="white"))+
  facet_wrap(vars(Reef),ncol=1,scales="free")+
  ylab("Calls per hour")+
  scale_fill_viridis_d()

