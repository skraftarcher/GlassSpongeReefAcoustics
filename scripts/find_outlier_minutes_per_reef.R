# pull out outlier files to examine


source("scripts/install_packages_function.R")
lp("tidyverse")

# bring in hydrophone location info
hyd<-read_csv("odata/downloaded_2024-05-17_hydrophone_locations.csv")

# bella bella
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
  select(Begin.File,cal.time,file.start.date,Confidence)%>%
  mutate(cal.time=cut(cal.time,breaks = "1 mins"))%>%
  group_by(Begin.File,cal.time)%>%
  mutate(ncalls=n())

iqr15=quantile(bb2$ncalls,probs=0.75)+(1.5*(quantile(bb2$ncalls,probs=0.75)-quantile(bb2$ncalls,probs=0.25)))
  
bb3<-bb2%>%
  filter(ncalls>iqr15)%>%
  select(Begin.File,
         file.start.date,
         cal.time,
         ncalls)%>%
  distinct()%>%
  mutate(intofile=ymd_hms(as.character(cal.time))-file.start.date)%>%
  arrange(Begin.File)

write.csv(bb3,"wdata/bb_outliers_examine.csv",row.names = FALSE)

# lions bay
# find empty files first
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
  select(Begin.File,cal.time,file.start.date,Confidence)%>%
  mutate(cal.time=cut(cal.time,breaks = "1 mins"))%>%
  group_by(Begin.File,cal.time)%>%
  mutate(ncalls=n())

iqr15=quantile(lb2$ncalls,probs=0.75)+(1.5*(quantile(lb2$ncalls,probs=0.75)-quantile(lb2$ncalls,probs=0.25)))

lb3<-lb2%>%
  filter(ncalls>iqr15)%>%
  select(Begin.File,
         file.start.date,
         cal.time,
         ncalls)%>%
  distinct()%>%
  mutate(intofile=ymd_hms(as.character(cal.time))-file.start.date)%>%
  arrange(Begin.File)

write.csv(lb3,"wdata/lb_outliers_examine.csv",row.names = FALSE)


# Hecate     
fls<-list.files("odata/hec_dfo1")

hd1<-read.delim(file.path("odata","hec_dfo1",fls[1]),sep="\t")
for(i in 2:length(fls))hd1<-rbind(hd1,read.delim(file.path("odata","hec_dfo1",fls[i]),sep="\t"))


# get date and time of each observation
hd12<-hd1%>%
  separate(Begin.File,
           into = c("bg","yr","mnth","dy","hr","mns","secs"),
           sep=c(9,11,13,15,17,19,21),
           remove = FALSE)%>%
  mutate(file.start.date=ymd_hms(paste(yr,mnth,dy,hr,mns,secs)),
         cal.time=file.start.date+File.Offset..s.)%>%
  select(Begin.File,cal.time,file.start.date,Confidence)%>%
  mutate(cal.time=cut(cal.time,breaks = "1 mins"))%>%
  group_by(Begin.File,cal.time)%>%
  mutate(ncalls=n())

iqr15=quantile(hd12$ncalls,probs=0.75)+(1.5*(quantile(hd12$ncalls,probs=0.75)-quantile(hd12$ncalls,probs=0.25)))

hd13<-hd12%>%
  filter(ncalls>iqr15)%>%
  select(Begin.File,
         file.start.date,
         cal.time,
         ncalls)%>%
  distinct()%>%
  mutate(intofile=ymd_hms(as.character(cal.time))-file.start.date)%>%
  arrange(Begin.File)

write.csv(hd13,"wdata/hecate_dfo1_outliers_examine.csv",row.names = FALSE)


# dfo 2
fls<-list.files("odata/hec_dfo2")

hd2<-read.delim(file.path("odata","hec_dfo2",fls[1]),sep="\t")
for(i in 2:length(fls))hd2<-rbind(hd2,read.delim(file.path("odata","hec_dfo2",fls[i]),sep="\t"))

# get date and time of each observation
hd22<-hd2%>%
  separate(Begin.File,
           into = c("bg","yr","mnth","dy","hr","mns","secs"),
           sep=c(9,11,13,15,17,19,21),
           remove = FALSE)%>%
  mutate(file.start.date=ymd_hms(paste(yr,mnth,dy,hr,mns,secs)),
         cal.time=file.start.date+File.Offset..s.)%>%
  select(Begin.File,cal.time,file.start.date,Confidence)%>%
  mutate(cal.time=cut(cal.time,breaks = "1 mins"))%>%
  group_by(Begin.File,cal.time)%>%
  mutate(ncalls=n())

iqr15=quantile(hd22$ncalls,probs=0.75)+(1.5*(quantile(hd22$ncalls,probs=0.75)-quantile(hd22$ncalls,probs=0.25)))

hd23<-hd22%>%
  filter(ncalls>iqr15)%>%
  select(Begin.File,
         file.start.date,
         cal.time,
         ncalls)%>%
  distinct()%>%
  mutate(intofile=ymd_hms(as.character(cal.time))-file.start.date)%>%
  arrange(Begin.File)

write.csv(hd23,"wdata/hecate_dfo2_outliers_examine.csv",row.names = FALSE)


# dfo 3
fls<-list.files("odata/hec_dfo3")

hd3<-read.delim(file.path("odata","hec_dfo3",fls[1]),sep="\t")
for(i in 2:length(fls))hd3<-rbind(hd3,read.delim(file.path("odata","hec_dfo3",fls[i]),sep="\t"))

# get date and time of each observation
hd32<-hd3%>%
  separate(Begin.File,
           into = c("bg","yr","mnth","dy","hr","mns","secs"),
           sep=c(9,11,13,15,17,19,21),
           remove = FALSE)%>%
  mutate(file.start.date=ymd_hms(paste(yr,mnth,dy,hr,mns,secs)),
         cal.time=file.start.date+File.Offset..s.)%>%
  select(Begin.File,cal.time,file.start.date,Confidence)%>%
  mutate(cal.time=cut(cal.time,breaks = "1 mins"))%>%
  group_by(Begin.File,cal.time)%>%
  mutate(ncalls=n())

iqr15=quantile(hd32$ncalls,probs=0.75)+(1.5*(quantile(hd32$ncalls,probs=0.75)-quantile(hd32$ncalls,probs=0.25)))

hd33<-hd32%>%
  filter(ncalls>iqr15)%>%
  select(Begin.File,
         file.start.date,
         cal.time,
         ncalls)%>%
  distinct()%>%
  mutate(intofile=ymd_hms(as.character(cal.time))-file.start.date)%>%
  arrange(Begin.File)

write.csv(hd33,"wdata/hecate_dfo3_outliers_examine.csv",row.names = FALSE)


# dfo 4

fls<-list.files("odata/hec_dfo4")

hd4<-read.delim(file.path("odata","hec_dfo4",fls[1]),sep="\t")
for(i in 2:length(fls))hd4<-rbind(hd4,read.delim(file.path("odata","hec_dfo4",fls[i]),sep="\t"))


# get date and time of each observation
hd42<-hd4%>%
  separate(Begin.File,
           into = c("bg","yr","mnth","dy","hr","mns","secs"),
           sep=c(10,12,14,16,18,20,22),
           remove = FALSE)%>%
  mutate(file.start.date=ymd_hms(paste(yr,mnth,dy,hr,mns,secs)),
         cal.time=file.start.date+File.Offset..s.)%>%
  select(Begin.File,cal.time,file.start.date,Confidence)%>%
  mutate(cal.time=cut(cal.time,breaks = "1 mins"))%>%
  group_by(Begin.File,cal.time)%>%
  mutate(ncalls=n())

iqr15=quantile(hd42$ncalls,probs=0.75)+(1.5*(quantile(hd42$ncalls,probs=0.75)-quantile(hd42$ncalls,probs=0.25)))

hd43<-hd42%>%
  filter(ncalls>iqr15)%>%
  select(Begin.File,
         file.start.date,
         cal.time,
         ncalls)%>%
  distinct()%>%
  mutate(intofile=ymd_hms(as.character(cal.time))-file.start.date)%>%
  arrange(Begin.File)

write.csv(hd43,"wdata/hecate_dfo4_outliers_examine.csv",row.names = FALSE)

# dfo 5

fls<-list.files("odata/hec_dfo5")

hd5<-read.delim(file.path("odata","hec_dfo5",fls[1]),sep="\t")
for(i in 2:length(fls))hd5<-rbind(hd5,read.delim(file.path("odata","hec_dfo5",fls[i]),sep="\t"))

# get date and time of each observation
hd52<-hd5%>%
  separate(Begin.File,
           into = c("bg","yr","mnth","dy","hr","mns","secs"),
           sep=c(9,11,13,15,17,19,21),
           remove = FALSE)%>%
  mutate(file.start.date=ymd_hms(paste(yr,mnth,dy,hr,mns,secs)),
         cal.time=file.start.date+File.Offset..s.)%>%
  select(Begin.File,cal.time,file.start.date,Confidence)%>%
  mutate(cal.time=cut(cal.time,breaks = "1 mins"))%>%
  group_by(Begin.File,cal.time)%>%
  mutate(ncalls=n())

iqr15=quantile(hd52$ncalls,probs=0.75)+(1.5*(quantile(hd52$ncalls,probs=0.75)-quantile(hd52$ncalls,probs=0.25)))

hd53<-hd52%>%
  filter(ncalls>iqr15)%>%
  select(Begin.File,
         file.start.date,
         cal.time,
         ncalls)%>%
  distinct()%>%
  mutate(intofile=ymd_hms(as.character(cal.time))-file.start.date)%>%
  arrange(Begin.File)

write.csv(hd53,"wdata/hecate_dfo5_outliers_examine.csv",row.names = FALSE)

# fj1
fls<-list.files("odata/hec_fj1")

hd6<-read.delim(file.path("odata","hec_fj1",fls[1]),sep="\t")
for(i in 2:length(fls))hd6<-rbind(hd6,read.delim(file.path("odata","hec_fj1",fls[i]),sep="\t"))


# get date and time of each observation
hd62<-hd6%>%
  separate(Begin.File,
           into = c("bg","yr","mnth","dy","hr","mns","secs"),
           sep=c(11,13,15,17,19,21,23),
           remove = FALSE)%>%
  mutate(file.start.date=ymd_hms(paste(yr,mnth,dy,hr,mns,secs)),
         cal.time=file.start.date+File.Offset..s.)%>%
  select(Begin.File,cal.time,file.start.date,Confidence)%>%
  mutate(cal.time=cut(cal.time,breaks = "1 mins"))%>%
  group_by(Begin.File,cal.time)%>%
  mutate(ncalls=n())

iqr15=quantile(hd62$ncalls,probs=0.75)+(1.5*(quantile(hd62$ncalls,probs=0.75)-quantile(hd62$ncalls,probs=0.25)))

hd63<-hd62%>%
  filter(ncalls>iqr15)%>%
  select(Begin.File,
         file.start.date,
         cal.time,
         ncalls)%>%
  distinct()%>%
  mutate(intofile=ymd_hms(as.character(cal.time))-file.start.date)%>%
  arrange(Begin.File)

write.csv(hd63,"wdata/hecate_fj1_outliers_examine.csv",row.names = FALSE)

# fj3

fls<-list.files("odata/hec_fj3")

hd7<-read.delim(file.path("odata","hec_fj3",fls[1]),sep="\t")
for(i in 2:length(fls))hd7<-rbind(hd7,read.delim(file.path("odata","hec_fj3",fls[i]),sep="\t"))

# get date and time of each observation
hd72<-hd7%>%
  separate(Begin.File,
           into = c("bg","yr","mnth","dy","hr","mns","secs"),
           sep=c(11,13,15,17,19,21,23),
           remove = FALSE)%>%
  mutate(file.start.date=ymd_hms(paste(yr,mnth,dy,hr,mns,secs)),
         cal.time=file.start.date+File.Offset..s.)%>%
  select(Begin.File,cal.time,file.start.date,Confidence)%>%
  mutate(cal.time=cut(cal.time,breaks = "1 mins"))%>%
  group_by(Begin.File,cal.time)%>%
  mutate(ncalls=n())

iqr15=quantile(hd72$ncalls,probs=0.75)+(1.5*(quantile(hd72$ncalls,probs=0.75)-quantile(hd72$ncalls,probs=0.25)))

hd73<-hd72%>%
  filter(ncalls>iqr15)%>%
  select(Begin.File,
         file.start.date,
         cal.time,
         ncalls)%>%
  distinct()%>%
  mutate(intofile=ymd_hms(as.character(cal.time))-file.start.date)%>%
  arrange(Begin.File)

write.csv(hd73,"wdata/hecate_fj1_outliers_examine.csv",row.names = FALSE)
