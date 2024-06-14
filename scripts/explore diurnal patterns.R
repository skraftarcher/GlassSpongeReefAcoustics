# script to try out circular data analysis

# load packages
source("scripts/install_packages_function.R")
lp("tidyverse")
lp("bpnreg")
lp("circular")

# load data
fchr<-read.csv("wdata/allreefs_hourlysummary.csv")%>%
  mutate(md=ymd(md))

fchr$circ<-circular(fchr$hr,units = "hours",template="clock24")
theme_set(theme_bw()+theme(panel.grid = element_blank()))
fchr$Reef<-factor(fchr$Reef,levels=c("Lions Bay","Bella Bella",
                                     "Hecate On Reef","Hecate Off Reef"))
ggplot(data=fchr)+
  # geom_segment(
  #   aes(x = hr, xend = hr, y = 0, yend = calls), 
  #   size = 1.2
  # )+
  geom_point(
    aes(x=hr,y=as.factor(ymd(paste(yr,mnth,dy))),size=calls,color=calls),
    alpha=.7
  )+
  coord_polar()+
  facet_wrap(~Reef,scales="free")+
  ylab("")+
  theme(axis.text.y=element_blank(),
        axis.ticks = element_blank())+
  scale_color_viridis_c(option = "turbo",name="Calls per hour")

ggsave("figures/circularplot_allreefs.png")

fchr2<-fchr%>%
  group_by(Reef,ID,hr)%>%
  summarize(m.calls=mean(calls),
            md.calls=median(calls),
            se.calls=sd(calls)/sqrt(n()))%>%
  bind_rows(data.frame(fchr%>%select(Reef,ID)%>%distinct(),
                       hr=rep(24,length(unique(fchr$ID)))))

ggplot(data=fchr2)+
  geom_segment(
    aes(x = hr, xend = hr, y = 0, yend =ifelse(is.na(md.calls),0,1),color=md.calls),
    size = 2,
    alpha=.5
  )+
  coord_polar()+
  facet_wrap(Reef~ID,scales="free")+
  ylab("")+
  theme(axis.text.y=element_blank(),
        axis.ticks = element_blank())+
  scale_color_viridis_c(option = "turbo",name="Median calls per hour")

ggsave("figures/circularplot_allreefs3.png")

ggplot(data=fchr2%>%
         filter(!Reef %in% c("Lions Bay","Bella Bella")))+
  geom_segment(
    aes(x = hr, xend = hr, y = 0, yend =ifelse(is.na(md.calls),0,1),color=md.calls),
    size = 2,
    alpha=.5
  )+
  coord_polar()+
  facet_wrap(Reef~ID,scales="free")+
  ylab("")+
  theme(axis.text.y=element_blank(),
        axis.ticks = element_blank())+
  scale_color_viridis_c(option = "turbo",name="Median calls per hour")

ggsave("figures/circularplot_hecate.png")

ggplot(data=fchr)+
  geom_point(aes(x=hr,y=calls),size=2,alpha=.4)+
  facet_wrap(~Reef,scales="free")

par(mfrow=c(2,2))
# try a model with a circular predictor
b.diurnal<-lm(log(calls+1)~sin(2*pi*(hr/24))+cos(2*pi*(hr/24)),data=fchr%>%
                filter(Reef=="Bella Bella"))
l.diurnal<-lm(log(calls+1)~sin(2*pi*(hr/24))+cos(2*pi*(hr/24)),data=fchr%>%
                filter(Reef=="Lions Bay"))
hecate.on<-lm(log(calls+1)~sin(2*pi*(hr/24))+cos(2*pi*(hr/24)),data=fchr%>%
                filter(Reef=="Hecate On Reef"))
hecate.off<-lm(log(calls+1)~sin(2*pi*(hr/24))+cos(2*pi*(hr/24)),data=fchr%>%
                filter(Reef=="Hecate Off Reef"))

plot(b.diurnal)
plot(l.diurnal)
plot(hecate.on)
plot(hecate.off)

summary(b.diurnal)
summary(l.diurnal)
summary(hecate.on)
summary(hecate.off)

# lions bay and bella bella have a significant diurnal pattern, hecate does not. 