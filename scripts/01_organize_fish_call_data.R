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

# bring in functions we've written
source("scripts/organizemanualchecks.R")
source("scripts/organizeautodetections.R")

# this is how you call the function

function.check<-fishcall.bymin.auto(seltable=lb)


# start reviewing manual detections
bella<-man.check.organize("BellaBellaDectionsCheck.txt")

bella$fish.call.entry.check
bella[[1]]

bella$quick.look
bella[[2]]
bella$quick.look[1,]
bella[[2]]$Auto.Class

theme_set(theme_bw()+theme(panel.grid = element_blank()))

ggplot(bella$by.min.summary)+
  geom_jitter(aes(x=n.man.fishcall,y=n.auto.fishcall))+
  geom_abline(aes(intercept=0,slope=1))

ggplot(bella$full.data)+
  geom_jitter(aes(x=Confidence,y=Auto.Class,color=agree),alpha=.5)+
  facet_wrap(~Call.type)
