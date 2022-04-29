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
trial<-man.check.organize("LionsBayDectionsCheck_reviewer3.txt")
names(trial)
trial[[4]]
trial$by.min.summary

