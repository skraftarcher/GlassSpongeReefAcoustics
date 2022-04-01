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

# organizing by minute

lb2<-lb[1:600,]%>% # this checks the first six hundred lines to make sure it works
  separate(`Begin File`,
           into=c("stid","yr","mnth","d","hr","min","sec","ext"),
           sep = c(-16,-14,-12,-10,-8,-6,-4))






