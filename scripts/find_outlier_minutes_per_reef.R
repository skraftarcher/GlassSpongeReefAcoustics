# pull out outlier files to examine


source("scripts/install_packages_function.R")
lp("tidyverse")



#write function
getouts<-function(infile,outfile){
  bb2<-read.csv(paste0("wdata/",infile))%>%
  mutate(cal.time=cut(ymd_hms(cal.time),breaks = "1 mins"))%>%
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
  mutate(intofile=ymd_hms(as.character(cal.time))-ymd_hms(file.start.date))%>%
  arrange(Begin.File)

write.csv(bb3,paste("wdata/",outfile),row.names = FALSE)
}

#now reefs
# bella bella
getouts(infile="bella_fullcalldataset.csv",
        outfile = "bb_outliers_examine.csv")
# lions bay
getouts(infile="lionsbay_fullcalldataset.csv",
        outfile = "lb_outliers_examine.csv")

# Hecate     
#DFO1
getouts(infile="hecate_dfo1_fullcalldataset.csv",
        outfile = "dfo1_outliers_examine.csv")
#DFO2
getouts(infile="hecate_dfo2_fullcalldataset.csv",
        outfile = "dfo2_outliers_examine.csv")
#DFO3
getouts(infile="hecate_dfo3_fullcalldataset.csv",
        outfile = "dfo3_outliers_examine.csv")
#DFO4
getouts(infile="hecate_dfo4_fullcalldataset.csv",
        outfile = "dfo4_outliers_examine.csv")
#DFO5
getouts(infile="hecate_dfo5_fullcalldataset.csv",
        outfile = "dfo5_outliers_examine.csv")
#FJ1
getouts(infile="hecate_fj1_fullcalldataset.csv",
        outfile = "fj1_outliers_examine.csv")
#FJ3
getouts(infile="hecate_fj3_fullcalldataset.csv",
        outfile = "fj3_outliers_examine.csv")
