#Lindsey Bell
#5/16/2023
#AmeriFulux FluxNet data: Santa Rita Creosote

#Load data into studio
dat_SRC=read.csv("AMF_US-SRC_FLUXNET_SUBSET_HH_2008-2014_3-5.csv", header = TRUE, na.strings= "-9999", skip=0, sep = ",")

datSRC="AMF_US-SRC_FLUXNET_SUBSET_HH_2008-2014_3-5.csv"

#assigning NULL to remove from values
#only for char?
dat_SRC[["NA"]]<-NULL
plot(dat_SRC$SWC_F_MDS_1, dat_SRC$TIMESTAMP_START)
plot(swc_clean)

#removing NAs from data frame p.136
dat_src_clean = na.omit(dat_SRC)
swc_clean = na.omit(dat_SRC$SWC_F_MDS_1)

library(tidyr)
tibble(dat_SRC)
library(lubridate)

#plotting swc vs time results in gaps because R doesn't recognize the time values yet
plot(dat_SRC$TIMESTAMP_START, dat_SRC$SWC_F_MDS_1)

dat_SRC$TIMESTAMP_START= ymd_hm(as.character(dat_SRC$TIMESTAMP_START))


plot(dat_SRC$TIMESTAMP_START, dat_SRC$SWC_F_MDS_1)
    #now R recognizes the time format and has made a continuous plot



#standardizing data and plotting
  #use scale to standardize
 swc=dat_SRC$SWC_F_MDS_1
   swc_stand= scale(dat_SRC$SWC_F_MDS_1)
  #basic scatterplot with labels
  plot(dat_SRC$TIMESTAMP_START, swc_stand,
       main= "Soil Water Content vs. Time",
       xlab= "Time (yr)",
       ylab= "Soil Water Content",
       )
  
  # extracting month to reformat x axis
  dat_ymdhm= as.Date(dat_SRC$TIMESTAMP_START, format = YYYY-MM-DD-HH-MM)
  t_doy = format (dat_ymdhm, "%j")
  t_md = substr(dat_ymdhm, 6, 13)
  t_md= as.Date(t_md, format = "%m-%d")
  
  exdat= format(dat_SRC$TIMESTAMP_START, "y%-%m-%d")
  
  library(ggplot2
          )
  #ggplot
  
  ggplot(dat_SRC, aes(TIMESTAMP_START, y=swc_stand),
    scale_x_continuous(n.breaks = 10))
    
  ggplot(dat_SRC, aes(x=as.Date(TIMESTAMP_START), y=swc_stand))+
    geom_line()+
    #geom_point()+
    scale_x_date(date_breaks="6 month")+
    theme_minimal()+
    xlab("Time")+
    ylab("Soil Water Content")+
    theme(axis.text.x= element_text(angle = 45, hjust = 1)
          )
  
  #line graph p.241
  lg=plot(dat_SRC$TIMESTAMP_START, swc_stand, 
          main= "Soil Water Content vs. Time",
          xlab= "Time (yr)",
          ylab= "Soil Water Content",
          type = "l",
          col= "black",
          lwd= 1,
          fg= "black",
          )
                 
  par(bg="white")
  grid()
  abline(h=0, lty="dotted", col="red", lwd=3)
  
max(dat_SRC$TIMESTAMP_START)
     axis(side=1,
      las=1,
       dat_SRC$TIMESTAMP_START,
       format(dat_SRC$TIMESTAMP_START, "y%-%m-%d"),
      scale_x_continuous(n.breaks=10000),
      
      scale_x_date(date_breaks = "months", date_labels = "%b-%y")
  )

     scale_x_continuous(n.breaks=100)
     
     scale_x_date(date_breaks = "months", date_labels = "%b-%y")
  
  theme(axis.text.x = element_text(angle = 45, vjust=1, hjust = 1))
  
  theme(axis.text.x = element_text(angle = 45))
  
    
  text(x=1:length(format(dat_SRC$TIMESTAMP_START, "y%-%m-%d")),
       y=par("usr")[3]-0.45,
       labels = names(format(dat_SRC$TIMESTAMP_START, "y%-%m-%d")),
       xpd=NA,
       srt=35,
       adj=0.965,
      cex = 1)
       
       
       
       
     las=2,
       dat_SRC$TIMESTAMP_START,
       format(dat_SRC$TIMESTAMP_START, "y%-%m-%d"),
       )
  
library(ggplot2)
  install.packages("scales")
  library(scales)
  ggplot(dat_)

  gl+scale_x_date(breaks=date_breaks("months"),
                  labels = date_format("%b"))
  
mean(swc1, na.rm=TRUE)

