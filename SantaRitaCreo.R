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
 
  
#ggplot for time and soil moisture
  library(ggplot2)
      ggplot(dat_SRC, aes(x=as.Date(TIMESTAMP_START), y=swc_stand))+
        geom_line()+
        scale_x_date(date_breaks="6 month")+
        theme_minimal()+
        xlab("Time")+
        ylab("Soil Water Content")+
        theme(axis.text.x= element_text(angle = 45, hjust = 1))+
        geom_hline(yintercept = 0, linetype="dashed", color="red", size=1.2)
      
#base R plot for time and soil moisture
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
       
    #formatting x axis by itself 
     axis(side=1,
      las=2,
       dat_SRC$TIMESTAMP_START,
       format(dat_SRC$TIMESTAMP_START, "y%-%m-%d"))
      
#linear regression for soil moisture and GPP  
    #quick check of plot:
      plot(dat_SRC$SWC_F_MDS_1, dat_SRC$GPP_DT_VUT_25) 
    
    #Using ggplot to plot variables and linear regression
      plot_1=ggplot(dat_SRC, aes(x=dat_SRC$SWC_F_MDS_1,y=dat_SRC$GPP_DT_VUT_25 ))
        plot_1+
          geom_point()+ 
          labs(x="soil water content", y="GPP")+
          geom_smooth(method = "lm")
        
    #Calculating correlation coeff. (esp for data with missing values)
        sm_gpp_cor= cor.test(dat_SRC$SWC_F_MDS_1, dat_SRC$GPP_DT_VUT_25, method="pearson", conf.level=0.95)      
      #or use:
        cor(dat_SRC$SWC_F_MDS_1, dat_SRC$GPP_DT_VUT_25, use ="pairwise.complete.obs")
 
#linear regression for soil heat flux (G) and latent heat flux (LE)
    plot(dat_SRC$LE_CORR_25, dat_SRC$G_F_MDS)
      plot_2=ggplot(dat_SRC, aes(x=LE_CORR_25, y=G_F_MDS))  
      plot_2+
        geom_point()+
        labs(x="latent heat flux", y="soil heat flux")+
        geom_smooth(method="lm")
  hf_shf_corr= cor.test(dat_SRC$LE_CORR_25, dat_SRC$G_F_MDS, method = "pearson", conf.level = 0.95)

  summary(lm(formula=dat_SRC$LE_CORR_25~dat_SRC$G_F_MDS))

#cleaning up data by finding the monthly averages across seven years (swc and gpp)
 library(lubridate)
   dat_SRC$TIMESTAMP_START= ymd_hm(as.character(dat_SRC$TIMESTAMP_START))
   df_1=dat_SRC$TIMESTAMP_START
  #extract months and years either in number format (1) or word format (2)
    dat_SRC$month= format(dat_SRC$TIMESTAMP_START, "%m")
    dat_SRC$Month=months(dat_SRC$TIMESTAMP_START)
    
    dat_SRC$year=format(dat_SRC$TIMESTAMP_START, "%y")

    
   aggregate(df_1,
             list()
             dat_SRC$Month + dat_SRC$year, dat_SRC, mean)  

  
  
  
