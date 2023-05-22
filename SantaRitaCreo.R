#Lindsey Bell
#5/16/2023
#AmeriFulux FluxNet data: Santa Rita Creosote

#Load data into studio
dat_SRC=read.csv("AMF_US-SRC_FLUXNET_SUBSET_HH_2008-2014_3-5.csv", header = TRUE, na.strings= "-9999", skip=0, sep = ",")

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

 #using aggregate to find the mean for swc for each month 
    #create new data frame with just the two columns of interest (month and swc); check with print
    m_dat=data.frame(subjects=dat_SRC$Month,
                   val=dat_SRC$SWC_F_MDS_1)
    print(m_dat)
    
    #use mean function with new df and list in aggregate; print to check:
  print(aggregate(m_dat$val, list(m_dat$subjects), FUN=mean))

    #also need to find the monthly average for each year: 
      #extract month and year
          dat_SRC$year= format(dat_SRC$TIMESTAMP_START, "%Y")
          dat_SRC$Month=months(dat_SRC$TIMESTAMP_START)
      #create new df with three columns (yr, mon, and swc) and check w/ print
          my_dat=data.frame(years=dat_SRC$year,
                            mon=dat_SRC$Month,
                            val=dat_SRC$SWC_F_MDS_1)
          print(my_dat)
      #use aggregate with new df and mean function; check with print
          #this only finds the total average for each year
          print(aggregate(my_dat$val, list(dat_SRC$year), FUN=mean))
    
          #this only finds monthly average across all years; majority returning NA
          ag_grp= (aggregate(group_by(my_dat, years, mon), list(dat_SRC$Month), FUN=mean))
              print(ag_grp)
              
  #trying to group months by year:
      library(dplyr)
         #makes the correct number of groups (84)
           print(group_by(my_dat, years, mon))
        
           #can manually calculate using subset
               my_sub_March= subset(my_dat, years== 2008:2010 & mon=="March")
                 my_sub_March= subset(my_dat, mon=="March")
                    print(my_sub_March)
          mn_March2010=print(mean(my_sub_March$val))
          
      #monthly mean 2010, using 2010 subset
          my_sub_2010= subset(my_dat, years==2010)
          mm2_2010= print(aggregate(my_sub_2010, by=list(my_sub_2010$mon), FUN = mean))
          
  #monthly mean without subset
    mon_avg=aggregate(val~ years + mon, data = my_dat, FUN=mean, na.rm=TRUE)      
            
  #finding anomalies in a particular month by
      #finding the average for that month and plotting all months on that scale
#only returns three non-NA averages, so use na.rm=TRUE      
     
     mon_dat=print(data.frame( mon=dat_SRC$Month,
                          val=dat_SRC$SWC_F_MDS_1))
     yr_avg = print(aggregate(mon_dat$val, list(dat_SRC$Month), FUN=mean)) 
     
     yr_avg = print(aggregate(mon_dat$val, list(dat_SRC$Month), FUN=mean, na.rm=TRUE)) 

#monthly average across all years for swc:
      yr_avg = print(aggregate(mon_dat$val, list(dat_SRC$Month), FUN=mean, na.rm=TRUE)) 
#monthly average for each year for swc:
      mon_avg=print(aggregate(val~ years + mon, data = my_dat, FUN=mean, na.rm=TRUE))

 #finding monthly average for each year and all years with GPP:
      my_gppdat=data.frame(years=dat_SRC$year,
                        mon=dat_SRC$Month,
                        gpp=dat_SRC$GPP_NT_VUT_25)   
      mon_dat_gpp=print(data.frame( mon=dat_SRC$Month,
                                gpp=dat_SRC$GPP_NT_VUT_25))
      
      #monthly average across all years for gpp
      yr_avg_gpp = print(aggregate(mon_dat_gpp$gpp, list(dat_SRC$Month), FUN=mean, na.rm=TRUE))
      
      #monthly average for each year for gpp 
      mon_avg_gpp=print(aggregate(gpp~ years + mon, data = my_gppdat, FUN=mean, na.rm=TRUE))
      
#plot avg swc and gpp
      plot(mon_avg_gpp$mon, mon_avg_gpp$gpp)
      plot(mon_avg$mon, mon_avg$val)
      plot(mon_avg$mon, mon_avg$val)
      
?join
      
    mon_avg_joined= print(left_join(mon_avg, mon_avg_gpp))
       plot(mon_avg_joined$val, mon_avg_joined$gpp)
    
    maj_Jan=subset(mon_avg_joined, mon=="January")

#plotting swc anomalies for April manually
    maj_Apr=print(subset(mon_avg_joined, mon=="April"))
    mean(maj_Apr$val)
    plot(maj_Apr$years, maj_Apr$val,
         abline(h=5.955438))
   std_swc_Apr=scale(maj_Apr$val)   
    plot(maj_Apr$years, std_swc_Apr,
         abline(h=0))   
   
#generating standardized plots for each month all at once!    
    par(mfrow=c(3,4))
    loop.vector= mon_avg_joined$mon
    for (i in loop.vector){
      x=mon_avg_joined$val[,i]
    }
    plot(x, scale()
         )
    
    
    
    
#Loops
  #creating a df using for loops:
    #make empty df
     prac_dat=data.frame(
       col1= numeric(), col2= character(), col3=numeric()
     )
     print(prac_dat)
    #set parameters for each cloumn
      for(i in 1:10) {
        vec_prac= c(i-4, LETTERS[i+1], i*5)
        prac_dat[i, ]=vec_prac
      }
      print(prac_dat) 
rnorm(prac_dat$col3)
plot(rnorm(prac_dat$col3)
)

x_p=rnorm(50)
print(x_p)
plot(x_p)
quantile(x_p)

hist(x_p, breaks = 5)
shapiro.test(x_p)

x_p= na.strings= c("0.15396632")
x_p=as.numeric(x_p)
hist(x_p, breaks = 5)
print(x_p)

hist(log(x_p))
hist(log(log(x_p)), breaks = 5)
