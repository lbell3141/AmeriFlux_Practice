#Lindsey Bell
#5/25/2023
#Making a workflow function for plotting annual anomalies 


work_flow_1 = function(dat_file = "AMF_US-SRC_FLUXNET_SUBSET_HH_2008-2014_3-5.csv") {
  library(lubridate)
  library(dplyr)
  library(ggplot2)

  
  
}


#setting up data
dat_file = "AMF_US-SRC_FLUXNET_SUBSET_HH_2008-2014_3-5.csv"
  dat_file = read.csv(dat_file, header = TRUE, na.strings= "-9999", skip=0, sep = ",")
  dat_file$TIMESTAMP_START = ymd_hm(dat_file$TIMESTAMP_START)
  dat_file$Months = months(dat_file$TIMESTAMP_START)
  dat_file$years = format(dat_file$TIMESTAMP_START, "%Y")
  #creating a df with variables of interest
  dat_file_fr = (data.frame(yr = dat_file$years,
                                 mon = dat_file$Months,
                                 swc = dat_file$SWC_F_MDS_1,
                                 gpp = dat_file$GPP_DT_VUT_25,
                                 nee = dat_file$NEE_VUT_25,
                                 temp_atmos = dat_file$TA_F,
                                 temp_soil = dat_file$TS_F_MDS_1,
                                 precip = dat_file$P_F) )
  dat_file_fr = dat_file_fr[dat_file_fr$mon %in% c('July','August','September','October','November'),]
  #standardizing data
  
  lt_mn = mean( , na.rm = TRUE)
  lt_sd = sd( , na.rm = TRUE)
  
  grszn_anom = dat_file_fr%>%
    group_by(years)%>%
    mutate(st_mn = mean())%>%
    mutate(st_stand =(st_mn - lt_mn)/(lt_sd))
  
  
  colMeans(dat_file_fr[ , c(3:8)], na.rm = TRUE)
  sapply(dat_file_fr[ , c(3:8)], sd, na.rm = TRUE)  
  
  
  
  for (q in (dat_file_fr[ , c(3:8)])) {
    lt_mn = mean(q , na.rm = TRUE)
    lt_sd = sd(q , na.rm = TRUE)
  }
    grszn_anom = dat_file_fr%>%
      group_by(years)%>%
      mutate(st_mn = mean(q))%>%
      mutate(st_stand =(st_mn - lt_mn)/(lt_sd))
  }

for column in dat_file_fr {
  lt_mn = mean(q , na.rm = TRUE)
  lt_sd = sd(q , na.rm = TRUE)
}

grszn_anom = dat_file_fr%>%
  group_by(years)%>%
  mutate(st_mn = mean(q))%>%
  mutate(st_stand =(st_mn - lt_mn)/(lt_sd))
}

