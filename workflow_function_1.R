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
 
#after writing out on whiteboard...
     #get dat frame
     dat_file_fr
     #find long term stats
     
     cols = 3:8
     og_col_names = colnames(dat_file_fr)[cols]
     col_mean = colMeans(dat_file_fr[, cols], na.rm = TRUE)
     for (i in seq_along(cols)){
       col_name = paste0("lt_mn_", og_col_names[i])
       dat_file_fr[[col_name]] = col_mean[i]
     }
     
     cols = 3:8
     og_col_names = colnames(dat_file_fr)[cols]
     col_sd = apply(dat_file_fr[, cols], 2, sd, na.rm = TRUE)
     for (i in seq_along(cols)){
       col_name = paste0("lt_sdev_", og_col_names[i])
       dat_file_fr[[col_name]] = col_mean[i]
     }
     
  #finding short term stats
     columns = c("swc", "gpp", "nee", "temp_atmos", "temp_soil", "precip")
     short_term_means = dat_file_fr%>%
       group_by(yr)%>%
       mutate(across(all_of(columns), mean, na.rm = TRUE))
     

     
  #finding z score
     variables = c("swc", "gpp", "nee", "temp_atmos", "temp_soil", "precip")
     results = lapply(variables, function(variable){
       lt_mean_col = paste0("lt_mn_", variable)
       lt_sd_col = paste0("lt_sdev_", variable)
         result = dat_file_fr%>%
           group_by(yr)%>%
           mutate(z_score = (short_term_means[[variable]]-.data[[lt_mean_col]])/.data[[lt_sd_col]])%>%
            pull(z_score)
            return(result)
          })
     
        head(result_df)  
      
        
  #join data to keep same length
        joined_dat = left_join(dat_file_fr, short_term_means, by = c("yr", "mon"))
head(joined_dat)
     

     
     
     
     