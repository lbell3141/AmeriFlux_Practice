#Lindsey Bell
#5/25/2023
#Making a workflow function for plotting annual anomalies 


work_flow_1 = function(dat_file = "AMF_US-SRC_FLUXNET_SUBSET_HH_2008-2014_3-5.csv") {
  library(lubridate)
  library(dplyr)
  library(ggplot2)
  library(gridExtra)
  
  dat_file = "AMF_US-SRC_FLUXNET_SUBSET_HH_2008-2014_3-5.csv"
  dat_file = read.csv(dat_file, header = TRUE, na.strings= "-9999", skip=0, sep = ",")
  dat_file$TIMESTAMP_START = ymd_hm(dat_file$TIMESTAMP_START)
  dat_file$Months = months(dat_file$TIMESTAMP_START)
  dat_file$years = format(dat_file$TIMESTAMP_START, "%y")
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
  
  cols = 3:8
  og_col_names = colnames(dat_file_fr)[cols]
  col_mean = colMeans(dat_file_fr[, cols], na.rm = TRUE)
  unique_years = unique(dat_file_fr$yr)
  lt_dat = data.frame(matrix(NA, nrow = length(unique_years), ncol = length(cols) + 1))
  colnames(lt_dat) = c("yr", paste0("lt_mn_", og_col_names))
  lt_dat[, 1] = unique_years
  
  col_sd = apply(dat_file_fr[, cols], 2, sd, na.rm = TRUE)
  for (i in seq_along(cols)){
    col_name_mean = paste0("lt_mn_", og_col_names[i])
    col_name_sd = paste0("lt_sdev_", og_col_names[i])
    dat_file_fr[[col_name_mean]] = col_mean[i]
    dat_file_fr[[col_name_sd]] = col_sd[i]
    lt_dat[[col_name_mean]] = col_mean[i]
    lt_dat[[col_name_sd]] = col_sd[i]
  }
  
  columns = c("swc", "gpp", "nee", "temp_atmos", "temp_soil", "precip")
  short_term_means = dat_file_fr %>%
    group_by(yr) %>%
    summarise(across(all_of(columns), mean, na.rm = TRUE, .names = "st_mn_{.col}")) %>%
    distinct()
  
  joined_dat = left_join(lt_dat, short_term_means, by = "yr")
  head(joined_dat)
  
  variables = c("swc", "gpp", "nee", "temp_atmos", "temp_soil", "precip")
  for (variable in variables){
    st_mean_col = paste0("st_mn_", variable)
    lt_mean_col = paste0("lt_mn_", variable)
    lt_sdev_col = paste0("lt_sdev_", variable)
    
    z_score_col = paste0("z_score_", variable)
    
    joined_dat = joined_dat%>%
      group_by(yr)%>%
      mutate(!!z_score_col := (.data[[st_mean_col]]-.data[[lt_mean_col]])/.data[[lt_sdev_col]])
  }
  
  z_score_vars = paste0("z_score_", variables)
  plots = lapply(z_score_vars, function(z_var){ 
    ggplot(data = joined_dat, 
           mapping = aes(x = yr, y = .data[[z_var]])) +
      geom_point() + 
      geom_line() +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = z_var)
  })
  grid.arrange(grobs = plots, ncol = 3)
  
  
}

# checking line by line before trying the function
#setting up data
dat_file = "AMF_US-SRC_FLUXNET_SUBSET_HH_2008-2014_3-5.csv"
  dat_file = read.csv(dat_file, header = TRUE, na.strings= "-9999", skip=0, sep = ",")
  dat_file$TIMESTAMP_START = ymd_hm(dat_file$TIMESTAMP_START)
  dat_file$Months = months(dat_file$TIMESTAMP_START)
  dat_file$years = format(dat_file$TIMESTAMP_START, "%y")
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
 
     #check dat frame
     head(dat_file_fr)
     
     #find long term stats and storing in data frame lt_dat
     #combined mean and sdev to fix empty column issue
     
     cols = 3:8
     og_col_names = colnames(dat_file_fr)[cols]
     col_mean = colMeans(dat_file_fr[, cols], na.rm = TRUE)
     unique_years = unique(dat_file_fr$yr)
     lt_dat = data.frame(matrix(NA, nrow = length(unique_years), ncol = length(cols) + 1))
     colnames(lt_dat) = c("yr", paste0("lt_mn_", og_col_names))
     lt_dat[, 1] = unique_years
     
     col_sd = apply(dat_file_fr[, cols], 2, sd, na.rm = TRUE)
     for (i in seq_along(cols)){
       col_name_mean = paste0("lt_mn_", og_col_names[i])
       col_name_sd = paste0("lt_sdev_", og_col_names[i])
       dat_file_fr[[col_name_mean]] = col_mean[i]
       dat_file_fr[[col_name_sd]] = col_sd[i]
       lt_dat[[col_name_mean]] = col_mean[i]
       lt_dat[[col_name_sd]] = col_sd[i]
     }
     
  #finding short term stats
     columns = c("swc", "gpp", "nee", "temp_atmos", "temp_soil", "precip")
     short_term_means = dat_file_fr %>%
       group_by(yr) %>%
       summarise(across(all_of(columns), mean, na.rm = TRUE, .names = "st_mn_{.col}")) %>%
       distinct()
   

         #join data to keep same length to prevent error below
     joined_dat = left_join(lt_dat, short_term_means, by = "yr")
     head(joined_dat)
     
     
  #finding z score and adding a column for each variable's z score
     variables = c("swc", "gpp", "nee", "temp_atmos", "temp_soil", "precip")
     for (variable in variables){
       st_mean_col = paste0("st_mn_", variable)
       lt_mean_col = paste0("lt_mn_", variable)
       lt_sdev_col = paste0("lt_sdev_", variable)
       
       z_score_col = paste0("z_score_", variable)
       
       joined_dat = joined_dat%>%
         group_by(yr)%>%
         mutate(!!z_score_col := (.data[[st_mean_col]]-.data[[lt_mean_col]])/.data[[lt_sdev_col]])
            }

  #generating plots of z-scores for each year  
     #not plotting the geom_line - says there's only one observation in the group
     #can't set y bounds without grod.arrange error
     z_score_vars = paste0("z_score_", variables)
     plots = lapply(z_score_vars, function(z_var){ 
       ggplot(data = joined_dat, 
              mapping = aes(x = yr, y = .data[[z_var]])) +
         geom_point() + 
         geom_line() +
         geom_hline(yintercept = 0, linetype = "dashed") +
         labs(title = z_var)
     })
          grid.arrange(grobs = plots, ncol = 3)
     
     
     
     
     

  

        
        
        
        
        
        
        