

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                    usage                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("C:/Users/Huanyuan/Desktop/carbon analysis 2023/total_soil_respiration/BOB TOTAL/2013/10) october")
setwd('C:/Users/Huanyuan Zhang-Zheng/Desktop/Stem_respiration')
#  The EGM-5 soil respiration measurement is in a different format to EGM-4,    
#  this file was used to import and correct column name of EGM-5                
library(tidyverse)
library(openxlsx)

myfilelist<-list.files(path = getwd(),pattern = ".dat",recursive = T)

for (i in 1:length(myfilelist)) {
  


# read the txt file
#setwd('F:/Side_project/african_data_workshop/not_to_share_with_student/Mackline/EGM-5 Data ALL')
EGM4_raw<-read.csv(myfilelist[i],header = T,sep='\t',skip=2)%>%
  filter(!is.na(RecNo)) # remove the useless last row
# The correct column header
column_header=c('egm_measurement',
                'rec_num',
                'random_original_day',
                'random_original_month',
                'hour',
                'minute',
                'co2ref',
                'mb.Ref',
                'mbR.Temp',
                'InputA_PAR',
                'InputB_Relative_humidity',
                'InputC_Temperature',
                'InputD',
                'time',
                'InputF',
                'InputG',
                'InputH',
                'atmp',
                'probe_type')


colnames(EGM4_raw)<-column_header

EGM4_raw<-cbind(year = NA, EGM4_raw)
EGM4_raw<-cbind(month = NA, EGM4_raw)
EGM4_raw<-cbind(day = NA, EGM4_raw)
EGM4_raw<-cbind(sub_plot_code = NA, EGM4_raw)
EGM4_raw<-cbind(plot_code = NA, EGM4_raw)

#  # Now you should save EGM5_clean, however, there might be strange rows at    
#  the beginning or the end, you should check mannually                         

openxlsx::write.xlsx(EGM4_raw,file = paste0(myfilelist[i],'.xlsx'),overwrite = T)
}
