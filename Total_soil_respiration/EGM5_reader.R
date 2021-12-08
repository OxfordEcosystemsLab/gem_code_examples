

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                    usage                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#  The EGM-5 soil respiration measurement is in a different format to EGM-4,    
#  this file was used to import and correct column name of EGM-5                
library(tidyverse)
library(openxlsx)

# read the txt file
setwd('F:/Side_project/african_data_workshop/not_to_share_with_student/Mackline/EGM-5 Data ALL')
EGM5_raw <- read.csv2("F:/Side_project/african_data_workshop/not_to_share_with_student/Mackline/EGM-5 Data ALL/EGM5/19061610.TXT",col.names='NANAN',header=F)

# The correct column header
column_header=c('probe_type',
                'Date',
                'exact_time',
                'egm_measurement',
                'rec_num',
                'co2ref',
                'atmp',
                'Flow Rate (cc_min) ',
                'H20 (mb)',
                'H20 Sensor',
                'O2(%)',
                'System Error D',
                'Aux Voltage (volt)',
                'PAR: (ppm = µmol m2 s-1)',
                'Tsoil_Temperature (oC)',
                'air_temp_c',
                'relative_humidity',
                'Process',
                'InputD',
                'time',
                'InputF',
                'soil quadratic respiration rate_umol m2 s-1')

# tidy up data
EGM5_clean<-EGM5_raw%>%
  mutate(length_of_info=nchar(NANAN))%>%
  filter(length_of_info>30)%>% # this is to remove empty rows
  select(-length_of_info)%>%
  mutate(NANAN=gsub('[\r\n\t]', '', NANAN))%>% # this is to remove empty space in data string
  separate(NANAN,into=column_header,sep=',')


#  # Now you should save EGM5_clean, however, there might be strange rows at    
#  the beginning or the end, you should check mannually                         

openxlsx::write.xlsx(EGM5_clean,file = 'your_file_name.xlsx')
