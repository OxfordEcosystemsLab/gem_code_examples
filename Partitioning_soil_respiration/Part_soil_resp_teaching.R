

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                           PART SOIL RESPIRATION                          ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                1. Information                            ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~
##  ···Authors----
##~~~~~~~~~~~~~~~~

#  This script was built upon joined efforts by lots of people in
#  Yadvinder Malhi's lab. However, I devided to write this script
#  from scratch - but with many others' idea incorporated - because
#  old scripts have been very messy.

#  Huanyuan Zhang hyzhang1996@gmail.com,
#  Script was checked by Guillaume Delhaye and Terhi Ruitta
# 2021 August 13 as part of the African data workshop


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                2. Meta data                              ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ···How does this script work?  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#  The first step is to do a very strict data quality control, there are two    
#  problems (1) on the day of measurement, there are crazy rain or the EGM      
#  was broken so the measurement is abnormal, although the collar               
#  installation is good (2) the collar (the experiment tube) was not well       
#  installed, all data from this collar is abnormal                             
#                                                                               
#  Then after data cleaning, we follow the same procedure as total soil         
#  respiration, but this time, each calculated flux was tagged with a           
#  experiment code.                                                             
#                                                                               
#  A percentage was calculated following the box bellow                         
#                                                                               
#  Yes, the output of this script is the percentage of several parts of soil    
#  respiration                                                                  


#··············································································?
#  We have: con_no_lit, CTRL, my_no_lit, so_no_lit                             ?
#                                                                              ?
#  con_nor_lit = control & normal litterfall,                                  ?
#  con_no_lit = control & no litterfall,                                       ?
#  con_doub_lit = control & double litterfall,                                 ?
#  my_nor_lit = mycorrhizae & normal litterfall,                               ?
#  my_no_lit = mycorrhizae & no litterfall,                                    ?
#  my_doub_lit = mycorrhizae & double litterfall,                              ?
#  so_nor_lit = soil & normal litterfall,                                      ?
#  so_no_lit = soil & no litterfall,                                           ?
#  so_doub_lit = soil & double litterfall.                                     ?
#  + ADD ml = mineral layer.                                                   ?
#                                                                              ?
#  So: we have:                                                                ?
#  total soil respiration = CTRL                                               ?
#  Litter respiration = (CTRL - con_no_lit) / CTRL                             ?
#  Heterotrophic respiration = (my_no_lit + Litter respiration) / CTRL         ?
#  Autotrophic Respiration = (con_no_lit - my_no_lit) / CTRL                   ?
#  Mycorrhizae respiration = (my_no_lit - so_no_lit) / CTRL                    ?
#                                                                              ?
#··············································································?

##~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ···Necessary input  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~

 # $ sub_plot                   : int  1 1 1 1 1 1 1 1 1 1 ...
 # $ plot_code                  : chr  "PP1" "PP1" "PP1" "PP1" ...
 # $ treatment_code_partitioning: chr  "con_nor_lit" "con_nor_lit" "con_nor_lit" "con_nor_lit" ...
 # $ co2ref_ppm_sec             : int  549 553 556 558 560 562 565 567 569 571 ...
 # $ InputD                     : int  0 0 0 9 11 13 15 17 19 21 ...
 # $ time                       : int  0 4 9 14 19 24 28 33 38 43 ...
 # $ InputF                     : num  0 0 0 0.59 0.54 0.52 0.5 0.49 0.48 0.47 ...
 # $ year                       : int  2013 2013 2013 2013 2013 2013 2013 2013 2013 2013 ...
 # $ month                      : int  9 9 9 9 9 9 9 9 9 9 ...
 # $ day                        : int  3 3 3 3 3 3 3 3 3 3 ...
 # $ atmp_mb                    : int  992 992 992 992 992 992 992 992 992 992 ...
 # $ replica                    : int  1 1 1 1 1 1 1 1 1 1 ...
 # $ collar_number              : logi  NA NA NA NA NA NA ...


##~~~~~~~~~~~~~~~~~~~~~~~
##  ···important note----
##~~~~~~~~~~~~~~~~~~~~~~~


#  You have to customize chapter 3 nad chapter 6, because treatment code     
#  is not standardized for every plot, make sure it will work for you        

#  You must run one plot at a time

#  The ultimate goal of this script is to calculate % of each respiration components per plot (one value a plot, there are too many noise, it is difficult to look at each date etc)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                             3. data preparation                          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~
##···quality check  ----
##~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls())
library(dplyr)
library(ggplot2)
library(lubridate)
library(outliers)
library(hrbrthemes)
setwd('F:/Oxford/Chapter_two/soil_respiration/raw_data_from_sam/BOB')

source("EGM_flux_recalculation.R")
source("soilrespiration_auxfunctions.R")
source("functions.r")


GEM_raw <-   read.table('Part_Soil_Resp_Gabon.csv', sep = ",",
                        header = T)%>%
  filter(plot_code == 'LPG-01')%>%
  #  #This is only for site having treatments plus 10 CTRL collars at the         
  #  middle, we take these CTRL away at the moment, you might want to use these   
  #  as part of the total soil respiration measurements, anyhow, this is not      
  #  relevant to partitioning experiment so not used in this script. It might 
  #  help you combine partitioning with total soil respiration by providing flux info every date.
  filter(!measurement_code=='CTRL') 

GEM_raw$plot_code <- as.factor(GEM_raw$plot_code)
GEM_raw$year <- as.numeric(GEM_raw$year)
GEM_raw$month <- as.numeric(GEM_raw$month)
GEM_raw$sub_plot <- as.factor(GEM_raw$sub_plot)
GEM_raw$plot_corner_code <- as.character(GEM_raw$plot_corner_code)
GEM_raw$replica <- as.factor(GEM_raw$replica)
GEM_raw$co2ref_ppm_sec <- as.numeric(GEM_raw$co2ref)
GEM_raw$treatment_code <- as.character(GEM_raw$treatment_code_partitioning)
GEM_raw$time <- as.numeric(GEM_raw$time)
GEM_raw$atmp_mb <- as.numeric(GEM_raw$atmp)
GEM_raw$InputF <- as.numeric(GEM_raw$InputF)
GEM_raw$measurement_code <- as.character(GEM_raw$measurement_code)

unique(GEM_raw$treatment_code)
unique(GEM_raw$plot_corner_code)

##~~~~~~~~~~~~~~~~~~~~~
##???your options  ----
##~~~~~~~~~~~~~~~~~~~~~

elevation = 399 #unit: m #This is site elevation, used to estimate missing atmp
GEM_raw$collar_diameter_cm = 11   #[cm] collar diameter
GEM_raw$collar_height_cm = 5      #[cm] collar height
GEM_raw$air_temp_c = 25.7

#  you might want to specify
#  collar height/diameter and temperature for each EGM record, in
#  which case, you should make another column in your dataset, OR,
#  you can try to left_join and bring information from other
#  datasets (like meteorological station)




one_measurement_id = c(
  'plot_code',
  'plot_corner_code',
  'collar_number',
  'day',
  'month',
  'year',
  'measurement_code',
  'treatment_code')

#this is 1 egm measurement so under one of 'one_measurement_id' you should see only 27 rows
#  This combination need to be a unique flag for each measurement, you
#  can customize yours. For example, if you have only one measurement under
#  sub_plot + date, then you can simply one_measurement_id =               
#  c('subplot','year','month','day')


One_experiment_id = c('plot_code',
                      'plot_corner_code', 
                      'day',
                      'month',
                      'year')
# one set (array). One experiment id is a unique id for each experiment, a experiment is a group of C1 S1 S2 S3, or a group of con_nor_lit, con_no_lit, my_no_lit, so_no_lit,ml
# Normally, according to GEM RAINFOR manual, each subplot should have maximum one experiment. 

Collar_id = c('plot_code',
              'plot_corner_code', 
              'treatment_code')
# this is a collar

# I just grab temperature from meteological station dataset
#Temp <- read.table("F:/Oxford/Chapter_two/stem_respiration/input/BOB_gapfill_erai_monthly_1979_2017.csv", sep = ",", header = TRUE)
#Temp$Serial_date_temp <- lubridate::decimal_date(lubridate::ymd(paste(Temp$year, Temp$month, 15, sep = "-") ))
#GEM_raw$Decimal_Date <- lubridate::decimal_date(as.Date(paste(GEM_raw$year, GEM_raw$month, GEM_raw$day, sep = '-')))
#for(i in 1:length(GEM_raw$Decimal_Date)){
# if(is.na(GEM_raw$Decimal_Date[i]) == FALSE){
#  GEM_raw$air_temp_c[i] = Temp$Tmean[which.min(abs(GEM_raw$Decimal_Date[i]- Temp$Serial_date_temp))]
# }
# else{GEM_raw$air_temp_c[i] = 25}
#}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##          4. loop through each EGM measurement and calculate flux         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


data1 = GEM_raw

# These several lines fill NA in atmosphere pressure, estimate missing atmospheric pressure whith temperature-dependent version of the barometric equation (see soilrespiration_auxfunctions)

data1 <- data1 %>%
  mutate(treatment_code=ifelse(is.na(treatment_code),measurement_code,treatment_code))%>%
  mutate(atmp_mb = ifelse(
    is.na(atmp_mb),
    barometric_equation_T(elevation = 399, temp = air_temp_c),
    atmp_mb
  )) %>%
  mutate(codew = paste(!!!rlang::syms(one_measurement_id),sep = "_")
  )
# codew is the unique identifier for your flux measurement, you might want to custimize it and make sure it is one code one flux measurement


# made a unique id for each measurement
uid <- unique(data1$codew)

sub      <- subset(data1, codew == uid[1])
EGM_output <- EGM_flux_recalculation(sub) #to initiate a dataframe
for (i in 1:length(uid)) {
  sub      <- subset(data1, codew == uid[i])
  EGM_output[i, ] <- EGM_flux_recalculation(sub)
  EGM_output$codew[i] <- sub$codew[1]
}
#You might want to double check whether "sub" is a single flux measurement

#  Then we sent each measurement into the calculator, if the calculation        
#  failed, we tried to estimate it from input_f (which is a flux calculated     
#  by EGM it self)                                                              

# the liner model simply fill NA in flux_umolm2sec according to last_inputf (the flux calculated by EGM machine it self)

lm_function <- lm(flux_umolm2sec ~ last_inputf, data = EGM_output)
res <- EGM_output %>%
  left_join(dplyr::distinct(data1[,c(one_measurement_id,'codew')]), by = 'codew') %>%
  mutate(
    flux_umolm2sec_modeled = ifelse(
      is.na(flux_umolm2sec),
      predict(lm_function, .),
      flux_umolm2sec
    ))



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                          5. UNIT conversion                              ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## Corrections and conversions
# add a temperature correction from Sotta et al. 2004 Q10=1.8 and k=0.0613
corrsresA = exp(-0.0695 * (1))
# Convert units umol m2 s-1 to MgC ha month = 1mo=2592000sec, 10000m2=1ha, 1000000umol = 1 mol, 1mol = 12 g, 1000000g=1Mg
convert = (2592000 * 10000 * 12) / (1000000 * 1000000)
convert_year = (2592000 * 10000 * 12) / (1000000 * 1000000) * 12
#The 12))/(1000000*1000000) is from umol to MgC, the 2592000 is from sec to month, the 10000 is for m-2

All_resp <- res %>%
  mutate(MgC_year_per_ha = flux_umolm2sec_modeled * convert_year * corrsresA)%>%
  mutate(date=parse_date_time(paste(year,month,day),"ymd"))




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                    6. Partitioning percentage calculation                ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

scores_na <- function(x, ...) {
  not_na <- !is.na(x)
  scores <- rep(NA, length(x))
  scores[not_na] <- outliers::scores(na.omit(x), ...)
  scores
} # This functions is to add outlier id 

#.............data cleaning - remove bad measurement.............

# The outlier removal method below only work if you have more than two years measurement, i.e. more than 10 records under a codew_collar 
# if you want this, you might keep group_by(codew_collar) to have outlier calculated for each plot independently


#  # the outlier removal idea here by collar is that, for one collar, the       
#  flux should be rather consistent through time, if one of the measurement     
#  is very high compared with other dates, then it is possible that the         
#  measurement is wrong (for example, a mouse dead beneath for that month)                                                        


#  # The second possibility of 'outlier' is that the collar might be placed     
#  on an ant net, or the collar plastic was broken etc, which make the collar   
#  wrong for any date, this will be diagnostic with the pivot table by          
#  comparing with other collars                                                 


All_resp_treatment <- All_resp %>%
  mutate(codew_exp = paste(!!!rlang::syms(One_experiment_id),sep = "_")) %>% 
  mutate(codew_collar = paste(!!!rlang::syms(Collar_id),sep = "_")) %>% 
  group_by(codew_exp) %>% 
  mutate(all_treatment_code = paste(treatment_code, collapse = "~")) %>%
  mutate(number_of_exp_records = length(treatment_code))%>%
  mutate(number_of_unique_exp_flags = length(unique(treatment_code)))%>%
  ungroup()%>% 
  group_by(codew_collar) %>% 
  mutate(Outlier_id = scores_na((MgC_year_per_ha), type = "iqr"))

#  # These codes will plot out some time series of each collars,help you        
#  check outliers. We displaying both flux measurements and corresponding       
#  outliers value, we need to find a reasonable outliers threshold to           
#  identify wrong measurements, normally, we use 1.5 as threshold               


uid<-unique(All_resp_treatment$codew_collar)
dir.create('plots')
for (i in 1:length(uid)) {
  
  wwwwww<-All_resp_treatment%>%
    filter(codew_collar==uid[i])%>%
    ggplot(aes(x=date)) +
    geom_line(aes(y=MgC_year_per_ha),color = 'blue')+
    geom_point(aes(y=MgC_year_per_ha),color = 'blue') +
    geom_line(aes(y=abs(Outlier_id)*10),color = 'red')+
    geom_point(aes(y=abs(Outlier_id)*10),color = 'red') +
    ggtitle(uid[i])+ scale_y_continuous(
      # Features of the first axis
      name = "MgC_year_per_ha",
      # Add a second axis and specify its features
      sec.axis = sec_axis(~./10, name="Outlier id, the larger, the worse"))+ 
    theme_ipsum() +
    theme(
      axis.title.y = element_text(color = 'blue', size=13),
      axis.title.y.right = element_text(color = 'red', size=13)
    ) 
  ggsave(filename = paste0('./plots/',uid[i],'.jpg'),plot =wwwwww )
}

# these codes change outlier into NA

#  Note that there are still some negative values remain, we remove them        
#  here, not in the above step becasuse removing values smaller than one will   
#  bias the outleir calculation                                                 


All_resp_treatment_no_outlier<- All_resp_treatment%>%
  mutate(
    MgC_year_per_ha_no_outlier = ifelse(abs(Outlier_id) < 1.5, MgC_year_per_ha, NA),
    Error = ifelse(
      abs(Outlier_id) < 1.5,
      Error,
      paste0(Error, ' .Outlier detected, replaced with NA')
    ))%>%
  mutate(
    Error = ifelse(
      (MgC_year_per_ha) > 0, 
      Error,
      paste0(Error, ' .calculated flux is negative')
    ),
    MgC_year_per_ha_no_outlier = ifelse((MgC_year_per_ha) > 0, MgC_year_per_ha_no_outlier, NA))

#?????????????????????????IMPORTANT NOTE?????????????????????????
#?????????????????????????IMPORTANT NOTE?????????????????????????
#?????????????????????????IMPORTANT NOTE?????????????????????????
#???????????????????????????????????????????????????????????????????????????????

#  Now you need to view(All_resp_treatment_no_outlier) and check                
#  [all_treatment_code] & [number_of_exp_records] &                             
#  [number_of_unique_exp_flags], in my case, both number must be 5              
#  (con_nor_lit, con_no_lit, my_no_lit, so_no_lit,ml)                           

# [number_of_exp_records] is the number of records under a given codew_exp
# [all_treatment_code] is the treatment codes of records under a given codew_exp
# [number_of_unique_exp_flags] is the length of unique(all_treatment_code)

#???????????????????????????????????????????????????????????????????????????????
#?????????????????????????IMPORTANT NOTE?????????????????????????
#?????????????????????????IMPORTANT NOTE?????????????????????????
#?????????????????????????IMPORTANT NOTE?????????????????????????
The_number_of_treatment_per_experiment = 4
# I have con_nor_lit, con_no_lit, my_no_lit, so_no_lit,ml
# Probably you have more? I am imposing a very strict data check later, the code may check 
# the number of records under One_experiment_id (codew_exp), it must be equal to The_number_of_treatment_per_experiment you defined here
# The below two lines of filter do this

PIVOT_table<-All_resp_treatment_no_outlier%>%
  filter(number_of_exp_records==The_number_of_treatment_per_experiment)%>%
  filter(number_of_unique_exp_flags==The_number_of_treatment_per_experiment)%>%
  mutate(id = codew_exp)%>%
  mutate(avg=MgC_year_per_ha_no_outlier)%>%
  tidyr::pivot_wider(id_cols = id, 
                     names_from = treatment_code, 
                     values_from = c("avg"))
  
#·························IMPORTANT NOTE························?
#·························IMPORTANT NOTE························?
#·························IMPORTANT NOTE························?

#  NOW have a look at your pivot table and think about how to calculate         
#  respiration proportion, as you can see, there are lots of NA, source from    
#  (1) missing in raw data (2) outlier removal                                  


#·························IMPORTANT NOTE························?
#·························IMPORTANT NOTE························?
#·························IMPORTANT NOTE························?

#.............data cleaning - remove bad experiment set up.......

Final_lovely_result<-PIVOT_table%>%
  mutate( Litter_respiration= ( con_nor_lit -  con_no_lit) /  con_nor_lit,
          Heterotrophic_respiration = ( so_no_lit +  con_nor_lit- con_no_lit) /  con_nor_lit,
          Autotrophic_Respiration = ( con_no_lit -  my_no_lit) /  con_nor_lit,
          Mycorrhizae_respiration = ( my_no_lit -  so_no_lit) /  con_nor_lit
  )%>%
  mutate(across(c('Autotrophic_Respiration','Heterotrophic_respiration','Litter_respiration'),~ replace(., (.< 0 | .>1),NA)))
#mutate(Mycorrhizae_respiration= replace(Mycorrhizae_respiration, (Mycorrhizae_respiration< -0.1 | Mycorrhizae_respiration>1),NA))

#Final_lovely_result<-Final_lovely_result[complete.cases(Final_lovely_result),]
# Only experiment with no NA finally survive (any na in any part of respiration will make the whole experiment removed)


summary(Final_lovely_result)
# id            Litter_respiration Heterotrophic_respiration Autotrophic_Respiration
# Length:30          Min.   :0.008736   Min.   :0.1912            Min.   :0.02829        
# Class :character   1st Qu.:0.155301   1st Qu.:0.6769            1st Qu.:0.07377        
# Mode  :character   Median :0.317882   Median :0.8343            Median :0.16570        
#                   Mean   :0.315990   Mean   :0.7721            Mean   :0.22795        
#                   3rd Qu.:0.456143   3rd Qu.:0.9262            3rd Qu.:0.32306        
# Max.   :0.726594   Max.   :0.9717            Max.   :0.80877        
# Mycorrhizae_respiration
# Min.   :-0.09370       
# 1st Qu.:-0.04778       
# Median : 0.03521       
# Mean   : 0.05714       
# 3rd Qu.: 0.08381       
# Max.   : 0.50068  

write.csv(Final_lovely_result,
          file = paste0(census$plot_code[1], "_total_soil_respiration.csv"))

     
     
     #            archived codes, ignore everything here           ~~~
     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     


######################################
Res$sub_plot<-as.numeric(Res$sub_plot)
All_resp <- Res %>%
  mutate(MgC_year_per_ha = Rflux_umolm2sec * convert_year * corrsresA)%>%
  rename(treatment_code=treatment_code_partitioning)%>%
  mutate(Treat_meant_id1= (sub_plot%%5))%>%
  mutate(Treat_meant_id2= (sub_plot%/%5))
  

One_experiment_id = c('plot_code',
                      'Treat_meant_id2',
                       'day',
                      'month',
                      'year')


All_resp_treatment <- All_resp %>%
  mutate(codew_exp = paste(!!!rlang::syms(One_experiment_id),sep = "_")) %>% 
  group_by(codew_exp) %>% 
  mutate(all_treatment_code = paste(treatment_code, collapse = "~")) %>%
  mutate(number_of_exp_records = length(treatment_code))%>%
  mutate(number_of_unique_exp_flags = length(unique(treatment_code)))%>%
  filter(all_treatment_code=="so_no_lit~Control~con_no_lit~my_no_lit~so_no_lit")%>%
  mutate(treatment_code=ifelse(Treat_meant_id1==4,'MS',treatment_code))


PIVOT_table<-All_resp_treatment%>%
  mutate(id = codew_exp)%>%
  mutate(avg=MgC_year_per_ha)%>%
  tidyr::pivot_wider(id_cols = id, 
                     names_from = treatment_code, 
                     values_from = c("avg"))
colnames(PIVOT_table)<-c('id','my_no_lit','con_no_lit','MS','so_no_lit','CTRL')



Final_lovely_result<-PIVOT_table%>%
  mutate( Litter_respiration= ( CTRL -  con_no_lit) /  CTRL,
          Heterotrophic_respiration = ( my_no_lit +  CTRL- con_no_lit) /  CTRL,
          Autotrophic_Respiration = ( con_no_lit -  my_no_lit) /  CTRL,
          Mycorrhizae_respiration = ( my_no_lit -  so_no_lit) /  CTRL
  )%>%
  mutate(across(c('Autotrophic_Respiration','Heterotrophic_respiration','Litter_respiration'),~ replace(., (.< 0 | .>1),NA)))%>%
  mutate(Mycorrhizae_respiration= replace(Mycorrhizae_respiration, (Mycorrhizae_respiration< -0.1 | Mycorrhizae_respiration>1),NA))%>%
  select(id,Litter_respiration:Mycorrhizae_respiration)


Final_lovely_result<-Final_lovely_result[complete.cases(Final_lovely_result),]

