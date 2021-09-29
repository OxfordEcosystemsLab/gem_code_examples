

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


##~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ···Necessary input  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~


#···············································································
#  We have: con_no_lit, CTRL, my_no_lit, so_no_lit                             ·
#                                                                              ·
#  con_nor_lit = control & normal litterfall,                                  ·
#  con_no_lit = control & no litterfall,                                       ·
#  con_doub_lit = control & double litterfall,                                 ·
#  my_nor_lit = mycorrhizae & normal litterfall,                               ·
#  my_no_lit = mycorrhizae & no litterfall,                                    ·
#  my_doub_lit = mycorrhizae & double litterfall,                              ·
#  so_nor_lit = soil & normal litterfall,                                      ·
#  so_no_lit = soil & no litterfall,                                           ·
#  so_doub_lit = soil & double litterfall.                                     ·
#  + ADD ml = mineral layer.                                                   ·
#                                                                              ·
#  So: we have:                                                                ·
#  total soil respiration = CTRL                                               ·
#  Litter respiration = (CTRL - con_no_lit) / CTRL                             ·
#  Heterotrophic respiration = (so_no_lit + Litter respiration) / CTRL         ·
#  Autotrophic Respiration = (con_no_lit - so_no_lit) / CTRL                   ·
#  Mycorrhizae respiration = (so_no_lit - my_no_lit) / CTRL                    ·
#                                                                              ·
#  (so_no_lit is larger than my_no_lit for my sites, so info in                ·
#     Rsoil_partitioning_20180307.R is wrong)                                  ·
#···············································································


##~~~~~~~~~~~~~~~~~~~~~~~
##  ···important note----
##~~~~~~~~~~~~~~~~~~~~~~~

1. You have to customize chapter 3 nad chapter 6, because treatment code is not standardized for every plot, make sure it will work for you

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

setwd(
  "F:/Side_project/african_data_workshop/General/Dataset examples/Partitioning_soil_respiration/"
)

source("EGM_flux_recalculation.R")
source("soilrespiration_auxfunctions.R")
source("functions.r")


GEM_raw <-   read.table('part_soil_resp_20180327_XXX.csv', sep = ",",
             header = T)%>%
  filter(plot_code == 'XXX-01')

GEM_raw$plot_code <- as.factor(GEM_raw$plot_code)
GEM_raw$year <- as.numeric(GEM_raw$year)
GEM_raw$month <- as.numeric(GEM_raw$month)
GEM_raw$sub_plot <- as.factor(GEM_raw$sub_plot)
GEM_raw$replica <- as.factor(GEM_raw$replica)
GEM_raw$co2ref_ppm_sec <- as.numeric(GEM_raw$co2ref)
GEM_raw$time <- as.numeric(GEM_raw$time)
GEM_raw$atmp_mb <- as.numeric(GEM_raw$atmp)
GEM_raw$InputF <- as.numeric(GEM_raw$InputF)
GEM_raw$treatment_code <- as.factor(GEM_raw$treatment_code)
GEM_raw$measurement_code <- as.factor(GEM_raw$measurement_code)



##~~~~~~~~~~~~~~~~~~~~~
##···your options  ----
##~~~~~~~~~~~~~~~~~~~~~

elevation = 550 #unit: m #This is site elevation, used to estimate missing atmp
GEM_raw$collar_diameter_cm = 12   #[cm] collar diameter
GEM_raw$collar_height_cm = 5      #[cm] collar height
GEM_raw$air_temp_c = 12
#  Unlike most British I don't like cold drink. But If I have
#  option, I want lemonade at 12 degree!


#  Well, just an example (joke) above, you might want to specify
#  collar height/diameter and temperature for each EGM record, in
#  which case, you should make another column in your dataset, OR,
#  you can try to left_join and bring information from other
#  datasets (like meteorological station)



One_experiment_id = c('plot_code',
                      'sub_plot', 
                      'day',
                      'month',
                      'year')
# One experiment id is a unique id for each experiment, a experiment is a group of C1 S1 S2 S3
# Normally, according to GEM RAINFOR manual, each subplot should have maximum one experiment. 

The_number_of_treatment_per_experiment = 4
# I have CTRL, con_no_lit, my_no_lit, so_no_lit
# Probably you have more? I am imposing a very strict data check later, the code will check 
# the number of records under One_experiment_id, it must be equal to The_number_of_treatment_per_experiment you defined here


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                    4. loop through each EGM measurement                  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data1 = GEM_raw

one_measurement_id = c(
  'plot_code',
  'sub_plot',
  'collar_number',
  'replica',
  'day',
  'month',
  'year',
  'measurement_code',
  'treatment_code')

#  This combination need to make be a unique flag for each measurement, you
#  can customize yours. For example, if you have only one measurement under
#  sub_plot + date, then you can simply one_measurement_id =               
#  c('subplot','year','month','day')                                       


# These several lines fill NA in atmosphere pressure, estimate missing atmospheric pressure whith temperature-dependent version of the barometric equation (see soilrespiration_auxfunctions)

data1 <- data1 %>%
  mutate(treatment_code=ifelse(is.na(treatment_code),measurement_code,treatment_code))%>%
  mutate(atmp_mb = ifelse(
    is.na(atmp_mb),
    barometric_equation_T(elevation = elevation, temp = air_temp_c),
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
#Then we need to pull in information from the raw daraset

scores_na <- function(x, ...) {
  not_na <- !is.na(x)
  scores <- rep(NA, length(x))
  scores[not_na] <- outliers::scores(na.omit(x), ...)
  scores
}

lm_function <- lm(flux_umolm2sec ~ last_inputf, data = EGM_output)
res <- EGM_output %>%
  left_join(dplyr::distinct(data1[,c(one_measurement_id,'codew')]), by = 'codew') %>%
  mutate(
    flux_umolm2sec_modeled = ifelse(
      is.na(flux_umolm2sec),
      predict(lm_function, .),
      flux_umolm2sec
    ),
    Outlier_id = scores_na((flux_umolm2sec_modeled), type = "iqr"),
    flux_umolm2sec_no_outlier = ifelse(abs(Outlier_id) < 2, flux_umolm2sec_modeled, NA),
    Error = ifelse(
      abs(Outlier_id) < 2,
      Error,
      paste0(Error, ' .Outlier detected, replaced with NA')
    )
  )

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
  mutate(MgC_year_per_ha = flux_umolm2sec_no_outlier * convert_year * corrsresA)




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                    6. Partitioning percentage calculation                ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

All_resp_treatment <- All_resp %>%
  mutate(codew_exp = paste(!!!rlang::syms(One_experiment_id),sep = "_")) %>% 
  group_by(codew_exp) %>% 
  mutate(all_treatment_code = paste(treatment_code, collapse = "~")) %>%
  mutate(number_of_exp_records = length(treatment_code))%>%
  mutate(number_of_unique_exp_flags = length(unique(treatment_code)))

#·························IMPORTANT NOTE·························
#·························IMPORTANT NOTE·························
#·························IMPORTANT NOTE·························
#···············································································
#  Now you need to view(All_resp_treatment) and check [all_treatment_code]     ·
#  & [number_of_exp_records] & [number_of_unique_exp_flags], think about       ·
#  how you could calculate soil partitioning percentage? Then we will loop     ·
#  through each experiment                                                     ·
#···············································································
#·························IMPORTANT NOTE·························
#·························IMPORTANT NOTE·························
#·························IMPORTANT NOTE·························

PIVOT_table<-All_resp_treatment%>%
  filter(number_of_exp_records==10)%>%
  filter(number_of_unique_exp_flags==10)%>%
  mutate(id = paste(plot_code, year,month, day, sep = "_"))%>%
  mutate(avg=MgC_year_per_ha)%>%
  tidyr::pivot_wider(id_cols = id, 
              names_from = treatment_code, 
              values_from = c("avg"))
#·························IMPORTANT NOTE·························
#·························IMPORTANT NOTE·························
#·························IMPORTANT NOTE·························
#···············································································
#  NOW have a look at your pivot table and think about how to calculate        ·
#···············································································
#·························IMPORTANT NOTE·························
#·························IMPORTANT NOTE·························
#·························IMPORTANT NOTE·························

Final_lovely_result<-PIVOT_table%>%
  mutate(avg_Litter_respiration= (avg_CTRL - avg_con_no_lit) / avg_CTRL,
         avg_Heterotrophic_respiration = (avg_my_no_lit + avg_Litter_respiration) / avg_CTRL,
         avg_Autotrophic_Respiration = (avg_con_no_lit - avg_my_no_lit) / avg_CTRL,
         avg_Mycorrhizae_respiration = (avg_my_no_lit - avg_so_no_lit) / avg_CTRL
  )





write.csv(Final_lovely_result,
          file = paste0(census$plot_code[1], "_total_soil_respiration.csv"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            Archived information                          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ···Get temperature from other dataset----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#data1 is your stem_resp dataset
data1$Decimal_Date <-
  lubridate::decimal_date(as.Date(paste(data1$year, data1$month, data1$day, sep = '-')))
# you first convert the format of date

#Temp is a temperature record, having Tmean as averaged temperature
#also should have year month day
Temp <-
  read.table(
    "input/KOG_gapfill_erai_monthly_1979_2017.csv",
    sep = ",",
    header = TRUE
  )
Temp$Serial_date_temp <-
  lubridate::decimal_date(lubridate::ymd(paste(Temp$year, Temp$month, Temp$day, sep = "-")))

#the for loop below will try to match the dataset up and fill in temperature
for (i in 1:nrow(data1)) {
  if (is.na(data1$Decimal_Date[i]) == FALSE) {
    data1$air_temp_c[i] = Temp$Tmean[which.min(abs(data1$Decimal_Date[i] - Temp$Serial_date_temp))]
  }
  else{
    data1$air_temp_c[i] = 25
  }
}
