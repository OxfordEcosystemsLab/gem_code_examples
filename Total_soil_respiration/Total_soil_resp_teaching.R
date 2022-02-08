

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                           TOTAL SOIL RESPIRATION                         ----
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


##~~~~~~~~~~~~~~~~~~~~~~~
##  ···important note----
##~~~~~~~~~~~~~~~~~~~~~~~

# run one plot by one plot
# one_measurement_id identify a EGM flux measurement, it is very importnat
# to get the id correct and unique for each measurement

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
  "F:/Side_project/african_data_workshop/gem_code_examples/Total_soil_respiration/"
)

source("EGM_flux_recalculation.R")
source("soilrespiration_auxfunctions.R")
source("functions.r")


GEM_raw <-
  read.table('tot_soil_resp_20190116_XXX.csv',
             sep = ",",
             header = T)

GEM_raw$plot_code <- as.factor(GEM_raw$plot_code)
GEM_raw$year <- as.numeric(GEM_raw$year)
GEM_raw$month <- as.numeric(GEM_raw$month)
GEM_raw$sub_plot <- as.factor(GEM_raw$sub_plot)
GEM_raw$replica <- as.factor(GEM_raw$replica)
GEM_raw$co2ref_ppm_sec <- as.numeric(GEM_raw$co2ref)
GEM_raw$time <- as.numeric(GEM_raw$time)
GEM_raw$atmp_mb <- as.numeric(GEM_raw$atmp)
GEM_raw$InputF <- as.numeric(GEM_raw$InputF)
GEM_raw$treatment_code_partitioning <- as.factor(GEM_raw$treatment_code_partitioning)
GEM_raw$measurement_code <- as.factor(GEM_raw$measurement_code)


##~~~~~~~~~~~~~~~~~~~~~
##···your options  ----
##~~~~~~~~~~~~~~~~~~~~~

elevation = 550 #unit: m #This is site elevation, used to estimate missing atmp
GEM_raw$collar_diameter_cm = 12   #[cm] collar diameter
GEM_raw$collar_height_cm = 5      #[cm] collar height
GEM_raw$air_temp_c = 12

#  The above step just give every record the same parameter,
#  you might want to specify
#  collar height/diameter and temperature for each EGM record, in
#  which case, you should make another column in your dataset, OR,
#  you can try to left_join and bring information from other
#  datasets (like meteorological station)

GEM_raw <- GEM_raw %>%
  filter(plot_code == 'XXX-01')
one_measurement_id = c(
  'plot_code',
  'sub_plot',
  'collar_number',
  'replica',
  'day',
  'month',
  'year',
  'measurement_code',
  'treatment_code_partitioning')

#  This combination need to make be a unique flag for each measurement, you
#  can customize yours. For example, if you have only one measurement under
#  sub_plot + date, then you can simply one_measurement_id =               
#  c('subplot','year','month','day')                                       
#  One measurement will be calculated into one flux value

#···············································································
#  normally you don't need to change anything below, but you might want to     ·
#  read and double-check that it works for your site                           ·
#···············································································
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                    4. loop through each EGM measurement                  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data1 = GEM_raw



# These several lines fill NA in atmosphere pressure, estimate missing atmospheric pressure whith temperature-dependent version of the barometric equation (see soilrespiration_auxfunctions)

data1 <- data1 %>%
  mutate(atmp_mb = ifelse(
     is.na(atmp_mb),
     barometric_equation_T(elevation = 550, temp = air_temp_c),
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
# last_inputf is the last input_f reported by EGM
# last time is the time the EGM used to measure this flux
# flux_umolm2sec  is the calculated flux
#Then we need to pull in information from the raw daraset

scores_na <- function(x, ...) {
  not_na <- !is.na(x)
  scores <- rep(NA, length(x))
  scores[not_na] <- outliers::scores(na.omit(x), ...)
  scores
} # This is used to remove outliers

lm_function <- lm(flux_umolm2sec ~ last_inputf, data = EGM_output)
res <- EGM_output %>%
  left_join(dplyr::distinct(data1[, c(one_measurement_id,'codew')]), by = 'codew') %>%
  mutate(
    flux_umolm2sec_modeled = ifelse(is.na(flux_umolm2sec),predict(lm_function, .),flux_umolm2sec),
    Outlier_id = scores_na((flux_umolm2sec_modeled), type = "iqr"),
    flux_umolm2sec_no_outlier = ifelse(abs(Outlier_id) < 2, flux_umolm2sec_modeled, NA),
    Error = ifelse(abs(Outlier_id) < 2,Error,
      paste0(Error, ' .Outlier detected, replaced with NA')
    )
  )
# This is a process to remove outliers, we first replace missing value (due to 
# failure in EGM_flux_recalculation) with an estimate from input_f,
# then if we found outliers in the overall flux measurement
# normally it is because the measurement is problematic, we flagged it with NA

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                          5. Summarize and visualize                      ----
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

All_resp_per_subplot <- All_resp %>%
  group_by(sub_plot) %>%
  summarise(
    MgC_year_per_ha_mean = mean(MgC_year_per_ha, na.rm = T),
    MgC_year_per_ha_ste = standard_error_calc(MgC_year_per_ha, na.rm =T),
    start_year = min(year),
    end_year = max(year),
    number_of_flux_measurement = sum(!is.na(MgC_year_per_ha))
  )

ggplot(All_resp, aes(x = as.factor(year), y = MgC_year_per_ha)) +
  geom_boxplot(fill = "slateblue", alpha = 0.2) +
  xlab("Total_soil_resp")

ggplot(All_resp, aes(x = as.factor(sub_plot), y = flux_umolm2sec_no_outlier)) +
  geom_boxplot(fill = "slateblue", alpha = 0.2) +
  xlab("Total_soil_resp")

#Looks awful? not to worry, I have something nicer for reviewer:
ggplot(All_resp_per_subplot,
       aes(x = sub_plot, y = MgC_year_per_ha_mean)) +
  geom_bar(stat = "identity",
           position = position_dodge(),
           fill = 'gray') +
  geom_errorbar(
    aes(
      ymin = MgC_year_per_ha_mean - MgC_year_per_ha_ste,
      ymax = MgC_year_per_ha_mean + MgC_year_per_ha_ste
    ),
    width = .2,
    position = position_dodge(.9)
  ) +
  theme_minimal()


write.csv(All_resp,
          file = paste0(GEM_raw$plot_code[1], "_total_soil_respiration_finest.csv"))

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
