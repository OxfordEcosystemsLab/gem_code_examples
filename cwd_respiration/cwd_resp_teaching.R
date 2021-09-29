
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                            NACROMASS RESPIRATION                         ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                1. Information                            ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#  Written by Huanyuan Zhang hyzhang1996@gmail.com
#  I will upscale nacromass respiration bassed on transect area instead of debris
#  surface area, because it is very difficult to estiamte total surface area

#  If you wished to upscale based on total surface area, pls check
#  https://github.com/OxfordEcosystemsLab/GEMcarbon.R/blob/master/coarsewoodres.R


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

#···············································································
#  1. you must think carefully how you are going to upsacle your cwd           ·
#  respiration measurement to plot level. the method used here is very         ·
#  simple, in this plot, people simply measure respiration in the field        ·
#  (similar to soil respiration but only include small cwd pieces in the       ·
#  colar)                                                                      ·
#···············································································



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                             3. data preparation                          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


##~~~~~~~~~~~~~~~~~~~~~~
##···quality check  ----
##~~~~~~~~~~~~~~~~~~~~~~
library(dplyr)
library(ggplot2)
library(lubridate)
library(outliers)

setwd(
  "F:/Side_project/african_data_workshop/General/Dataset examples/cwd_respiration/"
)

source("EGM_flux_recalculation.R")
source("soilrespiration_auxfunctions.R")
source("functions.r")

GEM_raw <-   read.table('cwd_resp_20170710_XXX.csv', sep = ",",
                        header = T)%>%
             filter(plot_code == 'XXX-01')

GEM_raw$plot_code <- as.factor(GEM_raw$plot_code)
GEM_raw$year <- as.numeric(GEM_raw$year)
GEM_raw$month <- as.numeric(GEM_raw$month)
GEM_raw$sub_plot <- as.factor(GEM_raw$sub_plot)
GEM_raw$cwd_transect_num <- as.factor(GEM_raw$cwd_transect_num)
GEM_raw$cwd_num <- as.factor(GEM_raw$cwd_num)
GEM_raw$replica <- as.factor(GEM_raw$replica)
GEM_raw$co2ref_ppm_sec <- as.numeric(GEM_raw$co2ref)
GEM_raw$time <- as.numeric(GEM_raw$time)
GEM_raw$atmp_mb <- as.numeric(GEM_raw$atmp)
GEM_raw$InputF <- as.numeric(GEM_raw$InputF)

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



one_measurement_id = c('plot_code',
                      'sub_plot', 
                      'cwd_transect_num',
                      'day',
                      'month',
                      'year')
#  This combination need to make be a unique flag for each measurement, you
#  can customize yours. For example, if you have only one measurement under
#  sub_plot + date, then you can simply one_measurement_id =               
#  c('subplot','year','month','day')   



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                    4. loop through each EGM measurement                  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data1 = GEM_raw

# These several lines fill NA in atmosphere pressure, estimate missing atmospheric pressure whith temperature-dependent version of the barometric equation (see soilrespiration_auxfunctions)
data1 <- data1 %>%
    mutate(atmp_mb = ifelse(
    is.na(atmp_mb),
    barometric_equation_T(elevation = 550, temp = t),
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
  mutate(MgC_year_per_ha = flux_umolm2sec_no_outlier * convert_year * corrsresA)%
  

# Then you need to think about how to upscale it?
# You can upscale by cwd surface area, or cwd dry weight





