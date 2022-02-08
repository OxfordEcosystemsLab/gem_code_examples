
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                              STEM RESPIRATION                            ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                1. Information                            ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#  script was originally designed by Dr Cecile Giradin, Modified by
#  Huanyuan Zhang hyzhang1996@gmail.com, 2021 August 13 as part of 
#  the African data workshop                                       



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                2. Meta data                              ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ···How does this script work?  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Read comments first and come back here to write a paragraph (in French if you
# prefer) to summaries how was branch NPP calculated

# This script calculate branch NPP from deadwood transects or quadrats....


##~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ···Necessary input  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~


#············dbh dataset (census data in long format)············


#···············································································
#  'data.frame': 2870 obs. of 46 variables:                                    ·
#  $ tree_tag : chr "1" "1.1" "10" "11" ...                                    ·
#  $ dbh : num 231 131 221 222 116 259 173 279 111 139 ...                     ·
#  $ year : num 2012 2012 2012 2012 2012 ...                                   ·
#  $ month : num 3 3 3 3 3 3 3 3 3 3 ...                                       ·
#  $ day : num 16 16 16 16 16 16 16 16 16 16 ...                               ·
#···············································································


#····················stem respiration dataset····················

#···············································································
#  'data.frame': 23040 obs. of 22 variables:                                   ·
#  $ plot_code : Factor w/ 1 level "BOB-01": 1 1 1 1 1 1 1 1 1 1 ...           ·
#  $ sub_plot : Factor w/ 42 levels "1","2","3","4",..: 1 1 1 1 1 1 1 ...      ·
#  $ tree_tag : chr "4" "4" "4" "4" ...                                        ·
#  $ replica : Factor w/ 1 level "1": NA NA NA NA NA NA NA NA NA NA ...        ·
#  $ year : num 2016 2016 2016 2016 2016 ...                                   ·
#  $ egm_measurement: int NA NA NA NA NA NA NA NA NA NA ...                    ·
#  $ day : int 2 2 2 2 2 2 2 2 2 2 ...                                         ·
#  $ month : num 6 6 6 6 6 6 6 6 6 6 ...                                       ·
#  $ co2ref_ppm_sec : num 475 480 482 480 482 483 485 479 482 486 ...          ·
#  $ time : num 0 4 9 14 19 24 28 33 38 43 ...                                 ·
#  $ InputF : num 0 0 0 0.31 0.28 0.25 0.26 0.14 0.12 0.13 ...                 ·
#  $ atmp_mb : int 993 993 993 993 993 993 993 993 993 993 ...                 ·
#···············································································

##~~~~~~~~~~~~~~~~~~~~~~~
##  ···important note----
##~~~~~~~~~~~~~~~~~~~~~~~


#···············································································
#  1. run one plot at a time                                                   ·
#                                                                              ·
#  2. Please check table "res" and column dbh, make sure you have tree tag     ·
#  well matched, so that dbh could be pulled from stem npp dataset             ·
#                                                                              ·
#  3. You will need to convert unit if your plot is not 1 ha                   ·
#                                                                              ·
#  4. The flux measurement of stem respiration is highly variable from         ·
#  month to month (strong seasonale and annual variation), be careful with     ·
#  the year which has some months missing                                      ·
#                                                                              ·
#  5. The "uid" includes replica and subplot, not egm_measurement. so in       ·
#  "All_resp", the unique rule is tree_tag + date + replica + subplot.         ·
#  However, I did not consider replica and subplot when avaeraging results,    ·
#  anyway, you should try to get your own average from All_resp                ·
#···············································································



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

setwd("F:/Side_project/african_data_workshop/gem_code_examples/stem_resp")
source("allometric_equations_2014.R")
source("EGM_flux_recalculation.R")
source("soilrespiration_auxfunctions.R")
source("functions.r")

#······················stem respiration data·····················

GEM_raw <- read.table('F:/Oxford/Chapter_two/stem_respiration/input/ANK_stem_resp/stem_resp_all_ghana_HYZ.csv', sep = ",",header = T)
GEM_raw<-GEM_raw%>%
  filter(plot_code=='BOB-01')

GEM_raw$plot_code <- as.factor(GEM_raw$plot_code)
GEM_raw$year <- as.numeric(GEM_raw$year)
GEM_raw$month <- as.numeric(GEM_raw$month)
GEM_raw$sub_plot <- as.factor(GEM_raw$sub_plot)
GEM_raw$tree_tag <- as.character(GEM_raw$tree_tag)
GEM_raw$replica <- as.factor(GEM_raw$replica)
GEM_raw$co2ref_ppm_sec <- as.numeric(GEM_raw$co2ref_ppm_sec)
GEM_raw$time <- as.numeric(GEM_raw$time)
GEM_raw$atmp_mb <- as.numeric(GEM_raw$atmp_mb)
GEM_raw$InputF <- as.numeric(GEM_raw$InputF)
if (any(is.na(GEM_raw$tree_tag))) {
  warning('NA detected in your tree tag....')
}


#··························grab DBH data·························

#Reading Census data in long format, we want dbh to calculate wood surface
census<- read.csv('BOB_01_census_data_long_format.csv', sep = ",", header = TRUE)
census$year<- as.numeric(census$year)
census$month<- as.numeric(census$month)
census$day<- as.numeric(census$day)
census$tree_tag<- as.character(census$tree_tag)
census$dbh<- as.numeric(census$dbh)
census<-census%>%
  filter(plot_code=='BOB_01')

census$census_date <- lubridate::decimal_date(as.Date(paste(census$year, census$month, census$day, sep = '-')))


##~~~~~~~~~~~~~~~~~~~~~
##···your options  ----
##~~~~~~~~~~~~~~~~~~~~~



elevation=550 #unit: m #This is site elevation, used to estimate missing atmp

GEM_raw$collar_diameter_cm = 12   #[cm] collar diameter
GEM_raw$collar_height_cm = 5      #[cm] collar height
# Here I am assuming you use the same type of collar for the whole plot?

GEM_raw$air_temp_c = 27 #if we have no information about temperature, I assume everyday is 27 (my favorite water temperature for scuba diving)
# You can grab temperature from other data set, or from meteological station, check archived information for codes to help you

#  Well, just an example (joke) above, you might want to specify  
#  collar height/diameter and temperature for each EGM record, in 
#  which case, you should make another column in your dataset, OR,
#  you can try to left_join and bring information from other      
#  datasets (like meteorological station)  



#···············································································
#  normally you don't need to change anything below, but you might want to     ·
#  read and double-check that it works for your site                           ·
#···············································································

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##            4. get air temperature and pressure into the dataset          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data1 = GEM_raw

# These several lines fill NA in atmosphere pressure, estimate missing atmospheric pressure whith temperature-dependent version of the barometric equation (see soilrespiration_auxfunctions)
data1<-data1%>%
  mutate(atmp_mb=ifelse(is.na(atmp_mb),barometric_equation_T(elevation=550, temp=air_temp_c),atmp_mb))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                    5. loop through each EGM measurement                  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


data1$codew <- paste(data1$tree_tag, data1$replica, data1$day, data1$month, data1$year,data1$sub_plot, sep=".")
# made a unique id for each measurement, so that we can loop through
uid <- unique(data1$codew)

sub      <- subset(data1, codew == uid[1]) 
EGM_output<-EGM_flux_recalculation(sub) #to create a dataframe
for (i in 1:length(uid)){
  sub      <- subset(data1, codew == uid[i]) 
  EGM_output[i,]<-EGM_flux_recalculation(sub)
  EGM_output$codew[i]<-sub$codew[1]
}

#Then we need to pull in information from the raw daraset

scores_na <- function(x, ...) {
  not_na <- !is.na(x)
  scores <- rep(NA, length(x))
  scores[not_na] <- outliers::scores(na.omit(x), ...)
  scores
}

lm_function<-lm(flux_umolm2sec~last_inputf,data=EGM_output)
res<-EGM_output%>%
  left_join(dplyr::distinct(data1[,c('codew','tree_tag','day','month','year','replica','sub_plot','plot_code')]),by='codew')%>%
  mutate(census_date= lubridate::decimal_date(as.Date(paste(year, month, day, sep = '-'))))%>%
  mutate(flux_umolm2sec_modeled=ifelse(is.na(flux_umolm2sec),predict(lm_function,.),flux_umolm2sec),
         Outlier_id=scores_na((flux_umolm2sec_modeled),type="iqr"),
         flux_umolm2sec_no_outlier=ifelse(abs(Outlier_id)<2,flux_umolm2sec_modeled,NA))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      6. Grab dbh from another dataset                    ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data_census<-census

res$census_date <- lubridate::decimal_date(as.Date(paste(res$year, res$month, res$day, sep = '-')))

for(i in 1:length(res$tree_tag)){
  if(is.na(match(res$tree_tag[i], data_census$tree_tag)) == FALSE){
    try(subset_census <- subset(data_census, data_census$tree_tag == res$tree_tag[i]))
    if(is.na(res$census_date[i]) == FALSE){
      subset_census<-subset_census%>%filter(!dbh==0)
      res$dbh[i] <- subset_census$dbh[which.min(abs(res$census_date[i]-subset_census$census_date))]
      #print(i)
    }else{res$dbh[i] <- NA}
  }else{res$dbh[i] <- NA}
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                          7. Summarize and visualize                      ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Convert units umol m-2 s-1 to MgC ha month = 1mo=2592000sec, 10000m2=1ha, 1000000umol = 1 mol, 1mol = 12 g, 1000000g=1Mg
#The 12))/(1000000*1000000) is from umol to MgC, the 2592000 is from sec to month
convert = ((2592000*12))/(1000000*1000000)          #from umol s-1 to [MgC month^-1] 
convert_year=convert*12 #From umol s-1 to [MgC year^-1] 

#························finese resolution·······················

All_resp<-res%>%
  mutate(stem_area=Chambers2004_surfaceArea(dbh/10), #[m^2] Stem Area per tree, , input dbh must be in [cm]!!!!!! 
         flux_umol_sec_per_stem=stem_area*flux_umolm2sec_no_outlier)

#·························per stem table·························


All_resp_per_stem<-All_resp%>%
  group_by(tree_tag)%>%
  summarise(flux_umolm2sec_per_stem_mean=mean(flux_umol_sec_per_stem,na.rm=T),
            flux_umolm2sec_per_stem_ste=standard_error_calc(flux_umol_sec_per_stem,na.rm=T),
            start_year=min(year),
            end_year=max(year),
            dbh=mean(dbh,na.rm=T),
            number_of_flux_measurement = sum(!is.na(flux_umol_sec_per_stem)))
# you can see lots of NA in flux_umolm2sec_per_stem_mean, this is normally because the code could not find dbh, or the flux measurement failed, you see more NA in standard error, this is because some tree has been measured once


#·························per year table························



#···············································································
#  pls note that on plot level, in order to match up with stem npp, we are     ·
#  calculating it by average all flux measurement, bypassing tree tag          ·
#  matching, otherwise, tree_tag does not match problem will remove some       ·
#  trees in census                                                             ·
#···············································································



Total_stem_area<-census%>%
  filter(dbh>0)%>%
  mutate(stem_area=Chambers2004_surfaceArea(dbh/10))%>% #[m^2] Stem Area per tree, , input dbh must be in [cm]!!!!!! 
  group_by(year)%>%
  summarise(total_stem_area_per_year = sum(stem_area,na.rm=T))

Per_year_table<-All_resp%>%
  group_by(year)%>%
  summarise(flux_umolm2sec_per_year_mean=mean(flux_umolm2sec_no_outlier,na.rm=T),
            flux_umolm2sec_per_year_ste=standard_error_calc(flux_umolm2sec_no_outlier,na.rm=T),
            number_of_flux_measurement = sum(!is.na(flux_umolm2sec_no_outlier)))%>%
  left_join(.,Total_stem_area,by='year')%>%
  mutate(MgC_year_per_ha=flux_umolm2sec_per_year_mean*total_stem_area_per_year*convert_year)

#·························per plot table························

avaerage_stem_area_m2=mean(Total_stem_area$total_stem_area_per_year)
avaerage_flux_=mean(All_resp$flux_umolm2sec_no_outlier,na.rm=T)
plot_level_result=avaerage_stem_area_m2*avaerage_flux_*convert_year


ggplot(All_resp, aes(x=as.factor(year), y=flux_umolm2sec_no_outlier)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("cyl")

ggplot(All_resp, aes(x=as.factor(month), y=flux_umolm2sec_no_outlier)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("cyl")

#···············································································
#  only All_resp is reasonable output, you can see that I have different       ·
#  algorithm for per plot and per date, because you need to consider           ·
#  date-matching, tree-tag-matching between respiration dataset and dbh        ·
#  dataset, you should also consider the great seasonal variation of stem      ·
#  respiration flux, you must check the calculation above before you           ·
#  publish your plot-level results                                             ·
#···············································································



write.csv(All_resp,file = paste0(census$plot_code[1],"_stem_respiration.csv"))



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            Archived information                          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ···Get temperature from other dataset----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#data1 is your stem_resp dataset
data1$Decimal_Date <- lubridate::decimal_date(as.Date(paste(data1$year, data1$month, data1$day, sep = '-')))
# you first convert the format of date

#Temp is a temperature record, having Tmean as averaged temperature
#also should have year month day
Temp <- read.table("input/KOG_gapfill_erai_monthly_1979_2017.csv", sep = ",", header = TRUE)
Temp$Serial_date_temp <- lubridate::decimal_date(lubridate::ymd(paste(Temp$year, Temp$month, Temp$day, sep = "-") ))

#the for loop below will try to match the dataset up and fill in temperature
for(i in 1:nrow(data1)){
  if(is.na(data1$Decimal_Date[i]) == FALSE){
    data1$air_temp_c[i] = Temp$Tmean[which.min(abs(data1$Decimal_Date[i]- Temp$Serial_date_temp))]
  }
  else{data1$air_temp_c[i] = 25}
}

##~~~~~~~~~~~~~~~~~~~
##  ···Other info----
##~~~~~~~~~~~~~~~~~~~

## someone propose a temperature correction
# add a temperature correction from Sotta et al 2004 Q10=1.8 and k=0.0613
tempcorr = exp(-0.0695*(1))   # However, I do not know where to use this

