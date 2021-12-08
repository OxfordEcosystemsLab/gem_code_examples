
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                              FINE LITTER FALL                            ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      1. Explanation                       ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~
##  ...Authors----
##~~~~~~~~~~~~~~~~

#  This script was built by Dr Cecilia Giradin
#  Modified by Huanyuan Zhang hyzhang1996@gmail.com,
#  Script was checked by Guillaume Delhaye and Terhi Ruitta
#   2021 August 13 as part of the African data workshop


##~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ important_note  ----
##~~~~~~~~~~~~~~~~~~~~~~~~


# you can add more classification of fine litter fall as new column like:
# needle_g_per_trap, anything_g_per_trap

#  If you say FALSE to Days_interval_recorded, you must run one plot at a      .
#  time, if you say TRUE, you can run all plot in one go                       .
#                                                                              .
#  You should mannually check daysBetween, make sure there are no value like   .
#  1 (1 day between collection is not possible) or 378 (in case you stop your  .
#  experiment for a year)                                                      .
#                                                                              .
#...............................................................................

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##          2. Data Preparation  (what you need to change) ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#### Litter traps analysis script - 25-06-2021 #####
library(tidyverse)
library(zoo)
library(tools)
library(lubridate)
# Set your working directory (the file where your data, code and functions.r are located )
# Be careful to use / and not \
setwd("F:/Side_project/african_data_workshop/gem_code_examples/fine_litter_fall/")

# Load pre prepared functions
source("functions_flf_v2.r")

# Load your data. 
# Since we are in the right directory, we can just write the name of the dataset
data_flf_name<-'flf_20190201_XXX.csv'

# Use the right separator for your data, depends on how you saved the .csv file (comma separator or ; separator)
data_flf <- read.csv(data_flf_name) %>%
  filter(plot_code == 'ABA-00')# for a good daysBetween estimate, you should run one plot at a time, unless you don't need to ask the R script to calculate daysBetween

# Check that the data are correct and that there are no mistakes (the most important step!!)
summary(data_flf)
str(data_flf)
View(data_flf)

## In the flf_test dataframe we have several problems. You probably don't have these
## problems in your data, but this gives you an example of the common problem encountered
## That can be solved easily with tidyverse.
## Here, the first variable is called ??..plot_code instead of plot_code (for some reason)
## -> Let's rename it correctly.
## Further, at the end of the dataframe, we have two rows of NA 
## -> filter by plot for example to remove
data_flf <- data_flf %>% 
  filter(!is.na(plot_code)) 


## First thing to do is to be sure that each
## variable is in the right format. To do that, let's modify the format of each variable
## into the right format for the analyses.
data_flf <- data_flf %>%
  mutate(
    plot_code = as.factor(plot_code),
    year = as.integer(year),
    month = as.integer(month),
    day = as.integer(day),
    sub_plot_code = as.factor(sub_plot_code),
    litterfall_trap_num = as.factor(litterfall_trap_num),
    litterfall_trap_size_m2 = as.numeric(litterfall_trap_size_m2),
    quality_code = as.factor(quality_code),
    comments = as.character(comments))%>%
    mutate(across(ends_with('_g_per_trap'),as.numeric)) 
  

str(data_flf) # OK, the data are in the right format! Let's start processing


Days_interval_recorded = FALSE

#  say True, if you have a column census$DaysBetween, unit in days,
#  say FALSE if you want this script to calculate DaysBetween based on your
#  year month day


#  we try to calculate days interval for each collection by looking
#  for the date of previous collection. if it is
#  not too much trouble, it is good to do this manually in excel because
#  NPP is calculated as roots_weight/days_interval

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            3. From biomass to NPP                        ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# note that the first several rows are NA because we don't know the daysbetween for the first record! 

# the following part do unit conversion, just run through**
### Conversions: flf per ha per month (for each trap)
# Raw data is in g / litter trap = g / 0.25m2
# Convert to ha: *(10000/0.25)
# Convert to Mg: *1 g = 0.000001 Mg
# Convert to C: *0.49

convert_unit =10000*0.000001*0.49*30

if (!Days_interval_recorded) {
  

    All_npp <- data_flf %>%
    mutate(date=parse_date_time(paste(year,month,day),"ymd"))%>% # make a date column
    group_by(plot_code)%>%
    mutate(DaysBetween=get_time_diffs2(date))%>%
    mutate(across(ends_with('_g_per_trap'), ~ .x * convert_unit /DaysBetween/litterfall_trap_size_m2 ))
# Note: At the moment, the code ignores the first collection. 
# In other word, you can put the first day data in the csv file, with total 
# litter fall = 0.
   message('I got days_between from your date, Pls check this:')
   distinct(All_npp[,c('DaysBetween','date')])

}else{
   All_npp <- data_flf %>%
    mutate(date=parse_date_time(paste(year,month,day),"ymd"))%>%
    mutate(across(ends_with('_g_per_trap'), ~ .x * convert_unit /DaysBetween/litterfall_trap_size_m2 ))
}

old_col_name<-colnames(All_npp)
colnames(All_npp)<-str_replace_all(old_col_name,'_g_per_trap','_MgC_ha_month')



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                    4. quality check and unit conversion                  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# The following checks if there are very large time gaps in your data. 
# You can ajust the maximum time gap acceptable by change the value (here 60 days)
if(max(All_npp$DaysBetween,na.rm = TRUE)>60){
  warnings("Check your time interval, there are very large value")}



write.csv(All_npp, file=paste0(data_flf$plot_code[1], "_fine_litter_fall_NPP_finest.csv"))

#There is no standard error because it is the finest resolution. We recommend to calculate standard error with 


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                          5. Organise and visulise                        ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# flf per ha per month (average of all the traps)
# calculate standard error sd/sqrt(length(unique(data3$year)))

data5 = All_npp %>% group_by(plot_code, year, month) %>% 
  dplyr::summarize(
                   totalflf_MgC_ha_month2 = mean(total_litterfall_MgC_ha_month, na.rm = T),

                   sd_totalflf = sd(total_litterfall_MgC_ha_month, na.rm = T)) 


# NPP litterfall in g m-2 mo-1
data5$totalflf_g_m2_mo <- data5$totalflf_MgC_ha_month2 * 0.49 * 10000 * 0.000001

# plot it
plotit = data5

# Reorder X axis so it follows month order
plotit$month <- factor(plotit$month, levels = c(1:12))
plotit %>% group_by(plot_code) %>% arrange(plot_code, month, year) %>% 
  ggplot(data=., aes(month, totalflf_MgC_ha_month2, colour=year)) + geom_point() +
  facet_wrap(~plot_code)

#check outliers
plotit = data5
plotit$month <- factor(plotit$month, levels = c(1:12))
plotit %>% group_by(plot_code) %>% arrange(plot_code, month, year) %>% 
  ggplot(data=., aes(month, totalflf_MgC_ha_month2, colour=year)) + geom_point() 

ggsave(filename = paste0(data_flf$plot_code[1], "_flf.png"))

