
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

# This script assumes that the size of litter fall trap is 0.25 m2
# you can run all plots at once
# Pls check the time interval calculated by the script is correct

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##          2. Data Preparation  (what you need to change) ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#### Litter traps analysis script - 25-06-2021 #####
library(tidyverse)
library(zoo)
library(tools)

# Set your working directory (the file where your data, code and functions.r are located )
# Be careful to use / and not \
setwd("F:/Side_project/african_data_workshop/General/Dataset examples/fine_litter_fall/")

# Load pre prepared functions
source("functions.r")
source("flf_core.r")
# Load your data. 
# Since we are in the right directory, we can just write the name of the dataset
data_flf_name<-'flf_20190201_XXX.csv'

# Use the right separator for your data, depends on how you saved the .csv file (comma separator or ; separator)
data_flf <- read.csv(data_flf_name) 

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


## OK, so the data are uploaded in R. First thing to do is to be sure that each
## variable is in the right format. To do that, let's modify the format of each variable
## into the right format for the analyses.
data_flf <- data_flf %>%
  mutate(
    plot_code = as.factor(plot_code),
    year = as.integer(year),
    month = as.integer(month),
    day = as.integer(day),
    litterfall_trap_num = as.factor(litterfall_trap_num),
    litterfall_trap_size_m2 = as.numeric(litterfall_trap_size_m2),
    leaves_g_per_trap = as.numeric(leaves_g_per_trap),
    twigs_g_per_trap = as.numeric(twigs_g_per_trap),
    flowers_g_per_trap = as.numeric(flowers_g_per_trap),
    fruits_g_per_trap = as.numeric(fruits_g_per_trap),
    seeds_g_per_trap = as.numeric(seeds_g_per_trap),
    bromeliads_g_per_trap = as.numeric(bromeliads_g_per_trap),
    epiphytes_g_per_trap = as.numeric(epiphytes_g_per_trap),
    other_g_per_trap = as.numeric(other_g_per_trap),
    palm_leaves_g_per_trap = as.numeric(palm_leaves_g_per_trap),
    palm_flowers_g_per_trap = as.numeric(palm_flowers_g_per_trap),
    palm_fruits_g_per_trap = as.numeric(palm_fruits_g_per_trap),
    total_litterfall_g_per_trap = as.numeric(total_litterfall_g_per_trap),
    quality_code = as.factor(quality_code),
    comments = as.character(comments)
  )
str(data_flf) # OK, the data are in the right format! Let's start processing



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            3. From biomass to NPP                        ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Tidy the data a little bit more
data_flf2 = data_flf %>% dplyr::rename(
  plot = plot_code,
  num = litterfall_trap_num,
  leaves = leaves_g_per_trap,
  twigs = twigs_g_per_trap,
  flowers = flowers_g_per_trap,
  fruits = fruits_g_per_trap,
  brom = bromeliads_g_per_trap,
  epi = epiphytes_g_per_trap,
  other = other_g_per_trap
) %>% # we just rename them for simplicity
  dplyr::mutate(
    seeds = NA,
    date = as.Date(
      paste(data_flf$year, data_flf$month, data_flf$day, sep = "-"),
      format = "%Y-%m-%d"
    ),# we convert date into a timestamp in R
    date = as.POSIXct(date),
    total = select(., leaves:palm_fruits_g_per_trap) %>% apply(1, sum, na.rm = TRUE)
  )# we sum up column from leaves to palm_fruits. This will be our total litter fall

# In some rows, only total litterfall is recorded, so the sum up would not work
total_only = data_flf2$total == 0 & !is.na(data_flf2$total_litterfall_g_per_trap) #where our sum up is 0 and there is valid number in column "total litter fall"

data_flf2[total_only, ]$total = data_flf2[total_only, ]$total_litterfall_g_per_trap #Replace the value where the situation happen 

# Rough data quality check
# Remove outliers with totalf > 1500
data_flf2$total[which(data_flf2$total>1500)] <- NA   

# remove implausible totallf (negative litter)
data_flf2$total[which(data_flf2$total<0)]   <- NA   

# Note: At the moment, the code ignores the first collection. 
# In other word, you can put the first day data in the csv file, with total 
# litter fall = 0.
data_flf2$codeb <- paste(data_flf2$plot, data_flf2$num, sep=".") 
data_flf2$codew <- paste(data_flf2$plot, data_flf2$num, data_flf2$year, data_flf2$month, data_flf2$day, sep=".") #these two lines just make unique identifier. check the instruction file for information about uniqueness

data2<-Calculate_flf_npp(data_flf2)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                    4. quality check and unit conversion                  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# The following checks if there are very large time gaps in your data. 
# You can ajust the maximum time gap acceptable by change the value (here 60 days)
if(max(data2$meas_int_days)>60){
  warnings("Check your time interval, there are very large value")}

# the following part do unit conversion, just run through**
### Conversions: flf per ha per month (for each trap)
# Raw data is in g / litter trap = g / 0.25m2
# Convert to ha: *(10000/0.25)
# Convert to Mg: *1 g = 0.000001 Mg
# Convert to C: *0.49
data3 = data2 %>% mutate(leavesflf_MgC_ha_month = (((bleavesflf_g_trap_day*(10000/0.25))*0.000001)*0.49)*30, 
                         twigsflf_MgC_ha_month   = (((btwigs*(10000/0.25))*0.000001)*0.49)*30,
                         flowersflf_MgC_ha_month = (((bflowers*(10000/0.25))*0.000001)*0.49)*30,
                         fruitsflf_MgC_ha_month  = (((bfruits*(10000/0.25))*0.000001)*0.49)*30,
                         bromflf_MgC_ha_month    = (((bbrom*(10000/0.25))*0.000001)*0.49)*30,
                         epiflf_MgC_ha_month     = (((bepi*(10000/0.25))*0.000001)*0.49)*30,
                         otherflf_MgC_ha_month   = (((bother*(10000/0.25))*0.000001)*0.49)*30,
                         totalflf_MgC_ha_month   = (((btotal*(10000/0.25))*0.000001)*0.49)*30,
                         plot_code = plot_code,
                         num = num,
                         year = year, 
                         month = month,
                         collectiondate = collectiondate)

data3 = na_if(data3, Inf)

write.csv(data3, file=paste0(tools::file_path_sans_ext(basename(data_flf_name)), "_fine_litter_fall_NPP_finest.csv"))

#There is no standard error because it is the finest resolution. We recommend to calculate standard error with 


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                          5. Organise and visulise                        ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# flf per ha per month (average of all the traps)
# calculate standard error sd/sqrt(length(unique(data3$year)))

data5 = data3 %>% group_by(plot_code, year, month) %>% 
  dplyr::summarize(leavesflf_MgC_ha_month2 = mean(leavesflf_MgC_ha_month, na.rm = T),
                   twigsflf_MgC_ha_month2 = mean(twigsflf_MgC_ha_month, na.rm = T),
                   flowersflf_MgC_ha_month2 = mean(flowersflf_MgC_ha_month, na.rm = T),
                   fruitsflf_MgC_ha_month2 = mean(fruitsflf_MgC_ha_month, na.rm = T),
                   bromflf_MgC_ha_month2 = mean(bromflf_MgC_ha_month, na.rm = T),
                   epiflf_MgC_ha_month2 = mean(epiflf_MgC_ha_month, na.rm = T),
                   otherflf_MgC_ha_month2 = mean(otherflf_MgC_ha_month, na.rm = T),
                   totalflf_MgC_ha_month2 = mean(totalflf_MgC_ha_month, na.rm = T),
                   sd_leavesflf = sd(leavesflf_MgC_ha_month, na.rm = T),
                   sd_twigsflf = sd(twigsflf_MgC_ha_month, na.rm = T),
                   sd_flowersflf = sd(flowersflf_MgC_ha_month, na.rm = T),
                   sd_fruitsflf = sd(fruitsflf_MgC_ha_month, na.rm = T),
                   sd_bromflf = sd(bromflf_MgC_ha_month, na.rm = T),
                   sd_epiflf = sd(epiflf_MgC_ha_month, na.rm = T),
                   sd_otherflf = sd(otherflf_MgC_ha_month, na.rm = T),
                   sd_totalflf = sd(totalflf_MgC_ha_month, na.rm = T)) 

data5 = data.frame(data5)


# NPP litterfall in g m-2 mo-1
data5$totalflf_g_m2_mo <- data5$totalflf_MgC_ha_month2 * 0.49 * 10000 * 0.000001

# plot it
plotit = subset(data5, plot_code %in% 
                  # Hereafter write the name of your plots that you want to plot
                  c("ABA-00","ABA-01"))

# Reorder X axis so it follows month order
plotit$month <- factor(plotit$month, levels = c(1:12))
plotit %>% group_by(plot_code) %>% arrange(plot_code, month, year) %>% 
  ggplot(data=., aes(month, leavesflf_MgC_ha_month2, colour=year)) + geom_point() +
  facet_wrap(~plot_code)

#check outliers
plotit = subset(data3, plot_code %in% c("ABA-00","ABA-01"))
plotit$month <- factor(plotit$month, levels = c(1:12))
plotit %>% group_by(plot_code) %>% arrange(plot_code, month, year) %>% 
  ggplot(data=., aes(month, leavesflf_MgC_ha_month, colour=year)) + geom_point() 

