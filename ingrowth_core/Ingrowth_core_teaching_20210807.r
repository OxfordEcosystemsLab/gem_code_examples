

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                         INGROWTH CORE GEM PROTOCOL                       ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                1. Information                            ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#  Script originally designed by Dr Terhi Riutta
#
#  Modified by Huanyuan Zhang hyzhang1996@gmail.com
#
#  2021 August 07 as part of the African data workshop



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                2. Meta data                              ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ···How does this script work?  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Read comments first and come back here to write a paragraph (in French if you
# prefer) to summaries how was root NPP calculated


##~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ···Necessary input  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~


#···············································································
#  plot_code......The Forest Plots code                                        ·
#  ingrowth_core_num......The number of the ingrowth core                      ·
#  year......The year of the measurement, numeric four digits                  ·
#  month......The month of year of the measurement, numeric between 1 and      ·
#     12                                                                       ·
#  day......The day of the month of the measurement                            ·
#  is_stock......Should be y or n (means yes and no). 'Stock' is the first     ·
#     root measurement when setting up the ingrowth core. From that, you get   ·
#     root biomass stock (not productivity). And from the subsequent harvests  ·
#     you get productivity, because you know that all those roots were         ·
#     produced to an empty core over the collection interval. when y, it is    ·
#     bioamss stock, when n, it is npp                                         ·
#  ol_layer_depth......measured in centimetres                                 ·
#  ml_layer_depth......measured in centimetres                                 ·
#  time_step......valid values are 1 to 5, normally we have 4 steps            ·
#  time_step_minutes......How many minutes have you spent for this time        ·
#     step, if you spent 10 minutes for each time step, this column are all 10 ·
#  ol_under_2mm......measured in grams, ol is organic layer (surface           ·
#     soil), and ml is mineral soil (deeper soil, with the division judged     ·
#     approximately by soil colour; a couple of spadefuls of                   ·
#  soil for each will be sufficient and the rest may be discarded well away    ·
#     from the rhizotron area)."                                               ·
#  ol_over_2mm......measured in grams                                          ·
#  ml_under_2mm......measured in grams                                         ·
#  ml_over_2mm......measured in grams                                          ·
#  ol_2mm_to_3mm......measured in grams                                        ·
#  ml_2mm_to_3mm......measured in grams                                        ·
#  ol_3mm_to_4mm......measured in grams                                        ·
#  ml_3mm_to_4mm......measured in grams                                        ·
#  ol_4mm_to_5mm......measured in grams                                        ·
#  ml_4mm_to_5mm......measured in grams                                        ·
#  ol_over_5mm......measured in grams                                          ·
#  ml_over_5mm......measured in grams                                          ·
#  total_g......sum of all the roots (sum of the above columns), grams         ·
#···············································································

# str(census) 
#···············································································
#  $ my_data : num 3.72 4.4 4.59 4.71 2.41 3.03 3.22 3.39 3.21 3.4 ...         ·
#  $ plot_code : Factor w/ 1 level "BSouth": 1 1 1 1 1 1 1 1 1 1 ...           ·
#  $ Date_value : Date, format: "2011-09-19" "2011-09-19" "2011-09-19"         ·
#     "2011-09-19" ...                                                         ·
#  $ is_stock : Factor w/ 2 levels "N","Y": 2 2 2 2 2 2 2 2 2 2 ...            ·
#  $ DaysBetween : int 1 1 1 1 1 1 1 1 1 1 ...                                 ·
#  $ time_step : num 1 2 3 4 1 2 3 4 1 2 ...                                   ·
#  $ ingrowth_core_num: Factor w/ 16 levels "1","2","3","4",..: 1 1 1 1 2 2    ·
#     2 2 3 3 ...                                                              ·
#  $ time_step_minutes: num 10 10 10 10 10 10 10 10 10 10 ...                  ·
#  $ year : Factor w/ 3 levels "2011","2012",..: 1 1 1 1 1 1 1 1 1 1 ...       ·
#  $ day : Factor w/ 7 levels "01","06","08",..: 6 6 6 6 6 6 6 6 6 6 ...       ·
#  $ month : Factor w/ 7 levels "02","04","05",..: 6 6 6 6 6 6 6 6 6 6 ...     ·
#···············································································



##~~~~~~~~~~~~~~~~~~~~~~~
##  ···important note----
##~~~~~~~~~~~~~~~~~~~~~~~



#·····································································
#  1. the script go with total (sum of all root sizes), you can      ·
#     calculate a specific root class by changing the "total"        ·
#                                                                    ·
#  2. the code assume that you have 30 cm ingrowth core depth, with  ·
#     this assumption, we upscale the results for 1 meter depth      ·
#                                                                    ·
#  3. this code assume that you have 4 time steps for each sample,   ·
#     each step is the minutes recorded in time_step_minutes         ·
#                                                                    ·
#  4. run one plot at a time (if running everything at once failed)  ·
#·····································································


#  we will have searching time = time_step * time_step_minutes
#  you should have only one value for column time_step_minutes, otherwise
#  this codes get confused.


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                             3. data preparation                          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



##~~~~~~~~~~~~~~~~~~~~~~
##···quality check  ----
##~~~~~~~~~~~~~~~~~~~~~~
library(nlme)
library(tidyverse)
source('Terhi_40_to_120_magical_conversion.R')
source('functions.R')
setwd("F:/Side_project/african_data_workshop/General/Dataset examples/ingrowth_core")
census <- read.csv('ic_imported_20191107_XXX(example).csv',
                   sep = ",",
                   header = T)
str(census) # Use this to check input data, compare with metadata above
#View(census)
#summary(census)
census<-census%>%
  filter(plot_code=='ABC-01')
# One plot at a time for code simplicity

census$plot_code <- as.factor(census$plot_code)
census$ingrowth_core_num <- as.factor(census$ingrowth_core_num)
census$year <- as.factor(census$year)
census$month <- as.factor(census$month)
census$day <- as.factor(census$day)
census$is_stock <- as.factor(census$is_stock)
census$my_data <-
  as.numeric(census$total_g) #change here if you want fine root or any other root
census$time_step <- as.numeric(census$time_step)
census$time_step_minutes <- as.numeric(census$time_step_minutes)

##~~~~~~~~~~~~~~~~~~~~~
##···your options  ----
##~~~~~~~~~~~~~~~~~~~~~

Good_time_step = FALSE

#  Use true if you want to use your time step, 
#  If false, the script will assume that you do not have time step, and generate 
#  new timestep by the increasing pattern of root grams, make sure you 
#  have Root_found_accumulated assigned right.

Root_found_accumulated = FALSE

#  If FALSE, your dry mass looks like 2.56, 1.2, 0.3, 0.2
#  If TRUE, your dry mass looks like 2.56, 3.76, 4.06, 4.26

#  as default in GEM, total_g records total weight you found during
#  this time step, not an accumulated value for the whole core, so
#  for each core with four time steps


Days_interval_recorded = FALSE

#  say True, if you have a column census$DaysBetween, unit in days,
#  say FALSE if you want this script to calculate DaysBetween based on your 
#  year month day


#  we try to calculate days interval for each collection by looking
#  for the date of previous collection. if it is
#  not too much trouble, it is good to do this manually in excel because
#  NPP is calculated as roots_weight/days_interval




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                          4.from 40 to 120 minutes                        ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




#  if you don't know what is 40 minutes search time and why we are
#  converting to 120 minutes, then you must read rainfor-gemmanual
#  v3.0.pdf again!


if (!Days_interval_recorded) {
  census$DaysBetween <- NA
} # In case you don't have census$timestep

if (!Good_time_step) {
  census$time_step <- NA
} # In case you don't have census$DaysBetween


# We first make unique id for core and sample (a core might have multiple 
# samples because of multiple dates, then the time series will be used to calculate NPP)
# If you have sub-core (i.e. three depths in a core, you need to include sub-core in core ID)
# One sample should contain 4 timesteps (as per GEM manual)
cen <- census %>%
  select(#Subset columns using their names
    plot_code,
    year,
    month,
    day,
    is_stock,
    ingrowth_core_num,
    my_data,
    DaysBetween,
    time_step,
    time_step_minutes
  ) %>%
  
  mutate(core_id = paste(plot_code, ingrowth_core_num,sep = '_')) %>%
  mutate(core_sample_id = paste(plot_code, ingrowth_core_num, year, month, day,is_stock, sep =
                                  '_'))

list_of_core_id = unique(cen$core_id)# a list to loop through
list_of_core_sample_id = unique(cen$core_sample_id)


# Now we loop through each sample
Core_sample_pool = list()

for (i in 1:length(list_of_core_sample_id)) {
  one_sample_only <- cen %>%
    filter(core_sample_id == list_of_core_sample_id[i]) 
  
  print(paste0('sample number', i, 'sample id', list_of_core_sample_id[i]))
  # we extract a sample
  
  
  
  #··················if root record accumulated?···················
  
  
  if (Root_found_accumulated) {
    one_sample_only <- one_sample_only[order(one_sample_only$my_data), ]
    # if Root_found_accumulated, then we rank it from smallest to largest
  } else{
    one_sample_only <- one_sample_only[order(-one_sample_only$my_data), ]
    # we rank it from largest to smallest and do accumulated sum
    one_sample_only$my_data = cumsum(one_sample_only$my_data)
  }
  
  
  
  #·················if you do not have the timestep·················
  
  
  if (!Good_time_step) {
    one_sample_only$time_step <- 1:nrow(one_sample_only)
  }
  
  
  
  #········lets send it to Terhi magical 40 to 120 converter·······
  
  one_sample_only$time_step <- one_sample_only$time_step * one_sample_only$time_step_minutes
  # convert from time step to accumulated time
  #  We will have searching time = time_step * time_step_minutes, time_step
  #  is always 1234, time_step_minutes in my case is all 10, so max searching 
  #  time is 40 minutes for me                                                
  
  
  output <- Terhi_40_to_120_magical_conversion(one_sample_only$time_step,
                                               one_sample_only$my_data)
  
  # you get two results here, one is the estimate of 120 min, one is the method for estimation
  one_sample_only_output <- one_sample_only %>%
    slice(1) %>%
    select(!c('time_step','my_data'))
  # just to remove unnecessary column and row
  #Absorb new info from our estimate function
  # the number of time step
  # the estimate after 120 min
  # the method of estimate
  one_sample_only_output$number_of_timesteps <- nrow(one_sample_only)
  one_sample_only_output$estimate_of_120 <- output[[1]]
  one_sample_only_output$method_of_estimate <- output[[2]]
  # deposit results into a pool
  Core_sample_pool[[i]] <- one_sample_only_output
}

Core_sample_120 = do.call(rbind, Core_sample_pool)



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            5.from biomass to NPP                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Now we loop through each core
Sys.setenv(TZ = "UTC")
Stock_pool = list()
npp_pool = list()
for (i in 1:length(list_of_core_id)) {
  one_core_only <- Core_sample_120 %>%
    filter(core_id == list_of_core_id[i]) %>% #Take one of the core
    mutate(date=parse_date_time(paste(year,month,day),"ymd"))%>%
    arrange(date) #Generate a date column
  
  #··················if you dont have DaysBetween··················
  
  if (!Days_interval_recorded) {
    one_core_only[1, 'DaysBetween'] = 0 #The first one must be is_stock=Y
    if (one_core_only[1, 'is_stock'] == 'N') {
      stop (
        paste0(
          'the first record of core',
          list_of_core_id[i],
          'is not Y, you need to add a stock record for this core, otherwise I dont know time interval for your npp sample'
        )
      )
    }
    #Then, the other rows could get a days interval
    one_core_only[2:nrow(one_core_only), 'DaysBetween'] <-
      get_time_diffs(one_core_only$date)
  }
  
  
  #·····················separate stock and npp·····················
  
  Stock_pool[[i]] <- one_core_only %>%
    filter(is_stock == 'Y')
  
  npp_pool[[i]] <- one_core_only %>%
    filter(is_stock == 'N') %>%
    mutate(npp_g_per_day = estimate_of_120 / DaysBetween)
  
  #·····································································
  #  this is where npp is calculated, we have the sum of root mass of  ·
  #  several samples, and sum of days interval, then npp =             ·
  #  sum_root_mass / sum_days_interval                                 ·
  #·····································································
}

All_stock = do.call(rbind, Stock_pool)
All_npp = do.call(rbind, npp_pool)



#·······································································
#  For All_npp:                                                   ·
#  - number of timsteps > How many time steps were used to calculate   ·
#     averaged npp for this core (this has been weighted according to  ·
#     days interval length)                                            ·
#  - total_days_between > the sum of days interval from all time       ·
#     steps                                                            ·
#  - total_root_mass > the sum of root mass (standardized to 120       ·
#     minutes) of all time steps for this core                         ·
#  - method_used > which curve was used to standardize to 120          ·
#     minutes?                                                         ·
#·······································································



#····································································
#  For All_stock:                                                   ·
#  - my_data is the input data you choose at the beginning of this  ·
#     script                                                        ·
#  - estimate_of_120 is my_data standardized to 120 minutes, same   ·
#     unit as my_data (gram according to GEM protocol)              ·
#  - Other things similar to Per_Core_npp                           ·
#····································································

#·····································································
#  For time_series_plot:                                             ·
#  We average across all cores to get a plot level stat for each     ·
#  recording date, if you want one value for the whole plot, you     ·
#  need to consider weighted average because timestep with more      ·
#  "days between is more important". Therefore, I would suggest you  ·
#  to avearge Per_Core_npp to get a plot stat.                       ·
#·····································································



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              6.unit conversion                           ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




DepthCorr_Khoon_Yoda <- 1.125
# From Yoda 1983, used by Khoon
# Depth profile of roots. Estimate how much below the 30 cm in-growth core depth
# Calculate to 1 m
# Use same correction factor as Khoon

# Unit conversion from g biomass per core per day
# to Mg C ha-1 year-1


# Core dimensions (m)
d <- 0.12
h <- 0.30

A <- (d / 2) ^ 2 * pi

# Carbon content
cc <- 0.45 #according to Walter's tropical study


# Divided by A --> per m2
# Multiplied by 10000 --> per ha
# Multiplied by 365 --> per year
# Divided by 10^6 --> from g to Mg
# Multiplied by carbon content --> From biomass to carbon

unit_correct <- DepthCorr_Khoon_Yoda / A * 10000 * 365 / (10 ^ 6) * cc



All_stock$estimate_of_120 = All_stock$estimate_of_120 * unit_correct/365
# No need to correct day unit, not applicable to stock
All_npp$npp_MgC_ha_year = All_npp$npp_g_per_day * unit_correct



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            7. data visualization                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

time_series_plot <- All_npp %>%
  group_by(date) %>%
  summarise(npp_MgC_ha_year_mean = mean(npp_MgC_ha_year,na.rm=T),
            across(plot_code:method_of_estimate,  first))

source('functions.r')
library(Hmisc)

Per_Core_npp <- All_npp %>%
  group_by(ingrowth_core_num)%>%
  summarise(
    npp_MgC_ha_year_transect = wtd.mean(npp_MgC_ha_year,weights = DaysBetween,na.rm=T),
    npp_MgC_ha_year_transect_ste = weighted.var.se(npp_MgC_ha_year,  DaysBetween,na.rm=T),
    #npp_MgC_ha_year_transect = mean(npp_MgC_ha_year),
    #npp_MgC_ha_year_transect_ste =standard_error_calc(npp_MgC_ha_year),
    plot_code= first(plot_code),
    first_date=first(date),
    last_date=last(date))

# Well, let's save it first
plot_name=All_stock$plot_code[1]
write.csv(All_stock, file = paste0(plot_name,"_fine_root_stock_finest.csv"))
write.csv(All_npp, file = paste0(plot_name,"_fine_root_NPP_finest.csv"))

# get some sweet art work
ggplot(Per_Core_npp, aes(x = ingrowth_core_num, y = npp_MgC_ha_year_transect)) +
  geom_segment(aes(
    x = ingrowth_core_num,
    xend = ingrowth_core_num,
    y = 0,
    yend = npp_MgC_ha_year_transect
  )) +
  geom_point(
    size = 5,
    color = "red",
    fill = alpha("orange", 0.3),
    alpha = 0.7,
    shape = 21,
    stroke = 2
  )
time_series_plot$date <- as.character(time_series_plot$date)
ggplot(time_series_plot, aes(x = date , y = npp_MgC_ha_year_mean)) +
  geom_segment(aes(
    x = date ,
    xend = date ,
    y = 0,
    yend = npp_MgC_ha_year_mean
  )) +
  geom_point(
    size = 5,
    color = "red",
    fill = alpha("orange", 0.3),
    alpha = 0.7,
    shape = 21,
    stroke = 2
  )




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           8. Archived information                        ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




#  Terhi's email:
#
#
#
#  - I used total mass, not fine mass, for NPP (not for stock), as I
#  assumed that all new roots would be fine enough. However, I think
#  we should use true fine roots only.
#
#  - I've been using 'date.value', which is the numeric version of
#  the date from Excel (in Excel, convert a date column to numeric).
#  But you might want to go for something more sophisticated. My new
#  favourite is to turn everything into decimal year, created using
#  'year', 'month' and 'day' column, so that I don't need to deal
#  with any date formats. Plus decimal year works beautifully in
#  figures.
#
#  - Because we have a lot of missed collections in Malaysia, I
#  tried to correct for the uneven collection frequency in the code,
#  but I would, for simplicity's change, eliminate that part of the
#  code.
#
#  - The estimate for roots until 1 m depth is highly site-specific,
#  so not sure if it makes sense to share that part of the code.
#  Best to work with 0-30 cm productivity. If we incorporate the
#  depth correction, create a separate variable for depth corrected
#  estimate, so it can be used and/or eliminated easily. I would
#  definitely do it that way, if I started over. And I would put
#  the depth correction at the end, not in the middle of the code,
#  so that I wouldn't need to deal with the two resulting variables
#  (NPP_0-30 and NPP_0-100) separately.
#
#  - Root carbon content is not 50%, Walter did a nice analysis
#  lately, for tropical roots it's 45%.
