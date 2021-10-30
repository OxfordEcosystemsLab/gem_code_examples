
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                     BRANCH NPP FROM COARSE WOOD DEBRIS                   ----
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
#  2021 August 13 as part of the African data workshop



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


#  please read GEM ptotocol and cwd_meta_data.csv for explaination
#  of the dataset   

#···············································································
#  $ plot_code : chr "XXX-04" "XXX-04" "XXX-04" "XXX-04" ...                   ·
#  $ cwd_transect_num : int 1 1 1 1 1 2 2 2 2 2 ...                            ·
#  $ transect_area_m2 : int 4 4 4 4 4 4 4 4 4 4 ...                            ·
#  $ measurement_num : int 1 2 3 4 5 1 2 3 4 5 ...                             ·
#  $ year : int 2015 2015 2015 2015 2015 2015 2015 2015 2015 2015 ...          ·
#  $ month : int 7 7 7 7 7 7 7 7 7 7 ...                                       ·
#  $ day : int 2 2 2 2 2 2 2 2 2 2 ...                                         ·
#  $ decay_class : int 1 2 3 4 5 1 2 3 4 5 ...                                 ·
#  $ diameter_top_1_cm : logi NA NA NA NA NA NA ...                            ·
#  $ diameter_top_2_cm : logi NA NA NA NA NA NA ...                            ·
#  $ diameter_bottom_1_cm : logi NA NA NA NA NA NA ...                         ·
#  $ diameter_bottom_2_cm : logi NA NA NA NA NA NA ...                         ·
#  $ length_cm : logi NA NA NA NA NA NA ...                                    ·
#  $ forest_weight_total_g : int 0 0 0 0 0 0 0 0 1200 1500 ...                 ·
#  $ forest_weight_sample_g : int 0 0 0 0 0 0 0 0 120 150 ...                  ·
#  $ dry_weight_sample_g : num 0 0 0 0 0 0 0 0 52.9 64.3 ...                   ·
#  $ volumn_cm3 : logi NA NA NA NA NA NA ...                                   ·
#  $ is_stock : chr "Y" "Y" "Y" "Y" ...                                        ·
#  $ include_as_branch_npp : chr "N" "N" "N" "N" ...                           ·
#  $ DaysBetween : int 1 1 1 1 1 1 1 1 1 1 ...                                 ·
#···············································································

##~~~~~~~~~~~~~~~~~~~~~~~
##  ···important note----
##~~~~~~~~~~~~~~~~~~~~~~~



#···············································································
#  1. one row is one measruement taken from a transect, you can have           ·
#     several measurements per transect, a single measurement could be a bag   ·
#     containing multiple pieces, OR could be a big branch that you can't      ·
#     weight, OR a single small branch that you can weight. Every measurement  ·
#     within a transect should have a unique measurement number, just like     ·
#     Rec_no of EGM flux measruement. The sum of all measurements under a      ·
#     transect will be the total weight for this transect.                     ·
#     A single measurement could be either dry weight, or diameter/length, the ·
#     script will convert them into dry mass depending on which one is         ·
#     available
#                                                                              ·
#  2. this csv is for branch NPP and small nacromass stock, don't use this     ·
#     for snag survey or nacromass survey, the diameter and length here is for ·
#     branch, not dead stem                                                    ·
#                                                                              ·
#  3. NA and 0 is treated differently in this script. If you put NA, the       ·
#     script is going to remove the record, understanding it as unknown/       ·
#     not_measured. If you put 0, the script understand that there is no       ·
#     branch under such transect and decay class. For example, if you have     ·
#     big branch that you can't lift, you need to put in length and diameter,  ·
#     with Forest_weight as NA. If you did not find anything under decay       ·
#     class 1, you should still have a row with decay class one but you put    ·
#     Forest_weight as 0, Sample_weight as 0.                                  ·
#                                                                              ·
#  4. is_stock = Y will be used in nacromass estimation,                       ·
#     include_as_branch_npp = Y will be used in branch NPP calculation
#     It is possible that both of them are N, like dead tree, or whatever you
#     dont want to include
#
#  5. Run one plot at a time
#···············································································



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                             3. data preparation                          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~
##···quality check  ----
##~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())
library(nlme)
library(tidyverse)
library(lubridate)
library(Hmisc)
library(here)
setwd(here())
setwd("F:/Side_project/african_data_workshop/gem_code_examples//branch_npp/")
census <- read.csv('cwd_20170710_XXX.csv',
                   sep = ",",
                   header = T)
str(census) # Use this to check input data, compare with metadata above
source('functions.r')

#View(census)
#summary(census)
census<-census%>%
  filter(plot_code=='XXX-04')
census$plot_code <- as.factor(census$plot_code)
census$cwd_transect_num  <- as.factor(census$cwd_transect_num)
census$year <- as.numeric(census$year)
census$month <- as.numeric(census$month)
census$day <- as.numeric(census$day)
census$is_stock <- as.factor(census$is_stock)
census$forest_weight_total_g <- as.numeric(census$forest_weight_total_g)
census$forest_weight_sample_g <- as.numeric(census$forest_weight_sample_g)
census$dry_weight_sample_g <- as.numeric(census$dry_weight_sample_g)
census$transect_area_m2 <- as.numeric(census$transect_area_m2)
census$decay_class <- as.numeric(census$decay_class)
census$measurement_num<- as.numeric(census$measurement_num )

census$diameter_top_1_cm <- as.numeric(census$diameter_top_1_cm)
census$diameter_top_2_cm <- as.numeric(census$diameter_top_2_cm  )
census$diameter_bottom_1_cm    <- as.numeric(census$diameter_bottom_1_cm)
census$diameter_bottom_2_cm    <- as.numeric(census$diameter_bottom_2_cm)
census$length_cm               <- as.numeric(census$length_cm )
census$include_as_branch_npp   <- as.factor(census$include_as_branch_npp    )





##~~~~~~~~~~~~~~~~~~~~~
##···your options  ----
##~~~~~~~~~~~~~~~~~~~~~

Days_interval_recorded = FALSE


#  we try to calculate days interval for each collection by looking
#  for the date of previous collection. If you have days interval
#  recorded and you don't want me to look for days interval, you
#  must have a column census$DaysBetween, unit in days, if it is
#  not too much trouble, it is good to do this mannually because
#  NPP is calculated as cwd_weight/days_interval

# represent wood density from decay class 1 to 5
decomposed_wd_g_cm3<- c(0.501, 0.434, 0.421, 0.308, 0.185)
Live_wood_density<-0.523 
# Live_wood_density is weighted average of alive wood density of your plot
# This will be used to estimate alive wood mass from branch volumn measurements
# Will also be used to back calculate decayed wood dry mass into alive wood
# drymass

# if you wish to calculate Live_wood_density from census data, or to calculate 
# decomposed_wd_g_cm3from coarse woody debris sample, check "Archived information"
# in this script

decay_class = c(1,2,3,4,5)
# this must match up with WoodDensity_g_cm3
WD_ConversionFactorToAlive <- Live_wood_density/WoodDensity_g_cm3
df_decay_wood_density<-data.frame(decay_class,WoodDensity_g_cm3,Live_wood_density,WD_ConversionFactorToAlive)

#···············································································
#  normally you don't need to change anything below, but you might want to     ·
#  read and double-check that it works for your site                           ·
#···············································································

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##              4. estimate the weight of big branch from its size          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# You might have some big branch that you can't weight

if (!Days_interval_recorded) {
  census$DaysBetween <- NA
} # In case you don't have days interval

cen<-census%>%
  mutate(diameter_cm = rowMeans(select(., starts_with("dia")), na.rm = TRUE))%>%
  # average diameter... there might be better algorithm but anyway, I am lazy now
  mutate(Volumn_cm3 = (3.142 * (diameter_cm / 2)^2 * length_cm),   # volumn in cm3
         surface_area_m2 = (2 * 3.142 * ((diameter_cm / 2)^2)) + (2 * 3.142 * (diameter_cm / 2) * length_cm))%>%
  left_join(.,df_decay_wood_density,by='decay_class')%>%
  # to bring in WD_ConversionFactorToAlive
  # we use this ratio to convert decayed wood into live wood
  # we want branch npp so we want to know the weight when the wood is alive
  mutate(Dry_mass_from_volumn_g=Volumn_cm3*WoodDensity_g_cm3)
  


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##               5. weight of small branch from moisture content            ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
# I just grab this function here, https://stackoverflow.com/questions/9322773/how-to-replace-na-with-mean-by-group-subset


cen2<-cen%>%
  mutate(dry_mass_content=dry_weight_sample_g/forest_weight_sample_g)%>%
  # Dry mass content - to correct water soaked in wood
  mutate(date=parse_date_time(paste(year,month,day),"ymd"))%>%
  # make a date series
  
  group_by(date,decay_class,size_class,cwd_transect_num)%>%
  mutate(dry_mass_content = impute.mean(dry_mass_content))%>%
  group_by(date,decay_class,size_class)%>%
  mutate(dry_mass_content = impute.mean(dry_mass_content))%>%
  group_by(date,decay_class)%>%
  mutate(dry_mass_content = impute.mean(dry_mass_content))%>%
  group_by(date)%>%
  mutate(dry_mass_content = impute.mean(dry_mass_content))%>%
  ungroup()%>%
  # This might look awful, but the idea is that, we first group by 
  # (date,decay_class,size_class,cwd_transect_num)
  # for each group, we do:
  # calculate a mean for a gorup, fill NA in this group with such mean
  # yes, it is possible that the whole group is NA, so NA was not filled
  # then we group_by(date,decay_class,size_class)
  # for each group again, calculate mean and fill NA in this group
  # then we group_by(date,decay_class)
  # group_by(date)
  # then we mangae to fill all NA, because for a given date, you must have 
  # non-NA value
  arrange(date,decay_class,size_class,cwd_transect_num)%>%
# arrange order by date etc so that it is easy to navigate

  mutate(dry_mass_back_calculated_g=dry_mass_content*forest_weight_total_g,
         dry_mass_calculated_combined_g=pmax(dry_mass_back_calculated_g,Dry_mass_from_volumn_g,na.rm = T),
         dry_mass_when_alive=dry_mass_calculated_combined_g*WD_ConversionFactorToAlive)
  # first account for water content in forest wood sample, it is dry_mass_back_calculated_g
  # for huge wood that we cant lift, we don't need to consider water content, just Dry_mass_from_volumn_g
  # pmax just coalesce dry_mass_back_calculated_g and Dry_mass_from_volumn_g
  # I dont use coalesce() because people might accidently put 0 there instead of NA







##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            6.From biomass to NPP                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

transect_id<-unique(cen$cwd_transect_num)
Stock_pool = list()
npp_pool = list()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ···loop through every transect----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for (i in 1:length(transect_id)) {
  one_transect_only <- cen2 %>%
    
    filter(cwd_transect_num == transect_id[i])%>%
    arrange(date) #Take one of the core
  
  one_transect_only_stock <- one_transect_only %>%filter(is_stock == 'Y')
  # The if sentence below check whether your is_stock all come from the first date.
  # We believe that your stock must come from the first day only, otherwise, the codes will stop
  if (!isTRUE(length(unique(one_transect_only_stock$date))==1)) {
   warning (immediate. = T,
      paste0(
        'have a look at this record: ',
        transect_id[i],
        'we found two stock records under this flag, which is confusing! we can have only one to calculate NPP'
      )
    )
  }
  
 
  #··················if you dont have DaysBetween··················
  
  if (!Days_interval_recorded) {
    #The first one must be is_stock=Y
    if (one_transect_only[1, 'is_stock'] == 'N') {
     warning (immediate. = T,
        paste0(
          'the first record of transect of',
          transect_id[i],
          'is not Y, you need to add a stock record for this transect, otherwise I dont know time interval for your npp sample'
        )
      )
    }
    one_transect_only[, 'DaysBetween'] <-
      get_time_diffs2(one_transect_only$date)
    # This fill in days interval according to date record
  }
  
  #···················Get branch nacromass stock···················
  
  Stock_pool[[i]]<-one_transect_only_stock
  # NPP should be mass / days interval
  npp_pool[[i]]<- one_transect_only %>%
    filter(include_as_branch_npp == 'Y')%>%
    mutate(NPP_g_day=dry_mass_when_alive/DaysBetween)
  #················this is where NPP was calculated················
  #················this is where NPP was calculated················
  #················this is where NPP was calculated················
  #················this is where NPP was calculated················
  }

All_stock = do.call(rbind, Stock_pool)
All_npp = do.call(rbind, npp_pool)




##~~~~~~~~~~~~~~~~~~~~~~~~
##  ···unit conversion----
##~~~~~~~~~~~~~~~~~~~~~~~~
# Unit conversion
# The current unit is grams of estimated living biomass per quadrat per collection interval
# Convert to Mg C ha-1 year-1

# Wood carbon content 0.45 (assuming deadwood carbon content is the same as living wood)
cc <- 0.45

# Divide by 10^6 --> g to Mg
# Multiply by carbon content --> from biomass to carbon
# Divide by quadrat area and multiply by 10000 --> from NPP per quadrat to NPP per m2 to NPP per ha
#  multiply by 365 --> from NPP over collection interval to NPP per day to NPP per year

Unit_correct=cc/ 10^6 / mean(cen$transect_area_m2)* 10000* 365
All_stock$stock_nacromass_MgC_ha = All_stock$dry_mass_calculated_combined_g * Unit_correct /365
All_npp$npp_MgC_ha_year = All_npp$NPP_g_day * Unit_correct 



write.csv (All_stock, file = paste0(census$plot_code[1],"_branch_nacromass_stock_finest.csv"))

write.csv (All_npp, file = paste0(census$plot_code[1],"_branch_NPP_finest.csv"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            7. data visualization                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#···············································································
#  I need to put explaination to output files here, but you can read the       ·
#  name to understand                                                          ·
#···············································································

Sum_all_decay_class_stock<-All_stock%>%
  group_by(cwd_transect_num,date)%>%
  summarise(stock_nacromass_MgC_ha = sum(stock_nacromass_MgC_ha,na.rm=T), #sum across different decay classes
            across(c(plot_code:day),  first))


Sum_all_decay_class_npp<-All_npp%>%
  group_by(cwd_transect_num,date)%>%
  summarise(npp_MgC_ha_year = sum(npp_MgC_ha_year,na.rm=T),
            DaysBetween=mean(DaysBetween), # well, they should have the same DaysBetween
            across(c(plot_code:day),  first))


All_npp_by_date<-All_npp%>%
  group_by(date)%>% #change it to year if you want x axis to be year
  summarise(npp_MgC_ha_year_mean = mean(npp_MgC_ha_year,na.rm=T),
            npp_MgC_ha_year_std = standard_error_calc(npp_MgC_ha_year,na.rm=T),
            across(c(plot_code:day,DaysBetween),  first))

All_npp_whole_plot<-All_npp%>%
  ungroup()%>%
  summarise(
    npp_MgC_ha_year_plot = wtd.mean(npp_MgC_ha_year,weights = DaysBetween),
    npp_MgC_ha_year_ste = weighted.var.se(npp_MgC_ha_year, DaysBetween),
    across(c(plot_code),  first))
# The standard error is a mean over all quadrats and time step

All_npp_per_transect<-All_npp%>%
  group_by(cwd_transect_num)%>%
  summarise(
    npp_MgC_ha_year_transect = wtd.mean(npp_MgC_ha_year,weights = DaysBetween),
    npp_MgC_ha_year_transect_ste = weighted.var.se(npp_MgC_ha_year,  DaysBetween),
    #npp_MgC_ha_year_transect = mean(npp_MgC_ha_year),
    #npp_MgC_ha_year_transect_ste =standard_error_calc(npp_MgC_ha_year),
    across(c(plot_code),  first))

All_stock_plot<-All_stock%>%
  summarise(stock_nacromass_MgC_ha_mean = mean(stock_nacromass_MgC_ha,na.rm=T),
            stock_nacromass_MgC_ha_ste = standard_error_calc(stock_nacromass_MgC_ha,na.rm=T))


All_npp_by_date$date <- as.character(All_npp_by_date$date)
ggplot(All_npp_by_date, aes(x = date , y = npp_MgC_ha_year_mean))+
geom_segment(aes(
  x = date ,
  xend = date ,
  y = 0,
  yend = npp_MgC_ha_year_mean
))+
geom_point(
  size = 5,
  color = "red",
  fill = alpha("orange", 0.3),
  alpha = 0.7,
  shape = 21,
  stroke = 2
)


ggplot(All_npp_per_transect, aes(x = cwd_transect_num, y = npp_MgC_ha_year_transect))+
geom_segment(aes(
  x = cwd_transect_num,
  xend = cwd_transect_num,
  y = 0,
  yend = npp_MgC_ha_year_transect
))+
geom_point(
  size = 5,
  color = "red",
  fill = alpha("orange", 0.3),
  alpha = 0.7,
  shape = 21,
  stroke = 2
)

ggplot(All_npp_per_transect, aes(x=cwd_transect_num, y=npp_MgC_ha_year_transect)) +
  geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
  geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=npp_MgC_ha_year_transect-npp_MgC_ha_year_transect_ste, ymax=npp_MgC_ha_year_transect+npp_MgC_ha_year_transect_ste)) +
  scale_fill_manual(values=c("#CCCCCC","#FFFFFF")) +
  scale_y_continuous(breaks=seq(1:100)) +
  theme_bw()



#...............................................................................
#                                                                              .
#  This is the end of calculation                                              .
#...............................................................................




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                          Archived info from Terhi                        ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#         R codes to calculate decay/live wood density         ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#·····················grab live wood density·····················

STEM_npp <- read.csv("F:/Side_project/african_data_workshop/not_to_share_with_student/KOG_04_census_data_long_format.csv", sep=",", header=T) 
#This is the table you used for stem_npp, we will get plot level live wood density from it
STEM_npp<-STEM_npp%>%
  filter(year==STEM_npp$year[1])%>% #get only one year of data
  select(dbh,density)%>%
  mutate(dbh=dbh^2)%>% # weighted average based on basal area
  drop_na()
Live_wood_density=weighted.mean(STEM_npp$density,STEM_npp$dbh,na.rm = T)
# Live_wood_density<-0.523
# If you don't want stem npp dataset, you might want to define a value of 
# live wood density

if (!isTRUE(Live_wood_density>0)) {
  stop('Check your stem npp input table, do you have dbh and density?')
}
# check whether your wood density exist 

#·····················grab live wood density end·····················


#···············calculate decay class wood density···············

Make_my_own_density = FALSE

#  if you want to calculate wood density from your dataset by   
#  dry_mass/volumn, you say TRUE!!! if you want to use global   
#  values of wood density, or if you want to define wood density
#  mannually, you say FALSE  

if (Make_my_own_density){
  
  #  Then lets calculate wood density for each decay class 
  #  you might have another lab datasheet for wood density
  #  calculation, you should do:
  
  # cwd_lab_data_20170710_XXX.csv should simply contain at least three columns
  # volumn_cm3
  # dry_weight_g
  # decay_class
  
  WoodDensity_g_cm3<-read.csv('cwd_lab_data_20170710_XXX.csv',
                              sep = ",",
                              header = T)%>%
    select(volumn_cm3,dry_weight_g,decay_class)%>%
    mutate(wood_denS=dry_weight_g/volumn_cm3)%>%
    group_by(decay_class)%>%
    summarise(WoodDensity_g_cm3=mean(wood_denS,na.rm=T))%>%
    arrange(decay_class)
  WoodDensity_g_cm3<-WoodDensity_g_cm3$WoodDensity_g_cm3
  
  
} else {
  decomposed_wd_g_cm3<- c(0.501, 0.434, 0.421, 0.308, 0.185)
  # represent wood density from decay class 1 to 5
}


decay_class = seq(from=1, to=length(WoodDensity_g_cm3), by=1) 
df_decay_wood_density <- data.frame(WoodDensity_g_cm3,decay_class)
print(Live_wood_density)
print(df_decay_wood_density)
df_decay_wood_density$WD_ConversionFactorToAlive <- Live_wood_density / df_decay_wood_density$WoodDensity_g_cm3





#                      Terhis old R codes                      ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





setwd("C:/Users/triutta/Dropbox/SAFE_Terhi/NPP")
getwd()

data.all <- read.table("F:/Side_project/african_data_workshop/not_to_share_with_student/SAFE_DAN-04_DAN-05_SmallDeadwoodQuadrats_toR.txt", header=T)
names(data.all)
dim(data.all)
str(data.all)

# Calculate NPP only

data1 <- subset(data.all, DataType=="NPP")
dim(data1)


# The data has a lot of zeros, as it's been entered so that each quadrat has each decay class
# Size classes not taken into acount
# These are true zeros, many empty quadrats, so important to retain.


# Calculate dry matter content based on the samples that were brought to the lab
# I have this available for every sample
# It would be more common to calculate moisture content, but this works better for this purpose
# But if not, calculate the average dry matter content by decay class and by date (and by size class?) and apply that

data1$DryContent <- ifelse(data1$ForestMass_Total>0, data1$DryMass_Sample / data1$ForestMass_Sample, NA)
# Sanity check
mean(data1$DryContent, na.rm=T)

# Estimate total dry mass based on the total fresh mass and sample dry content

data1$DryMass_Total <- ifelse(data1$ForestMass_Total>0, data1$DryContent * data1$ForestMass_Total, 0)


# Table of wood densities by Decay Class
# Note that these are site-speficic (this one is for moist tropical forest in Malaysian Borneo)

# In the case of Malaysia, the densities have been estimated during a separate wood decomposition study (a separate dataset)
# I'm applying the mean WD by decay class from this large study here
# But some site might have density measurments collected from their transects
# However, Huanyuan feel overwhelmed by the flexibility of considering too much, let's just have 6 categories of wood density anyway 
# The 0 category is the mean wood density of the living trees

DecayClass <- seq(from=0, to=5, by=1)
WoodDensity <- c(0.523, 0.501, 0.434, 0.421, 0.308, 0.185)

WD.Table <- data.frame(DecayClass, WoodDensity) 


# We convert the decayed mass to the estimated alive mass 
# Important for NPP, becayse the branches were, of course, produced alive
# Don't do this if you are actually interested in the necromass

# Calculate the conversion factor (Alive WD divided by the WD of each decay class)
WD.Table$WD_ConversionFactorToAlive <- WD.Table$WoodDensity[WD.Table$DecayClass==0] / WD.Table$WoodDensity


# Find the right conversion factor for each decay class in the main dataframe

data1 <- merge(data1, WD.Table)
names(data1)
dim(data1)

# Estimate the alive mass of the branches
data1$Mass_ConvertedToAlive <- data1$DryMass_Total * data1$WD_ConversionFactorToAlive 


# Unit conversion
# The current unit is grams of estimated living biomass per quadrat per collection interval
# Convert to Mg C ha-1 year-1

# Wood carbon content 0.45 (assuming deadwood carbon content is the same as living wood)
cc <- 0.45

# Divide by 10^6 --> g to Mg
# Multiply by carbon content --> from biomass to carbon
# Divide by quadrat area and multiply by 10000 --> from NPP per quadrat to NPP per m2 to NPP per ha
# Divide by DaysBetween and multiply by 365 --> from NPP over collection interval to NPP per day to NPP per year

data1$BranchNPP <- data1$Mass_ConvertedToAlive / 10^6 * cc / data1$QuadratArea_m2 * 10000 / data1$DaysBetween * 365 


# Sum by Plot, Date and Quadrat, to get the total for each quadrat at each occasion, this is sum over decay class

BranchNPP_byQuarat_byDate <- aggregate(BranchNPP ~ PlotCode  Date  Quadrat, data1, sum)
View(BranchNPP_byQuarat_byDate)
names(BranchNPP_byQuarat_byDate)

# Take the average by Plot and by Quadrat
BranchNPP_byQuarat <- aggregate(BranchNPP ~ PlotCode  Quadrat, BranchNPP_byQuarat_byDate, mean)


# Calculate the mean and standard error by Plot
# This is SE represent the spatial variation within plot (temporal variation not taken into account)

BranchNPP_byPlot_Mean_SE <- aggregate(BranchNPP ~ PlotCode, BranchNPP_byQuarat, function(x) c(Mean = mean(x), SE = sd(x)/sqrt(length(x))))
BranchNPP_byPlot_Mean_SE

write.table(BranchNPP_byPlot_Mean_SE, "BranchNPP_DAN-04_DAN-05_fromR.txt", row.names=F)  

