

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------------ SMALL STEM NPP---------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      1. Explanation                       ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~
##  ...Authors----
##~~~~~~~~~~~~~~~~

#  This script was built upon joined efforts by lots of people in
#  Yadvinder Malhi's lab. Thanks Dr Kathryn Brun-Jeffery for providing example 
#  dataset

#  Contact: Huanyuan Zhang, hyzhang1996@gmail.com

##~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ necessary input  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~


##~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ important_note  ----
##~~~~~~~~~~~~~~~~~~~~~~~~


#  1. Must run one plot at a time, due to error propogation on plot scale       
#                                                                               
#  2. It is best to read the script of big stem NPP first, then do this one,    
#  the idea of small stem npp is the same as big stem npp, the only             
#  difference is that we now have to average several diameter measurement       
#  into one                                                                     
#                                                                               
#  3. The example script does not have species(for wood density) and tree       
#  height information, it is suggested to have them                             


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##               2. Data Preparation  (what you need to change)             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load libraries
library(tidyverse)
#library(brms) 
library(BIOMASS)
rm(list=ls())
setwd("F:/Side_project/african_data_workshop/gem_code_examples/small_stem_npp/")
cen   <- openxlsx::read.xlsx("F:/Side_project/african_data_workshop/gem_code_examples/small_stem_npp/small_stem_npp_MNG04_20220330.xlsx") 

str(cen)
# Use these two lines to check your data with the explanation above
plotname = "MNG-04" #if you have multiple plots, you need to go one by one
## subset data, correct data format
cen <- subset(cen, plot_code==plotname)
cen <- mutate(cen, unique_id=paste0(tree_tag,year,month))
cen$year<-as.factor(cen$year)
cen$tree_tag<-as.factor(cen$tree_tag)
cen$Species<-as.factor(cen$Species)
cen$wood_density_g_m2<-as.numeric(cen$wood_density_g_m2)
cen$height_m<-as.numeric(cen$height_m)
cen$plot_code<-as.factor(cen$plot_code)
cen$year<-as.factor(cen$year)
cen$month<-as.factor(cen$month)
cen$day<-as.factor(cen$day)

source("allometric_equations_2014.R")
source("error_propogation_based_on_BIOMASS.r") 
source("grab_total_error.R")
#  I modify a script in package BIOMASS, this will bring you function        
#  AGBmonteCarlo_no_allometric; I have disabled error from allometric          
#  equation. This will allow you to see error sourced from the random error    
#  of dbh measurements only, by doing this, you can know that the random       
#  error from dbh measurements is negligible, so we consider only systematic   
#  error, check this paper   https://doi.org/10.1111/2041-210X.12753                                                                     


allometric_option = "Default" # see chapter below for details
height_correction_option = 2
above_below_ground_ratio = 0.21 
# (see gem protocol) Root:shoot ratio (R/S) (i.e. the ratio belowground_biomass/aboveground_biomass)

# for wood_density, you can draw information from global data_base based on species, see stem npp script and BEIN_database.R for more information, or, to simplify, you can make use of your results of stem npp: KOG_04_census_data_long_format.csv, and take a plot averaged wood_density for all small trees, like the value here 0.6606522, see archived information at the end of this script for doing weighted average of wood density
cen$density<- 0.6606522
#cen$density<-wood_density_g_m2

# now think carefully how you are going to determine dbh (diameter at breast height) for each tree? here I just take a simple average
cen$DBH_north_south_horizontal_cm<-as.numeric(cen$DBH_north_south_horizontal_cm)
cen$DBH_east_west_horizontal_cm<-as.numeric(cen$DBH_east_west_horizontal_cm)
cen$DBH_north_south_vertical_cm<-as.numeric(cen$DBH_north_south_vertical_cm)
cen$DBH_east_west_vertical_cm<-as.numeric(cen$DBH_east_west_vertical_cm)
cen$DBH_north_south_maximum_cm<-as.numeric(cen$DBH_north_south_maximum_cm)
cen$DBH_east_west_maximum_cm<-as.numeric(cen$DBH_east_west_maximum_cm)

cen<-cen %>%
  rowwise()%>%
  mutate(dbh = mean(c(
    DBH_north_south_horizontal_cm,
    DBH_east_west_horizontal_cm ,
    DBH_north_south_vertical_cm ,
    DBH_east_west_vertical_cm ,
    DBH_north_south_maximum_cm ,
    DBH_east_west_maximum_cm), na.rm=T)  * 10)
# note that dbh here should be in mm unit

#we will need dbh_log for model fitting
cen <- mutate(cen, dbh_log=log(dbh))%>%
  separate(.,col=Species,into =  c('genus','species'),sep=" " ) 



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                    3 options and settings                   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



##~~~~~~~~~~~~~~~~~~~~~~~~~~
##  allometric options  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~


# allometric_option could be 2 or 3 or 4 or 5
# Corresponding to four allometric equations


#  Set of allometric equations after Chave et al. 2005 and Chave et al. 2014   
#  are defined in allometricEquations.R. Options defined here:                 


if (allometric_option == 2 | allometric_option == "dry") {
  allometrix <- 2
  print("dry equation  is used for estimating AGB, model I.3 (see Chave et al., 2005)")
} else if (allometric_option == 3 | allometric_option == "moist" | allometric_option == "Default" | allometric_option == 1) {
  allometrix <- 3
  print("moist equation  is used for estimating AGB, model I.6 (see Chave et al., 2005)")
} else if (allometric_option == 4 | allometric_option == "wet") {
  allometrix <- 4
  print("wet equation  is used for estimating AGB, model I.3 (see Chave et al., 2005)")
} else if (allometric_option == 5 | allometric_option == "Chave2014") {
  allometrix <- 5
  print("pantropical equation is used for estimating AGB, model (4) (see Chave et al., 2014)")
} else {
  print("Please specify a valid allometric_option!")
  return()
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Height correction options  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#height_correction_option 1 means you are going to do NA filled based on measured height
#height_correction_option 2 DELETE all your height data, and replaced with height estimate

if (height_correction_option == 1 | height_correction_option == "Default" ) {
  predheight <- 1
  print("If you have height for more than 50 trees in your plot, estimate local diameter-height relationship. If not, choose height correction option 2.")
} else if (height_correction_option == 2) {
  predheight <- 2
  print("height correction estimated as described by Feldpauch et al. (2012). Please check Feldpauch regional parameters in the code. Default is Brazilian shield.")
} else {
  print("Please specify a valid height_correction_option!")
  return()
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                    4. Height propogation                   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#   predheight grabs the method to fill height, the first one is based on         
#   measured height in a year, the other one based on Feldpauch et al. (2012)     
#                                                                                 
#   The idea of method one is height propogation proposed by Terhi Ruitta:        
#                                                                                 
#   We first pick the year with lots of height measurement, build a glmm model    
#   with this year, and fill NA in this year. Then we move on to other years,     
#   let height increase or decrease proportionally to the change of corresponding   
#   dbh.  

if (predheight == 1) {
  
  # This is to identify the year with lots of height measurement
  
  cen_height_available<-cen%>%drop_na(height_m) 
  list_of_year<-table(cen_height_available$year)
  Year_with_height<-names(list_of_year[list_of_year==max(list_of_year)])
  
  
  #  now we get the year with height measurements, extract it for dbh-height      
  #  model fitting, any height recorded out of this year will be removed,         
  #  because the forestplot was designed as only one year height record.if you    
  #  want to specify a year to fit the dbh-height relationship, you can rewrite   
  #  Year_with_height here                                                        
  
  
  cen[!cen$year==Year_with_height,'height_m']<-NA
  the_year_with_height <- subset(cen, year==Year_with_height)
  if (nrow(the_year_with_height)<49) {warnings("$$$$$>>>the_year_with_height has height records less than 50! You should choose option 2 for height corretion") } 
  
  df2 <- the_year_with_height %>% filter(!is.na(height_m))  
  
  # This will take a long time to run (Or you can try the other methods for fit):
  # You will get warning message, don't panic!
  # fit<-brm(height_m ~dbh_log + (1 + dbh_log | species/genus/Family), data = df2)
  
  #  Guillaume proposed to use brm, this is the best option, but if this does   
  #  not work out for you, please try:                                  
  fit <- lm(height_m ~dbh_log, data = df2)
  # fit <- lmer(height_m ~dbh_log + (1 + dbh_log |species/genus/Family), data = df2)
  
  cen <- cen %>% 
    mutate(height_modeled = predict(fit, ., allow.new.levels = T)) 
  
  a_list_of_tree_tag<-unique(cen$tree_tag)
  
  datalist = list()
  
  for (i in 1:length(a_list_of_tree_tag)) { #go through every tree
    cen_of_a_tree <- subset(cen, tree_tag==a_list_of_tree_tag[i])
    Measured_record<-cen_of_a_tree%>%drop_na(height_m)
    if (rapportools::is.empty(Measured_record$tree_tag[1])) {
      
      #  when this is empty, it means that tree height was not measured for this   
      #  tree. we have nothing to reply on, but to use modeled height only 
      
      cen_of_a_tree$height_final<-cen_of_a_tree$height_modeled
      cen_of_a_tree$delta_height<-NA
    }else{
      # but if we have measured height, we propagate the modeled height to match up with measured height
      cen_of_a_tree$delta_height<-cen_of_a_tree$height_modeled - Measured_record$height_modeled[1]
      cen_of_a_tree$height_final<-cen_of_a_tree$delta_height+ Measured_record$height_m[1]
    }
    datalist[[i]] <- cen_of_a_tree # add it to your list
    
  }
  cen = NA
  cen = do.call(rbind, datalist)
  
  
  
  #                   then for height option 2                   ~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  
} else if (predheight == 2) {
  # In this option, we delete all the height records and model all tree heights, 
  # if we try to keep several height records, we get to a problem that height differ so much for same tree different years
  
  # Option 2 parameters: you have height for less than 50 trees in your plot. Use Fedpauch equation.
  # Height-diameter allometry of tropical forest trees doi:10.5194/bg-8-1081-2011, Table A1
  #ADD PARAMETER: Feldpauch region. 
  
  ## Feldpauch correction procedure for heights, diameters and densitys:
  # Brazilian shield
  Bo    = 0.6373
  B1    = 0.4647  # E.C. Amazonia
  
  So1   = 0.012  # E.C. Amazonia
  Abar  = 20.4  # mean cenetered basal area m-2 ha-1
  
  n01   = 0.0034 # E.C. Amazonia
  Pvbar = 0.68 # mean centered precipitation coefficient of variation
  
  n02   = -0.0449 # E.C. Amazonia
  Sdbar = 5     # mean centered dry season length no months less than 100mm
  
  n03   = 0.0191  # E.C. Amazonia
  Tabar = 25.0  # mean centered annual averge temperature
  cen$height_final <- exp(Bo + B1*log(cen$dbh/10) + Abar*So1 + n01*Pvbar + n02*Sdbar + n03*Tabar)
} 





##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##              5. calculate biomass of each tree             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



## loop through each tree to estimate biomass (bm) and convert to above ground carbon (agC)
for (ii in 1:nrow(cen)) {    # this is just looping through each row.
  dbh_tree <- cen$dbh[ii]/10
  den_tree <- cen$density[ii]
  h_tree   <- cen$height_final[ii]
  
  #  this uses allometric equations that you selected, this line   
  #  calculate biomass for each tree from dbh, wood density and height 
  
  
  
  if (allometrix == 2) {
    bm <- Chave2005_dry(diax=dbh_tree, density=den_tree, height=h_tree)
  } else if (allometrix == 3) {
    bm <- Chave2005_moist(diax=dbh_tree, density=den_tree, height=h_tree)
  } else if (allometrix == 4) {
    bm <- Chave2005_wet(diax=dbh_tree, density=den_tree, height=h_tree)
  } else if (allometrix == 5) {
    bm <- Chave2014(diax=dbh_tree, density=den_tree, height=h_tree)
  }
  # convert from biomass to carbon, and unit conversion
  carbon_content_of_wood=0.456
  cen$agC[ii] <- (bm)*0.001*carbon_content_of_wood # convert kg to Mg with 1/1000 and convert to carbon = 45%
}


# This will loop through different combination of year
List_of_year<-t(combn(unique(cen$year), 2))
pool_plot = list()
pool_tree = list()
# census1_year = "2014" #The code will take the first recorded day in this year as start_day
# census2_year = "2015" #The code will take the latest recorded day in this year as end_day; 
# The difference between these two days will be taken as npp for this period
# I make a for loop here to run though all years, but you might want to check
# specific year
for (r_i in 1:nrow(List_of_year)) {
  census1_year=List_of_year[r_i,1]
  census2_year=List_of_year[r_i,2]
  
  ## find start and end date, to calculate time interval
  cen$date     <- as.Date(paste(cen$year, cen$month, cen$day, sep="."), format="%Y.%m.%d") 
  agC_mydates     <- subset(cen, cen$year == census1_year | cen$year == census2_year, select = c(plot_code, tree_tag, year, month, day, date, agC))
  min_date <- as.character(min(agC_mydates$date)) 
  max_date <- as.character(max(agC_mydates$date)) 
  start_date <- as.Date(format(min(strptime(min_date, format="%Y-%m-%d")))) 
  end_date   <- as.Date(format(max(strptime(max_date, format="%Y-%m-%d"))))
  census_interval <- as.numeric(difftime(end_date, start_date, units="days"))
  
  # extract the corresponding years
  agC_1         <- subset(cen, cen$year == census1_year, select = c(plot_code, tree_tag, dbh, year, month, day, agC))
  agC_2         <- subset(cen, cen$year == census2_year, select = c(plot_code, tree_tag, dbh, year, month, day, agC))
  number_of_tree_year1 = colSums(!is.na(agC_1))[['agC']]
  number_of_tree_year2 = colSums(!is.na(agC_2))[['agC']]
  
  # this one calculate npp per tree by 
  # npp = ( carbon.year 2 -  carbon.year 1) / census_interval
  npp           <- left_join(agC_1,agC_2[,c('tree_tag','year','agC','dbh')],by='tree_tag')%>%
    dplyr::rename(agC_year1 = agC.x)%>%
    dplyr::rename(agC_year2 = agC.y)%>%
    dplyr::rename(year1 = year.x)%>%
    dplyr::rename(year2 = year.y)%>%
    dplyr::rename(dbh_year1 = dbh.x)%>%
    dplyr::rename(dbh_year2 = dbh.y)%>%
    mutate(npp_per_tree_MgC_year  = (agC_year2-agC_year1) / census_interval *365)
  
  
  # this one calculate total npp and total biomass for the whole plot
  NPP_plot <- npp %>%
    dplyr::summarise(npp_per_ha_MgC_year=sum(npp_per_tree_MgC_year,na.rm=T))
  NPP_plot$biomass_year1_per_ha_MgC<-sum(agC_1$agC,na.rm=T)
  NPP_plot$biomass_year2_per_ha_MgC<-sum(agC_2$agC,na.rm=T)
  NPP_plot$number_of_tree_year1=number_of_tree_year1
  NPP_plot$number_of_tree_year2=number_of_tree_year2
  
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                      6. Error propogation                   ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  # get ready for error propagation, 
  # You can check grab_total_error.R if you want to know how to calculate error
  cen_error_propogate<-cen[,c('species','genus','year','height_final','density','dbh')]
  NPP_plot<-grab_total_error(cen_error_propogate,census1_year,NPP_plot)
  # if you need to convert unit, for example kg to g:
  # if your plot is not 1 hectare, you can convert it here
  #   NPP_plot$npp_per_tree_MgC_year = NPP_plot$npp_per_tree_MgC_year / 1000
  #   NPP_plot$npp_std = NPP_plot$npp_std /1000
  #   NPP_plot$biomass_year1_per_ha_MgC = NPP_plot$biomass_year1_per_ha_MgC /1000
  #   NPP_plot$biomass_year2_per_ha_MgC = NPP_plot$biomass_year2_per_ha_MgC /1000
  
  NPP_plot$plot_code<-plotname
  NPP_plot$census1_year<-census1_year
  NPP_plot$census2_year<-census2_year
  
  
  pool_plot[[r_i]]<-NPP_plot
  pool_tree[[r_i]]<-npp
}

tree_table <- do.call(rbind, pool_tree)
plot_table <- do.call(rbind, pool_plot) 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      7. data organisation                   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Tree_biomass<-cen[,c('species','genus','year','height_final','density','dbh','agC','date')]%>%
  dplyr::rename(AG_biomass_MgC=agC)%>%
  mutate(ABG_biomass_MgC = (above_below_ground_ratio+1) * AG_biomass_MgC,
         BG_biomass_MgC = ABG_biomass_MgC - AG_biomass_MgC)


tree_table<-tree_table%>%
  dplyr::rename(AG_npp_per_tree_MgC_year=npp_per_tree_MgC_year)%>%
  mutate(ABG_npp_per_tree_MgC_year = AG_npp_per_tree_MgC_year *
           (above_below_ground_ratio+1),
         BG_npp_per_tree_MgC_year = ABG_npp_per_tree_MgC_year - AG_npp_per_tree_MgC_year )

plot_table<-plot_table%>%
  dplyr::rename(AG_npp_per_ha_MgC_year=npp_per_ha_MgC_year)%>%
  dplyr::rename(AG_npp_per_ha_MgC_year_se=npp_std)%>%
  mutate_with_error(ABG_npp_per_ha_MgC_year ~ AG_npp_per_ha_MgC_year *  (above_below_ground_ratio+1))%>%
  mutate_with_error(BG_npp_per_ha_MgC_year ~ AG_npp_per_ha_MgC_year * above_below_ground_ratio)

write.csv(tree_table, file=paste0(plotname,"_stem_NPP_finest.csv"))
write.csv(plot_table, file=paste0(plotname, "_stem_NPP_plot.csv"))
write.csv(Tree_biomass, file=paste0(plotname, "_stem_stock_finest.csv"))

plot_table<-plot_table%>%
  mutate(time_range=paste(census1_year,census2_year,sep='_'))

ggplot(plot_table,
       aes(x = time_range, y = npp_per_ha_MgC_year )) +
  geom_bar(stat = "identity",
           position = position_dodge(),
           fill = 'gray') +
  geom_errorbar(
    aes(
      ymin = npp_per_ha_MgC_year  - npp_std ,
      ymax = npp_per_ha_MgC_year  + npp_std 
    ),
    width = .2,
    position = position_dodge(.9)
  ) +
  theme_minimal()


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#  Tree_biomass, tree_table and plot_table is your final output  >
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#   pls check the codes get the following variable correct               >
#                                                                        >
#   + census_interval                                                    >
#                                                                        >
#   + start_date                                                         >
#                                                                        >
#   + end_date                                                           >
#                                                                        >
#   + carbon_content_of_wood                                             >
#                                                                        >
#   Important: you need to convert unit if your plot is not one          >
#      hectare (check comments inline)                                   >
#                                                                        >
#   Important: if your point of dbh measurement (POM) change, you        >
#      need to correct dbh, this script assume everthing is measured at  >
#      the same POM, at 1.3m                                             >
#                                                                        >
#                                                                        >
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>







##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            Archived information                          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


STEM_npp <- read.csv("F:/Side_project/african_data_workshop/not_to_share_with_student/KOG_04_census_data_long_format.csv", sep=",", header=T) 
#This is the table you used for stem_npp, we will get plot level live wood density from it
STEM_npp<-STEM_npp%>%
  filter(year==STEM_npp$year[1])%>% #get only one year of data
  select(dbh,density)%>%
  mutate(dbh=dbh^2)%>% # weighted average based on basal area
  drop_na()
Live_wood_density=weighted.mean(STEM_npp$density,STEM_npp$dbh,na.rm = T)
Live_wood_density