
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###                                                                      ~~~
###                               STEM NPP                              ----
###                                                                      ~~~
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      1. Explanation                       ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~
##  ...Authors----
##~~~~~~~~~~~~~~~~

#  This script was built upon joined efforts by lots of people in
#  Yadvinder Malhi's lab. However, I devided to write this script
#  from scratch - but with many others' idea incorporated - because
#  old scripts have been very messy.

#  Huanyuan Zhang hyzhang1996@gmail.com,
#  Script was checked by Guillaume Delhaye and Terhi Ruitta
# 2021 August 13 as part of the African data workshop


##~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ···Necessary input  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~

#   Your Census should contain                                                        
#   $ tree_tag : int 201 202 203 204 205 206 207 208 209 210 ...            
#   $ Species : chr "Terminalia avicennioides" "Terminalia avicennioides"...                         
#   $ density : num 0.636 0.636 0.405 0.514 0.636 ...                       
#   $ dbh : int 267 353 118 341 153 470 218 159 117 466 ...                 
#   $ height_m : num NA NA NA NA NA NA NA NA NA NA ...                      
#   $ plot_code : chr "KOG_04" "KOG_04" "KOG_04" "KOG_04" ...               
#   $ year : int 2012 2012 2012 2012 2012 2012 2012 2012 2012 2012 ...      
#   $ month : int 12 12 12 12 12 12 12 12 12 12 ...                         
#   $ day : int 3 3 3 3 3 3 3 3 3 3 ...      

# (pls prepare your data similar to the exmaple data sheet)
#
# dbh is diameter at breast height, unit in mm, unmeric
# height_m is tree height, unit in m
# density is wood density, g/cm3
# tree_tag 
# year 4 digit of data collection
# month 2 digit
# day 2 digit
# Species (Genus + space + Species)
# All missing value must be NA

##~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ important_note  ----
##~~~~~~~~~~~~~~~~~~~~~~~~

# This script assumes that the plot is one hectare! You should correct plot size
# if you have something different, simply change the unit conversion

# NPP was calculated as the change of biomass of each tree between start_day and end_day

census %>%
  dplyr::select(tree_tag, POM, dbh) %>%
  count(tree_tag, POM) %>%
  View

#  First check that your point of measurement (POM) does not change from year
#  to year, standardize dbh if it changed for any tree                       
#  If your data were downloaded from ForestPlot, POM has been standardized by ForestPLOT


# cen$density check that you have wood density, you can try to auto fill with
# BIEN_Database.R OR with package BIOMASSS

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##          2. Data Preparation  (what you need to change) ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load libraries
library(tidyverse)
#library(brms) 
library(BIOMASS)
rm(list=ls())
setwd("F:/Side_project/african_data_workshop/General/Dataset examples/stem_NPP/")
census   <- read.csv("F:/Side_project/african_data_workshop/not_to_share_with_student/KOG_04_census_data_long_format.csv", sep=",", header=T) 
census<-census%>%
  mutate(dbh=ifelse((Flag1==0),NA,dbh))
# mutate(dbh=ifelse((F1==0),NA,dbh)) #This is to choose alive tree
# don't use filter because filter delete some rows, it will affect the tree matching later

str(census)
View(census)
# Use these two lines to check your data with the explanation above



##~~~~~~~~~~~~~~~~~~~~~~~~~
##  possible setting1  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~


plotname = "KOG_04" #if you have multiple plots, you need to go one by one

allometric_option = "Default" # see chapter below for details
height_correction_option = 2
above_below_ground_ratio = 0.21 
# (see gem protocol) Root:shoot ratio (R/S) (i.e. the ratio BGLB/AGLB)

source("allometric_equations_2014.R")
source("error_propogation_based_on_BIOMASS.r") 
source("grab_total_error.R")
#  I modify a script in package BIOMASS, this will bring you function        
#  AGBmonteCarlo_no_allometric; I have disabled error from allometric          
#  equation. This will allow you to see error sourced from the random error    
#  of dbh measurements only, by doing this, you can know that the random       
#  error from dbh measurements is negligible, so we consider only systematic   
#  error, check this paper   https://doi.org/10.1111/2041-210X.12753                                                                     

## subset data, correct data format
cen <- subset(census, plot_code==plotname)
cen <- mutate(cen, unique_id=paste0(tree_tag,year,month))
cen$year<-as.factor(cen$year)
cen$tree_tag<-as.factor(cen$tree_tag)
cen$Species<-as.factor(cen$Species)
cen$density<-as.numeric(cen$density)
cen$dbh<-as.numeric(cen$dbh)
cen$height_m<-as.numeric(cen$height_m)
cen$plot_code<-as.factor(cen$plot_code)
cen$year<-as.factor(cen$year)
cen$month<-as.factor(cen$month)
cen$day<-as.factor(cen$day)


cen <- mutate(cen, dbh_log=log(dbh))%>%
  separate(.,col=Species,into =  c('genus','species'),sep=" " ) 
#we will need this for model fitting


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#   you can try to run everything below, but do read the comment  >
#                                                                 >
#   tree_table and plot_table will be your final output           >
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



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
    mutate(above_below_ground_ratio_se=0)%>% # lets assume the magical number 0.21 here does not have error, but you might want to specify an arbitrary error for this ratio
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
  
  
  
  
  
  

  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                  7. Additional information                 ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  

# Written by: Cecile Girardin July 2014
# Modified by Huanyuan Zhang July 2021

## This script estimates largeTreeNPP annual NPP values based on census data
# We then use this to estimate a scaling factor for the monthly dendrometer data (NPPcensus ha-1 yr-1 / NPPdend ha-1 yr-1).
  
# How to deal with new recruits? (Terhi Riutta)


#   that is, those stems that have reached the diameter threshold (in my case,    
#   10 cm), during the census interval? If the code is simplistic, it will        
#   assume that the diameter for the new stem was 0 in the first census. But      
#   that will overestimate NPP, because the stem was probably already >0 cm       
#   during the first census. What I've done in my file is to assume that the      
#   tree reached the 10 cm threshold in the middle of the census interval.        
#   However, as the NPP of the smaller trees is not very big, it's not a major    
#   issue.                                                                        
#                                                                                 
#   Similar to dead trees                                                         
#                                                                                 
#   if a tree dies between the 1st and 2nd census, should it have an NPP of       
#   zero? I have assumed no, I assumed it was still growing until in the middle   
#   of the census interval (I assigned a growth rate from a lookup table based    
#   on the diameter). However, in hindsight, it could be argued that a tree       
#   that was dying would have been growing very slowly, so an NPP of zero is      
#   probably ok.                                                              
#                                                                                 


#   Over all, in this codes, we disregard the tree if it is absent in one of      
#   the census (remember that this code is always comparing two censuses to       
#   get NPP, not to do all the census at the same time). The growth rate method   
#   will not provide much info for the final value - stem NPP per plot. Not to    
#   complicate the codes with small tree because it is trivial. If a tree is      
#   missing in any census, just leave it as NA, this will make the npp of that    
#   tree as NA for the given year. If you have multiple census, you should get    
#   NPP for every year, then use mean(na.rm=T) to get the overall NPP             
#                                                                                 
#   no buttresses correction in this script                                       
#                                                                                 
#   How to solve NA in tree height column? This R code try to propagate tree      
#   height based on dbh-treeheight correlation. That is, we try to complete       
#   tree height for a census first, then we propogate height into other census    
#   using dbh-tree height correlation. Well, the relationship between dbh and     
#   height is not linear, but log(dbh) and height is linear.                      


# If you don't have wood density yet, you might want to try this:


#   cen_error_propogate<-tidyr::separate(cen_error_propogate,col=Species,into =        
#   c('genus','Species'),sep=" " )                                                   
#   Taxo<-                                                                             
#   correctTaxo(genus=cen_error_propogate$genus,species=cen_error_propogate$Species)      
#   cen_error_propogate$genusCorr<-Taxo$genusCorrected                                 
#   cen_error_propogate$speciesCorr<-Taxo$speciesCorrected                           
#   WDdata<-getWoodDensity(genus=cen_error_propogate$genusCorr,                      
#   species=cen_error_propogate$speciesCorr,                                         
#   stand=cen_error_propogate$year)                                                   
#   cen_error_propogate$WD_by_BIOMASS_package<-WDdata$meanWD                          
#   cen_error_propogate$sdWD<-WDdata$sdWD 
  
  #well, if you want to calculate per tree biomass using BIOMASS package, this is the language
  #BIOMASS_way_of_AGB<-by(cen_error_propogate, cen_error_propogate$year,
  #     function(x) computeAGB(D=x$dbh,WD=x$density,H=x$height_final),
  #     simplify=F) 
  
  # You can see that the BIOMASS package calculate a mean very similar to ours

# This will also bring you wood density and its standard deviation
# which will allow you to calculate the corresponding error. You can try this if you are interested in.
# We believe that wood density error 
# is a systematic error which should not be included for stem npp.
  
  
# What if you want to loop through everthing, assuming that you have so many census?
# Self defined function like this:
  # my_lovely_function is the self-defined function containing all the codes
  
  #  return(NPP_plot)
  #  }
# KJ change this to set the plot code and the interval years to analyse; each interval is set in the order of what's
# in the brackets, so to add on analysis of interval 2014-2015, put 2014 in 4th place after 2013 in census1_year
# and put 2015 in 4th place after 2014 in census2_year. You could add a different plot name to do more plots
# and don't forget to add the plot name each time as these values populate the rows of the meta_table
  #plotname<-c('LPG_01','LPG_01',"LPG-01")
  #census1_year<-c('2011','2013',"2014")
  #census2_year<-c('2013','2014',"2015") 
  #meta_table<-data.frame(plotname,census1_year,census2_year)
  #data_pool<-list()
  #for (row_num in 1:nrow(meta_table)) {
  #data_pool[[row_num]]<-my_lovely_function(meta_table$plotname[row_num],meta_table$census1_year[row_num],meta_table$census2_year[row_num])
  #print(row_num)
  #}

  #my_final_table<-do.call(rbind,data_pool)
  
  
