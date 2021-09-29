
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###                                                                      ~~~
###                               STEM NPP                              ----
###                                                                      ~~~
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# A string, first line of the comment. If "", the zero-length string, only the
# top lines of the banner are made. If missing, in an interactive session the
# user will be prompted for the input strings, one per line, in the console.



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      1. Explaination                       ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# Workflow of this code:

#   1. data input and column class check                                       
#                                                                              
#   2. options to be made for "height correction" and "allometric equations"   
#                                                                              
#   3. Height NA filled, height propagation (proposed by Terhi Riutta and      
#      Guillaume)                                                              
#                                                                              
#   4. Error propagation                                                       

# Modified by Huanyuan Zhang (huanyuan.zhang@ouce.ox.ac.uk) on 04/07/2021

# data required:
# use str(census) to check this

#   Your Census should contain                                                        
#                                                                           
#   $ tree_tag : int 201 202 203 204 205 206 207 208 209 210 ...            
#                                                                           
#   $ Species : chr "Terminalia avicennioides" "Terminalia avicennioides"   
#   "Lannea microcarpa" "Sterculia tragacantha" ...                         
#                                                                           
#   $ density : num 0.636 0.636 0.405 0.514 0.636 ...                       
#                                                                           
#   $ dbh : int 267 353 118 341 153 470 218 159 117 466 ...                 
#                                                                           
#   $ height_m : num NA NA NA NA NA NA NA NA NA NA ...                      
#                                                                           
#   $ plot_code : chr "KOG_04" "KOG_04" "KOG_04" "KOG_04" ...               
#                                                                           
#   $ year : int 2012 2012 2012 2012 2012 2012 2012 2012 2012 2012 ...      
#                                                                           
#   $ month : int 12 12 12 12 12 12 12 12 12 12 ...                         
#                                                                           
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



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                    2. Data Preparation                     ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load libraries
library(tidyverse)
library(brms)
library(BIOMASS)

setwd("F:/Side_project/african_data_workshop/not_to_share_with_student/")
census   <- read.csv("KOG_04_census_data_long_format.csv", sep=",", header=T) 
str(census)
View(census)
# Use these two lines to check your data with the explanation above



##~~~~~~~~~~~~~~~~~~~~~~~~~
##  possible setting1  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~


plotname = "KOG_04" #if you have multiple plots, you need to go one by one
census1_year = "2014" #The code will take the first recorded day in this year as start_day
census2_year = "2015" #The code will take the latest recorded day in this year as end_day; 
#NPP was calculated as the change of biomass of each tree between start_day and end_day
allometric_option = "Default" 
height_correction_option = 1
source("allometric_equations_2014.R")
source("error_propogation_based_on_BIOMASS.r")


##~~~~~~~~~~~~~~~~~~~~~~~~~
##  possible setting2  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~


#census   <- read.csv("formattedcensus_TAM05_Mar17.csv", sep=",", header=T) 
#plotname = "ABC-05" 
#census1_year = "2005" 
#census2_year = "2006" 
#allometric_option = "Default" 
#height_correction_option = "Default"
 
## subset data, correct data format
cen <- subset(census, plot_code==plotname)
cen <- mutate(cen, unique_id=paste0(tree_tag,year,month))
cen$year<-as.factor(cen$year)
cen$dbh<-as.numeric(cen$dbh)
cen <- mutate(cen, dbh_log=log(dbh))%>%
  separate(.,col=Species,into =  c('genus','species'),sep=" " ) 
#we will need this for model fitting


## fill implausible values, check whether your density value is ok, for example"

# cen$height_m[which(cen$height_m>120)] <- 120 
# cen$height_m[which(cen$height_m<2)]   <- 2 
# xdensity <- mean(cen$density, na.rm=T) 
# cen$density[which(is.na(cen$density)) | which(cen$density==0)] <- xdensity


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
##                    3. Height propogation                   ----
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
    #fit<-brm(height_m ~dbh_log + (1 + dbh_log | species/genus/Family), data = df2)
    
    
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
    cen$height_final <- 10^(Bo + B1*log10(cen$dbh/10) + Abar*So1 + n01*Pvbar + n02*Sdbar + n03*Tabar)
  } 
  

  
  

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##              4. calculate biomass of each tree             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#   pls check the codes get the following variable correct                  >
#                                                                           >
#   + census_interval                                                       >
#                                                                           >
#   + start_date                                                            >
#                                                                           >
#   + end_date                                                              >
#                                                                           >
#   + carbon_content_of_wood                                                >
#                                                                           >
#                                                                           >
#                                                                           >
#   %%Important%% you need to convert unit if your plot is not one hectare  >
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


    
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
  npp           <- left_join(agC_1,agC_2[,c('tree_tag','year','agC')],by='tree_tag')%>%
    dplyr::rename(agC_year1 = agC.x)%>%
    dplyr::rename(agC_year2 = agC.y)%>%
    dplyr::rename(year1 = year.x)%>%
    dplyr::rename(year2 = year.y)%>%
    mutate(npp_per_tree_MgC_year  = (agC_year2-agC_year1) / census_interval *365)
    

  # this one calculate total npp and total biomass for the whole plot
  NPPacw_MgC_ha_yr <- npp %>%
    dplyr::summarise(npp_per_ha_MgC_year=sum(npp_per_tree_MgC_year,na.rm=T),
                     biomass_year1_per_ha_MgC=sum(agC_year1,na.rm=T),
                     biomass_year2_per_ha_MgC=sum(agC_year2,na.rm=T)
                     )


  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                      5. Error propogation                   ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  
  # get ready for error propagation
  
                                                        
  
  
  cen_error_propogate<-cen[,c('species','year','height_final','density','dbh')]
  cen_error_propogate$sdWD<-0
  cen_error_propogate$HRSE<-1 # assume an error of 1 m on measured H
  cen_error_propogate$dbh<-  cen_error_propogate$dbh/10
  
  
  
  
  #  This is the line actually calculate standard error. Dpropag="chave2004" is   
  #                                                                               
  #  the only error we adopted, the measurement random error of dbh. We believe   
  #                                                                               
  #  that others are all systematic error, set them as 0                          
  
  
  resultMC_Allom_H_WD_D<-by(cen_error_propogate, cen_error_propogate$year,
                            function(x) AGBmonteCarlo_no_allometric(D=x$dbh,WD=x$density,errWD=x$sdWD*0, H=x$height_final,
                                                                    errH=x$HRSE*0, Dpropag="chave2004",Carbon = T,Carbon_sd=0),
                            simplify=F)
  
  resultMC_Allom_H_WD_D<-by(cen_error_propogate, cen_error_propogate$year,
                            function(x) AGBmonteCarlo(D=x$dbh,WD=x$density,errWD=x$sdWD, H=x$height_final,
                                                                    errH=x$HRSE, Dpropag="chave2004",Carbon = T),
                            simplify=F)

  #well, if you want to calculate per tree biomass using BIOMASS package, this is the language
  
  #BIOMASS_way_of_AGB<-by(cen_error_propogate, cen_error_propogate$year,
  #     function(x) computeAGB(D=x$dbh,WD=x$density,H=x$height_final),
  #     simplify=F) 
 
  # You can see that the BIOMASS package calculate a mean very similar to ours
  
  # this line just take everything together and get a proportional standard deviation for 
  # our biomass and npp
  NPPacw_MgC_ha_yr<- NPPacw_MgC_ha_yr %>%
    mutate(biomass_year1_std =  biomass_year1_per_ha_MgC * (resultMC_Allom_H_WD_D[[census1_year]]$sdAGC) / resultMC_Allom_H_WD_D[[census1_year]]$meanAGC ,
           biomass_year2_std =  biomass_year2_per_ha_MgC * (resultMC_Allom_H_WD_D[[census2_year]]$sdAGC) / resultMC_Allom_H_WD_D[[census2_year]]$meanAGC ,
           number_of_tree_year1=number_of_tree_year1,
           number_of_tree_year2=number_of_tree_year2,
           npp_std=sqrt(biomass_year1_std^2 + biomass_year2_std^2)
# if you need to convert unit:
#          npp_per_ha_MgC_year = npp_per_ha_MgC_year* 10
#          npp_std = npp_std* 10
           )
  
  NPPacw_MgC_ha_yr$plot_code<-plotname
  NPPacw_MgC_ha_yr$census1_year<-census1_year
  NPPacw_MgC_ha_yr$census2_year<-census2_year
  write.csv(npp, file=paste0(plotname,census1_year,census2_year,"NPPpertree.csv"))
  write.csv(NPPacw_MgC_ha_yr, file=paste0(plotname,census1_year,census2_year,"NPPper_ha.csv"))


  

  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                  6. Additional information                 ----
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

# This will also bring you wood density and its standard deviation
# which will allow you to calculate the corresponding error. You can try this if you are interested in.
# We believe that wood density error 
# is a systematic error which should not be included for stem npp.
  
  
# What if you want to loop through everthing, assuming that you have so many census?
# Self defined function like this:
  # my_lovely_function is the self-defined function containing all the codes
  
  #  return(NPPacw_MgC_ha_yr)
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