source("error_propogation_based_on_BIOMASS.r") 

grab_total_error<-function(cen_error_propogate,census1_year,NPP_plot){
  
  cen_error_propogate$HRSE<-2 # assume an error of 2 m on measured H
  cen_error_propogate$dbh<-  cen_error_propogate$dbh/10 #the package work on cm
  
  
  Taxo<-correctTaxo(genus=cen_error_propogate$genus,species=cen_error_propogate$Species,useCache=T)
  #Use the above line if your species name is not typed correctly
  #and use cen_error_propogate$genusCorr and cen_error_propogate$speciesCorr in the following line
  cen_error_propogate$genusCorr<-Taxo$genusCorrected
  cen_error_propogate$speciesCorr<-Taxo$speciesCorrected
  WDdata<-getWoodDensity(genus=cen_error_propogate$genusCorr,
                         species=cen_error_propogate$speciesCorr,
                         stand=cen_error_propogate$year)
  cen_error_propogate$WD_by_BIOMASS_package<-WDdata$meanWD
  cen_error_propogate$sdWD<-WDdata$sdWD
  # We need to grab sdWD - standard deviation is wood density
  
  
  error_random<-by(cen_error_propogate, cen_error_propogate$year,
                   function(x) AGBmonteCarlo_no_allometric(D=x$dbh,WD=x$density,errWD=x$sdWD*0, H=x$height_final,
                                                           errH=x$HRSE*0, Dpropag=0.2,Carbon = T,Carbon_sd=0),
                   simplify=F)# Dpropag was 0.2 cm, the standard error of dbh measurement 
  
  
  #  error_random calculate the random error brought by measurements of dbh, by   
  #  assuming that measurement error of dbh is 0.2 cm, Dpropag=0.2, I have to     
  #  modify the package with new function AGBmonteCarlo_no_allometric to remove   
  #  allometric error, which is systematric here                                  
  
  #  you might find that this function give you slightly different results   
  #  everytime you run it, this is caused by Monte Carlo simulation 
  
  
  #  also note that we use sd from year1 only, because the sd_year1 - sd_year2   
  #  is sometime negative, sometime positive, due to mente carlo simulation      
                         
  
  
  Random_error_npp<- NPP_plot %>%
    mutate(biomass_year1_std =  biomass_year1_per_ha_MgC * (error_random[[census1_year]]$sdAGC) / error_random[[census1_year]]$meanAGC ,
           biomass_year2_std =  biomass_year2_per_ha_MgC * (error_random[[census1_year]]$sdAGC) / error_random[[census1_year]]$meanAGC ,
           npp_std_random=sqrt(biomass_year1_std^2 + biomass_year2_std^2))%>%
    select(npp_std_random)
  
  
  error_systematic<-by(cen_error_propogate, cen_error_propogate$year,
                            function(x) AGBmonteCarlo(D=x$dbh,WD=x$density,errWD=x$sdWD, H=x$height_final,
                                                      errH=x$HRSE, Dpropag=0,Carbon = T),
                            simplify=F)
  #This one has all systematic errors considered,we are 
  #going to calculate npp systematic error by:
  #NPP_sd_system = biomass_sd_year2 - biomass_sd_year1
  #npp_std_overall=sqrt(npp_std_systematic^2 + npp_std_random^2))
  
  
  # this line just take everything together and get a proportional standard deviation for 
  # our biomass and npp
  systematic_error_npp<- NPP_plot %>%
    mutate(biomass_year1_std =  biomass_year1_per_ha_MgC * (error_systematic[[census1_year]]$sdAGC) / error_systematic[[census1_year]]$meanAGC ,
           biomass_year2_std =  biomass_year2_per_ha_MgC * (error_systematic[[census1_year]]$sdAGC) / error_systematic[[census1_year]]$meanAGC ,
           npp_std_systematic=(biomass_year2_std - biomass_year1_std))%>%
    select(npp_std_systematic)
          
  
   npp_std_overall=sqrt(systematic_error_npp[1,1]^2 + Random_error_npp[1,1]^2)
  
   
   
   #                       std for biomass                       ~~~
   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
   error_biomass<-by(cen_error_propogate, cen_error_propogate$year,
                        function(x) AGBmonteCarlo(D=x$dbh,WD=x$density,errWD=x$sdWD, H=x$height_final,
                                                  errH=x$HRSE, Dpropag=2 ,Carbon = T),
                        simplify=F)
   
   NPP_plot2<- NPP_plot %>%
     mutate(biomass_year1_std =  biomass_year1_per_ha_MgC * (error_biomass[[census1_year]]$sdAGC) / error_biomass[[census1_year]]$meanAGC ,
            biomass_year2_std =  biomass_year2_per_ha_MgC * (error_biomass[[census1_year]]$sdAGC) / error_biomass[[census1_year]]$meanAGC ,
            )     #  again, lets use year1 only and grab a rough range  
   NPP_plot2$npp_std<-npp_std_overall
  return(NPP_plot2)
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


mutate_with_error = function(.data, f) {
  exprs = list(
    # expression to compute new variable values
    deparse(f[[3]]) %>%
      paste(collapse=''),
    # expression to compute new variable errors
    sapply(all.vars(f[[3]]), function(v) {
      dfdp = deparse(D(f[[3]], v))
      sprintf('(%s_se*(%s))^2', v, dfdp)
    }) %>%
      paste(collapse='+') %>%
      sprintf('sqrt(%s)', .)
  )
  names(exprs) = c(
    deparse(f[[2]]),
    sprintf('%s_se', deparse(f[[2]]))
  )
  myexprs <- purrr::map( exprs, rlang::parse_expr )
  
  .data %>%
    # the standard evaluation alternative of mutate()
    mutate(!!!myexprs)
}


#  I got the above equation from this website, I modify it a bit because        
#  mutate_ is deprecated https://www.r-bloggers.com/2015/01/easy-error-propagation-in-r/                                                       
# How to use this? example here

#  The idea is that, you have some variables (A, B, C) to mutate, you need to   
#  call the standard error (or deviation) of them as A_se, B_se, C_se, yes, just      
#  add _se, and then when you muate_with_error(new_thing = A + B *C), you will    
#  auto get dnew_thing   

