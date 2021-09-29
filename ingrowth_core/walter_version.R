

#   Census should be                                                        
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



##~~~~~~~~
##  XX  ~~
##~~~~~~~~
##---------------------------------------------------------------------------------
##   1. A harness is a set of straps which fit under a person's arms and fasten   -
##   around their body in order to keep a piece of equipment in place or to       -
##   prevent the person moving from a place.                                      -
##                                                                                -
##   2. A harness is a set of straps which fit un                                 -
##                                                                                -
##   3. A harness is a set of straps which fit                                    -
##                                                                                -
##   4. A harness is a set of straps which                                        -
##---------------------------------------------------------------------------------



st<-read.csv("/Users/Walter/Desktop/maliau_stock.csv",header=T)

fr<-subset(st,time_step_minutes=="10") # subset root stock

#to declare 
name.med<-levels(fr$id)##nombre del ID de los IC
rename.med<-levels(fr$comments)
rootwest_ml <-c()
coef_ml <- c()
intercept_ml <- c()

# mineral layer ml_under_2mm_g

for(i in 1:length(name.med)){
  subset_fr <- subset(fr,fr$id==name.med[i])    
  time_acu <- c()
  root_ml_under_2mm <- c()
  
  for(j in 1:nrow(subset_fr)){
    if(j==1){
      time_acu[j] = subset_fr$time_step_minutes[j] #### time acumulate from 2.5 to 10
      root_ml_under_2mm[j] = subset_fr$ml_under_2mm_g[j]
      
    }else{time_acu[j] = time_acu[j-1]+subset_fr$time_step_minutes[j]   
    root_ml_under_2mm[j] = root_ml_under_2mm[j-1]+subset_fr$ml_under_2mm_g[j] 
    } 
  }
  #plot(time_acu,root_acu)
  if(all(is.na(root_ml_under_2mm)) == TRUE){
    rootwest_ml[i] <- NA
  }
  else{
    fit_ml<- lm(root_ml_under_2mm~log(time_acu))
    ##### function to get equation 
    coef_ml[i]<-as.numeric(coef(fit_ml)["log(time_acu)"]) ####
    intercept_ml[i]<-as.numeric(coef(fit_ml)["(Intercept)"])
    ###         
    time_x=seq(from=10,to=120,by=10) 
    root_est_ml=coef_ml*log(time_x)+intercept_ml #### the all values of root accumulated
    rootwest_ml[i]=tail(root_est_ml,1)####### to get the last value of each soil layer of root estimated
    #rootwest_ml[i]=tail(root_est,1)
  }
}


final_stock <- as.data.frame(cbind(name.med,rootwest_ml))


######## to calculate NPP 

fr_npp<-read.csv("/Users/Walter/Dropbox/GEM-WALTER HUARACA HUASCO-PRIVADO/PhD thesis data and report/Second_paper/Data/Ingrowth cores data/Outputs/Ingrowth core_NPP_mass_world.csv",header=T)

#montane forests
montane_npp<-subset(fr_npp, (plot_code %in% c("ACJ-01","ESP-01","WAY-01","TRU-04","SPD-01","SPD-02","PAN-02","PAN-03","YAY-17","YAY-55")))
montane_npp_ic<-montane_npp %>% group_by(continent,plot_code,year,month,day,ic_number) %>% summarise(dry_mass=sum(mass_gr))

npp_montane<-montane_npp_ic %>% group_by(continent,plot_code,year,month,day,ic_number) %>% summarise(
  biomass=dry_mass/(113.0973),
  carbon=biomass*0.45) #### 0.45 from FRED database

get_time_diffs <- function(date_vec){
  # This function calculates difference between dates, returning the number of days
  date_diff_vec <- numeric(length(date_vec)-1)
  for(i in 2:length(date_vec)){
    date_diff_vec[i-1] <- lubridate::int_length(lubridate::interval(date_vec[i-1], date_vec[i]))/86400
  }
  return(date_diff_vec);
}

npp_montane_2009_2012<-subset(npp_montane,(year %in% c(2009,2010,2011,2012)))

npp_montane_2009_2012$coldate = as.Date(paste(npp_montane_2009_2012$year, npp_montane_2009_2012$month, npp_montane_2009_2012$day, sep="-"), format="%Y-%m-%d")
npp_montane_2009_2012$ic_id = paste(npp_montane_2009_2012$continent,npp_montane_2009_2012$plot_code,npp_montane_2009_2012$ic_number,sep="_")

npp_montane_2009_2012<-npp_montane_2009_2012 %>% group_by(ic_id) %>% # don't deactivate dplyr
  arrange(ic_id,coldate) %>%    # Order data by rhizotron core and date.
  mutate(coldate = as.POSIXct(coldate),   # Convert dates to POSIXct
         interval  = c(90, get_time_diffs(coldate)),  # Get collection interval using get_time_diffs (sourced from functions.r)
         day_npp= carbon/interval,# daily npp
         month_npp= day_npp*30,
         year_npp = day_npp*365)

npp_montane_first<-npp_montane_2009_2012

