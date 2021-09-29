
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                             CANOPY RESPIRATION                           ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                1. Information                            ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#  Script originally designed by Dr Christopher E Doughty 2013/09/30
#  Modified by Dr Cecile Giradin in 2016
#
#  Modified by Huanyuan Zhang hyzhang1996@gmail.com
#
#  2021 August 07 as part of the African data workshop

#  Huanyuan: In fact, this is the last script I am writing for African data
#  workshop, because it is the most challenging....however, it is interesting 
#  because it was originally written in Matlab. I was once a Matlab user who
#  has opinions against R. However, now I am mainly using R because it makes
#  the calculation process more obvious and make codes more readable.



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


##~~~~~~~~~~~~~~~~~~~~~~~
##  ···important note----
##~~~~~~~~~~~~~~~~~~~~~~~



#  These codes arbitrarily set two seasons (wet and dry), this is simply     
#  because in Chris's site Kenia, Bolivia, they measured leaf respiration in 
#  November and June only, two months, represent two seasons....Therefore, if
#  your datasets of lai and leaf respiration have a perfect match of time,   
#  you might want to join them, instead of summarising into two distinct     
#  season, Or, if your leaf respiration was measured in one month only.....  
#  you don't even bother with season...Similarly, if you don't have sun share
#  leaves, simply fill r_shade and r_sun with the same value. Remember, the  
#  simpler, the better!                                                      




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                             3. data preparation                          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


##~~~~~~~~~~~~~~~~~~~~~~
##···quality check  ----
##~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
setwd("F:/Side_project/african_data_workshop/General/Dataset examples/leaf_respiration")

datalr <- read.csv('datalr_20191107_ABC.csv',
                   sep = ",",
                   header = T)%>%filter(plot_code=='1')

datalai <- read.csv('Canopy_LAI_20191107_ABC.csv',
                   sep = ",",
                   header = T)%>%filter(plot_code=='1')



##~~~~~~~~~~~~~~~~~~~~~
##···your options  ----
##~~~~~~~~~~~~~~~~~~~~~

shaded=T #Do you have shade leaves in your plot? say TRUE if you want me to consider shade leaves
         #If you think you might have shade leaves only in some months, you might want to add another column "shaded" into datalai to indicate which row has shaded leaves
dryseason=c(8,9,10,11)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      4. Organise dark respiration data                   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_table<-datalr%>%
  mutate(across(c('r_shade','r_sun'),~ replace(., (.< -1.5 | .>0),NA)))%>%
  #remove respiration value larger than 0 and smaller than 1.5  
  mutate(season=ifelse(month %in% dryseason, 'dry','wet'))  clean_table$r_shade_mean=0
  clean_table$r_shade_mean=0
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                       5. Get total leaf area from LAI                    ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# get total leaf area

# where K is the light extinction coefficient, L is the leaf area
# index, and Lsun is the sunlit leaf fraction. The model assumed
# a random distribution of leaves, and calculated K as:
# where Z is the solar zenith angle which we set to 30. 
K = 0.5/cos(30*pi/180)



datalai$shaded=shaded
result<-datalai%>%
  mutate(sun_area = ifelse(shaded,  (1-exp(-K*lai))/K,  lai), #If true, you have shade leaves, sun_leaves is a proportion of LAI, otherwise, sun_leaves equal to LAI
         shade_area = lai - sun_area ) %>%## estimate how many sun leaves
  mutate(season=ifelse(month %in% dryseason, 'dry','wet'))%>%
  left_join(clean_table,by=c('season','plot_code'))%>%
  mutate(
    total_r_mean=r_shade_mean *shade_area + r_sun_mean*sun_area,
    total_r_std = r_shade_std *shade_area + r_sun_std*sun_area
   )%>%
  select(year,month,plot_code,total_r_mean,total_r_std,season,r_shade_n,r_sun_n,r_shade_mean,r_sun_mean,lai)
  
# After this, the original R codes (and its corresponding matlab code)
# want to bring the month to month variation of aridity (indicated by volumatric water content measured for soil respiraiton) into leaves respiration
# However, I (huanyuan) can't really understand the way the are calculating....
# Anyway, I am going to ignore aridity variation now....


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              6. Unit conversion                          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# convert units umol m2s-1 to MgC ha year = 1mo=2592000sec, 10000m2=1ha,
# 1000000umol = 1 mol, 1mol = 12 g, 1000000g=1Mg

convert = (2592000*10000*12)/(1000000*1000000)*12 *0.67
#multiply by 0.67 for daytime respiration

All_resp2<-result%>%
  mutate(total_r_mean = -total_r_mean * convert,
         total_r_std = total_r_std * convert)%>%
  mutate(total_r_ste = total_r_std /sqrt((r_shade_n+r_sun_n)/2))
# Unit conversion from umol m2s-1 to MgC ha year
mean(All_resp2$total_r_mean,na.rm=T)

# In Chris's script, they also apply a correction according to soil moisture, is this necessary?






##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            Archived information                          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#    Convert Chris Doughty matlab data into standard format    ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
setwd("F:/Side_project/african_data_workshop/General/Dataset examples/leaf_respiration")

#setwd("D:/Dokumente/My Dropbox/Carbon_Use_Efficieny_R/Original-matlab-code/Ken_Iq-Tang")
dir()
Dataall <- readMat(con="Original-matlab-code/Ken_Iq-Tang/Dataall.mat")

# leaf respiration:
datalr <- data.frame(Dataall$Resleafall)   # leaf respiration
datalai <- data.frame(Dataall$LAIall)      # LAI data
datarest <- data.frame(Dataall$Restotall)  # total respiration; # enter data for volumetric water contents

names(datalr) <- c("year","month","plot","tree_number","correction","A_sun","A_shade","r_sun","r_shade")
names(datalai) <- c("year","month","plot","N.N","N.N","lai","lai_std")
names(datarest) <- c("year","month","plot","point","N.N","temperature_C","volumetric_water_content","chamber_height","flux")

Dark_resp<-datalr%>%
  select(-c('A_sun','A_shade'))%>%
  gather(key='id',value='Dark_resp',c('r_sun','r_shade'))%>%
  separate(id,into=c('r','sun_or_shade'),sep='_')%>%
  select(-r)
  
Assimilation<-datalr%>%
  select(-c('r_sun','r_shade'))%>%
  gather(key='id',value='Assimilation',c('A_sun','A_shade'))%>%
  separate(id,into=c('A','sun_or_shade'),sep='_')%>%
  select(-A)

write.csv(datalr, file = 'datalr_20191107_ABC.csv')
write.csv(datalai, file = 'Canopy_LAI_20191107_ABC.csv')
write.csv(datarest, file = 'weather_tot_soil_resp_20190130_ABC.csv')



#                    useful across template                    ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


clean_table<-datalr%>%
  mutate(across(c('Dark_resp'),~ replace(., (.< -1.5 | .>0),NA)))%>%
  #remove respiration value larger than 0 and smaller than 1.5  
  mutate(season=ifelse(month %in% dryseason, 'dry','wet'))%>%
  #add dry wet season flag according to pre-defined 'dryseason'
  group_by(season,sun_or_shade)%>%
  summarise(across(c('r_sun','r_shade','A_sun','A_shade'),mean,.names = "{.col}.mean",na.rm=T),
            across(c('r_sun','r_shade','A_sun','A_shade'),sd,.names = "{.col}.std",na.rm=T),
            across(c('r_sun','r_shade','A_sun','A_shade'),~sum(!is.na(.)),.names = "{.col}.n")
  )



