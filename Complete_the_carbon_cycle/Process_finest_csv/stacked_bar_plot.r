library(ggplot2)

# create a dataset example
plot <- c(rep("ANK" , 3) , rep("BOB" , 3) , rep("LOP" , 3) , rep("WMA" , 3) )
component <- rep(c("Woody" , "grass" , "root") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(plot,component,value)

# WEN's plot
unique(census$plot_code)
library(tidyverse)
census<-read.csv('C:/Users/Huanyuan/Downloads/EKG_01_fine_litter_fall_NPP_finest.csv')%>%
  group_by(plot_code)%>%
  summarise(leaves_npp=mean(leavesflf_MgC_ha_month,na.rm=T),
            twigs_npp=mean(twigsflf_MgC_ha_month,na.rm=T),
            flowers_npp=mean(flowersflf_MgC_ha_month,na.rm=T))%>%
  pivot_longer(cols = ends_with("_npp"),
               names_to='components',
               values_to = 'npp_value')
  


# Stacked
ggplot(census, aes(fill=components, y=npp_value, x=plot_code)) + 
  geom_bar(position="stack", stat="identity")
