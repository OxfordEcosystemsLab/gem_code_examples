library(tidyverse)
setwd('F:/Side_project/african_data_workshop/General/Dataset examples/plot_something')
census<-read_csv('fine_litter_fall_NPP_finest_KATH.csv')
census$totalflf_MgC_ha_month<-as.numeric(census$totalflf_MgC_ha_month)
census$plot_code<-as.factor(census$plot_code)

     #.........................Self functions.........................

weighted.var.se <- function(x, w, na.rm=FALSE)
  #  Computes the variance of a weighted mean following Cochran 1977 definition
{
  if (na.rm) { w <- w[i <- !is.na(x)]; x <- x[i] }
  n = length(w)
  xWbar = weighted.mean(x,w,na.rm=na.rm)
  wbar = mean(w)
  out = n/((n-1)*sum(w)^2)*(sum((w*x-wbar*xWbar)^2)-2*xWbar*sum((w-wbar)*(w*x-wbar*xWbar))+xWbar^2*sum((w-wbar)^2))
  return(out)
}


standard_error_calc <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}

     #......................average the dataset.......................


NPP_by_plot<-census%>%
  mutate(totalflf_MgC_ha_year= totalflf_MgC_ha_month *12)%>%
  group_by(plot_code)%>%
  summarise(mean_canopy_NPP_MgC_ha_year = mean(totalflf_MgC_ha_year, na.rm=T),
            se_canopy_NPP_MgC_ha_year = standard_error_calc(totalflf_MgC_ha_year, na.rm=T))


NPP_by_year<-census%>%
  mutate(totalflf_MgC_ha_year= totalflf_MgC_ha_month *12)%>%
  group_by(year)%>%
  summarise(mean_canopy_NPP_MgC_ha_year = mean(totalflf_MgC_ha_year, na.rm=T))

NPP_by_year_by_plot<-census%>%
  mutate(totalflf_MgC_ha_year= totalflf_MgC_ha_month *12)%>%
  group_by(year,plot_code)%>%
  summarise(mean_canopy_NPP_MgC_ha_year = mean(totalflf_MgC_ha_year, na.rm=T),
            se_canopy_NPP_MgC_ha_year = standard_error_calc(totalflf_MgC_ha_year, na.rm=T))


NPP_by_year_by_plot_weighted<-census%>%
  mutate(totalflf_MgC_ha_year= totalflf_MgC_ha_month *12)%>%
  group_by(year,plot_code)%>%
  summarise(mean_canopy_NPP_MgC_ha_year = weighted.mean(totalflf_MgC_ha_year,meas_int_days, na.rm=T),
            se_canopy_NPP_MgC_ha_year = weighted.var.se(totalflf_MgC_ha_year,meas_int_days, na.rm=T))

     #........................Draw some plots.........................

ggplot(NPP_by_plot,
       aes(x = plot_code, y = mean_canopy_NPP_MgC_ha_year)) +
  geom_bar(stat = "identity",
           position = position_dodge(),
           fill = 'gray') +
  geom_errorbar(
    aes(
      ymin = mean_canopy_NPP_MgC_ha_year - se_canopy_NPP_MgC_ha_year,
      ymax = mean_canopy_NPP_MgC_ha_year + se_canopy_NPP_MgC_ha_year
    ),
    width = .2,
    position = position_dodge(.9)
  ) +
  theme_minimal()

# Facet wrap

ggplot(NPP_by_year_by_plot_weighted,
       aes(x = plot_code, y = mean_canopy_NPP_MgC_ha_year)) +
  geom_bar(stat = "identity",
           position = position_dodge(),
           fill = 'gray') +
  geom_errorbar(
    aes(
      ymin = mean_canopy_NPP_MgC_ha_year - se_canopy_NPP_MgC_ha_year,
      ymax = mean_canopy_NPP_MgC_ha_year + se_canopy_NPP_MgC_ha_year
    ),
    width = .2,
    position = position_dodge(.9)
  ) +
  facet_wrap(~year, ncol = 5)+
  theme_minimal()
