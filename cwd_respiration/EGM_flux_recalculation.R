EGM_flux_recalculation<-function(sub){
# https://github.com/davidorme/egm_r_tools

  diameter<-sub$collar_diameter_cm[1]    #[cm] collar diameter
  collar_height <-sub$collar_height_cm [1]   #[cm] collar height
  last_inputf=sub$InputF[length(sub$InputF)]
  last_time=sub$time[length(sub$time)]
  
P        = tail(sub$atmp_mb, n=1)*100                                           # ambient air pressure at t10 (Pa)
Ta       = tail(sub$air_temp_c, n=1)                                            # air temp at t10 (deg C)
A_collar = (pi*((diameter/2)/100)^2)                                      # Area of the collar (m2)
Vd       <- 0.0012287 # EGM chamber volumme [m3] (constant)
Ru       <- 8.31432    # [J mol^-1 K^-1] (constant)
T_kel    <- 273.15                 # to convert from Kelvin to ºC
V_total  <-  Vd + A_collar*(collar_height/100)   # [m^3] EGM chamber volume + collar volume
# Terhi: colar height might change within a plot, and it does matter to the result
# colloar diamater could be the same for teh whole plot

# This is the equation from the GEM manual, we have replaced it with the equation below.
#C10      = tail(ten_co2, n=1)                                                   # last CO2 measurement of last 10 measurements
#C1       = head(ten_co2, n=1)                                                   # first CO2 measurement of last 10 measurements
#t10      = tail(ten_time, n=1)                                                  # last time step of 10 last measurements
#t1       = head(ten_time, n=1)                                                  # first time step of 10 last measurements
#fl       = ((C10 - C1)/(t10 - t1)) * (P/(Ta + 273.15))*(Vd/A)*((44.01*0.36)/Ru) # CO2 efflux (g CO2 m-2 h-1). This is the equation we have in the GEM manual. We need to update the manal.
#flux     = (fl*A/Vd*(Va+Vd)/A)*6.312                                           # Convert to umol m-2 s-1, and correct for collar height.
# A =A_collar; Va = A_collar*(collarheight/100)
sub <-sub[!is.na(sub$co2ref_ppm_sec), ]
sub <- sub[!is.na(sub$time), ]
sub$co2ref_ppm_sec<-as.numeric(sub$co2ref_ppm_sec)
sub$time<-as.numeric(sub$time)

ten_co2  = sub$co2ref_ppm_sec[-c(1:3)]           # Drop first three values rather than only keep last 10 values.
ten_time =  sub$time [-c(1:3)] 

if (length(ten_co2) >= 4){                   # if at least 7 of the 10 values are different from NA, we apply a linear a regression to get CO2 flux
  
  
  fit      = lm(ten_co2~ten_time)
  Co2slope = fit$coefficients[2]                                       # ["ten_time"]
  flux    = Co2slope * P * V_total / (Ru * (Ta + 273.15)) / A_collar                 # output is in umol m-2 s-1. This equation was provided by Terhi Riutta, January 2018.
  NA_note = 'NA'
  if (!is.numeric(flux)) {
    warning(paste0('flux calculation fail in ','id'))
    NA_note = 'flux calculation fail'
  }
  
}else{
  flux = NA
  NA_note = 'not enough EGM values for fitting curve'
}
flux_umolm2sec<-as.double(round(flux,5))
Error<-NA_note

return(data.frame(flux_umolm2sec, Error,last_inputf,last_time))

}

#EGM_flux_recalculation(sub,12)
