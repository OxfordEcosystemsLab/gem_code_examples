Terhi_40_to_120_magical_conversion<-function(Search.time.cum,Total){
  Tot.SCorr<-list()
  #Search.time.cum=one_sample_only$time_step
  #Total=one_sample_only$my_data
  
  
  
  
  # Fit four alternative saturating functions to the data
  # Michaelis-Mente, Exponential rise, Power, Logarithmic. See GEM manual for details
  # Combined code identifies the data belonning to the same core at the same date
  
  
  # Michaelis-Menten model
  # a*TIME/(b+TIME)
  data.NPP<-data.frame(Search.time.cum,Total)%>%
    drop_na()
  if (nrow(data.NPP)<3) {
    Tot.SCorr[[1]]<-NA
    Tot.SCorr[[2]]<-'failed_due_to_NA_in_data'
    warning('The above sample have too many NA, can not calculate a result',immediate. = TRUE)
  }else{
  data.NPP$Combined.code.2<-'XXXXX'
  
  model1 <- nlsList(Total~b1*Search.time.cum/(b2+Search.time.cum)|Combined.code.2,
                    data=data.NPP, 
                    start=list(b1=1,b2=7),
                    control=list(maxiter=5000,minFactor=0.0000001))
  #summary(model1)
  if (is.null(model1$XXXXX)) {
    b1 <- NA
    b2 <- NA
  }else{
    b1 <- coef(model1)[[1]]
    b2 <- coef(model1)[[2]]
  }
  
  # Exponential rise to maximum model
  
  model2 <- nlsList(Total~b3*(1-exp(-b4*Search.time.cum))|Combined.code.2,
                    data=data.NPP, 
                    start=list(b3=1,b4=0.08),
                    control=list(maxiter=5000,minFactor=0.0000001))
  #summary(model2)
  
  if (is.null(model2$XXXXX)) {
    b3 <- NA
    b4 <- NA
  }else{
    b3 <- coef(model2)[[1]]
    b4 <- coef(model2)[[2]]
  }
  
  
  # Power model
  model3<-nlsList(Total~b5*Search.time.cum^b6 |Combined.code.2,
                  data=data.NPP, 
                  start=list(b5=1,b6=0.2),
                  control=list(maxiter=5000,minFactor=0.0000001))
  #summary(model3)
  
  if (is.null(model3$XXXXX)) {
    b5 <- NA
    b6 <- NA
  }else{
    b5 <- coef(model3)[[1]]
    b6 <- coef(model3)[[2]]
  }
  
  # Logarithmic model
  
  model4 <- nlsList(Total~b7+b8*log(Search.time.cum) |Combined.code.2,
                    data=data.NPP, 
                    start=list(b7=0,b8=0.07),
                    control=list(maxiter=5000,minFactor=0.0000001))
  #summary(model4)
  if (is.null(model4$XXXXX)) {
    b7 <- NA
    b8 <- NA
  }else{
    b7 <- coef(model4)[[1]]
    b8 <- coef(model4)[[2]]
  }
  
  
  # Look for the maximum value (root mass at the end of the search) for each core 
  # If none of the curves converged, use this as the estimate of the root mass
  
  max_search <- tapply(data.NPP$Total,data.NPP$Combined.code.2,max)
  
  
  # Create a dataframe that combines the parameters of the four alternative models and max root biomass
  
  parameters <- data.frame(b1,b2,b3,b4,b5,b6,b7,b8,max_search)
  Combined.code.2 <- rownames(parameters)
  rownames(parameters)=NULL
  parameters <- cbind(Combined.code.2, parameters)
  names(parameters)
  
  
  # Solve the four equations to 120 mins
  # We assume that after searching 120 mins we would have been able to extract all the roots
  # So this represent the total root mass in the core
  # Chris has used 100 mins and power law, but best to stick to the same method as Khoon (120 mins and log-curve as the default)
  
  Maxtime <- 120 #120 mins as the max search time
  
  # Each equation solved to 120 mins (--> total root mass in the core)
  MichaelisMenten <- b1*Maxtime/(b2+Maxtime)
  ExponentialRise <- b3*(1-exp(-b4*Maxtime))
  Power <- b5*Maxtime^b6
  Logarithmic <- b7+b8*log(Maxtime)
  

  
  # Total root NPP, assuming that all roots are extracted in 120 mins ('Search time corrected root NPP')
  # Create a data frame that combines the model parameters and the results from the four models (at 120 mins)
  
  Root.SCorr <- cbind(parameters, MichaelisMenten, ExponentialRise, Power, Logarithmic)
  names(Root.SCorr) <- c("Combined.code.2", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "max", "MichaelisMenten", "ExponentialRise", "Power", "Logarithmic") 
  names(Root.SCorr)
  
  
  
  # Let's assume Logarithmi is the best model, 
  # If that does not converge, then Michaelis-Mente
  # If that does not converge. then Power
  # If that does not converge, then ExponentialRis
  # If none of the curves converge, then used the total cumulative value from the raw data (max)
  
  Tot.SCorr[[1]] <- ifelse(!is.na(Root.SCorr$Logarithmic), Root.SCorr$Logarithmic, 
                      ifelse(!is.na(Root.SCorr$MichaelisMenten), Root.SCorr$MichaelisMenten, 
                             ifelse(!is.na(Root.SCorr$Power), Root.SCorr$Power, 
                                    ifelse(!is.na(Root.SCorr$ExponentialRise), Root.SCorr$ExponentialRise, Root.SCorr$max))))

  Tot.SCorr[[2]] <- ifelse(!is.na(Root.SCorr$Logarithmic), 'Logarithmic', 
                      ifelse(!is.na(Root.SCorr$MichaelisMenten), 'MichaelisMenten', 
                             ifelse(!is.na(Root.SCorr$Power), 'Power', 
                                    ifelse(!is.na(Root.SCorr$ExponentialRise), Root.SCorr$ExponentialRise, 'just_maximum'))))
  if (Tot.SCorr[[2]]=='just_maximum') {
    warning('All curves fitting failed! Method just_maximum was used, where the root mass at the final timestep was used, no 40 to 120 correction, if you see this lots of times, it is possible that one of your options is wrong')
  }
  
  }
  return(Tot.SCorr)
}
