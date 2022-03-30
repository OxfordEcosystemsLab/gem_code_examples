
#...............................................................................
#                                                                              .
#  This script try to bring all components of the carbon cycle - GEM site -    .
#  together, two main challenges to solve here, (1) error propogation (2)      .
#  significant test                                                            .
#                                                                              .
#...............................................................................
Sum_with_error <- function(x, na.rm=TRUE){
  if (na.rm) x <- na.omit(x)
  x2 <- x^2
  sqrt(sum(x2))
}

# Sum_with_error is basically equation 1b in https://www.geol.lsu.edu/jlorenzo/geophysics/uncertainties/Uncertaintiespart2.html

standard_error_calc <- function(x, na.rm=TRUE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}

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


#...............................................................................
#                                                                              .
#  > data %>% mutate_with_error(z ~ eex*y)                                     .
#                                                                              .
#          eex          y      eex_se       y_se           z       d_se        .
#                                                                              .
#  1 0.9238287 0.13630719 0.047671489 0.05553732 0.125924488 0.05171681        .
#                                                                              .
#  2 0.4824979 0.44565748 0.002748711 0.07477338 0.215028791 0.03609879        .
#                                                                              .
#  3 0.3833836 0.01762012 0.045559610 0.03176834 0.006755264 0.01220589        .
#                                                                              .
#  4 0.6018407 0.58707970 0.022582821 0.04066460 0.353328436 0.02783397        .
#                                                                              .
#  5 0.1342505 0.91195619 0.075698744 0.06574103 0.122430594 0.06959582        .
#                                                                              .
#...............................................................................

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                    NPP                                   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# I just grabbed the values from Dr Sam Moore's paper, GCB 2018

npp_table<-openxlsx::read.xlsx('F:/Side_project/african_data_workshop/gem_code_examples/Complete_the_carbon_cycle/Error_propogation/trait_NPP_ghana_organized.xlsx',sheet='NPP_organised')


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              stem_respiration                            ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Stem_resp<-read_csv('F:/Oxford/Chapter_two/stem_respiration/output/stem_respiration_Ghana_20220310_HYZZ_per_plot.csv')%>%
  group_by(plot_code)%>%
  summarise(MgC_year_per_ha=mean(MgC_year_per_ha,na.rm=T),
            MgC_year_per_ha_se = Sum_with_error(MgC_year_per_ha_se,na.rm=T)/length(MgC_year_per_ha_se))%>%
  mutate(plot_code=str_replace_all(plot_code,'-','_'))


#  # In the above line, I use Sum_with_error and divided by the number of       
#  measurement to calculate final standard error as a mean over multiple        
#  measurements (we believe that repeated measurement could reduce standard     
#  error)                                                                       


#...............................................................................
#                                                                              .
#  combine NPP with stem respiration                                           .
#                                                                              .
#...............................................................................

combined<- npp_table %>%
            left_join(Stem_resp,by='plot_code')%>%
# note that now I have MgC_year_per_ha and NPP, and MgC_year_per_ha_se and NPP_se
# you can customzie the below equation with anything, just make sure you have a corresponding _se
  mutate_with_error (NPP_plus_stem ~ MgC_year_per_ha + NPP)
# of course you can do XXXXX ~ MgC_year_per_ha + NPP + Respiration_root 
# and plus many more as long as you have corresponding Respiration_root_se


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              significance test                           ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# How to now whether the NPP_plus_stem is significantly different between plots? 

# Z test hahahha, 

# we are following this one https://cran.r-project.org/web/packages/distributions3/vignettes/two-sample-z-test.html

ANK_01<- combined%>%
  filter(plot_code == 'ANK_01')

KOG_02<- combined%>%
  filter(plot_code == 'KOG_02')

simple_z_test<-function(mean1,mean2,se1,se2){
  z_stat <- (mean1 - mean2) / 
    sqrt(se1^2 + se2^2)
  
  p_value<-1 - pnorm(abs(z_stat)) + pnorm(-abs(z_stat))
  
  return(data.frame(z_stat,p_value))
}

anyway<-simple_z_test(ANK_01$NPP,KOG_02$NPP,ANK_01$NPP_se,KOG_02$NPP_se)
