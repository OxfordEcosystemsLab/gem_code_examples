
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

standard_error_calc <- function(x, na.rm=TRUE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}

mutate_with_error = function(.data, f) {
  exprs = list(
    # expression to compute new variable values
    deparse(f[[3]]),
    # expression to compute new variable errors
    sapply(all.vars(f[[3]]), function(v) {
      dfdp = deparse(D(f[[3]], v))
      sprintf('(d%s*(%s))^2', v, dfdp)
    }) %>%
      paste(collapse='+') %>%
      sprintf('sqrt(%s)', .)
  )
  names(exprs) = c(
    deparse(f[[2]]),
    sprintf('d%s', deparse(f[[2]]))
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
#  call the standard error (or deviation) of them as dA, dB, dC, yes, just      
#  add d, and then when you muate_with_error(new_thing = A + B *C), you will    
#  auto get dnew_thing                                                          


#...............................................................................
#                                                                              .
#  > data %>% mutate_with_error(z ~ eex*y)                                     .
#                                                                              .
#          eex          y        deex         dy           z         dz        .
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

npp_table<-openxlsx::read.xlsx('trait_NPP_ghana_organized.xlsx',sheet='NPP_organised')


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              stem_respiration                            ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Stem_resp<-read_csv('F:/Oxford/Chapter_two/stem_respiration/output/stem_respiration_Ghana_20220310_HYZZ_per_plot.csv')%>%
  group_by(plot_code)%>%
  summarise(MgC_year_per_ha=sum(MgC_year_per_ha,na.rm=T),
            MgC_year_per_ha_se = Sum_with_error(MgC_year_per_ha_se,na.rm=T))%>%
  mutate(plot_code=str_replace_all(plot_code,'-','_'))



#...............................................................................
#                                                                              .
#  combine NPP with stem respiration                                           .
#                                                                              .
#...............................................................................

combined<- npp_table %>%
            left_join(Stem_resp,by='plot_code')%>%
  rename('Respiration_stem'=MgC_year_per_ha)%>%
  rename('dRespiration_stem'=MgC_year_per_ha_se)%>%
# note that now I have Respiration_stem and NPP, and dRespiration_stem and dNPP
  mutate_with_error (NPP_plus_stem ~ Respiration_stem + NPP)
# of course you can do XXXXX ~ Respiration_stem + NPP + Respiration_root 
# and plus many more as long as you have corresponding dRespiration_root
