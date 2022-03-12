These documents come from http://www.chem.hope.edu/~polik/Chem345-2000/errorpropagation.htm

And from https://www.geol.lsu.edu/jlorenzo/geophysics/uncertainties/Uncertaintiespart2.html#addsub

These two websites should introduce the idea of error propogation, then to do it in R: 

Well, now you dont need to calculate error your self but use the package: https://journal.r-project.org/archive/2018/RJ-2018-075/index.html; OR use this as reference https://cran.r-project.org/web/packages/errors/index.html

Alternatively, you can use this function https://www.r-bloggers.com/2015/01/easy-error-propagation-in-r/
I paste it here in case it get lost in the future
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
  .data %>%
    # the standard evaluation alternative of mutate()
    mutate_(.dots=exprs)
}


#  I got the above equation from this website, I modify it a bit because        
#  mutate_ is deprecated https://www.r-bloggers.com/2015/01/easy-error-propagation-in-r/                                                       
# How to use this? example here

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
