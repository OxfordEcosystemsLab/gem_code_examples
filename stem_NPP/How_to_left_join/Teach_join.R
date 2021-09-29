# Now you have long_format data after running either:
# (1) Organize_census_data_from_plotDump.R
# (2) Organize_census_data_from_SingleCensus.R

# if you use (2), then you are not relying on the output of forestplot, so you will need to "join" wood density data from somewhere else.

# Now assume that your output is XXX_02_census_data_long_format_no_density.csv
# Your wood density is in my_density_table1.xlsx, one row - one species - one wood density.
# Then you can do:
library(tidyverse)
setwd('F:/Side_project/african_data_workshop/General/Dataset examples - Exemples de jeux de donnée/stem_NPP/How_to_left_join')
long_format<-read.csv('XXX_02_census_data_long_format_no_density.csv')

density_data<-openxlsx::read.xlsx('my_density_table1.xlsx')%>%
  group_by(Species)%>%
  dplyr::summarise(density_unique=mean(density))
# This part average density according to species. we want to make sure species in unique in density_data

# Then we can do the join
final_data<-left_join(long_format,density_data,by='Species')
final_data$density<-final_data$density_unique
write.csv(final_data,file='XXX_02_census_data_long_format_with_density.csv')
