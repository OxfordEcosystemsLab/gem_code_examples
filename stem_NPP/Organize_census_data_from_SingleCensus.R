###########################################################################
###########################################################################
###                                                                     ###
###                  PREPARE PLOT CENSUS DATA FOR MARC                  ###
###                                                                     ###
###########################################################################
###########################################################################



#This script change the format of forest plots data (called plotDump)

##--------------------------------------------------------------------------------
##  ForestPlots files are in a wide format, unfortunately the GEM importers as   -
##     they stand expect it in the long format (one row per tree per census,     -
##  rather than one row for each tree with multiple censuses in that one row).   -
##--------------------------------------------------------------------------------

#Written at "Sat Aug 15 21:53:17 2020"
#Contact: Huanyuan Zhang Email: huanyuan.zhang@ouce.ox.ac.uk



###########################################################################
###########################################################################
###                                                                     ###
###                             INSTRUCTION                             ###
###                                                                     ###
###########################################################################
###########################################################################

##-------------------------------------------------------------------------------------------
##         The files from forestplot are, as I mentioned in wide format, they need to be    -
##                                      in long format.                                     -
##                                                                                          -
##            You will notice from the files that there are two heading rows. The           -
##           first row details the sections of data and the second the individual           -
##                                          fields.                                         -
##                                                                                          -
##                                    The sections are:                                     -
##                                                                                          -
##                                   Tree - common fields                                   -
##                            Census No: # {year.fracton of year}                           -
##                                                                                          -
##                                  The common fields are:                                  -
##                                                                                          -
##          Tree ID, Pv. Tag No, Tag No, T1, T2, X, Y, Family, Species, Tree Notes,         -
##                                        WD, WD Type                                       -
##                                                                                          -
##                                  The Census columns are:                                 -
##          DBH0, DBH1, DPOMtMinus1, DBH2, DBH3, DBH4, POM, F1, F2, F3, F4, LI, CI,         -
##                                 Height, F5, Census Notes                                 -
##                                                                                          -
##          You will need to move each census onto a separate row with the common           -
##          fields for that census repeated. You will also need to add a plot_code          -
##          column to each row with the plot code in the format ABC-## (where ## is         -
##           a number). You will also need to add a census_date column taking the           -
##                        value from the heading row for each census.                       -
##                                                                                          -
##          We will need to also create and populate the species_id, genus_id, and          -
##            family_apg_id fields.
##-------------------------------------------------------------------------------------------

############################################################################
############################################################################
###                                                                      ###
###                                 CODE                                 ###
###                                                                      ###
############################################################################
############################################################################
#
rm(list=ls())
library(rapportools)
library(stringr)
library(data.table)
library(openxlsx)
library(tidyverse)


##-----------------------------------------------------------------------------------
##  Working_folder is the only thing to change, it will extract plot code from its file name  
##-----------------------------------------------------------------------------------


###change this line to your folder containing PlotDump###
Working_folder="F:/Side_project/african_data_workshop/General/Dataset examples - Exemples de jeux de donnée/stem_NPP/"
###change this line to your folder containing PlotDump###


setwd(Working_folder) #Output folder
Files_to_process<-list.files(pattern = '*Single.*csv', full.names = T) #Input folder
# make sure Files_to_process have all the files you want

for (i in 1:length(Files_to_process)){
print(i)
file_name=Files_to_process[i]
pattern1 <- "[A-Z]{3}\\_[0-9]{2}" 
Plot_code<-stringr::str_extract_all(file_name, pattern1) #Extract plot code from its file name


Section_string<-read.csv(file_name,nrows = 2) #read the first and the second heading row
Section_string<-colnames(Section_string)
pattern <- "[0-9]{8}" #find census date, which should be year month day like: 20191230
Census_data<-stringr::str_extract_all(Section_string, pattern) 
Census_date<-Filter(Negate(is.empty), Census_data) #get census date and its column position in the dataframe


#read the whole table
whole_table<-fread(file_name,skip=1)
all_colnames<-colnames(whole_table)
missingcolumn<-which(str_detect(all_colnames, "^D$"))
all_colnames[missingcolumn]<-'dbh'
#This position also denotes the starting posotion of a census
end_of_column<-which(str_detect(all_colnames, "voucher collected")) #this is the last column of a census

#read the common fields
Common_fields<-fread(file_name,select=(1:(missingcolumn[[1]]-1)),skip=1)


#read the Census_field

for (census_id in 1:length(Census_date)){
  Census_field_1<-fread(file_name,select=(missingcolumn[[census_id]]):(end_of_column[[census_id]]),skip=1)#read the census fields
  Census_field_1$census_date<-Census_date[census_id]
  Census_field_1<-cbind.data.frame(Common_fields,Census_field_1) #Combine it with Common_fields
  
  
  
  
if (census_id==1) {
  Final_table<-Census_field_1
} else {
  Final_table<-rbind.data.frame(Final_table,Census_field_1)
}
}

Final_table$plot_code<-Plot_code[[1]]
Final_table2<-Final_table%>%
  dplyr::rename(tree_tag=!!as.name("Tag No"))%>%
  drop_na(tree_tag)%>%
  dplyr::rename(height_m=!!as.name("Height"))%>%
  dplyr::rename(dbh=!!as.name("D"))%>%
  mutate(year=substr(census_date,1,4))%>%
  mutate(month=substr(census_date,5,6))%>%
  mutate(day=substr(census_date,7,8))
##-----------------------
##  save file into new csv  
##-----------------------

#If you have plotDump, you can copy wood density data

write.csv(Final_table2,file=paste(Plot_code[[1]],'_census_data_long_format.csv',sep=''),row.names = F)
}

## column names required for this function:
#plot_code
#tree_tag
#dbh
#height_m
#density
#year
#month
#day