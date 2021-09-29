Calculate_flf_npp<-function(data_flf2){
uid             <- unique(data_flf2$codeb)
xx              <- c()
yy              <- c()
aa              <- c()
bb              <- c()
cc              <- c()
dd              <- c()
ee              <- c()
ff              <- c()
gg              <- c()
hh              <- c()
pb = txtProgressBar(max = length(uid), style = 3) # to create a process bar

# Deal with each litter fall trap
# It is important to note that, the code arrange the dataframe (for one trap) by date, 
# then take the difference between date as the collection interval. 
# You need to make sure your dataset fits such an assumption, your date must be 
# the date of litter fall collection
for (i in 1:length(uid)) { 
  # go through every litter fall trap number, once for one trap
  sub       <- subset(data_flf2, subset=(data_flf2$codeb == uid[i])) # let's focus on one trap
  sub       <- arrange(sub, date)                      # Order it by date.
  
  if(length(sub$codeb) > 1) {
    meas_int      <- get_time_diffs(sub$date) # difference between collection dates, in days. See get_time_diffs in functions.r
    aleaves       <- tail(sub$leaves,-1)
    atwigs        <- tail(sub$twigs,-1)
    aflowers      <- tail(sub$flowers,-1)
    afruits       <- tail(sub$fruits,-1)
    abrom         <- tail(sub$brom,-1)
    aepi          <- tail(sub$epi,-1)
    aother        <- tail(sub$other,-1)
    atotal        <- tail(sub$total,-1)
    # this is to drop the first row
    
    bleaves       <- aleaves/(meas_int) 
    btwigs        <- atwigs/(meas_int)
    bflowers      <- aflowers/(meas_int)
    bfruits       <- afruits/(meas_int)
    bbrom         <- abrom/(meas_int)
    bepi          <- aepi/(meas_int)
    bother        <- aother/(meas_int)
    btotal        <- atotal/(meas_int)
    # amount divided by time interval, to become a flux
    id            <- tail(sub$codew,-1) 
    xx            <- c(xx, id)
    yy            <- c(yy, meas_int)
    aa            <- c(aa, bleaves)
    bb            <- c(bb, btwigs)
    cc            <- c(cc, bflowers)
    dd            <- c(dd, bfruits)
    ee            <- c(ee, bbrom)
    ff            <- c(ff, bepi)
    gg            <- c(gg, bother)
    hh            <- c(hh, btotal)
    #collect them into a dataframe
  } else {  
    print(paste("row number:", i))
    print(paste("trap number:", sub$num))
    print(paste("subset length:", length(sub$codeb)))
    if(exists("error_df")) {
      error_df <- rbind(error_df, data.frame(row = i, trap = sub$num[i], sub_len = length(sub$codeb)))
    } else {
      error_df <- data.frame(row = i, trap = sub$num[i], sub_len = length(sub$codeb))
    }
  }
  setTxtProgressBar(pb, i)
}
close(pb)

# Check if there are errors
if(exists("error_df")) {
  error_df_g <<- error_df # assigning to global variable outside the function.
  print(paste(nrow(error_df), "errors in the data.  See error_df_g."))
}

# Now, we are going to gather information from each trap
# It is extremely important to check that your data2$meas_int_days (time interval between each collection) is realistic!**
data2 <- data.frame(xx, yy, aa, bb, cc, dd, ee, ff, gg, hh)
colnames(data2) <- c("id", "meas_int_days", "bleavesflf_g_trap_day", "btwigs", "bflowers", "bfruits", "bbrom", "bepi", "bother", "btotal")

# Separate info in id
# paste(data_flf2$plot, data_flf2$num, data_flf2$year, data_flf2$month, data_flf2$day, sep=".") 
data2$id                = as.character(data2$id)
temp                    = strsplit(data2$id, "[.]")                     # split this_core into the information we need (plot_code, year, month, day, ingrowth_core_num).
data2$plot_code         = unlist(lapply(temp, `[[`, 1))
data2$num               = unlist(lapply(temp, `[[`, 2))
data2$year              = unlist(lapply(temp, `[[`, 3))
data2$month             = unlist(lapply(temp, `[[`, 4)) 
data2$day               = unlist(lapply(temp, `[[`, 5))

data2$collectiondate    = as.Date(paste(data2$year, data2$month, data2$day, sep="-"), format="%Y-%m-%d") 
return(data2)
}