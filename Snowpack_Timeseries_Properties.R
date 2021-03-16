########################################################################################################
### Snowpack_Timeseries_Properties.R
###
### A series of functions that can be used to describe the seasonal snowpack
###
### These functions have two required inputs.
### 1) A timeseries of the dates in a water year (October 1 to September 29)
### 2) Snow Water Equivalnt [depth] for each date Note: Units in will equal units out, no conversion is done
###
### Peak SWE: The maximum amount of avaialbe snow water equivalnt [depth]
### Date of Peak SWE: The data where Peak Swe occurs
### Snow Disapearence Date: The data at which SWE first goes to zero
### Snow on date: The data that snow is measured for n consecutive days, where n = days_with_snow and can be defined in the function
### Center of Mass: The mass weighted average of occurrence of a hydrologic time series (date)
###
### Notes: not library or package is needed for these function.  To test these function there are several
###        lines of code that are commented out in this R-script. Finaly, this code works best on daily data.
###
### By: Mikey Johnson
### Last Edited: May 04, 2020
########################################################################################################

#library needed for the function
library(dplyr)      # data manipulation

# code to test the functions
#library(snotelr)    # downloading SNOTEL data
#Mt.Rose <- snotel_download(site_id = 652, internal = TRUE) # downloading Hogg Pass, SWE[mm] and temp[Degrees C]
#MR_WY_2018 <- filter(Mt.Rose, date <= "2018-09-30", date >= "2017-10-01") # seperating out the data (End Date, Start Date)



##### Peak SWE #####
Peak_SWE <- function(dates, SWE){
  peak <- max(SWE[1:300]) # Limiting snow season to August 1
  return(peak)
}
#Peak_SWE(MR_WY_2018$date, MR_WY_2018$snow_water_equivalent)



##### Date of Peak SWE #####
Date_of_Peak_SWE <- function(dates, SWE){
  peak <- max(SWE[1:300]) # Limiting snow season to August 1
  days_at_peak <- which(SWE == peak)
  peak_swe_date <- dates[first(days_at_peak)]
  return(peak_swe_date)
}
#Date_of_Peak_SWE(MR_WY_2018$date, MR_WY_2018$snow_water_equivalent)



##### Snow Disapearnece Date #####
SDD <- function(dates, SWE){
  peak <- max(SWE[1:300]) # Limiting snow season to August 1
  if(peak < 0.01 | is.na(peak)){
    sdd = NA
    } else{
    days_at_peak <- which(SWE == peak)
    peak_swe_date <- dates[first(days_at_peak)]
    zero_days_after_peak_swe <- which(SWE==0 & dates >= peak_swe_date)
    sdd <- dates[first(zero_days_after_peak_swe)]
    }
  return(sdd)
}
#SDD(MR_WY_2018$date, MR_WY_2018$snow_water_equivalent)



##### Snow-on date #####
Snow_on_Date <- function(dates,
                         SWE,
                         days_with_snow = 5   # Number of snowcovered days in a row to be consided the snow-on date
){
  days_above_zero <-which(SWE > 0)
  snow_check <- rep(NA,length(days_above_zero)) # Check to see if snow is on the gound by the number of days_with_snow
  days_check <- rep(NA,days_with_snow-1)
  
  for(i in 1:length(days_above_zero)){
    for (j in 1:days_with_snow-1){days_check[j] = days_above_zero[i+j] == days_above_zero[i]+j}
    snow_check[i] <- all(days_check)
    days_check <- rep(NA,days_with_snow-1)
  }
  SOD <- dates[days_above_zero[first(which(snow_check == TRUE))]] # First of the days where snow is on the gound longer than 5 days
  return(SOD)
}
#Snow_on_Date(MR_WY_2018$date, MR_WY_2018$snow_water_equivalent,5)



##### Center of Mass #####
Center_of_Mass <- function(dates,
                           SWE,
                           days_with_snow = 5 # days_with_snow for the Snow_on_Date function
                           ){
start_day <- which(dates == Snow_on_Date(dates,SWE, days_with_snow))
end_day <- which(dates == SDD(dates,SWE))
COM <- round(sum(start_day:(end_day-1) * SWE[start_day:(end_day-1)]) / sum(SWE[start_day:(end_day-1)]))
return(dates[COM])
}
#Center_of_Mass(MR_WY_2018$date, MR_WY_2018$snow_water_equivalent,5)


##### Cumulative Gain function #####
cumulative_gain <- function(SWE_timeseries){

  cume_gain <- rep(NA, length(SWE_timeseries))
  cume_gain[1] <- 0

  for (i in 2:length(SWE_timeseries)){
    cume_gain[i] <- ifelse(SWE_timeseries[i]>SWE_timeseries[i-1],
                           cume_gain[i-1]+abs(SWE_timeseries[i-1]-SWE_timeseries[i]),
                           cume_gain[i-1])
  }
  return(cume_gain)
}


##### Cumulative Loss function #####
cumulative_loss <- function(SWE_timeseries){
  
  cume_loss <- rep(NA, length(SWE_timeseries))
  cume_loss[1] <- 0
  
  for (i in 2:length(SWE_timeseries)){
    cume_loss[i] <- ifelse(SWE_timeseries[i]<SWE_timeseries[i-1],
                           cume_loss[i-1]-abs(SWE_timeseries[i-1]-SWE_timeseries[i]),
                           cume_loss[i-1])
  }
  return(cume_loss)
}





# ------------- Old Snow-on date function -------------------#
##### Snow-on Date #####
#Snow_on_Date <- function(dates, SWE){
#  days_above_zero <-which(SWE > 0)
#  snow_check <- rep(NA,length(days_above_zero)) # Check to see if snow is on the gound 5 days after this day
#  for(i in 1:length(days_above_zero)){
#    snow_check[i] <-days_above_zero[i+1] == days_above_zero[i]+1 && 
#      days_above_zero[i+2] == days_above_zero[i]+2 &&
#      days_above_zero[i+3] == days_above_zero[i]+3 &&
#      days_above_zero[i+4] == days_above_zero[i]+4 &&
#      days_above_zero[i+5] == days_above_zero[i]+5
#    }
#  SOD <- dates[days_above_zero[first(which(snow_check == TRUE))]] # First of the days where snow is on the gound longer than 5 days
#  return(SOD)
#}
##Snow_on_Date(MR_WY_2018$date, MR_WY_2018$snow_water_equivalent)
