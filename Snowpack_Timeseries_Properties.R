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
###
### By: Mikey Johnson
########################################################################################################

### Peak SWE ###
Peak_SWE <- function(dates, SWE){
  peak <- max(SWE)
  return(peak)
}

### Date of Peak SWE ###
Date_of_Peak_SWE <- function(dates, SWE){
  peak <- max(SWE)
  days_at_peak <- which(SWE == peak)
  peak_swe_date <- dates[first(days_at_peak)]
  return(peak_swe_date)
}

### Snow Disapearnece Date ###
SDD <- function(dates, SWE){
  peak <- max(SWE)
  days_at_peak <- which(SWE == peak)
  peak_swe_date <- dates[first(days_at_peak)]
  zero_days_after_peak_swe <- which(SWE==0 & dates >= peak_swe_date)
  sdd <- dates[first(zero_days_after_peak_swe)]
  return(sdd)
}


# Functions test
#d <- as.Date(c(1:50))
#swe <-c(0:20,25,30,20:1,1,1,0,0,0,1,0)
#Peak_SWE(d,swe)
#Date_of_Peak_SWE(d,swe)
#SDD(d,swe)
