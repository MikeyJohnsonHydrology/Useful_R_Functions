#############################################################################################################
### cumulative_gain_loss.r
###
### Two simple function to calucalte the cumulative gain and loss from a snow pack time series.
###
### Function inspired by Figure 2 in (Dickerson-Lange et al., 2017)
###
### Mikey Johnson, UNR, 2020-07-26
#############################################################################################################

### Gain function
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


### Loss function
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


## Test the function
#test_ts <- c(0,-10,-1,1,3,2,3,5,5,4,4,5,10)
#cumulative_gain(test_ts)
#cumulative_loss(test_ts)
