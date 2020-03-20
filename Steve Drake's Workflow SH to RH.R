###########################################################################################################
### Steve Drake's Code to Specific Humidity to Relative Humididty, using elevation and temperature
###
### This code was orignaly written in MatLab and I adapted it into an R Script
###
### Written by: Dr. Steve Drake
### Adapted by: Mikey Johnson
### Last Edited: March 19, 2020
###########################################################################################################

hypsometric_p <- function(p_in=101300,t_source,t_target,z_source,z_target){
  #Compute pressure at one height and adjust using the hypsometric equation.
  # The ouput pressure units are the same as the input pressure units.
  # ( Bluestein, 1992: use virtual temperature instead of temperature).
  
  # p_in - pressure in Pa or mb, same units in and out
  # t_source - temperature at the source location (degrees K) 
  # t_target - temperature at the target location (degrees K)
  # z_source - height of source location (meters)
  # z_target - height of target location (meters)
  
  # Constants
  Rd <- 287 # dry ideal gas constant (J/mole-kg)
  g <- 9.81 # gracity in m/s^2
  
  # Average temperature should be pressure weighted but we don't know the target pressure so just use 
  # a linear average (typical).
  t_bar <- (t_source + t_target) / 2
  
  # Pressure in the same units as the input pressure
  p <- p_in*exp (g*(z_source - z_target)/(Rd * t_bar))
  return(p)
}


esat_bolton <- function(T){  #Given temperature (degrees C), compute saturation vapor pressure (hPa).
  es <- 6.112 * exp(17.67 * T / (T + 243.5)) # Bolton (1980), eq. 2.17
  return(es)
}


SH_to_RH <- function(Tair_C,sh,elev){
# Work flow provided by Dr. Steve Drake
# You will need the functions "esat_bolton" and "hypsometric_p"

# Tair - air temperature (degrees c)
# sh - specific humidity (kg/kg)
# elev - elevation (meters)


T0 <- 273.16                           # for K to C
Tair_K <- Tair_C + 273.16              # Air Temperature in Kelvin
T2 <- Tair_K + (0.005 * elev)          # assume a 5 K/km lapse rate
p <-  hypsometric_p(p_in = 101300,     # pressure depending on elevation and temperature
                    t_source = T2,
                    t_target = Tair_K,
                    z_source = 0,
                    z_target = elev)              

es <- esat_bolton(Tair_C)              # saturated vapor pressure hPa
e = sh * p/100 / (0.387 * sh + 0.662)  # pressure now in hPa
rh <- e / es                           # RH (as a fraction) is a bit high but not unreasonable
return(rh)
}















