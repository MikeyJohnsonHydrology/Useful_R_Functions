###############################################################################
### SP_Snow_Zone.R                                                          ###
###                                                                         ###
### A function uses mean annuall snow persistance (SP) to define the snow   ###
### zone (Saavedra et al., 2017, Hammond et al., 2018).                     ###
###                                                                         ###
### One small adjustment to the logic is the addtions of a minimum snow     ###
### threshold.  This reduces small snow values generated in a snow model    ###
###                                                                         ###
###                                                                         ###
### Input data:                                                             ###
### - requires daily SWE readings [depth, any units]                        ###
### - min_snow, minimum snow depth in the same units as the SWE data        ###
###                                                                         ###
### Requires: dplyr package                                                 ###
###                                                                         ###
###############################################################################

SP_Snow_Zone <- function(
  daily_swe,
  min_snow = 0.02 * 1000,        # Note (Sterm, Holgrem & Liston, 1994) defines ephemeral to be bewteen 0-50 (cm)
  return_data_type = "numeric"   # choose "numeric" or "character"
){
  n_days <- length(daily_swe)
  n_snow_days <- length(which(daily_swe >= min_snow))
  
  # retunr the data as a character or a numeric value
  if(!(return_data_type == "numeric" | return_data_type == "character")){
    print("unrecognised return_data_type, use either 'numeric' or 'character'")
    stop()
  }
  
  types_numeric <- c(0,0.5,1,2)
  types_character <- c("Rain Zone","Ephemeral Snow Zone","Seasonal Snow Zone","Perminate Snow Zone")
  
  if(return_data_type == "numeric"){
    data_type = types_numeric
  } else {
    data_type = types_character
  }
  
  # Defining the zone
  if(n_snow_days/n_days <= 0.07){zone_type = data_type[1]}
  if(n_snow_days/n_days >= 0.07){zone_type = data_type[2]}
  if(n_snow_days/n_days >= 0.30){zone_type = data_type[3]}
  if(n_snow_days/n_days > 0.90){zone_type = data_type[4]}
  return(zone_type)
}


#### Demp code to run the function ############################################################

## loading librarys
#library(dplyr)   # easy data manipulation
#library(snotelr) # downloading SNOTEL data


#Hogg_Pass <- snotel_download(site_id = 526, internal = TRUE) # downloading Hogg Pass, SWE[mm] and temp[Degrees C]
#HP_2014 <- filter(Hogg_Pass, date <= "2014-09-30", date >= "2013-10-01") # Normal Snow Year
#HP_2015 <- filter(Hogg_Pass, date <= "2015-09-30", date >= "2014-10-01") # Low Snow Year


#SP_Snow_Zone(daily_swe = HP_2014$snow_water_equivalent,       #SWE in (mm)
#          min_snow = 0.02 * 1000,                             #SWE in (mm)
#          return_data_type = "character")

#SP_Snow_Zone(daily_swe = HP_2015$snow_water_equivalent,       #SWE in (mm)
#          min_snow = 0.02 * 1000,                             #SWE in (mm)
#          return_data_type = "character")

#SP_Snow_Zone(daily_swe = HP_2015$snow_water_equivalent,       #SWE in (mm)
#             min_snow = 0.02 * 1000)                          #SWE in (mm)

#SP_Snow_Zone(daily_swe = c(0,0,0,0,0,0,0,0,0),                #SWE in (mm)
#          min_snow = 0.02)


