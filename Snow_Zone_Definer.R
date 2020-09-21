###############################################################################
### Snow_Zone_Definer.R                                                     ###
###                                                                         ###
### A function to define the Snow Zone. Defined by the number consectutive  ###
### days that snow is on the ground and a minumum swe depth threshhold.     ###
###                                                                         ###
###                                                                         ###
### Input data:                                                             ###
### - requires daily SWE readings [any units]                               ###
### - max_days, how many days are required to define seasonal snow          ###
### - min_days, how many days are needed to define rain and ephemeral snow  ###
### - min_snow, minimum snow depth in the same units as the SWE data        ###
###                                                                         ###
### Requires: dplyr package                                                 ###
###                                                                         ###
###############################################################################


library(dplyr)      # data manipulation

snow_zone <- function(daily_swe,
                      max_days = 30,
                      min_days = 5,
                      min_snow = 0.02 * 1000,        # SWE in (mm) 
                      return_data_type = "numeric"   # choose "numeric" or "character"
                      
                      # Note (Sterm, Holgrem & Liston, 1994) defines ephemeral to be bewteen 0-50 (cm)
){

  tmp <- data.frame(SWE = daily_swe - min_snow) %>%
    mutate(snow_on = ifelse(SWE > 0,1,0)) %>%
    mutate(days_of_snow = sequence(rle(snow_on)$lengths)) %>%
    filter(SWE > 0)
  
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
  if(max(tmp$days_of_snow) <= min_days){zone_type = data_type[1]}
  else{zone_type = data_type[2]}
  if(max(tmp$days_of_snow) >= max_days){zone_type = data_type[3]}
  if(max(tmp$days_of_snow) >= 329){zone_type = data_type[4]} # approximently 90% of the days in the year ()
  return(zone_type)
}


#### Demp code to run the function ############################################################

## loading librarys
#library(snotelr)    # downloading SNOTEL data


#Hogg_Pass <- snotel_download(site_id = 526, internal = TRUE) # downloading Hogg Pass, SWE[mm] and temp[Degrees C]
##HP <- filter(Hogg_Pass, date <= "2015-09-30", date >= "2014-10-01") # Low Snow Year
#HP <- filter(Hogg_Pass, date <= "2014-09-30", date >= "2013-10-01") # Normal Snow Year

#snow_zone(daily_swe = HP$snow_water_equivalent,       #SWE in (mm)
#          min_snow = 0.02 * 1000,                     #SWE in (mm)
#          return_data_type = "character")

#snow_zone(daily_swe = HP$snow_water_equivalent,       #SWE in (mm)
#          min_snow = 0.02 * 1000)                     #SWE in (mm)

