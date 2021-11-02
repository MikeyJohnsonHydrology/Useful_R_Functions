#############################################################
### date_to_water_year
###
### A simple script to convert a date object to a water year
###
#############################################################

date_to_water_year <- function(date){
  year = as.numeric(format(date, "%Y"))
  month = as.numeric(format(date, "%m"))
  water_year <- ifelse(month < 10, year, year + 1)
  return(water_year)
  }

# Testing the function
#date_to_water_year(as.Date("2020-02-11"))
#date_to_water_year(as.Date("2020-09-10"))
#date_to_water_year(as.Date("2020-10-10")) 
