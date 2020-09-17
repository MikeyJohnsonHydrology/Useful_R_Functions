# Example script to collect and process SNOTEL date
# Example site is Hogg Pass in the McKenzie River Basin
#
# Mikey Johnson
# mikeyj@nevada.unr.edu
#
# Last Eddited: 2020-04-09
#
###############################################################################################
# loading librarys
library(snotelr)    # downloading SNOTEL data
library(dplyr)      # data manipulation
library(ggplot2)    # plotting
library(cowplot)    # publication-ready plots
library(plotly)     # interactive plotting

###############################################################################################
# loading the Hogg Pass SNOTEL data

site_meta_data <- snotel_info() # loading all the SNOTEL station data

Hogg_Pass <- snotel_download(site_id = 526, internal = TRUE) # downloading Hogg Pass, SWE[mm] and temp[Degrees C]
#HP <- filter(Hogg_Pass, date <= "2019-09-30", date >= "2012-10-01") # seperating out the data (End Date, Start Date)
#HP <- filter(Hogg_Pass, date <= "2015-09-30", date >= "2014-10-01") # Low Snow Year
HP <- filter(Hogg_Pass, date <= "2014-09-30", date >= "2013-10-01") # Normal Snow Year

# Mt. Rose
#Mt.Rose <- snotel_download(site_id = 652, internal = TRUE) # downloading Hogg Pass, SWE[mm] and temp[Degrees C]
#MR <- filter(Hogg_Pass, date <= "2019-09-30", date >= "2018-10-01") # seperating out the data (End Date, Start Date)

###############################################################################################
# plotting swe
ggplotly(ggplot()+
           geom_line(aes(as.Date(HP$date),HP$snow_water_equivalent)) +
           xlab("") + ylab("Snow Water Equivalant (mm)")+
           ggtitle("Hogg Pass SNOTEL")+
           theme_cowplot(12))


###############################################################################################
# calcualting cumulative gain and loss of swe

HP <- HP %>% mutate(cumulative_gain=rep(0,nrow(HP)))
HP$cumulative_gain[1] <- HP$snow_water_equivalent[1]
for (i in 2:nrow(HP)){
HP$cumulative_gain[i] <- ifelse(HP$snow_water_equivalent[i]>HP$snow_water_equivalent[i-1],
                                HP$cumulative_gain[i-1]+HP$snow_water_equivalent[i],
                                HP$cumulative_gain[i-1])
}


HP <- HP %>% mutate(cumulative_loss=rep(0,nrow(HP)))
for (i in 2:nrow(HP)){
  HP$cumulative_loss[i] <- ifelse(HP$snow_water_equivalent[i]<HP$snow_water_equivalent[i-1],
                                  HP$cumulative_loss[i-1]-HP$snow_water_equivalent[i],
                                  HP$cumulative_loss[i-1])
}


###############################################################################################
# flagging days where cumulative snowfall is >3[cm] or >30[mm]

HP <- HP %>% 
  mutate(del_swe = (c(0,diff(snow_water_equivalent)))) %>%
  mutate(Storm_Flag = ifelse(del_swe >= 30,1,0))
plot(as.Date(HP$date),HP$Storm_Flag)

###############################################################################################
# Counting Storms, # consider time step threshhold for gap between storms

HP <- HP %>%
  mutate(Storm_Count = ifelse(Storm_Flag == 0,NA,0))

i <- 1
j <- 1
storm_counter <- 0
while (i <= nrow(HP)){
  
  if (HP$Storm_Flag[i] == 1) {
    storm_counter <- storm_counter + 1
    HP$Storm_Count[i] <- storm_counter
    i <- i+1
    j <- i}
  else {i <- i+1}
    while (j <= nrow(HP)){
      if (HP$Storm_Flag[j] == 1){
        HP$Storm_Count[j] <- storm_counter
        j <- j+1
        i <- i+1
        }
      else { j <- nrow(HP)+1 }
    }
}
storm_counter


###############################################################################################
# Determining if the site is in the (seasonal snow zone) / (intermintent snow zone) / (rain zone)

snow_zone <- function(daily_swe = HP$snow_water_equivalent,
                      max_days = 30,
                      min_days = 5,
                      min_snow = 0.02 * 1000 #SWE in (mm) # Note (Sterm, Holgrem & Liston, 1994) defines ephemeral to be bewteen 0-50 (cm)
                      ){

  tmp <- data.frame(SWE = daily_swe - min_snow) %>%
    mutate(snow_on = ifelse(SWE > 0,1,0)) %>%
    mutate(days_of_snow = sequence(rle(snow_on)$lengths)) %>%
    filter(SWE > 0)

#  max(tmp$days_of_snow)

  if(max(tmp$days_of_snow) <= min_days){zone_type = "Rain Zone"}
  else{zone_type = "Ephemeral Snow Zone"}
  if(max(tmp$days_of_snow) >= max_days){zone_type = "Seasonal Snow Zone"}
  if(max(tmp$days_of_snow) >= 350){zone_type = "Perminate Snow Zone"}
  return(zone_type)
}

snow_zone()





