################################################################################
### SNOTEL_bcqc_data_collect.R
###
### Simple script to load and orgaizes the data from the SNOTEL bcqc dataset.
###
### Web site for the bcqc datset https://www.pnnl.gov/data-products
###
### Notes:  
###        You will need to downlode "BCQC SNOTEL data.zip" & 
###                                  "file of SNOTEL stations.zip
###
###         Next will then need to extract to zip files and have the new folders 
###         in the same directory as this R-script.
###
###         You might need to check to make sure you filepath names match mine
###
###         I will try to simplifiy this into a function at some point
###
### By: Mikey Johnson
### Last edited: 2020-10-04
################################################################################

### Loading Libraries
library(dplyr)      # data manipulation
library(ggplot2)    # plotting
library(plotly)     # interactive plotting
library(cowplot)    # publication-ready plots
library(devtools)   # developer tools, simplifying tasks


#### Setting the working directory
sfl <- dirname(rstudioapi::getActiveDocumentContext()$path)  # this is the source file location this script


### loading the SNOTRL_summary data

SNOTEL_summary <- read.csv(paste0(sfl,"/SNOTEL_summary.csv_/SNOTEL_summary.csv"))


### Station names
SNOTEL_summary$SNOTEL.Name


### Selecting the station (McKenzie for example)
station_info <- SNOTEL_summary %>% filter(SNOTEL.Name == "Mckenzie") # By Name
station_info <- SNOTEL_summary %>% filter(SNOTEL.ID == "619") # By Station number 619
 
# function to format lat and long
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))


### Loading the entire time series for the site
station_data_english <- read.table(paste0(sfl,
                                              "/bcqc_data_0/bcqc_data/bcqc_",
                                              specify_decimal(station_info$SNOTEL.Lat....,5),
                                              "_",
                                              specify_decimal(station_info$SNOTEL.Lon....,5),
                                              ".txt"),
                                       quote="\"", comment.char="")

# adding name
names(station_data_english) <- c("year",
                                 "month",
                                 "day",
                                 "daily_precipitation_in",
                                 "max_air_temperature_F",
                                 "min_air_temperature_F",
                                 "mean_air_temperature_F",
                                 "SWE_in")

# adding data
station_data_english$date <- as.Date(paste0(station_data_english$year,
                                            "-",
                                            station_data_english$month,
                                            "-",
                                            station_data_english$day))
                                     
                                 

### converting English to Metric

station_data_metric <- station_data_english

names(station_data_metric) <- c("year",
                                 "month",
                                 "day",
                                 "daily_precipitation_mm",
                                 "max_air_temperature_C",
                                 "min_air_temperature_C",
                                 "mean_air_temperature_C",
                                 "SWE_mm",
                                 "date")

station_data_metric$daily_precipitation_mm <- station_data_metric$daily_precipitation_mm / 25.4
station_data_metric$max_air_temperature_C <- (station_data_metric$max_air_temperature_C + 32) * 5/9
station_data_metric$min_air_temperature_C <- (station_data_metric$min_air_temperature_C + 32) * 5/9
station_data_metric$mean_air_temperature_C <- (station_data_metric$mean_air_temperature_C + 32) * 5/9
station_data_metric$SWE_mm <- station_data_metric$SWE_mm / 25.4


### Filtering by data
WY_2014 <- station_data_metric %>% filter(date <= "2014-09-30", date >= "2013-10-01")  # Normal Snow Year at McKenzie
WY_2015 <- station_data_metric %>% filter(date <= "2015-09-30", date >= "2014-10-01")  # Low Snow Year at McKenzie


### Plotting SWE
ggplotly(ggplot(data = WY_2014)+
           geom_line(aes(x = date, y = SWE_mm)) +
           xlab("") + ylab("Snow Water Equivalant (mm)")+
           ggtitle("Hogg Pass SNOTEL")+
           theme_cowplot(12))


ggplotly(ggplot(data = WY_2015)+
           geom_line(aes(x = date, y = SWE_mm)) +
           xlab("") + ylab("Snow Water Equivalant (mm)")+
           ggtitle("Hogg Pass SNOTEL")+
           theme_cowplot(12))







