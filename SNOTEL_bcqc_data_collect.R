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
###         You will then need to extract to zip files and have them in the 
###         same folder as this R-script
###
### By: Mikey Johnson
### Last edited: 2020-10-05
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


### Function to collect data for specific dates

SNOTEL_bcqc_data_collect <- function(FP_SNOTEL_summary,
                                     bcqc_data_folder,
                                     snotel_id,
                                     start_date,
                                     end_date,
                                     units = "Metric"){

#FP_SNOTEL_summary = paste0(sfl,"/SNOTEL_summary.csv_/SNOTEL_summary.csv")
#bcqc_data_folder = paste0(sfl,"/bcqc_data_0/bcqc_data")
#snotel_id = 619
#start_date = "2013-10-01"
#end_date = "2014-09-30"
#units = "Metric"

# Checking the Units
  if(!(units == "Metric" | units == "English")){
    print("Unrecodnised units, use 'Metric' or 'English' " )
    stop()
  }


# Checking the station ID
  SNOTEL_summary = read.csv(FP_SNOTEL_summary)
  if(!snotel_id %in% SNOTEL_summary$SNOTEL.ID){
    print(paste0("Station ",snotel_id," not available"))
    stop()
  }

# Loading the Station Data
  station_info <- SNOTEL_summary %>% filter(SNOTEL.ID == "619")  
  
  print(paste0("Loading Station ",snotel_id," (",station_info$SNOTEL.Name,")"))


# function to specify decimal and format lat long
  specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))


# loading the data
  station_data_english <- read.table(paste0(bcqc_data_folder,
                                            "/bcqc_",
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



# converting English to Metric

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

  station_data_metric$daily_precipitation_mm <- station_data_metric$daily_precipitation_mm * 25.4
  station_data_metric$max_air_temperature_C <- (station_data_metric$max_air_temperature_C + 32) * 5/9
  station_data_metric$min_air_temperature_C <- (station_data_metric$min_air_temperature_C + 32) * 5/9
  station_data_metric$mean_air_temperature_C <- (station_data_metric$mean_air_temperature_C + 32) * 5/9
  station_data_metric$SWE_mm <- station_data_metric$SWE_mm * 25.4


# choosing units
  if(units == "Metric"){
    station_data = station_data_metric
    } else {
    station_data = station_data_english  
    }


# filtering by data

  if(!(as.Date(start_date) %in% station_data$date & as.Date(end_date) %in% station_data$date)){
    print("Dates out of range")
    print(paste0("Min Date Range = ",min(station_data$date)))
    print(paste0("Max Date Range = ",max(station_data$date)))
    stop()
  }


  return_data <- station_data_metric %>% filter(date <= as.Date(end_date), date >= as.Date(start_date)) 

  return(return_data)

}



### Testing the function

McKinzie_WY_2014 <- SNOTEL_bcqc_data_collect(FP_SNOTEL_summary = paste0(sfl,"/SNOTEL_summary.csv_/SNOTEL_summary.csv"),
                                             bcqc_data_folder = paste0(sfl,"/bcqc_data_0/bcqc_data"),
                                             snotel_id = 619,
                                             start_date = "2013-10-01",
                                             end_date = "2014-09-30",
                                             units = "Metric")




McKinzie_WY_2015 <- SNOTEL_bcqc_data_collect(FP_SNOTEL_summary = paste0(sfl,"/SNOTEL_summary.csv_/SNOTEL_summary.csv"),
                                             bcqc_data_folder = paste0(sfl,"/bcqc_data_0/bcqc_data"),
                                             snotel_id = 619,
                                             start_date = "2014-10-01",
                                             end_date = "2015-09-30",
                                             units = "Metric")



### Plotting SWE
ggplotly(ggplot(data = McKinzie_WY_2014)+
           geom_line(aes(x = date, y = SWE_mm)) +
           xlab("") + ylab("Snow Water Equivalant (mm)")+
           ggtitle("Hogg Pass SNOTEL")+
           theme_cowplot(12))


ggplotly(ggplot(data = McKinzie_WY_2015)+
           geom_line(aes(x = date, y = SWE_mm)) +
           xlab("") + ylab("Snow Water Equivalant (mm)")+
           ggtitle("Hogg Pass SNOTEL")+
           theme_cowplot(12))


