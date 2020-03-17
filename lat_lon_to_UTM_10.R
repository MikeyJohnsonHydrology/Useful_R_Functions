########################################################################################################
### lat_long_to_UTM_10.r
###
### A simple function that will convert latitude and longitude to nothing and easing for zone UNM 10
###
### By: Mikey Johnson
########################################################################################################

### Loading Libraries ###
library(rgdal)

### defining function ###
lat_lon_to_UTM_10 <- function(latitude,longitude){
cord.dec <- SpatialPoints(cbind(longitude, latitude), proj4string=CRS("+proj=longlat"))
cord.UTM <- spTransform(cord.dec, CRS("+proj=utm +zone=10 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
temp <- extent(cord.UTM)
return.data <-c(temp@xmin,temp@ymax,10)
return(return.data)
}


### function test, Eugene Airport: northing 4876320, easing 491987, UTM Zone 10 ###
#lat_lon_to_UTM_10(44.039996,-123.1000122 )




