########################################################################################################
### lat_long_to_UTM_11.r
###
### A simple function that will convert latitude and longitude to nothing and easing for zone UNM 11
###
### By: Mikey Johnson
########################################################################################################

### Loading Libraries ###
library(raster)
library(rgdal)

### defining function ###
lat_lon_to_UTM_11 <- function(latitude,longitude){
  cord.dec <- SpatialPoints(cbind(longitude, latitude), proj4string=CRS("+proj=longlat"))
  cord.UTM <- spTransform(cord.dec, CRS("+proj=utm +zone=11 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
  temp <- extent(cord.UTM)
  return.data <- data.frame(Easting=temp@xmin, Northing=temp@ymax, Zone=11)
  return(return.data)
}


### function test, Eugene Airport: northing 4876320, easing 491987, UTM Zone 10 ###
#lat_lon_to_UTM_11(37.298180,-119.103371)
