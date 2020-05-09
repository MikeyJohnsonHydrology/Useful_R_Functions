###################################################################################
## wind_components_to_direction.r 
##
## This function calcualtes wind direction given wind components
## wind speed in u(easting), and v(northing)
##
###################################################################################

wind_components_to_directions <- function(u,v){
# u (easting)
# v (northing)

ws <- sqrt(u^2 + v^2) # wind speed

theta.rads <- acos(v/ws)
theta.deg <- 180 * theta.rads / pi
return(theta.deg)
}

#wind_components_to_directions(0,1) # 0, North
#wind_components_to_directions(1,1) # 45, North East
#wind_components_to_directions(1,0) # 90, East
#wind_components_to_directions(1,-1) # 90, South East
#wind_components_to_directions(0,-1) # 180, South
