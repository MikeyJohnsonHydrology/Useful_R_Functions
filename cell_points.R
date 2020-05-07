###################################################################################
# cell_points.R
#
# a simple script to compute the centroid and lower left corner of a cell
#
###################################################################################

cell.points <- function(cell_x,
                        cell_y,
                        one_one_cell = "LLC",  # location of cell 1,1 ("LLC" typiclay used in SnowModel Outputs, "ULC" typicaly used for rasters and common grids)
                        cell_size = 100,
                        llx = 489747,          # Lower left grid coridnate (I typicaly use UTM Zone 10)
                        lly = 4853924,
                        nx = 1121,             # number of x gridcells
                        ny = 759               # number of y gridcells
){
  cell_ll_x <- llx + cell_size * (cell_x-1)
  cell_ll_y <- lly + cell_size * (cell_y-1)
  
  cell_center_x <- cell_ll_x + cell_size/2
  cell_center_y <- cell_ll_y + cell_size/2
  
  cell_ll_y_ULC <- (lly + cell_size*ny) - cell_y * cell_size
  cell_center_y_ULC <- cell_ll_y_ULC + cell_size/2
  
  return_df_LLC <- data.frame(point = c("Center", "Lower Left Corner"),
                              x = c(cell_center_x, cell_ll_x),
                              y = c(cell_center_y, cell_ll_y))

  return_df_ULC <- data.frame(point = c("Center", "Lower Left Corner"),
                              x = c(cell_center_x, cell_ll_x),
                              y = c(cell_center_y_ULC, cell_ll_y_ULC))
  
  if (one_one_cell == "LLC"){return(return_df_LLC)}
  if (one_one_cell == "ULC"){return(return_df_ULC)}
  else{print("set one_one_cell to either 'LLC' or 'ULC' (lower left cell or upper left cell)")}
}

#cell.points(2,2, one_one_cell = "LLC")
#cell.points(2,2, one_one_cell = "ULC")
#cell.points(2,2, one_one_cell = "print")
