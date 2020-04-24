###################################################################
# cell_find.R
#
# a simple script to find cell on a grid of specifice size
# the code is written from the lower left corener
#
###################################################################

cell.find <- function(x = 591144,
                      y = 4919499,
                      cell_size = 100,
                      llx = 489747,
                      lly = 4853924
                      ) {
  cell_x <- ceiling((x-llx)/cell_size)
  cell_y <- ceiling((y-lly)/cell_size)
  cell <- c(cell_x, cell_y)
  return(cell)
}

# Test Data to run the function
#cell.find(591144,4919499)     #Hogg pass SnoTel Reported Location 
#cell.find(591025.39,4919205)  #Hogg Pass SnoTel Real Location

