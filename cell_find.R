###################################################################
# cell_find.R
#
# a simple script to find cell on a grid of specifice size
# the code is written from the lower left corener
#
###################################################################

cell.find <- function(x = 591144,            # Location of intrest (same coridnates as llx and lly)
                      y = 4919499,
                      cell_size = 100,       # gidcell spacing in (m), (100 and 30 are the most common)
                      llx = 489747,          # Lower left grid coridnate (I typicaly use UTM Zone 10)
                      lly = 4853924,
                      nx = 1121,             # number of x gridcells
                      ny = 759,              # number of y gridcells
                      one_one_cell = "LLC"   # location of cell 1,1 ("LLC" typiclay used in SnowModel Outputs, "ULC" typicaly used for rasters and common grids)
                      ) {
  cell_x <- ceiling((x-llx)/cell_size)
  cell_y <- ceiling((y-lly)/cell_size)
  cell_llc <- c(cell_x, cell_y)
  cell_ulc <- c(cell_x,ny+1-cell_y)
  
  if (one_one_cell == "LLC"){return(cell_llc)}
  if (one_one_cell == "ULC"){return(cell_ulc)}
  else{print("set one_one_cell to either 'LLC' or 'ULC' (lower left cell or upper left cell)")}
}

# Test Data to run the function
cell.find(591144, 4919499, one_one_cell = "LLC")     #Hogg pass SnoTel Reported Location 
cell.find(591025.39,4919205, one_one_cell = "LLC")  #Hogg Pass SnoTel Real Location
cell.find(591025.39,4919205, one_one_cell = "ULC")  #Hogg Pass SnoTel Real Location

