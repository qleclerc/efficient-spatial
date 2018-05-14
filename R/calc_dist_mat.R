#' @title Calculates the distance matrix
#'
#' @description Calculates the distance between all points in a RasterLayer object.
#'
#' @details This function calculates the distance matrix once to avoid doing it for every generation during an
#'          epidemic simulation. This takes a while, but is essential to later speed up the epidemic simulation.
#'          Also, it only needs to run once then you're good to go!
#'          Finally, note that you can run it using one RasterLayer object then use it for any other RasterLayer
#'          of the same spatial area (e.g. if you estimate it using the RasterLayer with population data for 0
#'          to 4 years old in the UK, you can use it for any RasterLayer of the UK of identical resolution)
#'
#' @return Returns one matrix object.
#'
#' @examples
#' dist_matrix = calc_dist_mat(s0_04)
#'
#' @export

calc_dist_mat = function(x){

  return("Hi")

}

#calculate distance matrix

#look at steve's function for inspiration

#to get xy values: raster::xyFromCell(rasterfile, cellnumber)
#to get cell i value: raster::values(rasterfile)[i]
#to get total cell number: raster::ncell(s0_04)

#for i in 1:number of cells
    #for j in 1:number of cells
      #if value(i) or value(j) == NA
        #distance[i,j] = NA
      #else
        #distance[i,j] = calculate this

#for later: consider adding a check that the distance matrix is of correct size
  #i.e. not using a matrix calculated at res 100 for res 1000
