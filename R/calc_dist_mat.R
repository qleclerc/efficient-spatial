#' @title Calculates the distance matrix
#'
#' @description Calculates the distance between all points in a RasterLayer object.
#'
#' @param raster_l The RasterLayer object to use to calculate the distance matrix.
#'
#' @details This function calculates the distance matrix once to avoid doing it for every generation during an
#'          epidemic simulation. This takes a while, but is essential to later speed up the epidemic simulation.
#'          To simplify matching areas to their distances, this generates a matrix with the total number of cells
#'          in the RasterLayer object as dimensions, therefore including NA values. While this is not the most
#'          optimal approach, it makes indexing easier.
#'          Finally, note that you can run it using one RasterLayer object then use it for any other RasterLayer
#'          of the same spatial area (e.g. if you estimate it using the RasterLayer with population data for 0
#'          to 4 years old in the UK, you can use it for any RasterLayer of the UK of identical resolution)
#'
#' @return Returns one matrix object.
#'
#' @examples
#' dist_mat = calc_dist_mat(s0_04)
#'
#' @export


calc_dist_mat = function(raster_l){

  if(class(raster_l) != "RasterLayer"){

    stop("The specified object is not a RasterLayer. Please provide a RasterLayer object for this function.")

  }
  #extracts number of cells in RasterLayer object:
  nc = raster::ncell(raster_l)

  #creates empty distance matrix:
  dist_mat = matrix(nrow=nc, ncol=nc)

  #specify this so that cells have a distance to themselves equal to 0:
  dist_mat[,] = 0

  #loops intelligently, only one calculation per (i,j) pair:
  for (i in 1:(nc-1)) {

    for (j in (i+1):nc) {

      #if a cell contains an NA value, its distance with every other cell is set to NA:
      if(is.na(raster::values(raster_l)[i]) | is.na(raster::values(raster_l)[j])){

        dist_mat[i,j] = NA
        dist_mat[j,i] = NA

      } else {

          x1 = raster::xFromCell(raster_l,i)
          y1 = raster::yFromCell(raster_l,i)
          x2 = raster::xFromCell(raster_l,j)
          y2 = raster::yFromCell(raster_l,j)

          dist = sqrt((x1-x2)^2+(y1-y2)^2)

          dist_mat[i,j] = dist_mat[j,i] = dist

      }
    }
  }

  return(dist_mat)

}




#for later: consider adding a check that the distance matrix is of correct size
  #i.e. not using a matrix calculated at res 100 for res 1000

