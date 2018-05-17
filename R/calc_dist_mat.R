#' @title Calculates the distance matrix
#'
#' @description Calculates the distance between all populated areas in a RasterLayer object.
#'
#' @param raster_l The RasterLayer object to use to calculate the distance matrix.
#'
#' @details This function calculates the distance matrix once to avoid doing it for every generation during an
#'          epidemic simulation. This takes a while, but is essential to later speed up the epidemic simulation.
#'          The matrix returned only contains distance between cells with non-NA values. (i.e. cells with people
#'          in them)
#'          Finally, note that you can run it using one RasterLayer object then use it for any other RasterLayer
#'          of the same spatial area (e.g. if you estimate it using the RasterLayer with population data for 0
#'          to 4 years old in the UK, you can use it for any RasterLayer of the UK of identical resolution)
#'
#' @return Returns one matrix object.
#'
#' @examples
#' dist_mat = calc_dist_mat(total_pop_data)
#'
#' @export


calc_dist_mat = function(rasterl){

  if(class(rasterl) != "RasterLayer"){

    stop("The specified object is not a RasterLayer. Please provide a RasterLayer object for this function.")

  }

  #extracts number of cells in RasterLayer object:
  nc = raster::ncell(rasterl)

  #creates empty distance matrix:
  dist_mat = matrix(nrow=nc, ncol=nc)

  #specify this so that cells have a distance to themselves equal to 0:
  dist_mat[,] = 0

  #identify which cells have non-NA values:
  good_values = which(!is.na(rasterl@data@values))

  #loops intelligently, only one calculation per (i,j) pair, and only calculates for non-NA cells:
  for (i in good_values[1:(length(good_values)-1)]) {

    for (j in (i+1):length(good_values)) {

          x1 = raster::xFromCell(rasterl,i)
          y1 = raster::yFromCell(rasterl,i)
          x2 = raster::xFromCell(rasterl,j)
          y2 = raster::yFromCell(rasterl,j)

          dist = sqrt((x1-x2)^2+(y1-y2)^2)

          dist_mat[i,j] = dist_mat[j,i] = dist

      }
    }

  #only keep rows and columns for non-NA cells:
  dist_mat = dist_mat[good_values, good_values]

  return(dist_mat)

}

