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

  #safety check:
  if(class(rasterl) != "RasterLayer"){

    stop("The specified object is not a RasterLayer. Please provide a RasterLayer object for this function.")

  }

  #identify which cells have non-NA values:
  good_values = which(!is.na(rasterl@data@values))

  #creates empty distance matrix:
  dist_mat = matrix(0, nrow=length(good_values), ncol=length(good_values))

  #extract values once:
  x = raster::xFromCell(rasterl, good_values)
  y = raster::yFromCell(rasterl, good_values)

  #loops intelligently, only one calculation per (i,j) pair, and only calculates for non-NA cells:
  for (i in 1:(length(good_values)-1)) {


    for (j in (i+1):length(good_values)) {

          x1 = x[i]
          y1 = y[i]
          x2 = x[j]
          y2 = y[j]

          dist = sqrt((x1-x2)^2+(y1-y2)^2)

          dist_mat[i,j] = dist_mat[j,i] = dist

      }
    }

  return(dist_mat)

}
