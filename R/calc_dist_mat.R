#' @title Calculates the distance matrix (Raster)
#'
#' @description Calculates the distance between all populated areas in a RasterLayer object.
#'
#' @param raster_l The RasterLayer object to use to calculate the distance matrix.
#'
#' @details This function calculates the distance matrix once to avoid doing it for every generation during an
#'          epidemic simulation. This is essential to later speed up the epidemic simulation. The matrix returned
#'          only contains distance between cells with non-NA values (i.e. cells with people in them). Note that this
#'          function assumes that the distances are in meters in the dataset! If that is not the case, you might want
#'          to either convert these, or adjust the output of this function.
#'
#' @return Returns one matrix object containg the distances between all populated areas.
#'
#' @examples
#'
#' #Create a RasterLayer object:
#' test_data = raster(nrow=10, ncol=10, xmn=1, xmx=100000, ymn=1, ymx=100000)
#' values(test_data) = runif(100, 1, 1000)
#'
#' dist_mat = calc_dist_mat(test_data)
#'
#' @export


calc_dist_mat = function(rasterl){

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

  #assumes distances are in meters, returns values as kilometers for consistency with calc_dist_kernel function:
  dist_mat = dist_mat/1000
  return(dist_mat)

}
