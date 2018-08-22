#' @title Calculates the distance matrix (Shapefile)
#'
#' @description Calculates the distance between all areas in a Shapefile object.
#'
#' @param shp The Shapefile object to use to calculate the distance matrix.
#'
#' @details This function calculates the distance matrix once to avoid doing it for every generation during an
#'          epidemic simulation. This is essential to later speed up the epidemic simulation. Note that this
#'          function assumes that the distances are in meters in the dataset! If that is not the case, you might want
#'          to either convert these, or adjust the output of this function.
#'
#' @return Returns one matrix object containg the distances between all populated areas.
#'
#'
#' @export


calc_dist_mat_shp = function(shp){

  #creates empty distance matrix:
  dist_mat = matrix(0, nrow=length(shp), ncol=length(shp))

  #extract values once:
  x = sp::coordinates(shp)[,1]
  y = sp::coordinates(shp)[,2]

  #loops intelligently, only one calculation per (i,j) pair, and only calculates for non-NA cells:
  for (i in 1:(length(shp)-1)) {

    for (j in (i+1):length(shp)) {

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
