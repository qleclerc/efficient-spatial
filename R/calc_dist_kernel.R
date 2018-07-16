#' @title Calculates the spatial kernel value (Raster)
#'
#' @description Calculates the spatial kernel value between all areas in a Rasterlayer using their distance, the
#'              destination population size, the destination population power, an offset distance and a distance
#'              power.
#'
#' @param distance The distance matrix between all areas.
#' @param rasterl The RasterLayer object containing the population data
#' @param alpha The destination population power.
#' @param offset The offset distance.
#' @param gamma The distance power.
#'
#' @return Returns one matrix object containing the spatial kernel values.
#'
#' @examples
#' K = calc_dist_kernel(x, y, 0.53, 10, 3)
#'
#' @export


calc_dist_kernel = function(dist_mat, rasterl, alpha, p, aa, delta=0.3){

  good_values = which(!is.na(rasterl@data@values))

  N = rasterl@data@values[good_values]

  N[which(N<1)] = 1

  #set up matrix to fill in:
  dist_kernel = matrix(0, nrow=nrow(dist_mat), ncol=ncol(dist_mat))

  #calculate this only once:
  K = (1+(dist_mat/aa)^(p))

  #works with loops because I haven't gotten around to switching to matrix multiplication instead (not a priority)
  for (i in 1:length(good_values)) {


    for (j in 1:length(good_values)) {

      #only adds delta if i == j (i.e. if calculating transmission kernel within an area)
      dist_kernel[i,j] = ((N[j]^alpha)) * ( 1/K[i,j] )#+ (if(i==j) delta else 0))

    }

  }

  #normalise so that rowSums = 1:
  dist_kernel = dist_kernel/rowSums(dist_kernel)

  return(dist_kernel)

}

