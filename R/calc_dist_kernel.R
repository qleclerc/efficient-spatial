#' @title Calculates the spatial kernel value
#'
#' @description Calculates the spatial kernel value between two areas using their distance, the destination
#'              population size, the destination population power, an offset distance and a distance power.
#'
#' @param distance The distance between areas 1 and 2.
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

#this kernel WORKS, tested by direct comparison to matlab code output so NO PROBLEM HERE!

#NOW NEED TO FIND CORRECT PARAMETERS

calc_dist_kernel = function(dist_mat, rasterl, alpha, p, aa){

  good_values = which(!is.na(rasterl@data@values))

  N = rasterl@data@values[good_values]

  N[which(N<1)] = 1

  #set up matrix to fill in:
  dist_kernel = matrix(0, nrow=nrow(dist_mat), ncol=ncol(dist_mat))

  #calculate this only once:
  K = (1+(dist_mat/aa)^(p))


  for (i in 1:length(good_values)) {


    for (j in 1:length(good_values)) {


      dist_kernel[i,j] = (N[i]*(N[j]^alpha)) / K[i,j]

    }

  }

  #normalise so that rowSums = 1:
  dist_kernel = dist_kernel/rowSums(dist_kernel)

  return(dist_kernel)

}

#K = calc_dist_kernel(d, test, 0.53, 10, 2)
