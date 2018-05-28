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


calc_dist_kernel = function(dist_mat, rasterl, alpha, offset, gamma){

  #safety check:
  if(class(rasterl) != "RasterLayer"){

    stop("The specified rasterl object is not a RasterLayer. Please provide a RasterLayer object for this function.")

  }

  #identify which cells have non-NA values:
  good_values = which(!is.na(rasterl@data@values))

  #safety check:
  if(length(good_values) != dim(dist_mat)[1]){

    stop("The specified distance matrix does not correspond to this RasterLayer. Recalculate it.")

  }

  #extract population sizes:
  N = rasterl@data@values[good_values]

  #set up matrix to fill in:
  dist_kernel = matrix(0, nrow=nrow(dist_mat), ncol=ncol(dist_mat))

  z=0

  #loops intelligently, only one calculation per (i,j) pair, and only calculates for non-NA cells:
  for (i in 1:(length(good_values)-1)) {

    z=z+1

    print(paste0(round(z/length(good_values)*100), "% done"))

    for (j in (i+1):length(good_values)) {

      pop_size = N[j]
      distance = dist_mat[i,j]

      dist_kernel[i,j] = dist_kernel[j,i] = pop_size^alpha/(1+distance/offset)^(-gamma)

    }
  }

  return(dist_kernel)

}

#K = calc_dist_kernel(d, test, 0.53, 10, 3)
