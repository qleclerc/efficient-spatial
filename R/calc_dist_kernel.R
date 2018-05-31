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

  #PROBLEM IF POP < 1 then kernel >1

  #Solution 1: set minimum pop to 1
  N[which(N<=1)] = 1

  #Solution 2: set kernel values to minimum if greater than 1
  #dist_kernel[which(dist_kernel>=1)] = min(dist_kernel)

  #set up matrix to fill in:
  dist_kernel = matrix(0, nrow=nrow(dist_mat), ncol=ncol(dist_mat))

  z=0

  #need one calculation for each (i,j) because depends on population of j so (i,j) != (j,i):
  for (i in 1:length(good_values)) {

    z=z+1

    print(paste0(round(z/length(good_values)*100), "% done"))

    for (j in 1:length(good_values)) {

      if(j == i){

        dist_kernel[i,j] = 1

      } else {

        pop_size = N[j]
        distance = dist_mat[i,j]

        dist_kernel[i,j] = ((pop_size^alpha)/(1+offset/distance))^(-gamma)

      }

    }
  }

  return(dist_kernel)

}

#K = calc_dist_kernel(d, test, 0.53, 10, 2)
