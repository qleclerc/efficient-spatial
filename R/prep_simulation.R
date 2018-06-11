#' @title Performs pre-calculations necessary for the simulation of an epidemic
#'
#' @description Creates the age mixing matrix and the spatial kernel using the load_contact_mat, calc_dist_mat,
#'              calc_dist_kernel and calc_beta functions.
#'
#' @param rasterl The RasterLayer object containing the population data.
#' @param R0 The desired R0 for the epidemic
#' @param sigma The desired recovery rate for the epidemic
#'
#' @return Returns two matrix objects and one value for beta.
#'
#' @examples
#' prep_simulation(toy_data, R0 = 2, sigma = 1/4.2)
#'
#' @export


prep_simulation = function(rasterl, R0 = 1.8, sigma= 1/2.6){

  assign("contact_mat", load_contact_mat(), envir=.GlobalEnv)

  print("Calculating distance matrix...")

  d = calc_dist_mat(rasterl)

  print("Calculating kernel matrix...")

  assign("kernel", calc_dist_kernel(d, rasterl, 0.95, 3.95, 13.5), envir=.GlobalEnv)

  print("Calculating beta...")

  assign("beta", calc_beta(rasterl, kernel, contact_mat, R0=R0, sigma=sigma), envir=.GlobalEnv)


}
