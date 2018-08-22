#' @title Performs pre-calculations necessary for the simulation of an epidemic (Shapefile)
#'
#' @description Creates the age mixing matrix and the spatial kernel using the \code{\link{load_contact_mat}},
#'              \code{\link{calc_dist_mat_shp}}, \code{\link{calc_dist_kernel_shp}} and \code{\link{calc_beta_shp}}
#'              functions.
#'
#' @param shp The Shapefile object containing the population structure.
#' @param pop_data The dataframe containing the population data
#' @param R0 The desired R0 for the epidemic
#' @param sigma The desired recovery rate for the epidemic
#' @param age Logical. If TRUE, the simulation will assume four distincts age groups in the population (default).
#'            If FALSE, the simulation will assume homogeneous mixing between age groups in the population.
#'
#' @return Returns one dataframe, two matrix objects and one value for beta.
#'
#'
#' @export


prep_simulation_shp = function(shp, pop_data, R0 = 1.8, sigma= 1/2.6, age = TRUE){

  shp_data = pop_data[which(pop_data$Area_name %in% shp$name),]

  #reorder pop data to match shp order:
  shp_data = shp_data[match(shp$name, shp_data$Area_name),]

  assign("shp_data", shp_data, envir=.GlobalEnv)


  assign("contact_mat", load_contact_mat(age), envir=.GlobalEnv)


  print("Calculating distance matrix...")

  d = calc_dist_mat_shp(shp)


  print("Calculating kernel matrix...")

  kernel = calc_dist_kernel_shp(d, dist_c = 87, shp_data, alpha=0.95, p=6.6, p2=1.53, aa=35)
  #0.95, 3.95, 13.5
  #0.52, 2.72, 0.58
  #0.89, 19,
  #dist_c = 87000, rasterl, alpha=0.95, p=6.6, p2=1.53, aa=35000


  print("Calculating beta...")

  assign("beta", calc_beta_shp(shp_data, kernel, contact_mat, R0=R0, sigma=sigma), envir=.GlobalEnv)


}
