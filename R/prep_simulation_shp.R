#' @title Performs pre-calculations necessary for the simulation of an epidemic using Shapefiles
#'
#' @description Creates the age mixing matrix and the spatial kernel using the load_contact_mat, calc_dist_mat_shp,
#'              calc_dist_kernel_shp and calc_beta_shp functions.
#'
#' @param shp The Shapefile object containing the areas of interest
#' @param pop_data The dataframe containing the population of all areas
#' @param R0 The desired R0 for the epidemic
#' @param sigma The desired recovery rate for the epidemic
#'
#' @return Returns two matrix objects and one value for beta.
#'
#' @examples
#' prep_simulation_shp(regions_shp, pop_data, R0 = 2, sigma = 1/4.2)
#'
#' @export


prep_simulation_shp = function(shp, R0 = 1.8, sigma= 1/2.6){

  shp_data = pop_data[which(pop_data$Area_name %in% shp$name),]

  #reorder pop data to match shp order:
  shp_data = shp_data[match(shp$name, shp_data$Area_name),]

  assign("shp_data", shp_data, envir=.GlobalEnv)


  assign("contact_mat", load_contact_mat(), envir=.GlobalEnv)


  print("Calculating distance matrix...")

  d = calc_dist_mat_shp(shp)


  print("Calculating kernel matrix...")

  assign("kernel", calc_dist_kernel_shp(d, shp_data, 0.95, 3.95, 13.5), envir=.GlobalEnv)


  print("Calculating beta...")

  assign("beta", calc_beta_shp(shp_data, kernel, contact_mat, R0=R0, sigma=sigma), envir=.GlobalEnv)


}
