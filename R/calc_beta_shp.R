#' @title Estimates beta from a given R0 (Shpaefile)
#'
#' @description Estimates beta for an epidemic with a given R0 in a specific population in a Shapefile object.
#'
#' @return Returns the value of beta.
#'
#' @param shp_data The Shapefile object containing the population data.
#' @param dist_kernel The distance kernel matrix.
#' @param contact_mat The contact matrix between age groups.
#' @param R0 The desired value for R0.
#' @param sigma The desired value for the recovery rate.
#'
#' @examples
#' beta = calc_beta_shp(shp_data, dist_kernel, contact_mat, R0=2, sigma=1/4.2)
#'
#' @export


calc_beta_shp = function(shp_data, dist_kernel, contact_mat, R0=1.8, sigma=1/2.6){

  N = shp_data$Population

  num_areas = length(N)   #derive number of areas
  num_ages = dim(contact_mat)[1]    #derive number of age categories from contact matrix

  N = matrix(N, nrow=num_areas, ncol=num_ages)

  N[,1] = N[,1]*(5/80)
  N[,2] = N[,2]*(14/80)
  N[,3] = N[,3]*(45/80)
  N[,4] = N[,4]*(16/80)

  sigma = 1/2.6 #recovery rate

  #calculate transposed matrices only once:
  t_kernel = t(dist_kernel)
  t_contact = t(contact_mat)

  #NGM: a matrix of dimension number categories * number categories
  #e.g. NGM[1,1]: expected number of new infection in cat 1 caused by a single infected from cat 1
  #so need proba that individual from cat 1 contacts that one infected individual from cat 1
  #contact_mat*kernel/Nj/sigma
  #so proba(contact age of i with age of j)*proba(contact area of i to area of j)/number people in j/sigma


  #calculating beta using next generation matrix:
  #population as vectors:
  A_tot = as.vector(t(N))

  #adjust since population present in an area at time t is not the same as area population
  N_tot = t_kernel%*%N
  N_tot = as.vector(t(N_tot))

  #expand contact matrix
  exp_contact_mat = matrix(rep(contact_mat, num_areas), ncol=ncol(contact_mat), byrow=T)
  exp_contact_mat = matrix(rep(exp_contact_mat, num_areas), nrow=nrow(exp_contact_mat), byrow=T)

  #expand kernel matrix
  exp_kernel = matrix(apply(t_kernel, c(1,2), function(x) rep(x,num_ages)), ncol=(ncol(dist_kernel)*num_ages), byrow=T)
  exp_kernel = matrix(apply(exp_kernel, 1, function(x) rep(x,num_ages)), ncol=ncol(exp_kernel), byrow=T)

  #apply dist_kernel from each to one area and sum
  X = A_tot*exp_contact_mat*exp_kernel/N_tot/sigma

  #this takes a while!
  eigen_vals = eigen(X, symmetric = F, only.values=T)$values

  R0a = max(Re(eigen_vals))

  beta=R0/R0a

  #export beta
  return(beta)

}
