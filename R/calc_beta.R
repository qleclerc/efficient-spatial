#' @title Estimates beta from a given R0 (Raster)
#'
#' @description Estimates beta for an epidemic with a given R0 in a specific population in a Rasterlayer object.
#'
#' @return Returns the value of beta.
#'
#' @param rasterl The RasterLayer object containing the population data.
#' @param dist_kernel The distance kernel matrix.
#' @param contact_mat The contact matrix between age groups.
#' @param R0 The desired value for R0.
#' @param sigma The desired value for the recovery rate.
#'
#' @examples
#' beta = calc_beta(toy_data, dist_kernel, contact_mat, R0=2, sigma=1/4.2)
#'
#' @export


calc_beta = function(rasterl, dist_kernel, contact_mat, R0=1.8, sigma=1/2.6){

  good_values = which(!is.na(rasterl@data@values))  #ignore inhabitable areas (i.e. with a population "NA")
  num_areas = length(good_values)   #derive number of areas
  num_ages = dim(contact_mat)[1]    #derive number of age categories from contact matrix

  N = matrix(rasterl@data@values[good_values], nrow=num_areas, ncol=num_ages)

  N[which(N<1)] = 1

  N[,1] = N[,1]*(5/80)
  N[,2] = N[,2]*(14/80)
  N[,3] = N[,3]*(45/80)
  N[,4] = N[,4]*(16/80)

  sigma = 1/2.6 #recovery rate

  #calculate transposed matrix only once:
  t_kernel = t(dist_kernel)


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

  #approximating eigenvalues if necessary:
  #happy to go into more details regarding my technique here, but basically if the matrix is very large I'm only
    #calculating the eigenvalues for part of it because we can consider it as a tridiagonal matrix (see the divide
    #and conquer eigenvalue calculation technique), I've tested it thoroughly and the approximation works perfectly
    #fine (i.e. still only get an epidemic if R0>1)

  if(dim(kernel)[1]>500){

    X1 = X[1:(dim(X)[1]/2), 1:(dim(X)[1]/2)]
    X2 = X[(dim(X1)[1]+1):dim(X)[1], (dim(X1)[1]+1):dim(X)[1]]

    if(dim(kernel)[1]>1000){

      X3 = X1[1:(dim(X1)[1]/2), 1:(dim(X1)[1]/2)]
      X1 = X1[(dim(X3)[1]+1):dim(X1)[1], (dim(X3)[1]+1):dim(X1)[1]]

      X4 = X2[1:(dim(X2)[1]/2), 1:(dim(X2)[1]/2)]
      X2 = X2[(dim(X4)[1]+1):dim(X2)[1], (dim(X4)[1]+1):dim(X2)[1]]

      eigen_vals1 = eigen(X1, symmetric = F, only.values=T)$values
      eigen_vals2 = eigen(X2, symmetric = F, only.values=T)$values
      eigen_vals3 = eigen(X3, symmetric = F, only.values=T)$values
      eigen_vals4 = eigen(X4, symmetric = F, only.values=T)$values

      R0a = max(Re(eigen_vals1), Re(eigen_vals2), Re(eigen_vals3), Re(eigen_vals4))


    } else {

      eigen_vals1 = eigen(X1, symmetric = F, only.values=T)$values
      eigen_vals2 = eigen(X2, symmetric = F, only.values=T)$values

      R0a = max(Re(eigen_vals1), Re(eigen_vals2))

    }

  } else {

    eigen_vals = eigen(X, symmetric=F, only.values = T)$values

    R0a = max(Re(eigen_vals))

  }

  beta=R0/R0a

  return(beta)

}
