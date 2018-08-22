#' @title Estimates beta from a given R0 (Shapefile)
#'
#' @description Estimates beta for an epidemic with a given R0 in a specific population in a Shapefile object,
#'              and calculates the expanded kernel matrix D to use when simulating an epidemic.
#'
#' @return Returns the value of beta and assigns the D matrix to the global environment.
#'
#' @param shp_data The dataframe object containing the population data extracted from the Shapefile object.
#' @param dist_kernel The distance kernel matrix.
#' @param contact_mat The contact matrix for mixing between age groups.
#' @param R0 The desired value for R0.
#' @param sigma The desired value for the recovery rate.
#'
#' @details This function is automatically executed when using the \code{\link{prep_simulation_shp}} function. It
#'          uses the Next Generation Matrix approach to derive beta from R0.
#'
#' @export


calc_beta_shp = function(shp_data, dist_kernel, contact_mat, R0=1.8, sigma=1/2.6){

  N = shp_data$Population

  num_areas = length(N)   #derive number of areas
  num_ages = dim(contact_mat)[1]    #derive number of age categories from contact matrix

  if(num_ages == 4){

    N = matrix(N, nrow=num_areas, ncol=num_ages)

    NN0 = as.vector(N)

    N[,1] = N[,1]*(5/81)
    N[,2] = N[,2]*(14/81)
    N[,3] = N[,3]*(46/81)
    N[,4] = N[,4]*(16/81)
    #this way, i in the matrix is the area and j the age group e.g. N[1,1] gives pop 0-4 in area 1

  } else if(num_ages == 1){

    NN0 = as.vector(N)

  } else {

    stop("Unsupported number of age categories, currently only supports 4 or none.")

  }


  Sstart = matrix(N, ncol=1)
  Sstart = matrix(Sstart, ncol=nrow(Sstart), nrow=nrow(Sstart))

  K1 = kronecker(diag(num_ages), dist_kernel)

  NNbar = matrix(N, ncol=1)
  Kbar = kronecker(matrix(1,num_ages,num_ages), dist_kernel)

  Mj=t(Kbar)%*%NNbar
  Mj[which(Mj==0)]=1
  Mjover=1/Mj
  Mjover = t(Mjover)
  Mjover = matrix(rep(Mjover, num_areas*num_ages), nrow=num_areas*num_ages, byrow=T)

  keye = diag(num_areas)
  kxeye = matrix(1, nrow=num_areas, ncol=num_areas) - keye
  Cbar = kronecker(contact_mat, keye) + kronecker(contact_mat, kxeye)

  DD=(Sstart*K1*Mjover)%*%(t(Kbar)*Cbar)

  X=DD/sigma


  ###this extra bit just calculates D, necessary for FOI calculation, it's then faster to run the simulation
  Ni=matrix(NN0,nrow=length(NN0),ncol=length(NN0),byrow=F)
  Nj=t(Ni)

  proper_D = (K1*Mjover)%*%(t(Kbar)*Cbar)
  proper_D = proper_D*Nj

  assign("expanded_D", proper_D, envir=.GlobalEnv)
  ###


  eigen_vals = eigen(X, symmetric=F, only.values = T)$values

  R0a = max(Re(eigen_vals))

  beta=R0/R0a

  return(beta)

}
