#need:
#- seed infection
  #can just define name of area

#' @title Runs the spatial epidemic simulation (Shapefile)
#'
#' @description Simulates an epidemic using the provided Shapefile, spatial kernel, contact matrix, and
#'              infection parameters.
#'
#' @param shp_data The dataframe object containing the population data extracted from the Shapefile object.
#' @param dist_kernel The distance kernel matrix.
#' @param contact_mat The contact matrix between age groups.
#' @param beta The beta value for the epidemic. (calculated from a given R0 using the calc_beta function)
#' @param sigma The desired recovery rate for the epidemic. (must match the one used to calculate beta from R0
#'              using the calc_beta function)
#' @param stoch Logical. If TRUE, the simulation is stochastic.
#' @param start_area Where to start the epidemic. You can provide the name of the area directly (you can check
#'                   which are possible by looking inside the pop_data dataframe). Default is area with highest
#'                   population (London).
#' @param start_fraction Fraction of infected individuals to start the epidemic.
#' @param t_max How many days to run the simulation for.
#'
#' @details This functions requires many parameters to run. These can be generated using the following functions:
#'
#' @return Returns one dataframe object containing the epidemic estimates of cases per day.
#'
#' @examples
#' results = run_simulation_shp(shp_data, kernel, contact_mat, beta = beta)
#'
#' @export


run_simulation_shp = function(shp_data, dist_kernel, contact_mat, beta, alpha=1, sigma = 1/2.6, stoch=FALSE, start_area=NA, start_fraction=0.0001, t_max=10){

  N = shp_data$Population

  num_areas = length(N)   #derive number of areas
  num_ages = dim(contact_mat)[1]    #derive number of age categories from contact matrix


  #### SETTING UP POPULATION: ####

  #N: total population
  #S: susceptibles
  #I: infected
  #R: recovered

  #!!! work in progress, only supports 0 or 4 age categories right now !!!#

  if(num_ages == 4){

    N = matrix(N, nrow=num_areas, ncol=num_ages)

    N[,1] = N[,1]*(5/80)
    N[,2] = N[,2]*(14/80)
    N[,3] = N[,3]*(45/80)
    N[,4] = N[,4]*(16/80)
    #this way, i in the matrix is the area and j the age group e.g. N[1,1] gives pop 0-4 in area 1

    S = N
    I = matrix(0, nrow=num_areas, ncol=num_ages)
    R = I

  } else if(num_ages == 0){

    S = N
    I = rep(0, num_areas)
    R = I

  } else {

    stop("Unsupported number of age categories, currently only supports 4 or 0.")

  }



  #### SEEDING: ####

  #currently seeds with an equal fraction in every age group, not just within one

  #!!! work in progress, currently only supports starting in area with highest density !!!#

  if(is.na(start_area)){

    #identify area with the most inhabitants: (effectively, London)

    if(num_ages == 0){

      start_area = which.max(N)
      S[start_area] = S[start_area] - N[start_area]*start_fraction
      I[start_area] = I[start_area] + N[start_area]*start_fraction

    } else {

      start_area = which.max(rowSums(N))
      S[start_area,3] = S[start_area,3] - N[start_area,3]*start_fraction
      I[start_area,3] = I[start_area,3] + N[start_area,3]*start_fraction

    }

  }

  else {

    start_area = which(shp_data$Area_name == start_area)

    if(num_ages == 0){

      S[start_area] = S[start_area] - N[start_area]*start_fraction
      I[start_area] = I[start_area] + N[start_area]*start_fraction

    } else {

      S[start_area,3] = S[start_area,3] - N[start_area,3]*start_fraction
      I[start_area,3] = I[start_area,3] + N[start_area,3]*start_fraction


  }
}


  #### RUN MODEL: ####

  t_kernel = t(dist_kernel)
  t_contact = t(contact_mat)


  full_model = function(t, y, .) {


    print(paste0(floor(t/t_max*100), "% done"))

    #obtain S, I and R starting values here:
    #(must be extracted and reconverted to arrays from the "y" object created by initial() which is a vector)
    S = matrix(y[1:(num_areas*num_ages)], ncol=ncol(N), nrow=nrow(N), byrow=F)
    I = matrix(y[(num_areas*num_ages+1):(num_areas*num_ages*2)], ncol=ncol(N), nrow=nrow(N), byrow=F)
    R = matrix(y[(num_areas*num_ages*2+1):length(y)], ncol=ncol(N), nrow=nrow(N), byrow=F)

    #FOI equation is essentially a big matrix multiplication:

    lambda = beta * (dist_kernel %*% (t_kernel%*%I%*%t_contact)/(t_kernel%*%N))^alpha

    #again, matrix multiplication so no need for indices:
    dSdt = -lambda*S
    dIdt = lambda*S - sigma*I
    dRdt = sigma*I

    list(c(dSdt, dIdt, dRdt))


  }

  results = deSolve::ode(func=full_model, y=c(S,I,R), times=seq(0,t_max,1))

  return(results)

}
