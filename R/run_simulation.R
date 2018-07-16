#' @title Runs the spatial epidemic simulation (Raster)
#'
#' @description Simulates an epidemic using the provided RasterLayer, distance matrix, spatial kernel,
#'              contact matrix, and infection parameters.
#'
#' @param rasterl The RasterLayer object containing the population data.
#' @param dist_kernel The distance kernel matrix.
#' @param contact_mat The contact matrix between age groups.
#' @param beta The beta value for the epidemic. (calculated from a given R0 using the calc_beta function)
#' @param sigma The desired recovery rate for the epidemic. (must match the one used to calculate beta from R0
#'              using the calc_beta function)
#' @param stoch Logical. If TRUE, the simulation is stochastic.
#' @param step Size of time step for stochastic simulation (default is 1 day).
#' @param start_area #NOT COMPLETE# Where to start the epidemic. 1: Most highly populated area (default), 2: A
#'                   random area in the middle of the country (typically medium population density), 3: A
#'                   random area in the north of the country (typically low population density).
#' @param start_num Number of infected individuals to start the epidemic.
#' @param t_max How many days to run the simulation for.
#'
#' @details This functions requires specific objects to run. These can be generated using the prep_simulation
#'          function (e.g. if you want to simulate an epidemic using the raster object "toy_data", you must run
#'          prep_simulation(toy_data) first)
#'
#' @return Returns one dataframe object containing the epidemic estimates per day.
#'
#' @examples
#' results = run_simulation(total_pop_data, kernel, contact_mat, beta = beta)
#'
#' @export


run_simulation = function(rasterl, dist_kernel, contact_mat, beta, alpha=1, sigma = 1/2.6, stoch=FALSE, step=1, start_area=1, start_num=1, t_max=100){


  #identify number of areas and age categories:
  good_values = which(!is.na(rasterl@data@values))  #ignore inhabitable areas (i.e. with a population "NA")
  num_areas = length(good_values)   #derive number of areas
  num_ages = dim(contact_mat)[1]    #derive number of age categories from contact matrix


  #safety check:
  if(num_areas != dim(dist_kernel)[1]){

    stop("The specified kernel matrix does not correspond to this RasterLayer. Recalculate it.")

  }


  #### SETTING UP POPULATION: ####

  #N: total population
  #S: susceptibles
  #I: infected
  #R: recovered

  #!!! work in progress, only supports 4 age categories right now !!!#

  if(num_ages == 4){

    N = matrix(rasterl@data@values[good_values], nrow=num_areas, ncol=num_ages)

    #set minimum population in an area to 1:
    N[which(N<1)] = 1

    N[,1] = N[,1]*(5/80)
    N[,2] = N[,2]*(14/80)
    N[,3] = N[,3]*(45/80)
    N[,4] = N[,4]*(16/80)
    #this way, i in the matrix is the area and j the age group e.g. N[1,1] gives pop 0-4 in area 1

    S = N
    I = matrix(0, nrow=num_areas, ncol=num_ages)
    R = I

    ## IGNORE ##
  } else if(num_ages == 0){

    N = rasterl@data@values[good_values]

    S = N
    I = rep(0, num_areas)
    R = I

  } else {

    stop("Unsupported number of age categories, currently only supports 4 or 0.")

  }



  #### SEEDING: ####

  #currently seeds by making one adult infected in the chosen starting area

  #!!! work in progress, currently only supports starting in area with highest density !!!#

  if(start_area == 1){

    #identify area with the most inhabitants: (effectively, London)

    if(num_ages == 0){

      start_area = which.max(N)
      S[start_area] = S[start_area] - start_num
      I[start_area] = I[start_area] + start_num

    } else {

      #which area has the highest total population? (rowSums gives total pop in each area since each column is an age group)
      start_area = which.max(rowSums(N))
      S[start_area,3] = S[start_area,3] - start_num
      I[start_area,3] = I[start_area,3] + start_num
      #3 is the adult age group

    }

  }

  else if(start_area == 2){

    stop("Unsupported starting area, currently only supports option 1 (start in area with highest density).")

  }

  else if(start_area == 3){

    stop("Unsupported starting area, currently only supports option 1 (start in area with highest density).")

  }

  else{

    stop("Invalid starting area choice. Please choose between 1 and 3.")

  }



  #### RUN MODEL: ####

  t_kernel = t(dist_kernel)
  t_contact = t(contact_mat)


  full_model = function(t, y, .) {

    #counter for keeping track of progress during model execution:
    print(paste0(floor(t/t_max*100), "% done"))

    #obtain S, I and R starting values here:
    #(must be extracted and reconverted to matrices from the "y" object created by initial() which is a vector)
    S = matrix(y[1:(num_areas*num_ages)], ncol=ncol(N), nrow=nrow(N), byrow=F)
    I = matrix(y[(num_areas*num_ages+1):(num_areas*num_ages*2)], ncol=ncol(N), nrow=nrow(N), byrow=F)
    R = matrix(y[(num_areas*num_ages*2+1):(num_areas*num_ages*3)], ncol=ncol(N), nrow=nrow(N), byrow=F)

    #FOI equation is essentially a big matrix multiplication:
    lambda = beta * dist_kernel %*% ((t_kernel%*%I%*%t_contact)^alpha)/(t_kernel%*%N)

    #again, matrix multiplication so no need for indices:
    dSdt = -lambda*S
    dIdt = lambda*S - sigma*I
    dRdt = sigma*I

    list(c(dSdt, dIdt, dRdt))

  }


  #execution changes between stochastic and deterministic:
  if(stoch == TRUE){

    S = ceiling(S)
    I = ceiling(I)
    R = ceiling(R)
    N = ceiling(N)

    beta = beta/step
    sigma = sigma/step

    results = matrix(0, (t_max*step+1), (num_areas*num_ages*3+1))

    results[,1] = seq(0,t_max*step,1)

    results[1,-1] = c(S,I,R)

    sigma = 1 - exp(-(sigma))

    for(t in 1:(t_max*step)){

      lambda = beta * dist_kernel %*% ((t_kernel%*%I%*%t_contact)^alpha)/(t_kernel%*%N)
      lambda = 1 - exp(-lambda)

      for(i in 1:length(N)){

        new_inf = rbinom(1,S[i],lambda[i])
        rec = rbinom(1, I[i], sigma)

        S[i] = S[i] - new_inf
        I[i] = I[i] + new_inf - rec
        R[i] = R[i] + rec

      }

      results[t+1,-1] = c(S,I,R)

    }


  } else {

    #set method to rk4 rather than default lsoda
    #required because otherwise deSolve requires waaay too much RAM
    #more stable performance but slower when smaller number of areas
    #parms=0 because rk4 requires a definition of parms, but effectively the function inherits the parameters from environment

    results = deSolve::ode(method="rk4", func=full_model, y=c(S,I,R), times=seq(0,t_max,1), parms=0)

  }


  return(results)

}
