#change dist matrix calculation to ignore NAs, then do the same here when defining the pops, this way indexing matches

#need:
  #- seed infection
    #perhaps manually define coordinates of areas of interest, then say where want to seed and randomly do that
    #or could seed in random areas but based on their density (eg seed in one with value in highest quantile)

  #- model
    #- need option to set mobility

  #- return stats
    #- peak time
    #- attack rate

#note that to get actual population, you need to divide density of area by 100
  #because at base level, it's density in km2 for a 0.01km2 area
  #then when increase res just sum up values
  #eg at 100m res: area 1 has 600, area 2 has 400 density
    #so pop in area 1 is 600/100 = 6, and area 2 is 4
    #pop in areas 1+2 must be 10
    #so when decrease res to aggregate both and indicate "sum" function, get 1000 density
    #1000/100 = 10, works out!

#interestingly, it appears that the values should not be divided by 100 to get the actual pop... effectively,
  #summing all the values from all layers gives the UK pop (about 58 million)!

#' @title Runs the spatial epidemic simulation
#'
#' @description Simulates an epidemic using the provided RasterLayers, distance matrix, spatial kernel,
#'              contact matrix, and R0.
#'
#' @param raster_list A list containing the names of the four RasterLayers (one for each age category).
#' @param dist_matrix The distance matrix.
#' @param s_kernel The spatial kernel.
#' @param contact_mat The contact matrix between age groups.
#' @param R0 The desired value for R0.
#' @param stoch Logical. If TRUE, the simulation is stochastic.
#'
#' @details This functions requires many parameters to run. These can be generated using the following functions:
#'
#' @return Returns one dataframe object containing the total epidemic estimates of cases per day.
#'
#' @examples
#' dist_matrix = calc_dist_mat(s0_04)
#'
#' @export


run_simulation = function(raster){

  #change to calculate age structured pop here
  #so extract total, then divide in four

  #select only the areas with non-NA values:
  good_values = which(!is.na(raster@data@values))


  #SETTING UP POPULATION:
  N = array(sapply(raster_list, FUN=function(x) return(x@data@values[good_values])), dim=c(length(good_values),4))
  #this way, i in the array is the area and j the age group e.g. N[1,1] gives pop 0-4 in area 1
  S = N
  I = array(0, dim=dim(N))
  R = I


  #SEEDING:
  #effectively start in area with highest density (London):
  start_area = which.max(N[,1])

  #starting percent of infected (probably specify that as function parameter):
  start_fraction = 0.01

  #in which pop to start? could assign randomly or select one pop
  #here, start with 20-64:
  S[start_area,3] = S[start_area,3] - N[start_area,3]*start_fraction
  I[start_area,3] = I[start_area,3] + N[start_area,3]*start_fraction


  #note: no need to keep new values, only retain sum of all infected, then split this to get daily incidence
  #MODEL
  for(t in 1:times){
    for(i in 1:length(good_values)){
      for(j in 1:4){
        for(ii in 1:length(good_values)){
          for(jj in 1:4){



          }
        }
      }
    }
  }
}


