#' @title Quick testing function
#'
#' @description Performs setup (loading data, calculating distance kernel and contact matrix) and runs a short
#'              simulation to check that there are no errors in the calculations. Some customisation of parameters
#'              possible for extreme values testing.
#'
#' @param R0 Desired R0 for the outbreak.
#' @param t_max Desired length of simulation.
#' @param start_fraction Desired starting proportion of infected in seeding area.
#'
#' @return Returns a summary of the sum of rows in the results (expected: only one value), the minimum value
#'         in the results (expected: 0) and the overall epidemic plot.
#'
#' @examples
#' test_all_run(R0 = 2, sigma = 1/4.2, t_max = 100)
#'
#' @export


test_all_run = function(R0=1.8, sigma = 1/2.6, t_max=365, start_fraction=0.00001){

  load_toy()

  #prep_simulation(toy_data, R0=R0, sigma=sigma)

  assign("contact_mat", load_contact_mat(), envir=.GlobalEnv)

  d = calc_dist_mat(toy_data)

  assign("kernel", calc_dist_kernel(d, toy_data, 0.95, 3.95, 13.5), envir=.GlobalEnv)

  beta = calc_beta(toy_data, kernel, contact_mat, R0=R0, sigma=sigma)

  assign("results", run_simulation(toy_data, kernel, contact_mat, beta=beta, start_fraction = start_fraction, t_max=t_max), envir=.GlobalEnv)

  print("Beta: ")
  print(beta)

  print("Summary of sum of rows:")
  print(table(rowSums(results[,-1])))

  print("Minimum value in results:")
  print(min(results))

  plot_simulation(results, num_areas=52, num_ages=4)

}
