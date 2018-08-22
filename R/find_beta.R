#' @title Fits beta from one resolution to another (Raster)
#'
#' @description Adjusts beta to fit the incidence of one resolution onto another.
#'
#' @param fit_data The incidence data to use for fitting.
#' @param rasterl The Rasterlayer to fit on.
#' @param interval The interval on which the fit should be performed, in days (default is 100 - i.e. the fit will
#'                 be performed on the first 100 days of the epidemic).
#' @param num_runs Number of stochastic runs to extract the median incidence for one beta value (default is 500).
#'                 Higher values will be longer to run, but yield more accurate results.
#'
#' @return Returns the fitted value of beta.
#'
#' @examples
#'
#' #Create a high resolution RasterLayer object:
#' htest_data = raster(nrow=20, ncol=20, xmn=1, xmx=100000, ymn=1, ymx=100000)
#' values(htest_data) = runif(400, 1, 1000)
#'
#' #Calculate the median incidence for the high resolution data:
#' prep_simulation(htest_data)
#' results_high = multi_stoch(500, htest_data, expanded_D, contact_mat, beta, t_max=100)
#' results_high = results_high$Median
#'
#'
#' #Create a low resolution RasterLayer object:
#' ltest_data = raster(nrow=10, ncol=10, xmn=1, xmx=100000, ymn=1, ymx=100000)
#' values(ltest_data) = runif(100, 1, 1000)
#'
#' #Fit low resolution on high resolution:
#' fitted_beta = find_beta(results_high, test, interval=100)
#'
#' @export


find_beta = function(fit_data, rasterl, interval=100, num_runs=500){

  if(length(fit_data) != (interval+1)) stop("Fit data length does not match the fitting interval!")

  prep_simulation(rasterl)

  #log likelihood calculation:
  fit_beta = function(fit_data, test_beta, rasterl){

    results = multi_stoch(num_runs, rasterl, expanded_D, contact_mat, test_beta, t_max=interval)

    #plot the fitted curve against the target each time a beta is estimated to visualise progress:
    plot(x=results$Time, y=fit_data, type="l", main=paste0("Beta: ", test_beta))
    lines(x=results$Time, y=results$Median, col="red")
    legend("topright", legend=c("Target", "Fitted"), col=c("black","red"), lty=1)

    results = results$Median

    #calculate total log likelihood of fit:
    ll = sum(dpois(fit_data, results, log=T))

    return(ll)

  }

  #optimise function to find beta yielding highest total log likelihood:
  best_beta = optimise(function(test_beta) fit_beta(fit_data, test_beta, rasterl), c(0.02,0.05), maximum = T)

  return(best_beta$maximum)

}
