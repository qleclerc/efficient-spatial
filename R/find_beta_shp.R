#' @title Fits beta from one resolution to another (Shapefile)
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
#'
#' @export


find_beta_shp = function(fit_data, shp, interval=100, num_runs=500){

  if(length(fit_data) != (interval+1)) stop("Fit data length does not match the fitting interval!")

  prep_simulation_shp(shp)

  #log likelihood calculation:
  fit_beta = function(fit_data, test_beta, shp_data){

    results = multi_stoch_shp(num_runs, shp_data, proper_D, contact_mat, test_beta, t_max=interval)

    #plot the fitted curve against the target each time a beta is estimated to visualise progress:
    plot(x=results$Time, y=fit_data, type="l", main=paste0("Beta: ", test_beta))
    lines(x=results$Time, y=results$Median, col="red")
    legend("topright", legend=c("Target", "Fitted"), col=c("black", "red"), lty=1)

    results = results$Median

    #calculate total log likelihood of fit:
    ll = sum(dpois(fit_data, results, log=T))

    return(ll)

  }

  #optimise function to find beta yielding highest total log likelihood:
  best_beta = optimise(function(test_beta) fit_beta(fit_data, test_beta, shp_data), c(0.02,0.05), maximum = T)

  return(best_beta$maximum)

}
