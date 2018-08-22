#' @title Loads required data for the simulation
#'
#' @description Loads the total population density file of England as a RasterLayer object. The population is
#'              divided over a 100 metre grid.
#'
#' @details Note that the function attempts to load the file in the RAM if possible for faster computation later.
#'
#' @return Creates one RasterLayer object in the Global Environment.
#'
#' @examples
#'
#' load_pop_data()
#' View(total_pop_data)
#'
#' @export


load_pop_data = function(){


  s = raster::raster("england_pop.asc")

  s = raster::readAll(s)

  assign("total_pop_data", s, envir = .GlobalEnv)

}
