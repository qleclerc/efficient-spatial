#' @title Loads required data for the simulation
#'
#' @description Loads the UK total population density file from the working directory as a RasterLayer object.
#'
#' @details This function is dangerous in the sense that it assigns a variable to the Global Environment
#'          automatically. If there are any errors, try running it with an empty Global Environment.
#'          Note that the function attempts to load the file in the RAM if possible for faster computation later.
#'
#' @return Creates one RasterLayer object in the Global Environment.
#'
#' @examples
#' load_pop_data()
#' View(total_pop_data)
#'
#' @export


load_pop_data = function(){

  #safety check:
  if(identical(list.files(pattern = ".asc"),character(0))){

    stop("No .asc file in working directory! Make sure that it is there")

  }

  #safety check:
  if(length(list.files(pattern = ".asc")) > 1){

    stop("More than one .asc file detected in working directory! Make sure that only the total population .asc file is there")

  }

  #load .asc file as RasterLayer in R:
  s = raster::raster("total_UK_population.asc")

  #if possible, loads the data in RAM for faster computation after:
  if(raster::canProcessInMemory(s) == T){

    s = raster::readAll(s)

  }

  assign("total_pop_data", s, envir = .GlobalEnv)

}
