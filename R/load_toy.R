#' @title Loads a low resolution RasterLayer object
#'
#' @description Loads a low resolution version of the Great Britain raster data to use when testing other
#'              functions. This is solely for rapid testing of other functions in the package.
#'              Note that the function attempts to load the file in the RAM if possible for faster computation later.
#'
#' @return Loads one RasterLayer objects in the Global Environment.
#'
#' @examples
#' load_toy()
#' View(toy_data)
#'
#' @export


load_toy = function(){

  toy_data = raster::raster("data/toy_data.asc")

  if(raster::canProcessInMemory(toy_data) == T){

    toy_data = raster::readAll(toy_data)

  }

  assign("toy_data", toy_data, envir = .GlobalEnv)

}
