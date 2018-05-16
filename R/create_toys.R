#' @title Generates low resolutions RasterLayer objects
#'
#' @description Generates low resolution versions of the Great Britain raster data to use when testing other
#'              functions. This is solely for rapid testing of other functions in the package. It is recommended
#'              to save these on the disk to fast load them again later using the load_toys function.
#'
#' @return Creates four RasterLayer objects in the Global Environment.
#'
#' @examples
#' create_toys()
#'
#' @export


create_toys = function(){

  #note that "aggregate" uses the "sum" function so that to estimate the population in an area, you will always
    #just need to do "area value/100"

  assign("t0_04", raster::aggregate(s0_04, fact=1000, fun=sum), envir = .GlobalEnv)
  assign("t05_19", raster::aggregate(s05_19, fact=1000, fun=sum), envir = .GlobalEnv)
  assign("t20_64", raster::aggregate(s20_64, fact=1000, fun=sum), envir = .GlobalEnv)
  assign("t65plus", raster::aggregate(s65plus, fact=1000, fun=sum), envir = .GlobalEnv)

}
