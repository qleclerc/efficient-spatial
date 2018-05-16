#' @title Loads low resolutions RasterLayer objects
#'
#' @description Loads low resolution versions of the Great Britain raster data to use when testing other
#'              functions. This is solely for rapid testing of other functions in the package. These objects must
#'              have been created using the create_toys function.
#'
#' @param in_memory Logical. If TRUE, the RasterLayer objects will be loaded in the RAM to avoid constantly
#'                  accessing the disk. These are small objects, therefore any system should be able to load them.
#'                  Nevertheless, this is set to FALSE as default for maximum compatibility.
#'
#' @return Loads four RasterLayer objects in the Global Environment.
#'
#' @examples
#' #does not load in RAM:
#' load_toys()
#' load_toys(in_memory = FALSE)
#'
#' #loads in RAM:
#' load_toys(in_memory = TRUE)
#'
#' @export


load_toys = function(in_memory = FALSE){

  if(in_memory == TRUE){

    assign("t0_04", raster::readAll(raster::raster("t0_04.asc")), envir = .GlobalEnv)
    assign("t05_19", raster::readAll(raster::raster("t05_19.asc")), envir = .GlobalEnv)
    assign("t20_64", raster::readAll(raster::raster("t20_64.asc")), envir = .GlobalEnv)
    assign("t65plus", raster::readAll(raster::raster("t65plus.asc")), envir = .GlobalEnv)

  } else {

    assign("t0_04", raster::raster("t0_04.asc"), envir = .GlobalEnv)
    assign("t05_19", raster::raster("t05_19.asc"), envir = .GlobalEnv)
    assign("t20_64", raster::raster("t20_64.asc"), envir = .GlobalEnv)
    assign("t65plus", raster::raster("t65plus.asc"), envir = .GlobalEnv)

  }

}
