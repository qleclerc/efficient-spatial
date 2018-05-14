#' @title Loads required data for the simulation
#'
#' @description Loads the UK total population density raster file from the working directory, and divides it into
#'              4 objects corresponding to the four age groups. Also creates the age contact matrix.
#'
#' @details This function is dangerous in the sense that it assigns variables to the Global Environment
#'          automatically. If there are any errors, try running it with an empty Global Environment.
#'          The approach of loading one raster file then subdividing it means that any person wishing to use this
#'          code only needs to have one .asc file in storage, compared to four.
#'          Note that this will work most effectively if your computer has at least 8Gb of RAM. Otherwise, the
#'          objects will not be loaded in memory and computation will be three times longer!
#'
#' @return Creates four RasterLayer objects and one matrix object in the Global Environment.
#'
#' @examples
#' load_pop_data()
#' View(contact_mat)
#' View(s0_4)
#'
#' @export


load_pop_data = function(){

  #check is .asc file is present in working directory:
  if(identical(list.files(pattern = ".asc"),character(0))){

    stop("No .asc file in working directory! Make sure that it is there.", call. = FALSE)

  }

  #load .asc file as RasterLayer in R:
  s = raster::raster(list.files(pattern = ".asc"))

  #if possible, loads the data in RAM for faster computation after:
  if(raster::canProcessInMemory(s) == T){

    s = raster::readAll(s)

  }

  #subdivide population in four groups and create objects in Global Environment:
  assign("s0_04", raster::calc(s, fun = function(x) x*(5/80)), envir = .GlobalEnv)
  assign("s05_19", raster::calc(s, fun = function(x) x*(15/80)), envir = .GlobalEnv)
  assign("s20_64", raster::calc(s, fun = function(x) x*(45/80)), envir = .GlobalEnv)
  assign("s65plus", raster::calc(s, fun = function(x) x*(16/80)), envir = .GlobalEnv)


  #create age mixing matrix:
  contact_mat = matrix(c(1.920, 1.760, 4.970, 0.230,
                      0.433, 8.740, 5.477, 0.297,
                      0.434, 1.869, 7.832, 0.666,
                      0.195, 1.265, 5.165, 1.755),
                    ncol = 4, byrow = T,
                    dimnames = list(c("0-4","5-19","20-64","65+"),
                                  c("0-4","5-19","20-64","65+")))

  assign("contact_mat", contact_mat, envir=.GlobalEnv)

}
