#' @title Loads all of the England Shapefiles and population data
#'
#' @description Loads three SHapefiles for various administrative units of England (regions, counties and districts).
#'              Also loads the dataframe containing all of the population data for all the units.
#'
#' @return Returns three Shapefile objects and one dataframe.
#'
#' @examples
#' load_shp()
#'
#' View(pop_data)
#'
#' plot(region_shp)
#'
#' @export


load_shp = function(){


  assign("region_shp", maptools::readShapeSpatial("data/Regions/infuse_rgn_2011.shp"), envir=.GlobalEnv)

  assign("county_shp", maptools::readShapeSpatial("data/Counties/england_fct_2011.shp"), envir=.GlobalEnv)

  assign("district_shp", maptools::readShapeSpatial("data/Districts/england_lad_2011.shp"), envir=.GlobalEnv)

  assign("pop_data", read.csv("pop_data.csv", stringsAsFactors = F), envir=.GlobalEnv)


}
