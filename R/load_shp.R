#' @title Loads all of the England Shapefiles, and the population data
#'
#' @description Loads three Shapefiles for administrative units of England (regions, counties and districts).
#'              Also loads the dataframe containing all of the population data for all the units.
#'
#' @details This function assumes that the Shapefiles and population data are in specific directories. If it fails,
#'          manually load the shapefiles and population data using readShapeSpatial and read.csv functions.
#'
#' @return Returns three Shapefile objects and one dataframe.
#'
#' @examples
#'
#' load_shp()
#'
#' View(pop_data)
#'
#' plot(region_shp)
#'
#' @export


load_shp = function(){


  assign("region_shp", maptools::readShapeSpatial("data/Regions/england_regions.shp"), envir=.GlobalEnv)

  assign("county_shp", maptools::readShapeSpatial("data/Counties/england_counties.shp"), envir=.GlobalEnv)

  assign("district_shp", maptools::readShapeSpatial("data/Districts/england_districts.shp"), envir=.GlobalEnv)


  assign("pop_data", read.csv("data/pop_data.csv", stringsAsFactors = F), envir=.GlobalEnv)


}
