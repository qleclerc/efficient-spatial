library(maptools)

pop_data = read.csv("pop_data.csv", stringsAsFactors = F)

shp = maptools::readShapeSpatial("data/Regions/infuse_rgn_2011.shp")

plot(shp)

shp2 = maptools::readShapeSpatial("data/Counties/infuse_cnty_lyr_2011.shp")

plot(shp2)

