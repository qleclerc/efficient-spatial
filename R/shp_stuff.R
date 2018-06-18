# # #TO DO:
# #   # - change shp data into characters (currently factors) and resave
# #   # - change pop_data names to normal case to match shp
# #   # - think about extracting coordinates: will get messy since some areas are both counties and districts
# #     #maybe combine all data in one shp file, then access coordinates using only this one?
# #       #check potential of stacking (much like rasterstack)
# #
# # #consider 4 levels:
# #   # - regions (9)
# #   # - counties(88)
# #   # - disticts (326)
# #   # - MSOA (about 8000)
# #
# library(maptools)
#
# pop_data = read.csv("pop_data.csv", stringsAsFactors = F)
#
#
#shp = maptools::readShapeSpatial("data/Regions/infuse_rgn_2011.shp")
#
# plot(shp)
#
#
# shp2 = maptools::readShapeSpatial("data/Counties/england_fct_2011.shp")
#
# plot(shp2)
#
#
# shp3 = maptools::readShapeSpatial("data/Districts/england_lad_2011.shp")
#
# plot(shp3)
#
#
# coordinates(shp) #returns x and y centroid coordinates for all areas in shp
#
#
# #extract pop data for corresponding shp:
# test = pop_data[which(pop_data$Area_name %in% shp$name),]
#
# #reorder pop data to match shp order:
# test = test[match(shp$name, test$Area_name),]
#
# #extract coordinates:
# test = cbind(test, "x" = coordinates(shp)[,1], "y" = coordinates(shp)[,2])
#
# #now should just be about adapting raster code to get coordinates and pop data from this!
#
