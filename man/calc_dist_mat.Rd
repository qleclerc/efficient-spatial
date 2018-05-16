% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_dist_mat.R
\name{calc_dist_mat}
\alias{calc_dist_mat}
\title{Calculates the distance matrix}
\usage{
calc_dist_mat(raster_l = s0_04)
}
\arguments{
\item{raster_l}{The RasterLayer object to use to calculate the distance matrix.}
}
\value{
Returns one matrix object.
}
\description{
Calculates the distance between all points in a RasterLayer object.
}
\details{
This function calculates the distance matrix once to avoid doing it for every generation during an
         epidemic simulation. This takes a while, but is essential to later speed up the epidemic simulation.
         To simplify matching areas to their distances, this generates a matrix with the total number of cells
         in the RasterLayer object as dimensions, therefore including NA values. While this is not the most
         optimal approach, it makes indexing easier.
         Finally, note that you can run it using one RasterLayer object then use it for any other RasterLayer
         of the same spatial area (e.g. if you estimate it using the RasterLayer with population data for 0
         to 4 years old in the UK, you can use it for any RasterLayer of the UK of identical resolution)
}
\examples{
dist_matrix = calc_dist_mat(s0_04)

}