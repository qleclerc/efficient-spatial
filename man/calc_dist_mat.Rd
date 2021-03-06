% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_dist_mat.R
\name{calc_dist_mat}
\alias{calc_dist_mat}
\title{Calculates the distance matrix (Raster)}
\usage{
calc_dist_mat(rasterl)
}
\arguments{
\item{raster_l}{The RasterLayer object to use to calculate the distance matrix.}
}
\value{
Returns one matrix object containg the distances between all populated areas.
}
\description{
Calculates the distance between all populated areas in a RasterLayer object.
}
\details{
This function calculates the distance matrix once to avoid doing it for every generation during an
         epidemic simulation. This is essential to later speed up the epidemic simulation. The matrix returned
         only contains distance between cells with non-NA values (i.e. cells with people in them). Note that this
         function assumes that the distances are in meters in the dataset! If that is not the case, you might want
         to either convert these, or adjust the output of this function.
}
\examples{

#Create a RasterLayer object:
test_data = raster(nrow=10, ncol=10, xmn=1, xmx=100000, ymn=1, ymx=100000)
values(test_data) = runif(100, 1, 1000)

dist_mat = calc_dist_mat(test_data)

}
