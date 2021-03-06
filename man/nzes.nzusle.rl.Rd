% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NZES-nzusle-RL.R
\name{nzes.nzusle.rl}
\alias{nzes.nzusle.rl}
\title{Model soil erosion and prevention of it using NZUSLE via Karl and Richard's method}
\usage{
nzes.nzusle.rl(topo, eR, eK, eU)
}
\arguments{
\item{topo}{Raster of digital elevation model of topographty}

\item{eR}{Raster of rainfall erosivity}

\item{eK}{Raster of soil erodibility K factor}

\item{eU}{Raster of vegetation cover factor}
}
\value{
Three level raster stack indicating A, SI, and the ecosystem services as a percentage reduction
}
\description{
This function models soil erosion ecosystem services using the Guerra et al. approach but using NZUSLE as the model
}
