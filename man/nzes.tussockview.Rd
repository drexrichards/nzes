% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NZES-tussockview.R
\name{nzes.tussockview}
\alias{nzes.tussockview}
\title{Model an index of tussock viewshed}
\usage{
nzes.tussockview(lcm, dem, agfactor, aesrecl, viewrecl)
}
\arguments{
\item{lcm}{Raster of land cover classes as integer}

\item{dem}{Digital elevation model raster}

\item{agfactor}{Value for aggregation of raster in calculation recommend 50}

\item{aesrecl}{Matrix for reclassification of desirable land covers on lcdb}

\item{viewrecl}{Matrix for reclassification of view distance on lcdb}
}
\value{
Raster layer of proportion of view that is tussock
}
\description{
This function models the proportion of viewshed that is tussock (or other land covers)
}
