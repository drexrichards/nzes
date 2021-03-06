% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NZES-fate-converter.R
\name{nzes.fate.converter}
\alias{nzes.fate.converter}
\title{Extract temporal land cover maps of dominant vegetation per pixel}
\usage{
nzes.fate.converter(fp, sc, lcdblookup)
}
\arguments{
\item{fp}{Text string folder for overall FATE version e.g. mac_test6}

\item{sc}{Text string simulation version e.g. V3}

\item{lcdblookup}{Lookup table of conversion codes for ldcb to pfg}
}
\value{
Named raster stack with 6 layers for different years.
}
\description{
This function is a helper to get land cover maps for different time points (fixed)
}
