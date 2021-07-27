#' General function for lookup table type ecosystem service model
#'
#' This function assigns values from a lookup table to a raster. It is just a wrapper for raster::reclassify, so better to use that function really...
#' @param lc Land cover or other integer raster representing categories
#' @param lt Lookup table with same number of classes. 2 column matrix
#' @return Raster indicating ecosystem service provision
#' @export

nzes.lookup <- function(lc, lt){
  
 ra<- raster::reclassify(lc, lt)
  ra
}
