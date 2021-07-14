#' Modelling of stock basic carrying capacity by crown equations
#'
#' This function models stock carrying capacity. Use m1 for model 1, m2 for model 2
#' @param luc Land use capability index. Integer, values 1-8
#' @param elev Elevation in metres above sea level
#' @param soilt Optional. Mean annual soil temperature in degrees C
#' @param aspect180 Value in degrees from 0 to 180. North is 0, south is 180. East or west are 90. Flat areas with a slope less than 7degrees have aspect 180 =0
#' @param vpdjan Optional. Mean vapour pressure deficit in January
#' @param meant Mean annual air temperature
#' @param precip Total annual precipitation
#' @param solar Optional. Total solar radiation in W per m2
#' @param vpdann Optional. Annual mean vapour pressure deficit
#' @param slope Slope in degrees.
#' @param latitude Latitude from WGS as a raster
#' @return Raster indicating modelled potential basic carrying capacity
#' @export

nzes.carrying.m1 <- function(luc, elev, soilt=NA, aspect180, vpdjan=NA, meant, precip){
  
  # Empirical corrections where data are missing
  if(is.na(soilt)){
    soilt <- -25.444250 + (precip * 0.002355) + 
      (meant * 14.854373) + (meant * precip * -0.000797)
  }
  
  if(is.na(vpdjan)){
    vpdjan <- 29.352599 + (precip * -0.004510) + 
      (meant * 4.456375) + (meant * precip * -0.000388)
  }
  
  # Lookup table for LUC
  lt<-cbind(c(1:8),
        c(0,0,-0.6819865, -1.0584458, -1.5318441, -2.0197741, -2.6343372, -4.9307393))
  
  luc2<- raster::reclassify(luc, lt)
  
  # Equation
  cc <- exp(1.4899984 + (elev * -0.0006978)+
              (aspect180 *0.0019576 ) +
              (soilt * -0.0077331 ) + 
              luc2 +
              (vpdjan * 0.0085309))
  
  cc
}
