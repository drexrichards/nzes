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

nzes.carrying.m1 <- function(luc, elev, soilt=NULL, aspect180, vpdjan = NULL, meanT, precip){
  
  # Empirical corrections where data are missing
  if(is.null(soilt)){
    soilt <- -25.444250 + (precip * 0.002355) + 
      (meant * 14.854373) + (meant * precip * -0.000797)
  }
  
  if(is.null(vpdjan)){
    vpdjan <- 29.352599 + (precip * -0.004510) + 
      (meant * 4.456375) + (meant * precip * -0.000388)
  }
  
  # Lookup table for LUC
  lt<-cbind(c(1:8),
        c(0,0,-0.6819865, -1.0584458, -1.5318441, -2.0197741, -2.6343372, -4.9307393))
  
  luc2<- reclassify(luc, lt)
  
  # Equation
  cc <- exp(1.4899984 + (elev * -0.0006978)+
              (aspect180 *0.0019576 ) +
              (soilt * -0.0077331 ) + 
              luc2 +
              (vpdjan * 0.0085309))
  
  return(cc)
}

nzes.carrying.m2 <- function(luc, slope, solar = NULL, vpdann = NULL, vpdjan = NULL, meanT, precip, latitude){
  
  # Empirical corrections where data are missing
  if(is.null(vpdann)){
    vpdann <- 13.484359 + (precip * -0.001526) + 
      (meant * 3.616275) + (meant * precip * -0.000353)
  }
  
  if(is.null(vpdjan)){
    vpdjan <- 29.352599 + (precip * -0.004510) + 
      (meant * 4.456375) + (meant * precip * -0.000388)
  }
  
  if(is.null(solar)){
    solar <- 377.415320
 + (precip * -0.037518) + 
      (meant * 1.400929) + 
      (latitude * 5.406675) +
      (meant * precip * -0.000041) +
    (latitude * precip * -0.000821) +
      (meant * latitude * 0.033392)
  }
  
  # Lookup table for LUC
  lt<-cbind(c(1:8),
            c(0,0,0,-0.457559805, -0.027060915, -0.810075484, -1.49966985, -4.017955247))
  
  luc2<- reclassify(luc, lt)
  
  # Equation
  cc <- exp(-5.971040435 + 
              (slope * 0.017525954) + 
              luc2 +
              (vpdjan * 0.046918408) + 
              (vpdann * -0.098763683) + 
              (solar * 0.043276909))
  
  return(cc)
}