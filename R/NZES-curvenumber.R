#' Model an index of recreation based roughly on Karl's approach
#'
#' This function models an indicator of recreational opportunity spectrum
#' @param curven Raster of curve number values between 0 and 100
#' @param precip Raster of precipitation
#' @return Raster indicating Q (stormwater runoff in mm)
#' @export

nzes.curvenumber<- function(curven,
                                   precip){
  
  
  # Take function from Claudia Vitolo https://github.com/cvitolo/curvenumber/blob/master/R/DirectStormRunoff.R
CN<-curven
P <- precip
    
      S <- 25400/CN - 254
  
      
      PCHECK<- P > 0.2*S
      
    

      Q1 <- ((P - 0.2*S)^2) / (P + 0.8*S)
      Q0 <- 0
      
      PCHECK[PCHECK>0]<- Q1[PCHECK>0]
    
    
     round(Q1, 3)
    
}
