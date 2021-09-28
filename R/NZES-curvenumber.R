#' Model runoff following curve number method
#'
#' This function models stormwater runoff using curve numbers
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
  
      
      PCHECK<- P > (0.2*S)
      
    

      Q1 <- ((P - 0.2*S)^2) / (P + 0.8*S)
      Q0 <- PCHECK
      Q0[,]<-0
      Q0[PCHECK==1]<- Q1[PCHECK==1]
    
    
     round(Q0, 3)
    
}
