#' Model soil erosion and prevention of it using NZUSLE
#'
#' This function models soil erosion ecosystem services using the Guerra et al. approach but using NZUSLE as the model
#' @param topo Raster of digital elevation model of topographty 
#' @param eR Raster of rainfall erosivity 
#' @param eK Raster of soil erodibility K factor
#' @param eU Raster of vegetation cover factor
#' @return Three level raster stack indicating A, SI, and the ecosystem services as a percentage reduction
#' @export

nzes.nzusle<- function(topo,
                     eR,
                     eK,
                     eU){
  
  # Generate temporary directory for whitebox
  tmpdir<- tempdir()
  raster::writeRaster(topo, 
              paste(tmpdir, "dem.tif", sep = ""), overwrite=T,
              datatype='INT2S')
  
  # Z - slope factor
  # convert slope from angle to grade
  esSlope <- raster::terrain(topo, "slope",unit="degrees", neighbours=8)
  # Cut to max 50% , following EU # https://esdac.jrc.ec.europa.eu/themes/slope-length-and-steepness-factor-ls-factor
  # 50% slope is 26.57 degrees
  esSlope1<-esSlope # this one is for m factor calculation
  
  
  whitebox::wbt_slope(
    dem=paste(tmpdir, "dem.tif", sep = "") , 
    output = paste(tmpdir,"out_slope.tif",sep=""), 
    zfactor=NULL, 
    units="percent"
  )
  
  eZ<-raster::raster(paste(tmpdir,"out_slope.tif",sep=""))
  eZ<- eZ/100
  eZ<- 0.065+(4.56 * eZ)+(65.41 *(eZ^2))
  
  # LS factor
  whitebox::wbt_breach_depressions(
    dem=paste(tmpdir, "dem.tif", sep = "") , 
    output = paste(tmpdir,"out_filleddem.tif",sep=""),
    #flat_increment=0.2, 
    fill_pits=TRUE
  )
  
  whitebox::wbt_max_upslope_flowpath_length(
    dem=paste(tmpdir,"out_filleddem.tif",sep=""),
    output = paste(tmpdir,"out_slopelength.tif",sep="")
  )
  

  # need to convert to m from map units
  tr<- raster::raster(paste(tmpdir,"out_slopelength.tif",sep=""))
  ar<-raster::area(tr)
  eL<-tr *((1/mean(raster::res(tr))) *sqrt(raster::cellStats(ar,mean)))*1000 
  rm(tr)
  #rm(ar)
  
  # Remove slope lengths of 0, they are about half a cell following Barriuso-Mediavilla et al. 2017 and Bolton et al 1995
  eL[eL==0]<- (sqrt(raster::cellStats(ar,mean))*1000  /2)
  # Constrain slope length to max 305 metres following Barriuso-Mediavilla et al. 2017 and Bolton et al 1995
  eL[eL>305]<-305
  eL<-(eL/22.13)
  
  # m calculation
  # m is equivalent to 0.5 for slopes steeper than 5%, 2.86
  # 0.4 for slopes between 3%–4%, 
  # 0.3 for slopes between 1%–3%
  #0.2 for slopes less than 1%. 0.6 degrees
  
  em<- esSlope1
  em[,]<-0.5
  em[eZ< 2.86]<-0.4
  em[eZ < 1.7]<-0.3
  em[eZ <0.6]<-0.2
  em<-raster::mask(em,eZ)
  eL <- eL^em
  
  # eR - rainfall erosivity
  eR <- (eR^2) 
  
  # eK - soil erodibility factor
  
  # eU - vegetation cover factor
 
  # Es (x,y) =  Î±P2 (x,y) K (x,y) L (x,y) Z (x,y) U (x,y)
  esSoilLoss <- 0.0012*eR*eK*eL*eZ*eU # This is what the Guerra et al model calls "A" or actual soil loss
  esSI <- 0.0012*eR*eK*eL*eZ # This is what the Guerra et al model calls "SI" or soil loss that would happen without vegetation
  esSoilES <- (esSI - esSoilLoss)/ esSI
  
  ra<- raster::stack(esSoilLoss,
                esSI,
                esSoilES)
  #names(ra)<-c("A","SI","erosionES")
  ra
}
