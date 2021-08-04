#' Model soil erosion and prevention of it using NZUSLE via Karl and Richard's method
#'
#' This function models soil erosion ecosystem services using the Guerra et al. approach but using NZUSLE as the model
#' @param topo Raster of digital elevation model of topographty 
#' @param eR Raster of rainfall erosivity 
#' @param eK Raster of soil erodibility K factor
#' @param eU Raster of vegetation cover factor
#' @return Three level raster stack indicating A, SI, and the ecosystem services as a percentage reduction
#' @export

nzes.nzusle.rl<- function(topo,
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
 # esSlope <- raster::terrain(topo, "slope",unit="degrees", neighbours=8)
#eZ<- 0.065 + ( 4.56 * ( {esSlope} / 100 ) ) + ( 65.41 * ({esSlope} / 100 ) ^ 2 )

  
  
  # L factor
  whitebox::wbt_flow_accumulation_full_workflow( dem=paste(tmpdir, "dem.tif", sep = "") , 
                                     out_dem = paste(tmpdir,"out_dem.tif",sep=""),
                                     out_pntr = paste(tmpdir,"out_pntr.tif",sep=""),
                                     out_accum = paste(tmpdir,"out_accum.tif",sep=""),
                                     out_type = "Specific Contributing Area",
                                     log = FALSE,
                                     clip = FALSE,
                                     esri_pntr = FALSE,
                                     wd = NULL,
                                     verbose_mode = T)


whitebox::wbt_slope(
  dem=paste(tmpdir, "dem.tif", sep = "") , 
  output = paste(tmpdir,"out_slope.tif",sep=""), 
  zfactor=NULL, 
  units="degrees"
)


whitebox::wbt_sediment_transport_index(
  sca=paste(tmpdir, "out_accum.tif", sep = ""),
  slope=paste(tmpdir,"out_slope.tif",sep=""),
  output=paste(tmpdir,"out_ls.tif",sep=""),
  sca_exponent = 0.4,
  slope_exponent = 1.3,
  wd = NULL,
  verbose_mode = FALSE)

 eLS<-raster::raster(paste(tmpdir,"out_ls.tif",sep=""))
# 
# ar<-raster::area(eL)
# eL<- sqrt(eL)*
#   ((raster::cellStats(ar,mean))*1000000)/ pi
# eL[eL>305]<-305
# eL<-eL/22
# eL<- eL^0.5



  
  # eR - rainfall erosivity
  eR <- (eR^2) 
  
  # eK - soil erodibility factor
  
  # eU - vegetation cover factor
 
  # Es (x,y) =  Î±P2 (x,y) K (x,y) L (x,y) Z (x,y) U (x,y)
  esSoilLoss <- 0.0012*eR*eK*eLS*eU # This is what the Guerra et al model calls "A" or actual soil loss
  esSI <- 0.0012*eR*eK*eLS # This is what the Guerra et al model calls "SI" or soil loss that would happen without vegetation
  esSoilES <- (esSI - esSoilLoss)/ esSI
  
  ra<- raster::stack(esSoilLoss,
                esSI,
                esSoilES,
                eLS)
  #names(ra)<-c("A","SI","erosionES")
  ra
}
