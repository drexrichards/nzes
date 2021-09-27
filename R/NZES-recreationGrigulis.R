#' Model an index of recreation based roughly on Karl's approach
#'
#' This function models an indicator of recreational opportunity spectrum
#' @param lcm Raster of land cover classes as integer
#' @param dem Digital elevation model raster
#' @param protect Raster of protected areas as binary
#' @param access Raster of road and track network
#' @param huts Raster of hut locations or other minor attractants and their impact score
#' @param touristspot Raster of major tourism attractants and their impact score
#' @param agfactor Value for aggregation of raster in calculation recommend 50
#' @param aesrecl Matrix for reclassification of aesthetic values on lcdb
#' @param viewrecl Matrix for reclassification of view distance on lcdb
#' @return Raster layer of modelled recreational opportunity
#' @export

nzes.recreationGrigulis<- function(lcm,
                            dem,
                            protect,
                            access,
                            huts,
                            touristspot){
  
  
  # Do zonal statistics
  # Function for buffering a circle
  drawImage <- function(mat, center, radius) {
    grid <- mat
    x.index <- round(center + radius * cos(seq(0, 2 * pi, 
                                               length = 3600)))
    y.index <- round(center + radius * sin(seq(0, 2 * pi, 
                                               length = 3600)))
    xyg <- data.frame(xind = round(center + radius * cos(seq(0, 
                                                             2 * pi, length = 3600))), yind = round(center + radius * 
                                                                                                      sin(seq(0, 2 * pi, length = 3600))))
    for (i in seq(x.index)) {
      fg <- range(xyg$yind[which(xyg$xind == xyg$xind[i])])
      grid[xyg$xind[i], fg[1]:fg[2]] <- 1
    }
    grid
  }
  
  # https://stackoverflow.com/questions/21841387/r-code-that-evaluates-line-of-sight-los-between-two-lat-lon-points
  cansee <- function(r, xy1, xy2, h1=0, h2=0){
    ### can xy1 see xy2 on DEM r?
    ### r is a DEM in same x,y, z units
    ### xy1 and xy2 are 2-length vectors of x,y coords
    ### h1 and h2 are extra height offsets
    ###  (eg top of mast, observer on a ladder etc)
    xyz = rasterprofile(r, xy1, xy2)
    np = nrow(xyz)-1
    h1 = xyz$z[1] + h1
    h2 = xyz$z[np] + h2
    hpath = h1 + (0:np)*(h2-h1)/np
    return(!any(hpath < xyz$z))
  }
  
  viewTo <- function(r, xy, xy2, h1=0, h2=0, progress="none"){
    ## xy2 is a matrix of x,y coords (not a data frame)
    #require(plyr)
    plyr::aaply(xy2, 1, function(d){cansee(r,xy,d,h1,h2)}, .progress=progress)
  }
  
  rasterprofile <- function(r, xy1, xy2){
    ### sample a raster along a straight line between two points
    ### try to match the sampling size to the raster resolution
    dx = sqrt( (xy1[1]-xy2[1])^2 + (xy1[2]-xy2[2])^2 )
    nsteps = 1 + round(dx/ min(res(r)))
    xc = xy1[1] + (0:nsteps) * (xy2[1]-xy1[1])/nsteps
    yc = xy1[2] + (0:nsteps) * (xy2[2]-xy1[2])/nsteps
    data.frame(x=xc, y=yc, z=r[cellFromXY(r,cbind(xc,yc))])
  }
  
  #### 1. Viewshed on aggregated raster ####
  dem2<- raster::aggregate(dem, agfactor,mean)
  dem3<- raster::rasterToPoints(dem2)
  dem4<-as.data.frame(dem3)
  dem4[,4]<-NA
  
  for(i in 1:length(dem4[,4])){
    dem4[i,4]<-
  sum(viewTo(dem2, dem3[i,1:2], dem3[,1:2], 2, 0),na.rm=T)/
    (length(dem3[,1])-1)
  }
  
  dem5<- raster::rasterize(dem4[,1:2], dem2, dem4[,4] )
  dem5<- raster::disaggregate(dem5, agfactor)
  dem5<-raster::resample(dem5, lcm)
  
  # add reduced viewshed in trees
  dem4<- raster::reclassify(lcm, viewrecl)
  dem4<-dem4/length(dem3[,1])
  dem5 <- min(raster::stack(dem4, dem5),na.rm=T)
  
  
  #### 2. Aesthetic values ####
  beauty<- raster::reclassify(lcm, aesrecl)
  
  
  #### 3. Distance from huts etc ####
  # Get size of one pixel in km
  ar<-raster::area(lcm)
  ar<-sqrt(raster::cellStats(ar,mean))
  # ar in m
 ar<- ar*1000
 
 # find number of pixels in 500m each side
 pd<-ceiling(500*2/ar)

 if(pd<2){
   huts<-huts
 }else{
   ndist<- drawImage(matrix(0,pd, pd), ceiling(pd/2), floor(pd/2))
   huts<- raster::focal(huts,
                           ndist,
                           max,
                           na.rm=TRUE,
                           pad = TRUE,
                           padValue = NA)
 }
  
 huts[huts>1]<-1 
 
 #### 4. LATER - Distance from water? ####
 
 #### 5. Rescale touristspot ####
 touristspot<-touristspot/raster::cellStats(touristspot,max)
 
 touristspot[is.na(touristspot)]<-0
 protect[is.na(protect)]<-0
 
 #### 5. Combine all ####
 ot<- (1+huts) * beauty *
   dem5 * access * (1+touristspot) *
   (1+protect )
   
 
  # send output raster
  ot
}
