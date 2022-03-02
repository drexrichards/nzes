#' Model an index of tussock viewshed
#'
#' This function models the proportion of viewshed that is tussock (or other land covers)
#' @param lcm Raster of land cover classes as integer
#' @param dem Digital elevation model raster
#' @param agfactor Value for aggregation of raster in calculation recommend 50
#' @param aesrecl Matrix for reclassification of desirable land covers on lcdb
#' @param viewrecl Matrix for reclassification of view distance on lcdb
#' @return Raster layer of proportion of view that is tussock
#' @export

nzes.tussockview<- function(lcm,
                            dem,
                            agfactor,
                            aesrecl,
                            viewrecl){
  
  
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
    nsteps = 1 + round(dx/ min(raster::res(r)))
    xc = xy1[1] + (0:nsteps) * (xy2[1]-xy1[1])/nsteps
    yc = xy1[2] + (0:nsteps) * (xy2[2]-xy1[2])/nsteps
    data.frame(x=xc, y=yc, z=r[raster::cellFromXY(r,cbind(xc,yc))])
  }
  
  #### 1. Viewshed on aggregated raster ####
  dem2<- raster::aggregate(dem, agfactor,mean)
  dem3<- raster::rasterToPoints(dem2)
  dem4<-as.data.frame(dem3)
  dem4[,4]<-NA
  
  lcm2<-raster::reclassify(lcm, aesrecl)
  lcm2<-raster::aggregate(lcm2, agfactor,mean)
  lcm3<- raster::rasterToPoints(lcm2)
  lcm3<-as.data.frame(lcm3)
  
  for(i in 1:length(dem4[,4])){
    
    dem4[i,4]<- mean((viewTo(dem2, dem3[i,1:2], dem3[,1:2], 2, 0) * 
      lcm3$layer),na.rm=T)
  }
  
  dem5<- raster::rasterize(dem4[,1:2], dem2, dem4[,4] )
  dem5<- raster::disaggregate(dem5, agfactor)
  dem5<-raster::resample(dem5, lcm)
  
  # add no viewshed in trees
  dem4<- raster::reclassify(lcm, viewrecl)
  dem4[!is.na(dem4)]<-0
  dem5 <- min(raster::stack(dem4, dem5),na.rm=T)
  
  
  # send output raster
  dem5
}
