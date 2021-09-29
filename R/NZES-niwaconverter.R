#' Extract key climate variables for hydrology, erosion, and stock unit carrying capacity models
#'
#' This function is a helper to get useful layers from the NIWA archives
#' @param niwadd Folder for source NIWA netcdfs
#' @param reprojraster Raster of resolution needed for modelling, to force NIWA layers into
#' @return Named raster stack with 4 layers. 
#' @export
nzes.niwaconverter<- function(niwadd,
                            reprojraster){
  require(raster)
  fl<-list.files(niwadd)
  
  tmax2<- raster::brick(paste0(niwadd, fl[grep("Max", fl)]))
  tmin2<- raster::brick(paste0(niwadd, fl[grep("Min", fl)]))
  precip2<- raster::brick(paste0(niwadd, fl[grep("Precip", fl)]))
  
  
  #precip2<-raster::crop(precip2, region)
  #precip2<-raster::mask(precip2, region)
  
  #tmax2<-raster::crop(tmax2, region)
  #tmax2<-raster::mask(tmax2, region)
  tmax2<- tmax2 + -272.15
  
  #tmin2<-raster::crop(tmin2, region)
  #tmin2<-raster::mask(tmin2, region)
  tmin2<-tmin2+ -272.15
  
  #pet<-raster::crop(pet, region)
  #pet<-raster::mask(pet, region)
  
  # split into years
  spltyr<- function(stk){
    
    yi<- rbind(seq(from = 1, to = raster::nlayers(stk), length.out=  ceiling(raster::nlayers(stk)/365)),
               seq(from = 1, to = raster::nlayers(stk), length.out=  ceiling(raster::nlayers(stk)/365))-1)
    yi[2, 1:(ncol(yi)-1)] <-  yi[2, 2:ncol(yi)]
    yi<-yi[,1:(ncol(yi)-1)]
    
    stk2<-list(ncol(yi))
    
    for(i in 1:ncol(yi)){
      stk2[[i]]<- stk[[yi[1,i] : yi[2,i] ]]
    }
    stk2
  }
  
  tmax<-spltyr(tmax2)
  tmin<-spltyr(tmin2)
  precip<-spltyr(precip2)

  
  # Create
  # average annual temperature
  p1<-tmin
  for(j in 1:length(tmin)){
    
    fg<-(tmin[[j]]+tmax[[j]])/2
    fg<- mean(fg,na.rm=TRUE)
    p1[[j]]<- fg 
  }
  p1 <-do.call(mean, p1)
  
  # Total annual precipitation
  p2<-tmin
  for(j in 1:length(tmin)){
    
    fg<-(precip[[j]])
    fg<- sum(fg,na.rm=TRUE)
    p2[[j]]<- fg 
  }
  p2 <-do.call(mean, p2)
  
  # Large storm event 1 in 10 year
  p3<- raster::calc(precip2, function(x){quantile(x, 0.99, na.rm=T)})
  
  ocro <-stack(p1,p2,p3,p4,p5)
  names(ocro)<-c("meant","tap","highprecip")
  ocro<-raster::projectRaster(ocro,reprojraster)
  #ocro<-raster::resample(ocro, reprojraster,"ngb")
  ocro
}