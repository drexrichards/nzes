#' Crop suitability for 6 key crops based on rules developed by PFR
#'
#' This function models suitability for 6 crops under irrigated and non irrigated situations
#' @param niwadd Folder for source NIWA netcdfs
#' @param reprojraster Raster of resolution needed for modelling, to force NIWA layers into
#' @return Named raster stack with 5 layers. 
#' @export
nzes.vegclimates<- function(niwadd,
                            reprojraster){
  require(raster)
  fl<-list.files(niwadd)
  
  tmax2<- raster::brick(paste0(niwadd, fl[grep("Max", fl)]))
  tmin2<- raster::brick(paste0(niwadd, fl[grep("Min", fl)]))
  precip2<- raster::brick(paste0(niwadd, fl[grep("Precip", fl)]))
  pet<- raster::brick(paste0(niwadd, fl[grep("PE_VCSN", fl)]))
  
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
  #rad<-spltyr(rad)
  precip<-spltyr(precip2)
  #tda<-spltyr(tda)
  pet<- spltyr(pet)
  
  # Create
  # min temperature in winter
  p1<-tmin
  for(j in 1:length(tmin)){
    
    fg<-tmin[[j]]
    fg<-fg[[c(152:243)]]
    
    fg<- min(fg)
    p1[[j]]<- fg 
  }
  p1 <-do.call(mean, p1)
  
  # growing degree days above 0 year-round
  p2<-tmin
  for(j in 1:length(tmin)){
    
    fg<-(tmin[[j]]+tmax[[j]])/2
    #fg<-fg[[c(152:243)]]
    
    fg[fg<0]<-0
    
    p2[[j]]<- sum(fg )
  }
  p2 <-do.call(mean, p2)
  
  # SMD in winter and autumn
  # If the rainfall for a given month
  #exceeds the monthly PET, the water deficit for that month is
  #set to zero. For other months the water deficit is monthly PET
  #minus monthly rainfall. http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.457.2390&rep=rep1&type=pdf#page=8
  p3<-tmin
  for(j in 1:length(tmin)){
    
    fg<-pet[[j]]-precip[[j]]
    fg<-fg[[c(60:243)]]
    
    fg[fg<0]<-0
    fg<- sum(fg)
    p3[[j]]<- fg 
  }
  p3 <-do.call(mean, p3)
  
  # SMD in spring and summer
  p4<-tmin
  for(j in 1:length(tmin)){
    
    fg<-pet[[j]]-precip[[j]]
    fg<-fg[[c(1:59,244:365)]]
    
    fg[fg<0]<-0
    fg<- sum(fg)
    p4[[j]]<- fg 
  }
  p4 <-do.call(mean, p4)
  
  # spring frost days
  p5<-tmin
  for(j in 1:length(tmin)){
    
    fg<-tmin[[j]]
    fg<-fg[[c(244:334)]]
    fg<- sum(fg<0)
    p5[[j]]<- fg 
  }
  p5 <-do.call(mean, p5)
  
  ocro <-stack(p1,p2,p3,p4,p5)
  names(ocro)<-c("mint","gdd","smdwinter","smdsummer","springfrost")
  ocro<-raster::projectRaster(ocro,reprojraster)
  #ocro<-raster::resample(ocro, reprojraster,"ngb")
  ocro
}
