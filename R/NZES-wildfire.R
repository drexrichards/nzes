#' Wildfire risk indices based on weather
#'
#' This function models wildfire risk based on climate
#' @param niwadd Folder for source NIWA netcdfs
#' @param region Cropping region as spatial polygons in wgs84
#' @return Named raster stack with 3 layers. Gives 3 fire indices - BUI, DC, DMC
#' @export

nzes.wildfire<- function(niwadd,
                        region){
  
  #### 1. Import datasets ####
  
  require(raster)
  
  fl<-list.files(niwadd)
  
  # make le vector (day length)
  le<- c(rep(12.4,31 ),
         rep(13.9,29 ),
         rep(12.4,31 ),
         rep(10.9, 30),
         rep(9.4,31 ),
         rep(8,30 ),
         rep(7,31 ),
         rep(6, 31 ),
         rep(6.5,30 ),
         rep(7.5,31 ),
         rep(9,30 ),
         rep(12.8,31 ))
  le<-le[1:59]
  
  # make le vector (day length)
  lf<- c(rep(6.4,31 ),
         rep(5,29 ),
         rep(2.4,31 ),
         rep(0.4, 30),
         rep(-1.6,31 ),
         rep(-1.6,30 ),
         rep(-1.6,31 ),
         rep(-1.6, 31 ),
         rep(-1.6,30 ),
         rep(0.9,31 ),
         rep(3.8,30 ),
         rep(5.8,31 ))
  lf<-lf[1:59]
  
  tmax2<- raster::brick(paste0(niwadd, fl[grep("Max", fl)]))
  tmin2<- raster::brick(paste0(niwadd, fl[grep("Min", fl)]))
  precip2<- raster::brick(paste0(niwadd, fl[grep("Precip", fl)]))
  humid2<- raster::brick(paste0(niwadd, fl[grep("RelHum", fl)]))
  pet2<- raster::brick(paste0(niwadd, fl[grep("PE_VCSN", fl)]))
  
  pet2<-raster::crop(pet2, region)
  pet2<-raster::mask(pet2, region)
  
  humid2<-raster::crop(humid2, region)
  humid2<-raster::mask(humid2, region)
  
  precip2<-raster::crop(precip2, region)
  precip2<-raster::mask(precip2, region)
  
  tmax2<-raster::crop(tmax2, region)
  tmax2<-raster::mask(tmax2, region)
  tmax2<- tmax2 + -272.15
  
  tmin2<-raster::crop(tmin2, region)
  tmin2<-raster::mask(tmin2, region)
  tmin2<-tmin2+ -272.15
  
  # split into years
  spltyr<- function(stk){
    
    yi<- rbind(seq(from = 1, to = raster::nlayers(stk), length.out=  ceiling(raster::nlayers(stk)/365)),
               seq(from = 1, to = raster::nlayers(stk), length.out=  ceiling(raster::nlayers(stk)/365))-1)
    yi[2, 1:(ncol(yi)-1)] <-  yi[2, 2:ncol(yi)]
    yi<-yi[,1:(ncol(yi)-1)]
    
    stk2<-list(ncol(yi))
    
    for(i in 1:ncol(yi)){
      stk2[[i]]<- stk[[yi[1,i] : yi[2,i] ]]
      stk2[[i]]<- stk2[[i]][[1:59]]
    }
    stk2
  }
  
  tmax<-spltyr(tmax2)
  tmin<-spltyr(tmin2)
  humid<-spltyr(humid2)
  precip<-spltyr(precip2)
  pet<-spltyr(pet2)
  #tda<-spltyr(tda)
  
 
  #### 2. Calculate DC factor ####
  # DC factor
  DC<-tmin[[1]][[1:length(tmin)]]
  DMC<-tmin[[1]][[1:length(tmin)]]
  BUI<-tmin[[1]][[1:length(tmin)]]
  
  for(j in 1:length(precip)){
    
    # calculate effective rainfall if above 2.8 mm of rain
    pd<- (0.83 * precip[[j]]) -1.27
    pd[precip[[j]] < 2.8 ]<-2.8
    
    # calculate moisture equivalent of previous day (for all days)
    # set up first day
    dc <- pd
    dc[[1]][,]<-15 #starting value must be 15
    
    q0 <- dc[[1]]
    q1<-q0
    
    # then loop through days
    for(y in 2:dim(dc)[3]){
      q1<- (800 * exp(1)^(-q0/400))+(3.937*pd[[y]])
      
      # make space to save potential dcrt if it rains
      q1 <- 400 * log((800/q1))
      q1[ q1 <0]<-0
      
      # calculate homemade pet
      tgo <- tmax[[j]][[y]]
      tgo [tgo < -2.8]<- -2.8
      pet2<- 0.36 * (tgo + 2.8) + lf[y]
      
      # calculate two values depending on whether precip is less than 2.8
      #more than 2.8
      dctR <- q1 + (0.5*pet[[j]][[y]])
      # less than 2.8
      dctN <- q0 + (0.5*pet[[j]][[y]])
      
      # select no rain data
      dctR[precip[[j]][y] < 2.8 ]<-dctN[precip[[j]][y]< 2.8]
      
      dc[[y]]  <-dctR
      q0<- dctR
    }
    
    #### 3. DMC ####
    # Now solve for DMC
    # calculate effective rainfall if above 1.5 mm of rain
    pd<- (0.92 * precip[[j]]) -1.27
    pd[precip[[j]] < 1.5 ]<-1.5
    
    dmc <- pd
    dmc[[1]][,]<-6 #starting value must be 6
    
    q0 <- dmc[[1]]
    q1<-q0
    
    # then loop through days
    for(y in 2:dim(dmc)[3]){
      q1 <- 20 + (exp(1)^(5.6348 - (q0/ 43.43)))
      
      # 
     bpot <- raster::stack((100/ (0.5 + 0.3 * q0)),
                   (14-1.3 * log(q0)),
                   (6.2 * log(q0)-17.2))
     # select b value based on dmc previous value
     bact<- bpot[[1]]
     bact[q0 >33 ] <- bpot[[2]][q0 >33 ]
     bact[q0 >65 ] <- bpot[[3]][q0 >65 ]
     
     # calculate mrt
     q1 <- q1 + ((1000 *pd[[y]])/
                              (48.77 + bact *pd[[y]]))
     
     # convert to dmc after rain
     q1 <- 244.72-43.43 * log(q1 -20 )
     q1 [q1 <0]<-0
     
      # calculate log drying rate
     tgo <- tmax[[j]][[y]]
     tgo [tgo < -1.1]<- -1.1
     
      kv <- 1.894 * (tgo + 1.1) * (100 - humid[[j]][[y]]) *
        (le[y] * 10^-6)
        
      
      # calculate two values depending on whether precip is less than 2.8
      #more than 1.5
      dctR <- q1 + 100 *kv
      # less than 1.5
      dctN <- q0 + 100 *kv
      
      # select no rain data
      dctR[precip[[j]][y] < 1.5 ]<-dctN[precip[[j]][y]< 1.5]
      
      dmc[[y]]  <-dctR
      q0<- dctR
    }
    
    #### 4. BUI ####
    bui <- 0.8 *((dmc * dc) / dmc +0.4*dc )
    buiL<- dmc - ( 1 - ((0.8*dc) /(dmc+0.4 * dc) ))
    buiL <- buiL * ((0.92 + (0.0114 * dmc)^1.7) )
    for(y in 1:dim(bui)[3]){
      bui[[y]][dmc[[y]] > (0.4 *dc[[y]])]<- buiL[[y]][dmc[[y]] > (0.4 *dc[[y]])]
    }
   
    bui<-bui[[2:dim(bui)[3]]]
    dc<-dc[[2:dim(dc)[3]]]
    dmc<-dmc[[2:dim(dmc)[3]]]
    
    #### 5. Select highest total BUI day per year ####
    # BUI[[j]]<- bui[[which.max(raster::cellStats(bui,sum))]]
    # DC[[j]]<- dc[[which.max(raster::cellStats(dc,sum))]]
    # DMC[[j]]<- dmc[[which.max(raster::cellStats(dmc,sum))]]
    BUI[[j]]<- calc(bui, function(x){quantile(x,0.95,na.rm=T)})
    DC[[j]]<- calc(dc, function(x){quantile(x,0.95,na.rm=T)})
    DMC[[j]]<- calc(dmc, function(x){quantile(x,0.95,na.rm=T)})
   
  
  }
  DC <-max(DC)
  DMC <-max(DMC)
  BUI<-max(BUI)
  

  #### 7. Wrap outputs ####
  ocro<-  raster::stack(BUI, DC, DMC)
  names(ocro)<-c("BUI","DC","DMC")
  ocro
}





