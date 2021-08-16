#' Crop suitability for 6 key crops based on rules developed by PFR
#'
#' This function models suitability for 6 crops under irrigated and non irrigated situations
#' @param tmax List ot stacked rasters of max daily temperature. Each stack 1 year
#' @param tmin List ot stacked rasters of min daily temperature. Each stack 1 year
#' @param precip List ot stacked rasters of daily precip. Each stack 1 year
#' @param soils Stack of soil parameters including NAMED LAYERS; drainclass, ph, prd, slope, paw, salinity, clay, 
#' @param croprules Table containing the crop rules. This is very specific and designed to make this function unusable without permission to use the dataset from PFR
#' @return Named raster stack with 10 layers. Not all crops are water limited, so they do not show irrigated/non difference
#' @export

nzes.pfrcrop<- function(tmax, 
         tmin,
         precip,
         soils, 
         croprules){
  
  
  
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
  
  tmax<-spltyr(tmax)
  tmin<-spltyr(tmin)
  #rad<-spltyr(rad)
  precip<-spltyr(precip)
  #tda<-spltyr(tda)
  
  
  
  #### 1. Chestnut ####
  cr<- croprules[croprules$crop=="chestnut",]
  unique(cr$parameter)
  
  # weather related first
  # annual precip
  p1<-precip
  for(j in 1:length(precip)){
    p1[[j]]<- sum(precip[[j]])  }
  p1 <-do.call(mean, p1)
  pann<-p1
  
  # winter chill hours 0-7 deg between april and august
  p2<-tmin
  tda<-(tmax[[j]] + tmin[[j]])/2
  for(j in 1:length(precip)){
    
    dcha<- (12 * (7-tmin[[j]]))/(tda-tmin[[j]])
    dchb<- (12 * (0-tmin[[j]]))/(tda-tmin[[j]])
    
    dcha[tmax[[j]] <= 7]<-24
    dchb[tmin[[j]] >=0]<-0
    
    dcha<- dcha-dchb
    dcha[dcha<0]<-0
    rm(dchb)
    
    # Old method. New method provided by Jing instead used
    # # tmin
    # fg<-tmin[[j]]
    # fg<-fg[[91:243]]
    # 
    # fg<- (fg >0 & fg < 7)*12
    # 
    # #tda
    # fg2<-tda[[j]]
    # fg2<-fg2[[91:243]]
    # 
    # fg2<- (fg2 >0 & fg2 < 7)*12
    # 
    # # ASSUME 12 hours a day are between min and mid, 12 hours are above mid
    # fg<-fg+fg2
    p2[[j]]<- sum(dcha )
  }
  p2 <-do.call(mean, p2)
  
  # frost less than -4 tmin between 15 sept and 31 jan
  p3<-tmin
  for(j in 1:length(precip)){
    
    fg<-tmin[[j]]
    fg<-fg[[c(1:31, 258:365)]]
    
    fg<- sum(fg < -4)
    p3[[j]]<- fg 
  }
  p3 <-do.call(mean, p3)
  
  # heat stress tmax >40 sep-nov
  p4<-tmax
  for(j in 1:length(precip)){
    
    fg<-tmax[[j]]
    fg<-fg[[c(244:34)]]
    
    fg<- sum(fg >40)
    p4[[j]]<- fg 
  }
  p4 <-do.call(mean, p4)
  
  # soil related and other
  #soils$drainclass
  #soils$ph
  #soils$prd
  #soils$slope
  
  #matg<-cbind(cr$rulemin, cr$rulemax, cr$sc)
  # do quantification
  classras<- function(rasg,
                      matg){
    
    matg[matg=="Infinite"]<-Inf
    matg<-as.matrix(matg)
    matg<-apply(matg,2, as.numeric)
    
    matg<-matg[!duplicated(matg[,1:2]),]
    
    rasg2<-raster::reclassify(rasg,
                      matg)
    rasg2
  }
  
  phras<- function(rasg,
                   matg){
    
    matg[matg=="Infinite"]<-Inf
    matg<-as.matrix(matg)
    matg<-apply(matg,2, as.numeric)
    
    matg<-matg[!duplicated(matg[,1:2]),]
    
    rasg2<-rasg
    rasg2[,]<- matg[,3][which.max(matg[,2]-matg[,1])]
    
    #remove and repeat
    matg<- matg[-which.max(matg[,2]-matg[,1]),]
    matg<-matg[order(matg[,2]-matg[,1],decreasing = T),]
    for(q in 1:length(matg[,1])){
      
      rasg2[rasg <  matg[q,2] & rasg >  matg[q,1]]<- matg[q,3]
      
    }
    rasg2
  }
  
  limitsoil<- raster::stack(classras(soils$drainclass,
                             cr[cr$parameter == "drainageclass",c(4,5,6)] ),
                    phras(soils$ph,
                          cr[cr$parameter == "ph",c(4,5,6)] ),
                    classras(soils$prd,
                             cr[cr$parameter == "prd",c(4,5,6)] ),
                    classras(soils$slope,
                             cr[cr$parameter == "slopedegrees",c(4,5,6)] ))
  names(limitsoil)<- c("drainclass","ph","prd","slope")
  limitsoil[limitsoil==0]<-4
  
  
  limitclim <- raster::stack(classras(p1,
                              cr[cr$parameter == "annualrainfall",c(4,5,6)] ),
                     classras(p2,
                              cr[cr$parameter == "winterchillhours0-7",c(4,5,6)] ),
                     classras(p3,
                              cr[cr$parameter == "frostprobability",c(4,5,6)] ),
                     classras(p4,
                              cr[cr$parameter == "heatstressprobability",c(4,5,6)] ))
  names(limitclim)<-c("precip","chill","frost","heatstress")
  limitclim[limitclim==0]<-4
  
  chestnutirri <- min(limitclim[[2:4]])
  chestnutnat <- min(limitclim)
  
  chestnutirri<-raster::resample(chestnutirri,limitsoil,"ngb")
  chestnutnat<-raster::resample(chestnutnat,limitsoil, "ngb")
  
  chestnutirri <- min(raster::stack(chestnutirri, limitsoil))
  chestnutnat <- min(raster::stack(chestnutnat, limitsoil))
  
  
  #### 2. Honey/ manuka ####
  
  cr<- croprules[croprules$crop=="manuka",]
  unique(cr$parameter)
  
  # weather related first
  # winter mean min temp
  p1<-tmin
  for(j in 1:length(precip)){
    
    fg<-tmin[[j]]
    fg<-fg[[173:265]]
    
    fg<- mean(fg)
    p1[[j]]<- fg 
  }
  p1 <-do.call(mean, p1)
  
  # summer mean tmax 15 oct to 31 jan
  p2<-tmax
  for(j in 1:length(precip)){
    
    fg<-tmax[[j]]
    fg<-fg[[c(1:31,288:365)]]
    
    fg<- mean(fg)
    p2[[j]]<- fg 
  }
  p2 <-do.call(mean, p2)
  
  # rain days more than 1 mm between 1 dec and 31 jan
  p3<-precip
  for(j in 1:length(precip)){
    
    fg<-precip[[j]]
    fg<-fg[[c(1:31, 355:365)]]
    
    fg<- sum(fg > 1)
    p3[[j]]<- fg 
  }
  p3 <-do.call(mean, p3)
  
  # soil related and other
  #soils$drainclass
  #soils$ph
  #soils$prd
  #soils$slope
  
  pg<-cr[cr$parameter == "pawclass",c(4,5,6)]
  
  
  limitsoil<- soils$paw
  limitsoil[,]<-pg$sc[4]
  limitsoil[soils$paw == as.numeric(pg$rulemin[1])]<- pg$sc[1]
  limitsoil[soils$paw == as.numeric(pg$rulemin[2])]<- pg$sc[2]
  limitsoil[soils$paw == as.numeric(pg$rulemin[3])]<- pg$sc[3]
  
  names(limitsoil)<- c("paw")
  
  
  limitclim <- raster::stack(classras(p1,
                              cr[cr$parameter == "wintermeanmint",c(4,5,6)] ),
                     classras(p2,
                              cr[cr$parameter == "summermeantmax",c(4,5,6)] ),
                     classras(p3,
                              cr[cr$parameter == "raindays",c(4,5,6)] ))
  names(limitclim)<-c("wintermeanmint","summermeantmax","raindays")
  limitclim[limitclim==0]<-4
  
  # No irrigation class for manuka because it does not need water
  # manukairri <- min(limitclim[[1:2]])
  manukanat <- min(limitclim)
  
  #manukairri<-resample(manukairri,limitsoil,"ngb")
  manukanat<-raster::resample(manukanat,limitsoil, "ngb")
  
  # manukairri <- min(stack(manukairri, limitsoil))
  manukanat <- min(raster::stack(manukanat, limitsoil))
  
  
  #### 3. Onions ####
  cr<- croprules[croprules$crop=="onions",]
  #unique(cr$parameter)
  
  # weather related first
  # spring frost
  p1<-tmin
  for(j in 1:length(precip)){
    
    fg<-tmin[[j]]
    fg<-fg[[305:334]]
    
    fg<- sum(fg <0 )
    p1[[j]]<- fg 
  }
  p1 <-do.call(mean, p1)
  # cut to 1 max to fit table
  p1[p1>1]<-1
  
  # heat at harvest
  p2<-tmax
  for(j in 1:length(precip)){
    
    fg<-tmax[[j]]
    fg<-fg[[1:60]]
    fg<-fg>31
    
    fg<- calc(fg, function(x){
      x2<-rle(x)
      x2<- x2$values[x2$lengths>3]
      x2<-x2[x2==1]
      length(x2)>0
    })
    
    fg<- sum(fg >0 & fg < 7)
    p2[[j]]<- fg 
  }
  p2 <-do.call(mean, p2)
  
  # rainfall during harvest period
  p3<-precip
  for(j in 1:length(precip)){
    
    fg<-precip[[j]]
    fg<-fg[[c(1:90)]]
    fg<-fg>10
    
    fg<- raster::calc(fg, function(x){
      x2<-rle(x)
      x2<- x2$values[x2$lengths>3]
      x2<-x2[x2==1]
      length(x2)>0
    })
    
    p3[[j]]<- fg 
  }
  p3 <-do.call(mean, p3)
  
  
  
  # soil related and other
  #soils$drainclass
  #soils$ph
  #soils$prd
  #soils$slop
  
  limitsoil<- raster::stack(classras(soils$drainclass,
                             cr[cr$parameter == "drainageclass",c(4,5,6)] ),
                    classras(soils$salinity,
                             cr[cr$parameter == "salinity",c(4,5,6)] ),
                    phras(soils$ph,
                          cr[cr$parameter == "ph",c(4,5,6)] ),
                    classras(soils$prd,
                             cr[cr$parameter == "prd",c(4,5,6)] ),
                    classras(soils$slope,
                             cr[cr$parameter == "slopedegrees",c(4,5,6)] ))
  names(limitsoil)<- c("drainclass","salinity", "ph","prd","slope")
  limitsoil[limitsoil==0]<-4
  limitsoil<-round(limitsoil,0)
  
  
  limitclim <- raster::stack(classras(p1,
                              cr[cr$parameter == "springfrostprob",c(4,5,6)] ),
                     classras(p2,
                              cr[cr$parameter == "heatharvestprob",c(4,5,6)] ),
                     classras(p3,
                              cr[cr$parameter == "wetharvestprob",c(4,5,6)] ))
  names(limitclim)<-c("springfrostprob","heatharvestprob","wetharvestprob")
  limitclim[limitclim==0]<-4
  limitclim<-round(limitclim,0)
  
  # no irrigation for onions
  #onionirri <- min(limitclim[[2:4]])
  onionnat <- min(limitclim)
  
  #onionirri<-resample(onionirri,limitsoil,"ngb")
  onionnat<-raster::resample(onionnat,limitsoil, "ngb")
  
  #onionirri <- min(stack(onionirri, limitsoil))
  onionnat <- min(stack(onionnat, limitsoil))
  
  
  #### 4. Peas ####
  cr<- croprules[croprules$crop=="peas",]
  #unique(cr$parameter)
  
  # weather related first
  # weather related first
  # annual precip
  p1<-pann
  
  # spring frosts
  p2<-tmin
  for(j in 1:length(precip)){
    
    fg<-tmin[[j]]
    fg<-fg[[244:334]]
    
    fg<- max(fg <0) # change to max to calculate binary, sum to calculate number of events
    p2[[j]]<- fg 
  }
  p2 <-do.call(mean, p2)
  
  # growing degree days september to april
  p3<-tda
  for(j in 1:length(precip)){
    
    fg<-tda
    fg<-fg[[c(1:120 , 244:365)]]
    fg<-fg-3
    
    p3[[j]]<- sum(fg )
  }
  p3 <-do.call(mean, p3)
  
  # heat stress flowering
  p4<-tmax
  for(j in 1:length(precip)){
    
    fg<-tmax[[j]]
    fg<-fg[[c(1:60 , 305:365)]]
    fg<-fg>30
    
    fg<- calc(fg, function(x){
      x2<-rle(x)
      x2<- x2$values[x2$lengths>2]
      x2<-x2[x2==1]
      length(x2)>0
    })
    
    p4[[j]]<- sum(fg )
  }
  p4 <-do.call(mean, p4)
  
  # soil related and other
  #soils$drainclass
  #soils$ph
  #soils$prd
  #soils$slop
  
  limitsoil<- raster::stack(classras(soils$drainclass,
                             cr[cr$parameter == "drainageclass",c(4,5,6)] ),
                    
                    phras(soils$ph,
                          cr[cr$parameter == "ph",c(4,5,6)] ),
                    
                    classras(soils$slope,
                             cr[cr$parameter == "slopedegrees",c(4,5,6)] ))
  names(limitsoil)<- c("drainclass","ph","slope")
  limitsoil[limitsoil==0]<-4
  
  limitclim <- raster::stack(classras(p1,
                              cr[cr$parameter == "precip",c(4,5,6)] ),
                     classras(p1,
                              cr[cr$parameter == "precip2",c(4,5,6)] ),
                     classras(p2,
                              cr[cr$parameter == "springfrostprob",c(4,5,6)] ),
                     classras(p3,
                              cr[cr$parameter == "growingdegday",c(4,5,6)] ),
                     classras(p4,
                              cr[cr$parameter == "heatstressprobability",c(4,5,6)] ))
  names(limitclim)<-c("precip","precip2", "springfrostprob","gdd","heatstressprob")
  limitclim[limitclim==0]<-4
  
  
  
  peairri <- min(limitclim[[2:5]])# irrigation does not account for min precip, but ca nstipp be overwatered
  peanat <- min(limitclim)
  
  peairri<-raster::resample(peairri,limitsoil,"ngb")
  peanat<-raster::resample(peanat,limitsoil, "ngb")
  
  peairri <- min(raster::stack(peairri, limitsoil))
  peanat <- min(raster::stack(peanat, limitsoil))
  
  
  
  #### 5. Potatoes ####
  cr<- croprules[croprules$crop=="potatoes",]
  #unique(cr$parameter)
  
  # weather related first
  # annual precip
  p1<-pann
  
  # low temp nov to feb
  p2<-tmin
  for(j in 1:length(precip)){
    
    fg<-tmin[[j]]
    fg<-fg[[c(1,60, 305, 365)]]
    
    fg<- max(fg <0) # change to max to calculate binary, sum to calculate number of events
    p2[[j]]<- fg 
  }
  p2 <-do.call(mean, p2)
  
  # low temp nov to feb
  p3<-tmax
  for(j in 1:length(precip)){
    
    fg<-tmax[[j]]
    fg<-fg[[c(1,60, 305, 365)]]
    
    fg<- max(fg >20) # change to max to calculate binary, sum to calculate number of events
    p2[[j]]<- fg 
  }
  p3 <-do.call(mean, p3)
  
  # soil related and other
  #soils$drainclass
  #soils$ph
  #soils$prd
  #soils$slop
  
  limitsoil<- raster::stack(classras(soils$clay,
                             cr[cr$parameter == "soilclaycontent",c(4,5,6)] ),
                    
                    classras(soils$salinity,
                             cr[cr$parameter == "salinity",c(4,5,6)] ),
                    classras(soils$ph,
                             cr[cr$parameter == "ph",c(4,5,6)] ), #even though ph, it is working in classras way
                    
                    classras(soils$prd,
                             cr[cr$parameter == "prd",c(4,5,6)] ),
                    classras(soils$drainclass,
                             cr[cr$parameter == "drainageclass",c(4,5,6)] ))
  names(limitsoil)<- c("soilclay", "salinity", "ph","prd", "drainclass")
  limitsoil[limitsoil==0]<-4
  
  
  
  limitclim <- raster::stack(classras(p1,
                              cr[cr$parameter == "precip",c(4,5,6)] ),
                     classras(p1,
                              cr[cr$parameter == "precip2",c(4,5,6)] ),
                     classras(p2,
                              cr[cr$parameter == "lowtempprob",c(4,5,6)] ),
                     classras(p3,
                              cr[cr$parameter == "hightempprob",c(4,5,6)] )
  )
  names(limitclim)<-c("precip","precip2", "lowtprob","hightprob")
  limitclim[limitclim==0]<-4
  
  
  
  potatoirri <- min(limitclim[[2:4]])
  potatonat <- min(limitclim)
  
  potatoirri<-raster::resample(potatoirri,limitsoil,"ngb")
  potatonat<-raster::resample(potatonat,limitsoil, "ngb")
  
  potatoirri <- min(raster::stack(potatoirri, limitsoil))
  potatonat <- min(raster::stack(potatonat, limitsoil))
  
  
  #### 6. Truffles ####
  cr<- croprules[croprules$crop=="truffles",]
  #unique(cr$parameter)
  
  # weather related first
  # annual precip
  p1<-pann
  
  # average temp jan to march
  p2<-tda
  for(j in 1:length(precip)){
    
    fg<-tda
    fg<-fg[[c(1,89)]]
    
    p2[[j]]<- mean(fg )
  }
  p2 <-do.call(mean, p2)
  
  # # sunshine hours per year WAS NOT USED BY PFR, DESPITE BEING IN THE REPORT
  # p3<-rad
  # for(j in 1:length(precip)){
  #   
  #   fg<-rad[[j]]
  #    p2[[j]]<- fg 
  # }
  # p3 <-do.call(mean, p3)
  
  limitsoil<- raster::stack(classras(soils$drainclass,
                             cr[cr$parameter == "drainageclass",c(4,5,6)] ),
                    phras(soils$ph,
                          cr[cr$parameter == "ph",c(4,5,6)] ),
                    
                    classras(soils$slope,
                             cr[cr$parameter == "slopedegrees",c(4,5,6)] ))
  names(limitsoil)<- c("drainage", "ph","slope")
  limitsoil[limitsoil==0]<-4
  
  
  
  limitclim <- raster::stack(classras(p1,
                              cr[cr$parameter == "precip",c(4,5,6)] ),
                     classras(p1,
                              cr[cr$parameter == "precip2",c(4,5,6)] ),
                     classras(p2,
                              cr[cr$parameter == "avtempautumn",c(4,5,6)]  )
  )
  names(limitclim)<-c("precip","precip2", "avtempautm")
  limitclim[limitclim==0]<-4
  
  
  
  trufflesirri <- limitclim[[3]]# irrigation does not account for min precip, but ca nstipp be overwatered
  trufflesnat <- min(limitclim)
  
  trufflesirri<-raster::resample(trufflesirri,limitsoil,"ngb")
  trufflesnat<-raster::resample(trufflesnat,limitsoil, "ngb")
  
  trufflesirri <- min(raster::stack(trufflesirri, limitsoil))
  trufflesnat <- min(raster::stack(trufflesnat, limitsoil))
  
  
  #### 7. Wrap outputs ####
  ocro<-  raster::stack(chestnutirri,
                chestnutnat,
                # manukairri,
                manukanat,
                #onionirri,
                onionnat,
                peairri,
                peanat,
                potatoirri,
                potatonat,
                trufflesirri,
                trufflesnat)
  names(ocro)<-c("chestnutI", "chestnut", "manuka", "onion","peaI","pea","potatoI","potato","truffleI","truffle")
  ocro
}





