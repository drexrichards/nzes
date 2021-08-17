#' Farmer innovation from Brown et al 2016 LUP
#'
#' This function models suitability for 6 crops under irrigated and non irrigated situations
#' @param agribase Spatial polygons dataframe from agribase including parameters
#' @param region Vector aligned with agribase including names of regions for all farms
#' @param srdmlookup Lookup table provided in package
#' @param luc Land cover map (LUCAS) rasterised as an integer version e.g. 71000, etc.
#' @return Raster layer with values between 0 and 10 indicating innovation index
#' @export

nzes.farminnovation<- function(agribase, 
         region,
         srdmlookup,
         luc
         ){
  # find land uses
  luc2<-luc
  luc2[,]<-0
  
  luc2[luc>70500 & luc < 75]<-1 # woodland and forestry
  luc2[luc ==75502] <- 2 #higraz
  luc2[luc ==75503] <- 3 #hiungraz
  luc2[luc ==76502] <- 4 #lograz
  luc2[luc ==76503] <- 5 #loungraz
  luc2[luc ==77000] <- 6 #crop1
  luc2[luc ==78000] <- 7 #crop2
  
  agribase$luc <- raster::extract(luc2, agribase,fun =function(x,...){length(na.omit(unique(x)))})
  rm(luc2)
  agribase$luc[,1][agribase$luc==0]<-1
  agribase$luc[,1][is.na(agribase$luc)]<-1
  # Generate probabilities for each farm
  # Male
  tg<- srdmlookup[srdmlookup$indicator == "sex",]
  tg<-tg[match(region, tg$region),]
  agribase$male<-rbinom(length(tg[,1]), 1, tg$fraction)
  
  # Maori
  tg<- srdmlookup[srdmlookup$indicator == "maori",]
  tg<-tg[match(region, tg$region),]
  agribase$maori<-rbinom(length(tg[,1]), 1, tg$fraction/100)
  
  # Financially robust
  tg<- srdmlookup[srdmlookup$indicator == "breakeven",]
  tg<-tg[match(region, tg$region),]
  agribase$finrob<-rbinom(length(tg[,1]), 1, tg$fraction/100)
  
  # Post sec and degree are linked such that an individual cannot be both
  # Post sec
  tg<- srdmlookup[srdmlookup$indicator == "postsec",]
  tg<-tg[match(region, tg$region),]
  agribase$postsec<-rbinom(length(tg[,1]), 1, tg$fraction/100)
  # Degree
  tg<- srdmlookup[srdmlookup$indicator == "degree",]
  tg<-tg[match(region, tg$region),]
  tg$fraction [ agribase$postsec == 1]<-0
  agribase$degree<-rbinom(length(tg[,1]), 1, tg$fraction/100)
  
  # effective area logged
  agribase$earealog<- log(agribase$size_ha)
  
  # owner/ family partner
  agribase$owner<-rbinom(length(agribase),1,0.54)
  
  # Age
  agribase$age<-NA
  tg<- srdmlookup[srdmlookup$indicator == "age",]
  for(u in 1:length(agribase)){
    tg2<- tg[tg$region == region[u],]
   agribase$age[u]<- as.numeric(sample(tg2$value,1,prob= tg2$fraction))
  }
  
  # Experience
  agribase$experience<-NA
  tg<- srdmlookup[srdmlookup$indicator == "experience",]
  for(u in 1:length(agribase)){
    tg2<- tg[tg$region == region[u],]
    agribase$experience[u]<- as.numeric(sample(tg2$value,1,prob= tg2$fraction))
  }
  
  
  # Estimate from equation in Appendix 1 - Model3
  # leave out non-focal effects including region
  agribase$mod3ols<- (agribase$male *0.525) +
  (agribase$maori * -0.094) +
  (agribase$age * -0.010) +
  (agribase$postsec * 0.185) +
  (agribase$degree * 0.228) +
  (agribase$earealog * 0.241) +
  (agribase$luc[,1] * 0.178) +
  (agribase$owner * 0.021) +
  (agribase$experience * -0.004) +
  (agribase$finrob * 0.118) +
  3.102 # constant
  
  vm2<-sf::st_as_sf(agribase)
  vm2<-fasterize::fasterize(sf=vm2, 
                 raster=luc,
                 field = "mod3ols",
                 fun="first")
  vm2<-raster::mask(vm2, luc)
  vm2
  
}





