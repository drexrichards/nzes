#' Model suitability for a crop based on climate and other parameters
#'
#' This function models suitability for crops using the rules in ECOCROP
#' @param crop Scientific name of the crop for searching ECOCROP 
#' @param tk Raster of min temperature 
#' @param tn Raster of min average temperature 
#' @param tx Raster of mac average temperature 
#' @param pr Stack of monthly precipitation
#' @param soils Stack of soils with parameters "ph", "salinity", "prd"(depth) from the NZ soil dataset
#' @return Raster indicating suitability (binary) for the crop or not
#' @export

nzes.crop<- function(crop,
                     tk,
                     tn,
                     tx,
                     pr,
                     soils){
  
  cropD<- dismo::getCrop(crop)
  cropT <- ECOcrops[ECOcrops$SCIENTNAME == crop,]
  
  # Make custom ecocrop analysis
  # binary  - suitable or not
  
  # length of growing season
  g1<- ceiling(cropT$GMIN/31) #number of months needed
  #cropT$GMAX
  
  # min and max temperature based on average
  t1<- tn > cropT$TMIN &
    tx < cropT$TMAX
  
  # rainfall sums over growing season
  
  # custom rolling sum function
  rollsum2<- function(x){
    
    x2 <- c(x, x[1:g1])
    
    x2<- zoo::rollsum(x2, g1)[1:(length(zoo::rollsum(x2, g1))-1)]
    
    (x2 >cropT$RMIN & x2< cropT$RMAX)
  }
  
  #Sys.time()
  pr2 <- raster::calc(pr, rollsum2)
  #Sys.time()
  
  # If too slow, use a simpler metric 
  # 90% of the monthly rainfall required
  #sx<- (cropT$RMIN/g1)*0.9
  #p1<- pr> sx
  
  # kill temp
  t2<- (tk > cropT$KTMP)
  
  # PH
  s1<- soils$ph > cropT$PHMIN & soils$ph < cropT$PHMAX
  
  # Salinity class
  # use the conversion from the lris classes
  sx<- 0
  sx[cropT$SALR == "H"] <- 0.69
  sx[cropT$SALR == "l"] <- 0.14
  sx[cropT$SALR == "L"] <- 0.14
  sx[cropT$SALR == "M"] <- 0.29
  
  s2<- soils$salinity < sx
  
  # depth
  #cropT$DEP
  #prd comes from modal
  sx<- 0
  sx[toupper(cropT$DEPR) == "S"] <- mean(c(0.2))
  sx[toupper(cropT$DEPR) == "M"] <- mean(c(0.5))
  sx[toupper(cropT$DEPR) == "D"] <- mean(c(1.5))
  
  s3<- soils$prd > sx
  
  # drainage - leave for now
  #cropT$DRAR
  #cropT$DRA
  
  
  # overall calculation
  #temporal
  o1<- (t1+
          t2+
          pr2)==3
  #collapse to months
  
  #irrigates
  o1i <- (t1+t2)==2
  
  o1<- raster::calc(o1,
            function(x,na.rm=T) {
              x2<- c(x, x[1:g1])
              seqe <- rle(x2)
              g<- sum(seqe$lengths >= g1 & seqe$values == 1)
              g>0
            })
  
  o1i<- raster::calc(o1i,
             function(x,na.rm=T) {
               x2<- c(x, x[1:g1])
               seqe <- rle(x2)
               g<- sum(seqe$lengths >= g1 & seqe$values == 1)
               g>0
             })
  
  #add soil only factors
  o2<- (s1+s2+s3)==3
  
  # convert
  o1 <- raster::resample(o1, o2,"ngb"  )
  o1i <- raster::resample(o1i, o2,"ngb"  )
  
  o3<- (o1+o2) ==2
  o3i<- (o1i+o2) ==2
  
  raster::stack(o3, o3i)
  
}
