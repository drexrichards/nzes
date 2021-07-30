#' Model pollination and honey bee potential hive density
#'
#' This function models floral resources for honey bee pollinators, following Ausseil et al. 2018 Ecological Applications
#' @param lcm Raster of land cover classes as integer
#' @param lcclasses Vector of land cover lookup classes in same order as lookup tables below
#' @param pollenlt Lookup table of pollen production kgha-1 per 12 months 
#' @param nectarlt Lookup table of nectar production kgha-1 per 12 months 
#' @param pollendemand Vector of pollen demand per month kg. Must have one named value of "September"
#' @param pollenrange Vector of pollen travel distance per month km
#' @param nectarrange Vector of nectar travel distance per month km
#' @param nectardemand Annual nectar requirement kg
#' @return Raster stack indicating the annual carrying capacity in colonies per pixel. Layer 1 is the more conservative estimate using all months  pollen to constrain. Layer 2 is using September as per Ausseil et al
#' @export

nzes.pollination<- function(lcm,
                            lcclasses,
                            pollenlt,
                            nectarlt,
                            pollendemand,
                            pollenrange,
                            nectarrange,
                            nectardemand = 200){
  
  # reclassify rasters
  pr <- lcm
  pr[,]<-0
  nr<-pr
  for(i in 1:12){
    plt<- cbind(lcclasses,
                pollenlt[,i])
    nlt<- cbind(lcclasses,
                nectarlt[,i])
    
    if(i ==1){
      pr<-raster::reclassify(lcm, plt)
      nr <- raster::reclassify(lcm, plt)
    }else{
      pr<-raster::stack(pr,
                        raster::reclassify(lcm, plt))
      nr<-raster::stack(nr,
                        raster::reclassify(lcm, nlt))
    }
  }
  
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
  
  # Get size of one pixel in km
  ar<-raster::area(lcm)
  ar<-sqrt(raster::cellStats(ar,mean))
  
  pr<-pr* ((ar^2 )/0.01)
  nr<-nr* ((ar^2 )/0.01)
  
  # Do loop for each month
  for( i in 1:12){
    
    #calculate number of pixels in focal buffer
    pdist<- ceiling(pollenrange[i]/ ar)
    ndist<- ceiling(nectarrange[i]/ ar)
    
    pdist<- (floor(pdist/2)*2)+1
    ndist<- (floor(ndist/2)*2)+1
    
    pdist<-as.numeric(pdist)
    ndist<-as.numeric(ndist)
    
    # if 1 or less, no need to focal
    if(pdist<2){
      pr[[i]]<-pr[[i]]
    }else{
      pdist<- drawImage(matrix(0,pdist, pdist), ceiling(pdist/2), floor(pdist/2))
      pr[[i]]<- raster::focal(pr[[i]],
                              pdist,
                              sum,
                              na.rm=TRUE,
                              pad = TRUE,
                              padValue = NA)
    }
    
    # if 1 or less, no need to focal
    if(ndist<2){
      nr[[i]]<-nr[[i]]
    }else{
      ndist<- drawImage(matrix(0,ndist, ndist), ceiling(ndist/2), floor(ndist/2))
      nr[[i]]<- raster::focal(nr[[i]],
                              ndist,
                              sum,
                              na.rm=TRUE,
                              pad = TRUE,
                              padValue = NA)
    }
    
    # Nectar is an annual threshold, but pollen needs to be monthly
    pr[[i]] <- pr[[i]]/as.numeric(pollendemand[i])
    
  }
  
  # find limiting carrying capacities
  nr<-sum(nr)/nectardemand
  prsept<- pr[[which(names(pollendemand) =="September")]]/
    as.numeric(pollendemand$September)
  pr<- min(pr)
  
  ra<- min(raster::stack(pr,nr))
  rb<- min(raster::stack(prsept,nr))
  # send output raster
  stack(ra,rb)
}
