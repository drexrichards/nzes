#' Wildfire fuel map based on land cover and veg height and BUI data
#'
#' This function models fuel based on landcover, vegheight, and BUI (for forest)
#' @param BUI Raster of BUI from nzes.wildfire
#' @param lcm Land/ water cover map, including vegetation categories. 
#' @param vegheight Raster of vegetation height in m
#' @param irri Raster of irrigated area in binary
#' @param grazing Raster of grazed area in binary
#' @param fueltable Lookup table of fuel loads
#' @param fuelconverter Lookup table to reclasify lcdb to fuel types in fueltable
#' @return Raster indicating fuel load
#' @export

nzes.fuel<- function(BUI,
                     lcm,
                     vegheight,
                     irri,
                     grazing,
                     fueltable,
                     fuelconverter){
  
  #### 1. Reclassify fuels index ####
  fu <- raster::reclassify(lcm,fuelconverter)
  
  # Add irigated and grazed values
  fu[lcm %in% c(40,41) & grazing ==1]<- 6.1
  fu[lcm %in% c(43,44) & grazing ==0]<- 8.1
  # make irrigated have a small fuel load
  fu[irri == 1]<-1
  vegheight[irri==1]<-0.2
  
  fui<- unique(fu)
  
  BUI<-raster::resample(BUI,lcm, "ngb")
  
  
  #### 2. For each unique type, go through and classify ####
  fuout <-list(length(fui))
  for(i in 1:length(fui)){
    
    # find if height or bui
    tte<- fueltable[fueltable$landcoverid == fui[i],]
    if(tte$unit[1] == "height"){
      cre <- vegheight
      cref<- (fu == fui[i])
      cref[cref==0]<-NA
      cre<- raster::mask(cre, cref, inverse = F, maskvalue = NA)
    } else { # make it BUI
      cre <- BUI
      cref<- (fu == fui[i])
      cref[cref==0]<-NA
      cre<- raster::mask(cre, cref, inverse = F, maskvalue = NA)
    }
    # reclassify cre into the correct ordinal value and then correct fuel value
    cre<-raster::reclassify(cre,
    cbind(c(0, tte$x, 1000)[1:(length(c(0, tte$x, 1000))-1)],
          c(0, tte$x, 1000)[2:(length(c(0, tte$x, 1000)))],
          c(0,tte$fuel)))
    fuout[[i]]<-cre
    
  }
  fuout<-stack(fuout)
  fuout<-max(fuout, na.rm=T)

  #### 3. Wrap outputs ####
  fuout
}





