#' Plot a nice map based on an lcdb class raster
#'
#' This function is a helper to get land cover maps for different time points (fixed)
#' @param lcm Raster using lcdb classes
#' @param lcdblookup Lookup table of lcdb classes to colours and names
#' @return Plots a tmap object 
#' @export

nzes.lcdb.mapper <- function(lcm, lcdblookup
){ 
  # This code is for plotting nicely using a colour scheme
   outm2LCM<-raster::reclassify(lcm,
                        cbind(lcdblookup$lcdbnum,
                              as.numeric(as.factor(lcdblookup$htmlcolour))))

   pl<- cbind(as.numeric(as.factor(lcdblookup$htmlcolour)),lcdblookup$htmlcolour, lcdblookup$legendname)
   pl<- pl[!duplicated(pl[,2]),]

   pl<-as.data.frame(na.omit(pl))
   pl<-pl[order(as.numeric(pl[,1])),]

   oe<- raster::getValues(outm2LCM)
   oe<- c(oe[,1],oe[,2],oe[,3],oe[,4],oe[,5],oe[,6])
   oe<-unique(oe)

   pl1<-pl[pl$V1 %in% as.character(oe),]
   
   s3<-list(outm2LCM, pl1)
   
   tmap::tm_shape(s3[[1]]) +
     tmap::tm_raster(c("1","20","40","60","80","100"),
                     palette = s3[[2]][,2],labels = s3[[2]][,3],title="Land cover", style="cat" )+
     tmap::tm_layout(legend.outside = TRUE,
                     legend.outside.position = "right",
                     legend.position = c(0.25, 0.25),
                     legend.title.size = 1,
                     legend.text.size = 0.6)
   
}