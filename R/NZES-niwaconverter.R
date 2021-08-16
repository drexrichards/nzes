#' Converter to move around files taken from NIWA climate change projetions and crop to an area of interest
#'
#' This function models soil erosion ecosystem services using the Guerra et al. approach but using NZUSLE as the model
#' @param niwadd Folder directory for raw NIWA inputs including a max temp, mintemp, precip, and solar rad netcdf file
#' @param newdd Folder directory for where to save raster
#' @param foldname Name for new folder
#' @param croparea SpatialPolygon areal extent in wgs84 to crop extent
#' @return Folder will be filled with 4 new rasters
#' @export

niwaconverter<-function(niwadd, 
                        newdd,
                        foldname,
                        croparea){
  
  fl<-list.files(niwadd)
  
  # Netcdf dataset
  #tmax<- nc_open("C:/Users/RichardsD/Documents/Offline data/NIWA climate offline/NorRpast-2005/MaxTempCorr_VCSN_NorESM1-M_2001-2005_RCPpast.nc")
  tmax<- brick(paste0(niwadd, fl[grep("Max", fl)]))
  tmin<- brick(paste0(niwadd, fl[grep("Min", fl)]))
  rad<- brick(paste0(niwadd, fl[grep("Rad", fl)]))
  precip<- brick(paste0(niwadd, fl[grep("Precip", fl)]))
  pet<- brick(paste0(niwadd, fl[grep("PE_VCSN", fl)]))
  
  # temp units are in kelvin
  # precip in mm
  # radiation in w per m2
  
  precip<-crop(precip, croparea)
  precip<-mask(precip, croparea)
  
  tmax<-crop(tmax, croparea)
  tmax<-mask(tmax, croparea)
  tmax<- tmax + -272.15
  
  tmin<-crop(tmin, croparea)
  tmin<-mask(tmin, croparea)
  tmin<-tmin+ -272.15
  
  rad<-crop(rad, croparea)
  rad<-mask(rad, croparea)
  
  pet<-crop(pet, croparea)
  pet<-mask(pet, croparea)
  
  # creat folder and save out
  dir.create(paste0(newdd,foldname))
  writeRaster(tmax,
              paste0(newdd,foldname,"/tmax.tif"), overwrite =T)
  writeRaster(tmin,
              paste0(newdd,foldname,"/tmin.tif"), overwrite =T)
  writeRaster(rad,
              paste0(newdd,foldname,"/rad.tif"), overwrite =T)
  writeRaster(precip,
              paste0(newdd,foldname,"/precip.tif"), overwrite =T)
  writeRaster(pet,
              paste0(newdd,foldname,"/pet.tif"), overwrite =T)
  
}