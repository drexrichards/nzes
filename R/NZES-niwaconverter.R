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
  tmax<- raster::brick(paste0(niwadd, fl[grep("Max", fl)]))
  tmin<- raster::brick(paste0(niwadd, fl[grep("Min", fl)]))
  rad<- raster::brick(paste0(niwadd, fl[grep("Rad", fl)]))
  precip<- raster::brick(paste0(niwadd, fl[grep("Precip", fl)]))
  pet<- raster::brick(paste0(niwadd, fl[grep("PE_VCSN", fl)]))
  
  # temp units are in kelvin
  # precip in mm
  # radiation in w per m2
  
  precip<-raster::crop(precip, croparea)
  precip<-raster::mask(precip, croparea)
  
  tmax<-raster::crop(tmax, croparea)
  tmax<-raster::mask(tmax, croparea)
  tmax<- tmax + -272.15
  
  tmin<-raster::crop(tmin, croparea)
  tmin<-raster::mask(tmin, croparea)
  tmin<-tmin+ -272.15
  
  rad<-raster::crop(rad, croparea)
  rad<-raster::mask(rad, croparea)
  
  pet<-raster::crop(pet, croparea)
  pet<-raster::mask(pet, croparea)
  
  # creat folder and save out
  dir.create(paste0(newdd,foldname))
  raster::writeRaster(tmax,
              paste0(newdd,foldname,"/tmax.nc"), overwrite =T,varname="time2", format="CDF")
  raster::writeRaster(tmin,
              paste0(newdd,foldname,"/tmin.nc"), overwrite =T,varname="time2", format="CDF")
  raster::writeRaster(rad,
              paste0(newdd,foldname,"/rad.nc"), overwrite =T,varname="time2", format="CDF")
  raster::writeRaster(precip,
              paste0(newdd,foldname,"/precip.nc"), overwrite =T,varname="time2", format="CDF")
  raster::writeRaster(pet,
              paste0(newdd,foldname,"/pet.nc"), overwrite =T,varname="time2", format="CDF")
  
}