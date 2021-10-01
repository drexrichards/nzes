#' Extract temporal land cover maps of dominant vegetation per pixel
#'
#' This function is a helper to get land cover maps for different time points (fixed)
#' @param fp Text string folder for overall FATE version e.g. mac_test6
#' @param sc Text string simulation version e.g. V3
#' @param lcdblookup Lookup table of conversion codes for ldcb to pfg
#' @return Named raster stack with 6 layers for different years. 
#' @export

# Make plotting function for neat habitat map ####
#fp<- "C:/Users/RichardsD/OneDrive - MWLR/Papers/Dynamic veg model NZ/Mackenzie/mac_test6/RESULTS/SIMUL_V3/ABUND_REL_perPFG_allStrata/"
#fp<- "C://Users/RichardsD/OneDrive - MWLR/Papers/Dynamic veg model NZ/Mackenzie/mac_test6/"
#sc<-"V18"
#lcdblookup <- read.csv("C:/Users/RichardsD/OneDrive - MWLR/Papers/Mackenzie case study/Parameters/lcdblookup.csv")


nzes.fate.converter <- function(fp , sc, lcdblookup
){ #which yeasr to subset)
  
  # create filepaths for parameters file and data folder results
  fpp<- paste0(fp,"PARAM_SIMUL/Simul_parameters_",sc,".txt")
  fpd <- paste0(fp,"RESULTS/SIMUL_", sc, "/ABUND_REL_perPFG_allStrata/")
  
  simfolder<-strsplit(fp,"/")
  simfolder<-simfolder[[1]]
  simfolder<-simfolder[length(simfolder)]
 # simfolder<- fp
  
  # first check if there is data in the folder
  if(length(grep("tif", list.files(fpd)))==0){
    RFate::POST_FATE.relativeAbund(
      substr(fp,1,nchar(fp)-1) ,
      file.simulParam = paste0("Simul_parameters_",sc,".txt"),
      years = c(1,10,20,30,40,50,60,70,80,90,100))
  }
  
  outm2<- raster::stack(paste0(fpd,
                       list.files(fpd
                       )
  ))
  
  
  
  # turn off some data to NA
  #macbase<- reclassify(macbase,
    #                   cbind(lcdblookup$lcdbnum,
     #                        lcdblookup$lcdbnum %in% c(0,1,2,5,6,20,21,22,30,33,40)))
  
  names(outm2)<- gsub("Abund_relative_" ,"" , names(outm2))
  names(outm2)<- gsub("_STRATA_all" ,"" , names(outm2))
  names(outm2)<- gsub("EAR_" ,"" , names(outm2))
  
 
  # blank
  eg<-outm2[[1]]
  eg[,]<-0
  eg[1,1]<-1
  names(eg)<-"blank"
  
  # Stacker to make plots
  lcmmaker<-function(stckof6){
    oi<-raster::which.max(stckof6)
    oi<-raster::reclassify(oi, 
                   cbind(1:8,
                         c(2,4,7,8,9,10,11,12)))
    oi
  }
  
  lcmmakerNOEXOTIC<-function(stckof6){
    
    emp <- stckof6[[1]]
    emp[,]<-0
    emp<-raster::mask(emp,stckof6[[1]])
    stckof8 <- raster::stack(stckof6[[1]],
                     emp,
                     stckof6[[2:6]],
                     emp)
    
    oi<-raster::which.max(stckof8)
    oi<-raster::reclassify(oi, 
                   cbind(1:8,
                         c(2,4,7,8,9,10,11,12)))
    oi
  }
  
  yearselector<- function(stckbig, yr){
    stc<- stckbig[[grep(yr, names(stckbig))]]
    
    nstc<-names(stc)
    nstc<-sapply(strsplit(nstc, "p"), function(x){x[[2]]})
    nstc<- paste0("p",nstc)
    
    
    eg<-stckbig[[1]]
    eg[,]<-0
    eg[1,1]<-1
    names(eg)<-"blank"
    
    stc
  }
  
  
  if(min(table(substr(names(outm2),1,4)))
     ==8){
    
    outm2LCM<- raster::stack( 
      lcmmaker(yearselector(outm2, "1_")),
      lcmmaker(yearselector(outm2, "20_")),
      lcmmaker(yearselector(outm2, "40_")),
      lcmmaker(yearselector(outm2, "60_")),
      lcmmaker(yearselector(outm2, "80_")),
      lcmmaker(yearselector(outm2, "100_")))
  } else if(min(table(substr(names(outm2),1,4)))
            ==6){
    
    outm2LCM<- raster::stack( 
      lcmmakerNOEXOTIC(yearselector(outm2, "1_")),
      lcmmakerNOEXOTIC(yearselector(outm2, "20_")),
      lcmmakerNOEXOTIC(yearselector(outm2, "40_")),
      lcmmakerNOEXOTIC(yearselector(outm2, "60_")),
      lcmmakerNOEXOTIC(yearselector(outm2, "80_")),
      lcmmakerNOEXOTIC(yearselector(outm2, "100_")))
  }
  
  # Neat plot map
  outm2LCM<-raster::reclassify( outm2LCM,
                        cbind(lcdblookup$pfg,
                              lcdblookup$lcdbnum))
  
 # # This code is for plotting nicely using a colour scheme
 #  outm2LCM<-raster::reclassify(outm2LCM,
 #                       cbind(lcdblookup$lcdbnum,
 #                             as.numeric(as.factor(lcdblookup$htmlcolour))))
 #  
 #  pl<- cbind(as.numeric(as.factor(lcdblookup$htmlcolour)),lcdblookup$htmlcolour, lcdblookup$legendname)
 #  pl<- pl[!duplicated(pl[,2]),]
 #  
 #  pl<-as.data.frame(na.omit(pl))
 #  pl<-pl[order(as.numeric(pl[,1])),]
 #  
 #  oe<- raster::getValues(outm2LCM)
 #  oe<- c(oe[,1],oe[,2],oe[,3],oe[,4],oe[,5],oe[,6])
 #  oe<-unique(oe)
 #  
 #  pl1<-pl[pl$V1 %in% as.character(oe),]
  
  
  names(outm2LCM)<-c("1","20","40","60","80","100")
  outm2LCM<-outm2LCM[[1:6]]
  outm2LCM
  #list(outm2LCM,pl1)
}