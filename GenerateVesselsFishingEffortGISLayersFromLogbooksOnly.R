
  # GENERAL SETTINGS

   args <- commandArgs(trailingOnly = TRUE)

   general <- list()

   if (length(args) < 2) {
     if(.Platform$OS.type == "windows") {
       general$application           <- "testexample" 
       general$main_path_gis         <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis", general$application)
       general$main.path.ibm         <- file.path("C:","Users","fbas","Documents","GitHub",paste("DISPLACE_input_", general$application, sep=''))
       general$igraph                <- 56  # caution: should be consistent with existing objects already built upon a given graph
       fit_to_GRID                   <- TRUE   
     }
  } else {
       general$application           <- args[1]
       general$main_path_gis         <- args[2]
       general$main.path.ibm         <- args[3]
       general$igraph                <- args[4]  # caution: should be consistent with existing vessels already built upon a given graph
       fit_to_GRID                   <- FALSE
  }
   cat(paste("START \n"))

  
   dir.create(file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep='')))
   dir.create(file.path(general$main_path_gis, "FISHERIES", "SpatialLayers"))

   cat(paste("The layers will be stored in /FISHERIES/SpatialLayers \n"))

  
  #!#!#!#!#!#
  #!#!#!#!#!#
  # choose your country (and adapt below accordingly)
  ctry <- "DNK"
  year <- "2015"
  #!#!#!#!#!#
  #!#!#!#!#!#
cat(paste("Country chosen is",ctry,"; otherwise adapt the script. \n"))



  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

getLogbooks<- function(year="2015", ctry="DNK") {
 
  
  load(file.path(general$main_path_gis, "FISHERIES",
           #paste("eflalo_","2015",".RData",sep='')))
           paste("logbooks_DNK_",year,".RData",sep='')))
      logbooks  <- logbooks[grep("DNK", as.character(logbooks$VE_REF)),]

  logbooks              <- subset(logbooks,FT_REF != 0)

  library(vmstools)
  logbooks <- cbind(logbooks, ICESrectangle2LonLat(logbooks$LE_RECT, midpoint=TRUE))
   
  logbooks$SI_LONG    <- as.numeric(as.character(logbooks$SI_LONG))
  logbooks$SI_LATI    <- as.numeric(as.character(logbooks$SI_LATI))
 
    # find effort in hours
  logbooks$FT_DDATIM  <- as.POSIXct(paste(logbooks$FT_DDAT,logbooks$FT_DTIME, sep = " "),
                               tz = "GMT", format = "%d/%m/%Y  %H:%M")
  logbooks$FT_LDATIM  <- as.POSIXct(paste(logbooks$FT_LDAT,logbooks$FT_LTIME, sep = " "),
                               tz = "GMT", format = "%d/%m/%Y  %H:%M")
  logbooks$LE_EFF         <- an(difftime(logbooks$FT_LDATIM, logbooks$FT_DDATIM, units="hours"))
  
  logbooks$ctry       <- ctry
 
 
  
 
 return(logbooks)
}

  # calls
  logbooks       <- getLogbooks(year=year, ctry=ctry)

 
     
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  
attachAGrid <- function(logbooks, resx, resy){  
  
  xrange <- range(as.numeric(as.character(logbooks$SI_LONG)), na.rm=TRUE)
  yrange <- range(as.numeric(as.character(logbooks$SI_LATI)), na.rm=TRUE)
  #xrange <- c(7,17)
  #yrange <- c(53,63)

  # Set grid
  library(vmstools)
  grd <- createGrid(xrange,yrange,resx=resx,resy=resy,type="SpatialGrid",exactBorder=TRUE)

  # Grid all tacsatSweptArea data
  # Convert all tacsat poins first to SpatialPoints
  coords <- SpatialPoints(cbind(SI_LONG=as.numeric(as.character(logbooks$SI_LONG)),SI_LATI=as.numeric(as.character(logbooks$SI_LATI))))
  idx <- over(coords,grd)
  logbooks$grID <- idx

  # Remove records that are not in the study area
  logbooks <- subset(logbooks,is.na(grID)==F)

  # Add midpoint of gridcell to dataset
  logbooks <- cbind(logbooks, CELL_LONG=coordinates(grd)[logbooks$grID,1], CELL_LATI=coordinates(grd)[logbooks$grID,2])

return(logbooks)
}

# calls
logbooks        <- attachAGrid(logbooks, resx=60/60, resy=30/60)  # at the ICES rectangle resolution


cat(paste("Overlay a grid on data....done \n"))
 
 
  
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

  # add quarter info
  logbooks$quarter <- quarter(logbooks$FT_DDATIM)




##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

subsetForVesselsVisitingGISpolygon <- function(logbooks, general=general, nameGISlayer="wbaltic_wgs84") {

   # relevant vessels only i.e. active in the area....
  
     # keep only the vessels fishnig in the western Baltic  (and kattegat because her.3a22, and East baltic because spr.2232)
    library(maptools)
    library(sp)
    handmade            <- readShapePoly(file.path(general$main_path_gis, "MANAGEMENT",  nameGISlayer))  # build in ArcGIS 10.1
    the_area            <- sapply(handmade@polygons, function(x) lapply(x@Polygons, function(x) x@coords)) # tricky there...
    in_area             <- point.in.polygon(logbooks[,'SI_LONG'],logbooks[,'SI_LATI'], the_area[[1]][,1],the_area[[1]][,2])

    # then subset here...
    vid_this_case_study <- as.character(unique(logbooks$VE_REF[in_area>0]))
    cat(paste(length(vid_this_case_study), " vessels in the area over ", length(unique(logbooks$VE_REF)), " in total" , "\n"))
    logbooks      <- logbooks[logbooks$VE_REF %in% vid_this_case_study,]
  
   # relevant vessels only i.e. with some recorded catches on the subsetted stocks.... 
     # keep only the vessels fishnig in the western Baltic  (and kattegat because her.3a22, and East baltic because spr.2232)
   
    agg     <- aggregate(logbooks[, grep('LE_KG_', colnames(logbooks))], list(logbooks$VE_REF), sum, na.rm=TRUE)
    agg$tot <- apply(agg[,-1], 1, sum, na.rm=TRUE)
    vid_with_no_landings_for_these_stocks <- as.character(agg[agg$tot<=0, 1])
    
    # then subset here...
    cat(paste(length(vid_with_no_landings_for_these_stocks), " vessels to remove over ", length(unique(logbooks$VE_REF)), " in total" , "\n"))
    logbooks      <- logbooks[!tacsatp$VE_REF %in% vid_with_no_landings_for_these_stocks,]
   
return(logbooks)
}

# calls
if(FALSE){
  logbooks              <- subsetForVesselsVisitingGISpolygon (logbooks,  general=general, nameGISlayer="wbaltic_wgs84") 
  }
  
  
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

makeOtherMetier <- function (logbooks, threshold_in_effort=90){

  # play with metier categorisation 
  # rough simplication:
  logbooks$LE_MET_rough <- factor(substr(logbooks$LE_MET, 1,3)) # init
  levels(logbooks$LE_MET_rough)
  levels(logbooks$LE_MET_rough)[levels(logbooks$LE_MET_rough) %in% c("DRB", "FPO", "GNS", "GTR", "LHP", "LLD", "LLS", "MIS", "No_" ) ] <- c("Passive")
  levels(logbooks$LE_MET_rough)[levels(logbooks$LE_MET_rough) %in% c("OTB", "OTM", "OTT", "PTB", "PTM", "TBB") ]                       <- c("Trawl")
  levels(logbooks$LE_MET_rough)[levels(logbooks$LE_MET_rough) %in% c("PS_", "SDN", "SSC") ]                                            <- c("Seine")
 
  # keep level 6 and built an OTHER metier
  levels(logbooks$LE_MET_level6)
  agg_per_met                 <- tapply(logbooks$LE_EFF, list(logbooks$LE_MET), sum, na.rm=TRUE)
  agg_per_met_ordered         <- agg_per_met[order(agg_per_met, decreasing =TRUE)]
  agg_per_met_ordered_and_cum <- cumsum(agg_per_met_ordered/sum(agg_per_met_ordered, na.rm=TRUE)*100)
  met_to_keep                 <- names(agg_per_met_ordered_and_cum[agg_per_met_ordered_and_cum < threshold_in_effort])   #  the ones that cumulatively cover 90% of the effort
  met_to_keep                 <- met_to_keep[!is.na(met_to_keep)]
 
  
  levels(logbooks$LE_MET)[!levels(logbooks$LE_MET)%in% met_to_keep] <- "other_non_vms_equipped"
  levels(logbooks$LE_MET)[levels(logbooks$LE_MET)%in% "No_Matrix6"]  <- "other_non_vms_equipped"
 
  # debug metier names
  levels(logbooks$LE_MET) <- gsub(">=", "o", levels(logbooks$LE_MET))  
  levels(logbooks$LE_MET) <- gsub("<",  "u", levels(logbooks$LE_MET))  
  levels(logbooks$LE_MET) <- gsub(">",  "o", levels(logbooks$LE_MET))  
  levels(logbooks$LE_MET) <- gsub("-",  "_", levels(logbooks$LE_MET))  
  
  
  # simplify metier names by removing the mesh size info (e.g. for ECOAST GRID) 
  #levels(logbooks$LE_MET) <- unlist(lapply(strsplit(as.character(levels(logbooks$LE_MET)), split="_"), function(x) paste(x[1:2], collapse="_")) )
  #levels(logbooks$LE_MET)[levels(logbooks$LE_MET) %in% "other_NA"] <- "other"
  
return(logbooks)
}

# calls
logbooks       <- makeOtherMetier(logbooks, threshold_in_effort=90)  # threshold_in_effort gives percentage of total effort of metiers kept.

cat(paste("Define the metiers and the metier 'other'...done\n"))


  
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!EXPORT F EFFORT GIS LAYERS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
exportEffortCtryMetierGISlayers <- function (logbooks, general=general, effort_field="LE_EFF", resx=60/60, resy=30/60){

  nm <- colnames(logbooks)
  
  if(!('CELL_LATI' %in% nm) || !('CELL_LONG' %in% nm)) stop('need a grid attached to the dataset!')
  
  # Aggregate the results by metier and grid ID (aggregate() can be slow: be patient)
  feffort    <- effort_field

  logbooks[,c(feffort)] <- as.numeric(as.character(logbooks[,c(feffort)]))
  agglogbooks <- aggregate(logbooks[,c(feffort)], by=list(logbooks$LE_MET, logbooks$grID,  logbooks$CELL_LONG, logbooks$CELL_LATI, logbooks$ctry),sum,na.rm=T)
  colnames(agglogbooks)[1:5] <- c("LE_MET","grID", "CELL_LONG", "CELL_LATI", "ctry")
  colnames(agglogbooks)[6]   <- "feffort"

  aggtacsatp$EFFORT <- aggtacsatp$feffort # ECOAST GRID spe

  agglogbooks$ctry <- factor(agglogbooks$ctry)
  agglogbooks$LE_MET <- factor(agglogbooks$LE_MET)
  
  # loop over relevant activity/metier
  for (ctry in levels(agglogbooks$ctry)){
   for (met  in levels(agglogbooks$LE_MET)){
   # for (quarter in  unique(aggResult$quarter)){
     #nameobj <- paste(ctry, "_gis_feffort_", met, "_", "quarter", quarter, sep="")
     nameobj <- paste(ctry, "_gis_feffort_logbk_only_", met, sep="")


     #aggResultSub <- agglogbooks[aggtacsatp$LE_MET==met & agglogbooks$quarter==quarter & agglogbooks$ctry==ctry,]
     aggResultSub <- agglogbooks[agglogbooks$LE_MET==met  & agglogbooks$ctry==ctry,]
     if(nrow(aggResultSub)>0){
     
     ## EXPORT IN SHAPE FILE
     uniqueCells           <- aggResultSub[!duplicated(aggResultSub$grID),c("grID","CELL_LONG","CELL_LATI")]
     grdc2plot             <- lonLat2SpatialPolygons(lst=lapply(as.list(1:nrow(uniqueCells)),
                                                function(x){
                                                  data.frame(SI_LONG=c(uniqueCells[x,"CELL_LONG"]-resx/2,
                                                                       rep(uniqueCells[x,"CELL_LONG"]+resx/2,2),uniqueCells[x,"CELL_LONG"]-resx/2),
                                                             SI_LATI=c(rep(uniqueCells[x,"CELL_LATI"]-resy/2,2),rep(uniqueCells[x,"CELL_LATI"]+resy/2,2)))}))

     idx                   <- match(uniqueCells$grID, aggResultSub$grID)
     rownames(aggResultSub) <- idx

     library(sp)
     proj4string(grdc2plot) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

     # export in .shp attaching the df with the levels
     IDs  <- sapply(grdc2plot@polygons, function(x) x@ID)
     spdf <- SpatialPolygonsDataFrame(grdc2plot, aggResultSub)
     writePolyShape(spdf, file.path(general$main_path_gis, "FISHERIES", "SpatialLayers", nameobj))
     } # if nrow!=0

     }
     #}
     }
     
 return()
 }    
     
  
  # calls
  exportEffortCtryMetierGISlayers (logbooks, general=general, effort_field="LE_EFF", resx=60/60, resy=30/60)  # => to spatialLayers folder for DISPLACE parameterisation

     
 
cat(paste(".....done\n"))
cat(paste("You might repeat but from a new country...\n"))



