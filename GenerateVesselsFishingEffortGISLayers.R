
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

getTacsatp<- function(what="weight", year="2015", ctry="DNK") {
 
  # a generic case
  if(what=="weight")  load(file=file.path(general$main_path_gis, "FISHERIES", paste("coupled_VMS_logbooks_",ctry,"_",year,".RData", sep=""))) # get the coupled_VMS_logbooks object
  if(what=="value")   load(file=file.path(general$main_path_gis, "FISHERIES", paste("coupled_VMS_logbooks_",ctry,"_",year,"_value.RData", sep="")))
  tacsatp            <- coupled_VMS_logbooks
  tacsatp            <- tacsatp[!is.na(as.numeric(as.character(tacsatp$SI_LONG))) &  !is.na(as.numeric(as.character(tacsatp$SI_LATI))), ]
  tacsatp$SI_LONG    <- as.numeric(as.character(tacsatp$SI_LONG))
  tacsatp$SI_LATI    <- as.numeric(as.character(tacsatp$SI_LATI))
  tacsatp$SI_STATE   <- as.numeric(as.character(tacsatp$SI_STATE))
  tacsatp$LE_EFF_VMS <- as.numeric(as.character(tacsatp$LE_EFF_VMS)) /60
  tacsatp            <- tacsatp[tacsatp$SI_STATE==1, ]  # keep fishing positions only
  tacsatp$ctry       <- ctry
 
 
  # e.g. a special case
  if(ctry=="SWE"){
   if(what=="weight") load(file=file.path(general$main_path_gis, paste("coupled_VMS_logbooks_",ctry,"_",year,".RData", sep="")))
   if(what=="value")  load(file=file.path(general$main_path_gis, paste("coupled_VMS_logbooks_",ctry,"_",year,"_value.RData", sep="")))
    tacsatp_swe            <- tacsat.swe
    tacsatp_swe$SI_LONG    <- as.numeric(as.character(tacsatp_swe$SI_LONG))
    tacsatp_swe$SI_LATI    <- as.numeric(as.character(tacsatp_swe$SI_LATI))
    tacsatp_swe$SI_STATE   <- as.numeric(as.character(tacsatp_swe$SI_STATE))
    tacsatp_swe$LE_MET_level6 <- tacsatp_swe$VE_MET  
    format_date            <- "%Y-%m-%d %H:%M:%S" 
    tacsatp_swe$SI_DATIM   <- as.POSIXct( tacsatp_swe$SI_DATIM, tz='GMT',   format_date)
    tacsatp_swe$LE_EFF_VMS <- abs(c(0, as.numeric( tacsatp_swe[-nrow( tacsatp_swe),"SI_DATIM"] - tacsatp_swe[-1,"SI_DATIM"], units="hours")))
    start.trip <- c(1,diff( tacsatp_swe[,"FT_REF"]))
    tacsatp_swe$all_effort <- tacsatp_swe$LE_EFF_VMS  # save...
    tacsatp_swe[start.trip!=0, "LE_EFF_VMS"] <- 0  # just correct for the trip change points
    tacsatp_swe$LE_EFF_VMS <- as.numeric(as.character(tacsatp_swe$LE_EFF_VMS))
    tacsatp_swe            <- tacsatp_swe[!is.na(as.numeric(as.character(tacsatp_swe$SI_LONG))) &  !is.na(as.numeric(as.character(tacsatp_swe$SI_LATI))), ]
    tacsatp_swe            <- tacsatp_swe[tacsatp_swe$SI_STATE==1, ]  # keep fishing positions only
    tacsatp_swe$ctry       <- "SWE"
    tacsatp                <- tacsatp_swe
   }
 
 
 return(tacsatp)
}

  # calls
  tacsatp       <- getTacsatp(what="weight", year=year, ctry=ctry)
  tacsatp_value <- getTacsatp(what="value", year=year, ctry=ctry)

 
     
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  
attachAGrid <- function(tacsatp, resx, resy){  
  
  xrange <- range(as.numeric(as.character(tacsatp$SI_LONG)), na.rm=TRUE)
  yrange <- range(as.numeric(as.character(tacsatp$SI_LATI)), na.rm=TRUE)
  #xrange <- c(7,17)
  #yrange <- c(53,63)

  # Set grid
  library(vmstools)
  grd <- createGrid(xrange,yrange,resx=resx,resy=resy,type="SpatialGrid",exactBorder=TRUE)

  # Grid all tacsatSweptArea data
  # Convert all tacsat poins first to SpatialPoints
  coords <- SpatialPoints(cbind(SI_LONG=as.numeric(as.character(tacsatp$SI_LONG)),SI_LATI=as.numeric(as.character(tacsatp$SI_LATI))))
  idx <- over(coords,grd)
  tacsatp$grID <- idx

  # Remove records that are not in the study area
  tacsatp <- subset(tacsatp,is.na(grID)==F)

  # Add midpoint of gridcell to dataset
  tacsatp <- cbind(tacsatp, CELL_LONG=coordinates(grd)[tacsatp$grID,1], CELL_LATI=coordinates(grd)[tacsatp$grID,2])

return(tacsatp)
}

# calls
tacsatp        <- attachAGrid(tacsatp, resx=3/60, resy=3/60)
tacsatp_value  <- attachAGrid(tacsatp_value, resx=3/60, resy=3/60)


cat(paste("Overlay a grid on data....done \n"))
 
 
  
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

  # add quarter info
  tacsatp$quarter <- quarter(tacsatp$SI_DATE)




##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

subsetForVesselsVisitingGISpolygon <- function(tacsatp, general=general, nameGISlayer="wbaltic_wgs84") {

   # relevant vessels only i.e. active in the area....
  
     # keep only the vessels fishnig in the western Baltic  (and kattegat because her.3a22, and East baltic because spr.2232)
    library(maptools)
    library(sp)
    handmade            <- readShapePoly(file.path(general$main_path_gis, "MANAGEMENT",  nameGISlayer))  # build in ArcGIS 10.1
    the_area            <- sapply(handmade@polygons, function(x) lapply(x@Polygons, function(x) x@coords)) # tricky there...
    in_area             <- point.in.polygon(tacsatp[,'SI_LONG'],tacsatp[,'SI_LATI'], the_area[[1]][,1],the_area[[1]][,2])

    # then subset here...
    vid_this_case_study <- as.character(unique(tacsatp$VE_REF[in_area>0]))
    cat(paste(length(vid_this_case_study), " vessels in the area over ", length(unique(tacsatp$VE_REF)), " in total" , "\n"))
    tacsatp      <- tacsatp[tacsatp$VE_REF %in% vid_this_case_study,]
  
   # relevant vessels only i.e. with some recorded catches on the subsetted stocks.... 
     # keep only the vessels fishnig in the western Baltic  (and kattegat because her.3a22, and East baltic because spr.2232)
   
    agg     <- aggregate(tacsatp[, grep('LE_KG_', colnames(tacsatp))], list(tacsatp$VE_REF), sum, na.rm=TRUE)
    agg$tot <- apply(agg[,-1], 1, sum, na.rm=TRUE)
    vid_with_no_landings_for_these_stocks <- as.character(agg[agg$tot<=0, 1])
    
    # then subset here...
    cat(paste(length(vid_with_no_landings_for_these_stocks), " vessels to remove over ", length(unique(tacsatp$VE_REF)), " in total" , "\n"))
    tacsatp      <- tacsatp[!tacsatp$VE_REF %in% vid_with_no_landings_for_these_stocks,]
   
return(tacsatp)
}

# calls
tacsatp              <- subsetForVesselsVisitingGISpolygon (tacsatp,  general=general, nameGISlayer="wbaltic_wgs84") 
tacsatp_value        <- tacsatp_value[tacsatp_value$VE_REF %in% unique(tacsatp$VE_REF),]
tacsatp_value$VE_REF <- factor(tacsatp_value$VE_REF)

  
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

makeOtherMetier <- function (tacsatp, threshold_in_effort=90){

  # play with metier categorisation 
  # rough simplication:
  tacsatp$LE_MET_rough <- factor(substr(tacsatp$LE_MET_level6, 1,3)) # init
  levels(tacsatp$LE_MET_rough)
  levels(tacsatp$LE_MET_rough)[levels(tacsatp$LE_MET_rough) %in% c("DRB", "FPO", "GNS", "GTR", "LHP", "LLD", "LLS", "MIS", "No_" ) ] <- c("Passive")
  levels(tacsatp$LE_MET_rough)[levels(tacsatp$LE_MET_rough) %in% c("OTB", "OTM", "OTT", "PTB", "PTM", "TBB") ]                       <- c("Trawl")
  levels(tacsatp$LE_MET_rough)[levels(tacsatp$LE_MET_rough) %in% c("PS_", "SDN", "SSC") ]                                            <- c("Seine")
 
  # keep level 6 and built an OTHER metier
  levels(tacsatp$LE_MET_level6)
  agg_per_met                 <- tapply(tacsatp[tacsatp$SI_STATE==1, ]$LE_EFF_VMS, list(tacsatp[tacsatp$SI_STATE==1, ]$LE_MET_level6), sum, na.rm=TRUE)
  agg_per_met_ordered         <- agg_per_met[order(agg_per_met, decreasing =TRUE)]
  agg_per_met_ordered_and_cum <- cumsum(agg_per_met_ordered/sum(agg_per_met_ordered, na.rm=TRUE)*100)
  met_to_keep                 <- names(agg_per_met_ordered_and_cum[agg_per_met_ordered_and_cum < threshold_in_effort])   #  the ones that cumulatively cover 90% of the effort
  met_to_keep                 <- met_to_keep[!is.na(met_to_keep)]
 
  agg_per_met_and_vid                 <- tapply(tacsatp[tacsatp$SI_STATE==1, ]$LE_EFF_VMS, list( tacsatp[tacsatp$SI_STATE==1, ]$VE_REF, tacsatp[tacsatp$SI_STATE==1, ]$LE_MET_level6), sum, na.rm=TRUE)
  #agg_per_met_and_vid[!is.na(agg_per_met_and_vid[, "PTM_SPF_16-31_0_0"]),]   # check if some vessels are polyvalent demersal-pelagic
  
  levels(tacsatp$LE_MET_level6)[!levels(tacsatp$LE_MET_level6)%in% met_to_keep] <- "other"
  levels(tacsatp$LE_MET_level6)[levels(tacsatp$LE_MET_level6)%in% "No_Matrix6"]  <- "other"
 
  # debug metier names
  levels(tacsatp$LE_MET_level6) <- gsub(">=", "o", levels(tacsatp$LE_MET_level6))  
  levels(tacsatp$LE_MET_level6) <- gsub("<",  "u", levels(tacsatp$LE_MET_level6))  
  levels(tacsatp$LE_MET_level6) <- gsub(">",  "o", levels(tacsatp$LE_MET_level6))  
  levels(tacsatp$LE_MET_level6) <- gsub("-",  "_", levels(tacsatp$LE_MET_level6))  
  
  
  # simplify metier names by removing the mesh size info (e.g. for ECOAST GRID) 
  #levels(tacsatp$LE_MET_level6) <- unlist(lapply(strsplit(as.character(levels(tacsatp$LE_MET_level6)), split="_"), function(x) paste(x[1:2], collapse="_")) )
  #levels(tacsatp$LE_MET_level6)[levels(tacsatp$LE_MET_level6) %in% "other_NA"] <- "other"
  
return(tacsatp)
}

# calls
tacsatp       <- makeOtherMetier(tacsatp, threshold_in_effort=90)  # threshold_in_effort gives percentage of total effort of metiers kept.
tacsatp_value <- makeOtherMetier(tacsatp_value, threshold_in_effort=90)  # threshold_in_effort gives percentage of total effort of metiers kept.

cat(paste("Define the metiers and the metier 'other'...done\n"))


  
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!EXPORT F EFFORT GIS LAYERS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
exportEffortCtryMetierGISlayers <- function (tacsatp, general=general, effort_field="LE_EFF_VMS", resx=3/60, resy=3/60){

  nm <- colnames(tacsatp)
  
  if(!('CELL_LATI' %in% nm) || !('CELL_LATI' %in% nm)) stop('need a grid attached to the dataset!')
  
  # Aggregate the results by metier and grid ID (aggregate() can be slow: be patient)
  feffort    <- effort_field

  tacsatp[,c(feffort)] <- as.numeric(as.character(tacsatp[,c(feffort)]))
  #aggtacsatp <- aggregate(tacsatp[,c(feffort)], by=list(tacsatp$LE_MET, tacsatp$grID,tacsatp$quarter),sum,na.rm=T)
  #colnames(aggtacsatp)[1:3] <- c("LE_MET","grID", "quarter")
  #colnames(aggtacsatp)[4]   <- "feffort"
  aggtacsatp <- aggregate(tacsatp[,c(feffort)], by=list(tacsatp$LE_MET_level6, tacsatp$grID,  tacsatp$CELL_LONG, tacsatp$CELL_LATI, tacsatp$ctry),sum,na.rm=T)
  colnames(aggtacsatp)[1:5] <- c("LE_MET","grID", "CELL_LONG", "CELL_LATI", "ctry")
  colnames(aggtacsatp)[6]   <- "feffort"

  aggtacsatp$EFFORT <- aggtacsatp$feffort # ECOAST GRID spe

  # Add midpoint of gridcell to dataset
  #aggResult <- cbind(aggtacsatp, CELL_LONG=coordinates(grd)[aggtacsatp$grID,1], CELL_LATI=coordinates(grd)[aggtacsatp$grID,2])
  #save(aggResult, file=file.path(outPath,"2015_aggtacsatp.RData"))

  aggtacsatp$ctry <- factor(aggtacsatp$ctry)
  aggtacsatp$LE_MET <- factor(aggtacsatp$LE_MET)
  
  # loop over relevant activity/metier
  for (ctry in levels(aggtacsatp$ctry)){
   for (met  in levels(aggtacsatp$LE_MET)){
   # for (quarter in  unique(aggResult$quarter)){
     #nameobj <- paste(ctry, "_gis_feffort_", met, "_", "quarter", quarter, sep="")
     nameobj <- paste(ctry, "_gis_feffort_", met, sep="")


     #aggResultSub <- aggtacsatp[aggtacsatp$LE_MET==met & aggtacsatp$quarter==quarter & aggtacsatp$ctry==ctry,]
     aggResultSub <- aggtacsatp[aggtacsatp$LE_MET==met  & aggtacsatp$ctry==ctry,]
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
  exportEffortCtryMetierGISlayers (tacsatp, general=general, effort_field="LE_EFF_VMS", resx=3/60, resy=3/60)  # => to spatialLayers folder for DISPLACE parameterisation

     
 sauv <- tacsatp
 sauv2 <- tacsatp_value
 

cat(paste(".....done\n"))
cat(paste("You might repeat but from a new country...\n"))





if(FALSE){
  # for the user going one step further to identify fishing grounds:
  
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!GET DELINEATION OF FISHING GROUNDS!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!FROM VARIOUS CRITERIA!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

#-----------
delineateFishingGrounds <- function (tacsatp,  type_agg="per_cell", based_on="feffort", threshold_in_percent=90){

 
    if(based_on=="revenue") tacsatp <- cbind.data.frame(tacsatp,
                          totfield= apply(tacsatp[,grep('EURO',colnames(tacsatp))], 1, sum, na.rm=TRUE) )
    if(based_on=="weight") tacsatp <- cbind.data.frame(tacsatp,
                          totfield= apply(tacsatp[,grep('KG',colnames(tacsatp))], 1, sum, na.rm=TRUE) )
    if(based_on=="feffort") tacsatp <- cbind.data.frame(tacsatp,
                          totfield= tacsatp$'LE_EFF_VMS')
    if(based_on=="spatialdependency1") tacsatp <- cbind.data.frame(tacsatp,
                          totfield= apply(tacsatp[,grep('EURO',colnames(tacsatp))], 1, sum, na.rm=TRUE) )
    if(based_on=="spatialdependency2") tacsatp <- cbind.data.frame(tacsatp,
                          totfield= apply(tacsatp[,grep('EURO',colnames(tacsatp))], 1, sum, na.rm=TRUE) )


    # per cell
    if(type_agg=="per_cell"){
    totfield_per_cell                                                             <- aggregate(tacsatp$totfield, list(tacsatp$grID, tacsatp$CELL_LONG, tacsatp$CELL_LATI), sum )
    colnames(totfield_per_cell)                                                   <- c("grID", "CELL_LONG", "CELL_LATI", "totfield")
    totfield_per_cell$percent                                                     <- totfield_per_cell$totfield / sum(totfield_per_cell$totfield) *100 # percentage per node
    totfield_per_cell_and_percentage                                              <- totfield_per_cell[order(totfield_per_cell$percent, decreasing=TRUE),]
    totfield_per_cell_and_percentage$cumpercent                                   <- cumsum(totfield_per_cell_and_percentage$percent)
    totfield_per_cell_and_percentage[,paste("thres.",threshold_in_percent, sep="")]  <- as.numeric(totfield_per_cell_and_percentage$cumpercent <  threshold_in_percent)
    totfield_per_cell_and_percentage                                              <-  totfield_per_cell_and_percentage[totfield_per_cell_and_percentage[,ncol(totfield_per_cell_and_percentage)]==1,]
    }
   
    if(type_agg=="per_cell_per_vessel"){
    # per cell, per vessel (i.e. a grid cell is kept if the cell belongs to the 90%  of at least one vessel....) 
    totfield_per_cell_per_vessel                                                  <- aggregate(tacsatp$totfield, list(tacsatp$VE_REF, tacsatp$grID, tacsatp$CELL_LONG, tacsatp$CELL_LATI), sum )
    colnames(totfield_per_cell_per_vessel)                                        <- c("VE_REF", "grID", "CELL_LONG", "CELL_LATI", "totfield")
    dd <- lapply(split(totfield_per_cell_per_vessel, f=totfield_per_cell_per_vessel$VE_REF),
       function(x){
         x$percent                                      <- x$totfield / sum(x$totfield) *100 # percentage per node
         x                                              <- x[order(x$percent, decreasing=TRUE),]
         x$cumpercent                                   <- cumsum(x$percent)
         x[,paste("thres.",threshold_in_percent, sep="")]  <- as.numeric(x$cumpercent <  threshold_in_percent)
        x
       })
    totfield_per_cell_per_vessel_and_percentage                                   <- do.call('rbind', dd)
    totfield_per_cell_per_vessel_and_percentage                                   <- totfield_per_cell_per_vessel_and_percentage[totfield_per_cell_per_vessel_and_percentage[,ncol(totfield_per_cell_per_vessel_and_percentage)]==1,]
    totfield_per_cell_per_vessel_and_percentage$VE_REF                            <- as.factor(totfield_per_cell_per_vessel_and_percentage$VE_REF)
    }
    
    
   if(based_on %in% c("revenue", "weight", "feffort"))
     {
     
     if(type_agg=="per_cell"){
      return(totfield_per_cell_and_percentage)
     }
     if(type_agg=="per_cell_per_vessel"){
      return(totfield_per_cell_per_vessel_and_percentage)
     }
     
     
     } else{   # one step further.....
     
          if(based_on %in% 'spatialdependency1' && type_agg!="per_cell_per_vessel")
             {
            
             # index 1
             revenue_per_vessel                                  <- tapply(tacsatp$totfield, list(tacsatp$VE_REF), sum)
             names_vessels_per_cell_this_threshold               <- tapply(tacsatp$VE_REF, list(tacsatp$grID), function(x) as.character(unique(x)))
             tot_revenue_of_the_vessels_visiting_this_cell       <- lapply(names_vessels_per_cell_this_threshold, function(x) sum(revenue_per_vessel[x], na.rm=TRUE))
             index1_importance_of_vessels                        <- unlist(tot_revenue_of_the_vessels_visiting_this_cell) / sum(revenue_per_vessel, na.rm=TRUE)  # index runs from 0 to 1
             # retrieve info on cells....
             index1_importance_of_vessels                        <- cbind.data.frame(totfield_per_cell_and_percentage, index1=index1_importance_of_vessels[as.character(totfield_per_cell_and_percentage$grID)])   # retrieve info
             index1_importance_of_vessels                        <- index1_importance_of_vessels[order(index1_importance_of_vessels$index1, decreasing=TRUE),]     # order
   
            
             return(index1_importance_of_vessels)
            
            } 
            
          if(based_on %in% 'spatialdependency2' && type_agg!="per_cell_per_vessel")
          {  
            
            # index 2
            revenue_per_vessel                                  <- tapply(tacsatp$totfield, list(tacsatp$VE_REF), sum)
            revenue_per_cell                                    <- tapply(tacsatp$totfield, list(tacsatp$grID), sum)
            index2_contribution_of_the_cell                     <- unlist(revenue_per_cell) / sum(revenue_per_vessel, na.rm=TRUE) 

            # index 3
            names_vessels_per_cell_this_threshold               <- tapply(tacsatp$VE_REF, list(tacsatp$grID), function(x) as.character(unique(x)))
            tot_revenue_of_the_vessels_visiting_this_cell       <- lapply(names_vessels_per_cell_this_threshold, function(x) sum(revenue_per_vessel[x], na.rm=TRUE))
            index3_dependency_to_the_cell                       <- unlist(revenue_per_cell) /  unlist(tot_revenue_of_the_vessels_visiting_this_cell)  # index runs from 0 to 1
            # retrieve info on cells....
            index3_dependency_to_the_cell                        <- cbind.data.frame(totfield_per_cell_and_percentage, index3=index3_dependency_to_the_cell[as.character(totfield_per_cell_and_percentage$grID)])   # retrieve info
            index3_dependency_to_the_cell                        <- index3_dependency_to_the_cell[order(index3_dependency_to_the_cell$index3, decreasing=TRUE),]     # reorder

         
            # note that index2 = index1 * index3
            # note that sum( index2_per_cell [,1])=1000 => per mille
           
            return(index3_dependency_to_the_cell)
           }
            
            
   }
   
   

}

#-----------
plotFishingGrounds <- function (cells=cells, resx=3/60, resy=3/60, plot_type="grid", add_to=FALSE, name_export_file=file.path(getwd(),"gis_layer_test")){
      
   the_colours <-  rev(heat.colors(10))
   xrange <- range(cells[,'CELL_LONG'])
   yrange <- range(cells[,'CELL_LATI'])
   
   if(!add_to) plot(0, 0, xlim=xrange, ylim=yrange, type="n", xlab="Longitude", ylab="Latitude")
   
   if(plot_type=="grid") {   # note that it is always possible to Dissolve polygons in ArcGIS afterwards....
      rect(cells[,'CELL_LONG']-resy/2, cells [,'CELL_LATI']-resx/2, cells[,'CELL_LONG']+resy/2, cells [,'CELL_LATI']+resx/2, col=the_colours[10])
      #legend("topright", title="Dependency on revenue (%)", legend=c("10","20","30","40","50","60","70","80","90","100"), fill= heat.colors(10), bty="n")
 
      uniqueCells   <- cells[!duplicated(cells$grID),c("grID","CELL_LONG","CELL_LATI")]
      coords        <- SpatialPoints(cbind(SI_LONG=as.numeric(as.character(uniqueCells$CELL_LONG)),SI_LATI=as.numeric(as.character(uniqueCells$CELL_LATI))))
      spix          <- SpatialPixels(coords)
      spol          <- as(spix, "SpatialPolygons")
      library(sp)
      proj4string(spol) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
  
      if(nrow(cells)!=nrow(uniqueCells)){ # remove the vessel dimension
       cells   <- aggregate(cells$percent, list(cells$grID, cells$CELL_LONG, cells$CELL_LATI), mean )
       colnames(cells)  <- c("grID", "CELL_LONG", "CELL_LATI", "avpercent")
      }
   
      cells[,ncol(cells)] <- as.numeric(cells[,ncol(cells)])
   
      # export in .shp attaching the df with the levels
      IDs  <- sapply(slot(spol, "polygons"), function(x) slot(x, "ID"))
      spdf <- SpatialPolygonsDataFrame(spol, data=cbind.data.frame(data.frame(id=row.names(spol),row.names=row.names(spol)), cells))
      writePolyShape(spdf, file.path(name_export_file))
      # note that 'percent' in attribute table can be used in ArcGIS symbology to color the relative importance of the cells
      }
      
      
return()
}


# calls
cells <- delineateFishingGrounds(tacsatp,         type_agg="per_cell",    based_on="feffort", threshold_in_percent=80)
plotFishingGrounds (cells=cells, resx=3/60, resy=3/60, plot_type="grid", add_to=FALSE, name_export_file=file.path(outPath,"2015_Danish_feffort_gis_layer_80percentthreshold"))
sum(cells$percent)

cells <- delineateFishingGrounds(tacsatp,         type_agg="per_cell",    based_on="weight",  threshold_in_percent=80)
plotFishingGrounds (cells=cells, resx=3/60, resy=3/60, plot_type="grid", add_to=FALSE, name_export_file=file.path(outPath,"2015_Danish_weight_gis_layer_80percentthreshold"))
sum(cells$percent)

cells <- delineateFishingGrounds(tacsatp_value,   type_agg="per_cell",    based_on="revenue", threshold_in_percent=80)
plotFishingGrounds (cells=cells, resx=3/60, resy=3/60, plot_type="grid", add_to=FALSE, name_export_file=file.path(outPath,"2015_Danish_revenue_gis_layer_80percentthreshold"))
sum(cells$percent)

cells <- delineateFishingGrounds(tacsatp,         type_agg="per_cell_per_vessel",    based_on="feffort", threshold_in_percent=80)
plotFishingGrounds (cells=cells, resx=3/60, resy=3/60, plot_type="grid", add_to=FALSE, name_export_file=file.path(outPath,"2015_Danish_feffort_gis_layer_80percentthreshold_atleast1vessel"))
sum(cells$percent)

cells <- delineateFishingGrounds(tacsatp,         type_agg="per_cell_per_vessel",    based_on="weight", threshold_in_percent=80)
plotFishingGrounds (cells=cells, resx=3/60, resy=3/60, plot_type="grid", add_to=FALSE, name_export_file=file.path(outPath,"2015_Danish_weight_gis_layer_80percentthreshold_atleast1vessel"))
sum(cells$percent)

cells <- delineateFishingGrounds(tacsatp_value,   type_agg="per_cell_per_vessel",    based_on="revenue", threshold_in_percent=80)
plotFishingGrounds (cells=cells, resx=3/60, resy=3/60, plot_type="grid", add_to=FALSE, name_export_file=file.path(outPath,"2015_Danish_revenue_gis_layer_80percentthreshold_atleast1vessel"))
sum(cells$percent)

cells <- delineateFishingGrounds(tacsatp_value, based_on="spatialdependency1", threshold_in_percent=80)
plotFishingGrounds (cells=cells, resx=3/60, resy=3/60, plot_type="grid", add_to=FALSE, name_export_file=file.path(outPath,"2015_Danish_spatialdependency1_gis_layer_80percentthreshold"))
sum(cells$percent)

cells <- delineateFishingGrounds(tacsatp_value, based_on="spatialdependency2", threshold_in_percent=80)
plotFishingGrounds (cells=cells, resx=3/60, resy=3/60, plot_type="grid", add_to=FALSE, name_export_file=file.path(outPath,"2015_Danish_spatialdependency2_gis_layer_80percentthreshold"))
sum(cells$percent)

#=> all layers exported in ArcGIS....then use Generalization>Dissolve tool when necessary

} # end FALSE
