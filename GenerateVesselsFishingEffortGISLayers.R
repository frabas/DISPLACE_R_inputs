
  inPath  <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis","balticRTI","FISHERIES")
  outPath <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis","balticRTI","FISHERIES")


  ctry <- "DNK"
  load(file=file.path(inPath,"all_merged_weight_DEN_2015.RData"))
  tacsatp_den <- all.merged
  tacsatp_den <- tacsatp_den[!is.na(as.numeric(as.character(tacsatp_den$SI_LONG))) &  !is.na(as.numeric(as.character(tacsatp_den$SI_LATI))), ]
  tacsatp_den$SI_LONG <- as.numeric(as.character(tacsatp_den$SI_LONG))
  tacsatp_den$SI_LATI <- as.numeric(as.character(tacsatp_den$SI_LATI))
  tacsatp_den$SI_STATE <- as.numeric(as.character(tacsatp_den$SI_STATE))
  tacsatp_den$LE_EFF_VMS <- as.numeric(as.character(tacsatp_den$LE_EFF_VMS)) /60
  tacsatp_den <- tacsatp_den[tacsatp_den$SI_STATE==1, ]  # keep fishing positions only
  tacsatp_den$ctry <- "DNK"


  ctry <- "SWE"
  load(file=file.path(inPath,"Displace2015_tacsat_swe_v2.RData"))
  tacsatp_swe <- tacsat.swe
  tacsatp_swe$SI_LONG <- as.numeric(as.character(tacsatp_swe$SI_LONG))
  tacsatp_swe$SI_LATI <- as.numeric(as.character(tacsatp_swe$SI_LATI))
  tacsatp_swe$SI_STATE <- as.numeric(as.character(tacsatp_swe$SI_STATE))
  tacsatp_swe$LE_MET_level6 <- tacsatp_swe$VE_MET  
  format_date <- "%Y-%m-%d %H:%M:%S" 
  tacsatp_swe$SI_DATIM <- as.POSIXct( tacsatp_swe$SI_DATIM, tz='GMT',   format_date)
  tacsatp_swe$LE_EFF_VMS <- abs(c(0, as.numeric( tacsatp_swe[-nrow( tacsatp_swe),"SI_DATIM"] - tacsatp_swe[-1,"SI_DATIM"], units="hours")))
  start.trip <- c(1,diff( tacsatp_swe[,"FT_REF"]))
  tacsatp_swe$all_effort <- tacsatp_swe$LE_EFF_VMS  # save...
  tacsatp_swe[start.trip!=0, "LE_EFF_VMS"] <- 0  # just correct for the trip change points
  tacsatp_swe$LE_EFF_VMS <- as.numeric(as.character(tacsatp_swe$LE_EFF_VMS))
  tacsatp_swe <- tacsatp_swe[!is.na(as.numeric(as.character(tacsatp_swe$SI_LONG))) &  !is.na(as.numeric(as.character(tacsatp_swe$SI_LATI))), ]
  tacsatp_swe <- tacsatp_swe[tacsatp_swe$SI_STATE==1, ]  # keep fishing positions only
  tacsatp_swe$ctry <- "SWE"

   
  nm <- intersect(colnames(tacsatp_den), colnames(tacsatp_swe))
  tacsatp <- rbind.data.frame(
                        tacsatp_den[,nm],
                         tacsatp_swe[,nm] #,
                         # tacsatp_ger[, nm]
                         )
  tacsatp <- tacsatp[c(grep("DNK", tacsatp$VE_REF), grep("DEU",tacsatp$VE_REF), grep("SWE", tacsatp$VE_REF)),]
  tacsatp$ctry <- factor(tacsatp$ctry)





  xrange <- range(as.numeric(as.character(tacsatp$SI_LONG)), na.rm=TRUE)
  yrange <- range(as.numeric(as.character(tacsatp$SI_LATI)), na.rm=TRUE)
  #xrange <- c(7,17)
  #yrange <- c(53,63)

  # Set grid
  library(vmstools)
  resx <- 3/60 #3 minute
  resy <- 3/60 #3 minute
  grd <- createGrid(xrange,yrange,resx=resx,resy=resy,type="SpatialGrid",exactBorder=T)

  # Grid all tacsatSweptArea data
  # Convert all tacsat poins first to SpatialPoints
  coords <- SpatialPoints(cbind(SI_LONG=as.numeric(as.character(tacsatp$SI_LONG)),SI_LATI=as.numeric(as.character(tacsatp$SI_LATI))))
  idx <- over(coords,grd)
  tacsatp$grID <- idx

  # Remove records that are not in the study area
  tacsatp <- subset(tacsatp,is.na(grID)==F)

  # add quarter info
  tacsatp$quarter <- quarter(tacsatp$SI_DATE)

  
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   # relevant vessels only i.e. active in the area....
  
     # keep only the vessels fishnig in the western Baltic  (and kattegat because her.3a22, and East baltic because spr.2232)
    library(maptools)
    handmade            <- readShapePoly(file.path(inPathManagement, "wbaltic_wgs84"))  # build in ArcGIS 10.1
    the_area            <- sapply(slot(handmade, "polygons"), function(x) lapply(slot(x, "Polygons"), function(x) x@coords)) # tricky there...
    in_area             <- point.in.polygon(tacsatp[,'SI_LONG'],tacsatp[,'SI_LATI'], the_area[[1]][,1],the_area[[1]][,2])

    # then subset here...
    vid_this_case_study <- as.character(unique(tacsatp$VE_REF[in_area>0]))
    cat(paste(length(vid_this_case_study), " vessels in the area over ", length(unique(tacsatp$VE_REF)), " in total" , "\n"))
    tacsatp      <- tacsatp[tacsatp$VE_REF %in% vid_this_case_study,]
  
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   # relevant vessels only i.e. with some recorded catches on the subsetted stocks....
  
     # keep only the vessels fishnig in the western Baltic  (and kattegat because her.3a22, and East baltic because spr.2232)
   
    agg     <- aggregate(tacsatp[, grep('LE_KG_', colnames(tacsatp))], list(tacsatp$VE_REF), sum, na.rm=TRUE)
    agg$tot <- apply(agg[,-1], 1, sum, na.rm=TRUE)
    vid_with_no_landings_for_these_stocks <- as.character(agg[agg$tot<=0, 1])
    
    # then subset here...
    cat(paste(length(vid_with_no_landings_for_these_stocks), " vessels to remove over ", length(unique(tacsatp$VE_REF)), " in total" , "\n"))
    tacsatp      <- tacsatp[!tacsatp$VE_REF %in% vid_with_no_landings_for_these_stocks,]
   
  
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  # play with metier categorisation 
  # rough simplication:
  tacsatp$LE_MET_rough <- factor(substr(tacsatp$LE_MET_level6, 1,3)) # init
  levels(tacsatp$LE_MET_rough)
  ##[1] "DRB" "FPO" "GNS" "GTR" "LHP" "LLD" "LLS" "MIS" "No_" "OTB" "OTM" "OTT" "PS_" "PTB" "PTM" "SDN" "SSC"
  levels(tacsatp$LE_MET_rough) <- c("Passive", "Passive", "Passive", "Passive", "Passive", "Passive", "Trawl","Trawl", "Trawl", "Trawl", "Seine", "Trawl", "Trawl","Seine","Seine")

  # keep level 6 and built an OTHER metier
  levels(tacsatp$LE_MET_level6)
  agg_per_met                 <- tapply(tacsatp[tacsatp$SI_STATE==1, ]$LE_EFF_VMS, list(tacsatp[tacsatp$SI_STATE==1, ]$LE_MET_level6), sum, na.rm=TRUE)
  agg_per_met_ordered         <- agg_per_met[order(agg_per_met, decreasing =TRUE)]
  agg_per_met_ordered_and_cum <- cumsum(agg_per_met_ordered/sum(agg_per_met_ordered, na.rm=TRUE)*100)
  met_to_keep                 <- names(agg_per_met_ordered_and_cum[agg_per_met_ordered_and_cum<90])   #  the ones that cumulatively cover 90% of the effort
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
  
  


  # Aggregate the results by metier and grid ID (aggregate() can be slow: be patient)
  feffort    <- "LE_EFF_VMS"

  tacsatp[,c(feffort)] <- as.numeric(as.character(tacsatp[,c(feffort)]))
  #aggtacsatp <- aggregate(tacsatp[,c(feffort)], by=list(tacsatp$LE_MET, tacsatp$grID,tacsatp$quarter),sum,na.rm=T)
  #colnames(aggtacsatp)[1:3] <- c("LE_MET","grID", "quarter")
  #colnames(aggtacsatp)[4]   <- "feffort"
  aggtacsatp <- aggregate(tacsatp[,c(feffort)], by=list(tacsatp$LE_MET_level6, tacsatp$grID,tacsatp$ctry),sum,na.rm=T)
  colnames(aggtacsatp)[1:3] <- c("LE_MET","grID", "ctry")
  colnames(aggtacsatp)[4]   <- "feffort"

  #- Add midpoint of gridcell to dataset
  aggResult <- cbind(aggtacsatp, CELL_LONG=coordinates(grd)[aggtacsatp$grID,1], CELL_LATI=coordinates(grd)[aggtacsatp$grID,2])
  save(aggResult, file=file.path(outPath,"2015_aggtacsatp.RData"))


  # loop over relevant activity/metier
  for (ctry in levels(aggResult$ctry)){
   for (met  in levels(aggResult$LE_MET)){
   # for (quarter in  unique(aggResult$quarter)){
     #nameobj <- paste(ctry, "_gis_feffort_", met, "_", "quarter", quarter, sep="")
     nameobj <- paste(ctry, "_gis_feffort_", met, sep="")


     #aggResultSub <- aggResult[aggResult$LE_MET==met & aggResult$quarter==quarter & aggResult$ctry==ctry,]
     aggResultSub <- aggResult[aggResult$LE_MET==met  & aggResult$ctry==ctry,]
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
     IDs  <- sapply(slot(grdc2plot, "polygons"), function(x) slot(x, "ID"))
     spdf <- SpatialPolygonsDataFrame(grdc2plot, aggResultSub)
     writePolyShape(spdf, file.path(outPath, nameobj))
     } # if nrow!=0

     }
     #}
     }




