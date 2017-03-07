
 general <- list()
      if(.Platform$OS.type == "windows") {
        general$application           <- "DanishFleet" # ...or myfish
        general$main.path.param.gis   <- file.path("C:","Users","fbas","Documents","GitHub", paste("DISPLACE_input_gis_", general$application, sep=''))
       }

  ## ROUTINE USED FOR NON-VMS EQUIPPED VESSELS (i.e. TACSATP NON AVAILABLE)
  ## CAUTION: SOME SPATIAL LAYERS (GIS SHAPE FILES in \SpatialLayers) FOR SOME METIERS SPECIFIC TO THESE VESSELS
  ## WILL NEED TO BE INFORMED
  
  
  
  
  #!#!#!#!#!#
  #!#!#!#!#!#
  # choose your country
  ctry <- "DEN"
  #!#!#!#!#!#
  #!#!#!#!#!#
  

  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  if(ctry=="DEN"){
  
  
  #------------------------------------
  # NON VMS-equipped vessels---------------
  #------------------------------------
 
  load(file.path(general$main.path.param.gis, "FISHERIES",
           #paste("eflalo_","2015",".RData",sep='')))
           paste("logbooks_DNK_","2015",".RData",sep='')))
      logbooks  <- logbooks[grep("DNK", as.character(logbooks$VE_REF)),]

    
    logbooks              <- subset(logbooks,FT_REF != 0)
  
   
  }
  
 
 
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ICESrectangle2LonLat <- function (statsq, midpoint = FALSE) # vmstools 
{
    latpart <- substr(statsq, 1, 2)
    lonpart <- substr(statsq, 3, 4)
    latlabels <- sprintf("%02i", 1:99)
    lat.mids <- seq(36, 85, 0.5) + 0.25
    lat.idx <- match(latpart, latlabels)
    lat <- lat.mids[lat.idx]
    lonlabels <- paste(rep(LETTERS[c(2:8, 10:13)], each = 10), 
        rep(0:9, 7), sep = "")
    lonlabels <- c("A0", "A1", "A2", "A3", lonlabels[-length(lonlabels)])
    lon.mids <- -44:68 + 0.5
    lon.idx <- match(lonpart, lonlabels)
    lon <- lon.mids[lon.idx]
    failed.codes <- is.na(lat) | is.na(lon)
    if (any(failed.codes)) {
        warning("Some stat squares are not valid. Please check the help files for ICESrectangle2LonLat() for more information about the formal definition of valid ICES rectangles.")
        lat[failed.codes] <- NA
        lon[failed.codes] <- NA
    }
    if (midpoint == FALSE) {
        lat <- lat - 0.25
        lon <- lon - 0.5
    }
    return(data.frame(SI_LATI = lat, SI_LONG = lon))
}

  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   # make the port names consistent with the graph construction
   # by substituting the port names in raw input data with port names from the graph
   
 
  port_names <- read.table(file=file.path(general$main.path.param.gis, "GRAPH",
                                  paste("harbours.dat", sep='')), sep=";", header=TRUE)


  library(vmstools)
  logbooks <- cbind(logbooks, ICESrectangle2LonLat(logbooks$LE_RECT, midpoint=TRUE))
   
  logbooks$SI_HARB         <- factor(logbooks$FT_LHAR)
  coord_ports             <- logbooks[!duplicated(logbooks$SI_HARB), c('SI_LATI',   'SI_LONG', 'SI_HARB')] 
  coord_ports             <- coord_ports[!is.na(coord_ports$SI_HARB) |  coord_ports$SI_HARB==" ",]

  # brute force to link nodes to harbours (with euclidian distance)
  idx <- rep(0, nrow(coord_ports))
  for(i in 1:nrow(coord_ports)){
    idx[i] <- which.min ( sqrt( ((coord_ports[i, "SI_LONG"] -   port_names  [, "lon"])^2) +  (((coord_ports[i, "SI_LATI"] -   port_names  [, "lat"]))^2)) )
    print(i)
  }
  coord_ports$harbour <- row.names(port_names)[idx]
  
  # then, substitute...
  rownames (coord_ports)  <- coord_ports$SI_HARB
  levels(logbooks$SI_HARB) <- coord_ports[levels(factor(logbooks$SI_HARB)), "harbour"]
 
  #check 
  table(is.na(logbooks$SI_HARB)) # =>some non-declared landings ports, or some record not merged with sales slips?
  
  logbooks <- logbooks[!is.na(logbooks$SI_HARB),]
  
  
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   # relevant vessels only i.e. active in the area....
  if(FALSE){
     # keep only the vessels fishnig in the western Baltic  (and kattegat because her.3a22, and East baltic because spr.2232)
    library(maptools)
    handmade            <- readShapePoly(file.path(general$main.path.param.gis, "MANAGEMENT", "wbaltic_wgs84"))  # build in ArcGIS 10.1
    the_area            <- sapply(slot(handmade, "polygons"), function(x) lapply(slot(x, "Polygons"), function(x) x@coords)) # tricky there...
    in_area             <- point.in.polygon(logbooks[,'SI_LONG'],logbooks[,'SI_LATI'], the_area[[1]][,1],the_area[[1]][,2])

    # then subset here...
    vid_this_case_study <- as.character(unique(logbooks$VE_REF[in_area>0]))
    cat(paste(length(vid_this_case_study), " vessels in the area over ", length(unique(logbooks$VE_REF)), " in total" , "\n"))
    logbooks      <- logbooks[logbooks$VE_REF %in% vid_this_case_study,]
    }
    
    
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   # relevant vessels only i.e. with some recorded catches on the subsetted stocks....
  
    agg     <- aggregate(logbooks[, grep('LE_KG_', colnames(logbooks))], list(logbooks$VE_REF), sum, na.rm=TRUE)
    agg$tot <- apply(agg[,-1], 1, sum, na.rm=TRUE)
    vid_with_no_landings_for_these_stocks <- as.character(agg[agg$tot<=0, 1])
    
    # then subset here...
    cat(paste(length(vid_with_no_landings_for_these_stocks), " vessels to remove over ", length(unique(logbooks$VE_REF)), " in total" , "\n"))
    logbooks      <- logbooks[!logbooks$VE_REF %in% vid_with_no_landings_for_these_stocks,]
   
    ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   # relevant vessels only i.e. NON-VMS EQUIPPED
  
    vid_smaller_than_12m_which_are_not_vms_equipped_by_definition <- unique(as.character(logbooks[logbooks$VE_LEN<=12, 1]))
    
    # then subset here...
    cat(paste(length(vid_smaller_than_12m_which_are_not_vms_equipped_by_definition), " vessels to keep over ", length(unique(logbooks$VE_REF)), " in total" , "\n"))
    logbooks      <- logbooks[logbooks$VE_REF %in% vid_smaller_than_12m_which_are_not_vms_equipped_by_definition,]
 
    ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   # relevant vessels only i.e. NON-VMS EQUIPPED
  
    ctries <- "DNK"
    vid_from_ctries_of_interest <- unique(as.character(logbooks$VE_REF[grep(ctries,logbooks$VE_REF)]))
    
    # then subset here...
    cat(paste(length(vid_from_ctries_of_interest), " vessels to keep over ", length(unique(logbooks$VE_REF)), " in total" , "\n"))
    logbooks      <- logbooks[logbooks$VE_REF %in% vid_from_ctries_of_interest,]
    
    logbooks$VE_REF <- factor(logbooks$VE_REF)
    
    
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   # play with metier categorisation
 
  # rough simplication:
  logbooks$LE_MET_rough <- factor(substr(logbooks$LE_MET, 1,3)) # init
  levels(logbooks$LE_MET_rough)
  ##[1] "DRB" "FPO" "GNS" "GTR" "LHP" "LLD" "LLS" "MIS" "No_" "OTB" "OTM" "OTT" "PS_" "PTB" "PTM" "SDN" "SSC"
 
  # rename
  levels(logbooks$LE_MET_rough) [levels(logbooks$LE_MET_rough) %in% c("DRB", "FPO", "GNS", "LLD", "No_", "GTR", "LHP", "LLS", "MIS" )] <- "Passive"
  levels(logbooks$LE_MET_rough) [levels(logbooks$LE_MET_rough) %in% c("OTB","OTT",  "OTM", "PTB", "PTM" )]       <- "Trawl"
  levels(logbooks$LE_MET_rough) [levels(logbooks$LE_MET_rough) %in% c("PS_", "SDN", "SSC")]               <- "Seine"
    
  # find effort in hours
  logbooks$FT_DDATIM  <- as.POSIXct(paste(logbooks$FT_DDAT,logbooks$FT_DTIME, sep = " "),
                               tz = "GMT", format = "%d/%m/%Y  %H:%M")
  logbooks$FT_LDATIM  <- as.POSIXct(paste(logbooks$FT_LDAT,logbooks$FT_LTIME, sep = " "),
                               tz = "GMT", format = "%d/%m/%Y  %H:%M")
  logbooks$LE_EFF         <- an(difftime(logbooks$FT_LDATIM, logbooks$FT_DDATIM, units="hours"))
  
  # keep level 6 and built an OTHER metier
  levels(logbooks$LE_MET)
  agg_per_met                 <- tapply(logbooks$LE_EFF, list(logbooks$LE_MET), sum, na.rm=TRUE)
  agg_per_met_ordered         <- agg_per_met[order(agg_per_met, decreasing =TRUE)]
  agg_per_met_ordered_and_cum <- cumsum(agg_per_met_ordered/sum(agg_per_met_ordered, na.rm=TRUE)*100)
  met_to_keep                 <- names(agg_per_met_ordered_and_cum[agg_per_met_ordered_and_cum<90])   #  the ones that cumulatively cover 90% of the effort
  met_to_keep                 <- met_to_keep[!is.na(met_to_keep)]
 
  
  levels(logbooks$LE_MET)[!levels(logbooks$LE_MET)%in% met_to_keep]  <- "other_non_vms_equipped"
  levels(logbooks$LE_MET)[levels(logbooks$LE_MET)%in% "No_Matrix6"]  <- "other_non_vms_equipped"
  
   # debug metier names for impossible symbols in file names
  levels(logbooks$LE_MET) <- gsub(">=", "o", levels(logbooks$LE_MET))  
  levels(logbooks$LE_MET) <- gsub("<",  "u", levels(logbooks$LE_MET))  
  levels(logbooks$LE_MET) <- gsub(">",  "o", levels(logbooks$LE_MET))  
  levels(logbooks$LE_MET) <- gsub("-",  "_", levels(logbooks$LE_MET))  
 
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 
  logbooks$nb_vessels <- 1 # a trick to retrieve the mean from the aggregate sum
   

 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!AGGREGATE (UTILS)!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   c.listquote <- function (...)
   {
    args <- as.list(match.call()[-1])
    lstquote <- list(as.symbol("list"))
    for (i in args) {
        if (class(i) == "name" || (class(i) == "call" && i[[1]] !=
            "list")) {
            i <- eval(substitute(i), sys.frame(sys.parent()))
        }
        if (class(i) == "call" && i[[1]] == "list") {
            lstquote <- c(lstquote, as.list(i)[-1])
        }
        else if (class(i) == "character") {
            for (chr in i) {
                lstquote <- c(lstquote, list(parse(text = chr)[[1]]))
            }
        }
        else stop(paste("[", deparse(substitute(i)), "] Unknown class [",
            class(i), "] or is not a list()", sep = ""))
    }
    return(as.call(lstquote))
   }
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

 library(data.table)
 nm <- names(logbooks)
 idx.col.sp <- grep('LE_KG_', nm)
 idx.col.e  <- grep('LE_EFF', nm)
 idx.col.lg  <- grep('VE_LEN', nm)
 idx.col.kw  <- grep('VE_KW', nm)
 idx.col.nb  <- grep('nb_vessels', nm)
 idx.col <- c(idx.col.sp, idx.col.e, idx.col.lg, idx.col.kw, idx.col.nb)
 DT  <- data.table(logbooks) # library data.table for fast grouping replacing aggregate()
 # AGGREGATE PER SPECIES ------> SUM
 eq1  <- c.listquote( paste ("sum(",nm[idx.col],",na.rm=TRUE)",sep="") )
 logbooks_agg <- DT[,eval(eq1),by=list(VE_REF,SI_HARB, LE_MET, LE_MET_rough)]
 logbooks_agg <- data.frame(logbooks_agg)
 colnames(logbooks_agg) <- c("VE_REF","SI_HARB", "LE_MET","LE_MET_rough", paste(nm[idx.col.sp], sep=''), "LE_EFF", "VE_LEN", "VE_KW", "nb_vessels")
 logbooks_agg <- logbooks_agg[order(logbooks_agg$LE_MET),] # order




  logbooks_agg$VE_LEN <-     logbooks_agg$VE_LEN / logbooks_agg$nb_vessels
  logbooks_agg$VE_KW <-     logbooks_agg$VE_KW / logbooks_agg$nb_vessels

  # then correct  (given it is individual data)
  logbooks_agg$nb_vessels <- 1

 
  #but caution the true nb of vessels per occurence is
  #nbvessels.per.met.per.harb <-  aggregate(tacsatp$VE_REF, list(tacsatp$VE_REF, tacsatp$SI_HARB, tacsatp$LE_MET_level6), function(x) length(unique(x)), simplify = FALSE)
  #colnames(nbvessels.per.met.per.harb) <-  c("VE_REF",'SI_HARB', 'LE_MET', 'nbvessels')
  #tacsatp_agg                    <- merge(tacsatp_agg, nbvessels.per.met.per.harb) # map


  # compute kg_per_h cpues
  # (Caution: catch will be slightly underestimated for these vessels 
  # because the present effort is not only fishing effort but the total trip effort instead....)
  nm <- colnames(logbooks_agg)
  logbooks_agg <- logbooks_agg[logbooks_agg$LE_EFF!=0,]
  logbooks_agg[, grep('LE_KG_', nm)]          <- logbooks_agg[, grep('LE_KG_', nm)]   /   logbooks_agg$LE_EFF
  colnames(logbooks_agg)[grep('LE_KG_', nm)]  <- paste(gsub('LE_KG_', '', nm[grep('LE_KG_', nm)]), '_kg_h', sep='')
  
  # add other features such as:
   # fishing speed knots	cruise speed knots	fuel cons h	ave storage fish kg	fuel tank liter	trip duration h	multip fuel steaming	multip fuel fishing	multip fuel ret port fish	multip fuel inactive	range km fish ground	
   # fuel_price_Euro/liter	weekEndStartDay	WeekEndEndDay	WorkHoursStart	WorkHoursEnd
   logbooks_agg$mean_LOA_m          <-  logbooks_agg$VE_LEN
   logbooks_agg$mean_kW             <-  logbooks_agg$VE_KW
   
   logbooks_agg$"fishing speed knots" <- 4
   logbooks_agg$"cruise speed knots"  <- 10
  
   # i.e. fuel cons
   table.fuelcons.per.engine       <-  read.table(file= file.path(general$main.path.param.gis, "FISHERIES", "IBM_datainput_engine_consumption.txt"), header=TRUE,sep="")
   linear.model                    <-  lm(calc_cons_L_per_hr_max_rpm~ kW2, data=table.fuelcons.per.engine)  # conso = a*Kw +b   # to guess its fuel consumption at maximal speed
   logbooks_agg$fuel.cons.h         <-  predict(linear.model, newdata=data.frame(kW2=as.numeric(as.character(logbooks_agg$VE_KW)))) # Liter per hour
  
   # i.e. fuel tank capacity
   logbooks$fuel_cons_rate         <-  predict(linear.model, newdata=data.frame(kW2=as.numeric(as.character(logbooks$VE_KW)))) # Liter per hour
   logbooks$fuelcons               <- as.numeric(as.character(logbooks$fuel_cons_rate)) * (as.numeric(as.character(logbooks$LE_EFF))) # liter per hour
   DT                    <- data.table(logbooks) 
   eq1                   <- c.listquote( paste ("sum(","fuelcons",", na.rm=TRUE)",sep="") )
   DT$VE_REF             <- as.factor(DT$VE_REF)
   x.agg                 <- DT[,eval(eq1),by=list(VE_REF,FT_REF)]
   x.agg                 <- data.frame( x.agg)
   colnames(x.agg)       <- c("VE_REF", "FT_REF", "totcons")
   tank.capacity.per.vid <- tapply(x.agg$totcons, x.agg$VE_REF, function(x) {x<- x[x<100000]; quantile(x, probs=0.9)})# {x<- x[x<100000]; quantile(x, probs=0.99)})
  
   cap                             <- tank.capacity.per.vid[!is.na(tank.capacity.per.vid)]
   vessel.length.per.vid           <- logbooks[!duplicated(logbooks$VE_REF), c("VE_REF","VE_LEN")]
   rownames(vessel.length.per.vid) <- vessel.length.per.vid [,"VE_REF"]
   dd                              <- cbind(cap=cap, VE_LEN=vessel.length.per.vid[names(cap),"VE_LEN"])
   #plot(dd [,'VE_LEN'], dd [,'cap'])
   nls1                            <- nls(cap~ b*VE_LEN^a, data=as.data.frame(dd), start=list(a=1, b=100))
   #points(dd [,'VE_LEN'], predict(nls1), col=4)
   logbooks_agg$fuel.tank.liter     <-   round ( predict(nls1, logbooks_agg) ) 
  

  
   # i.e. fish storage capacity
   DT         <- data.table(logbooks)
   nm         <- colnames(logbooks)
   idx.col    <- grep("KG", nm)
   eq1        <- c.listquote( paste ("sum(",nm[idx.col],",na.rm=TRUE)",sep="") )
   DT$VE_REF  <- as.factor(DT$VE_REF)
   x.agg      <- DT[,eval(eq1),by=list(VE_REF, FT_REF)]
   x.agg      <- data.frame(x.agg)
   x.agg$tot_landings              <- apply(x.agg[, -c(1:2)], 1, sum, na.rm=TRUE)   # kg
   carrying.capacity.per.vid       <- tapply(x.agg$tot_landings, x.agg$VE_REF, function(x) {; quantile(x, probs=0.9, na.rm=TRUE)})  
   cap                             <- carrying.capacity.per.vid[!is.na(carrying.capacity.per.vid)]
   vessel.length.per.vid           <- logbooks[!duplicated(logbooks$VE_REF), c("VE_REF","VE_LEN")]
   rownames(vessel.length.per.vid) <- vessel.length.per.vid [,"VE_REF"]
   dd                              <- cbind(cap=cap, VE_LEN=vessel.length.per.vid[names(cap),"VE_LEN"])
   dd                              <- dd[dd[,'VE_LEN']<11.0,] # CAUTION if LOA<11m bc nls too hard to converge otherwise 
   #plot(dd [,'VE_LEN'], dd [,'cap'])
   nls2                            <- nls(cap~ b*VE_LEN^a, data=as.data.frame(dd), start=list(a=1, b=100))
   #points(dd [,'VE_LEN'], predict(nls2), col=4)
   logbooks_agg$ave.storage.fish.kg <-  round ( predict(nls2, logbooks_agg) ) 
  
    
   logbooks_agg$nb_pings_per_trip   <-  10
   logbooks_agg$Gamma_shape         <-  0.4485   # Gamma shape # not used if GoFishing dtree activated
   logbooks_agg$Gamma_scale         <-  336.7618 # Gamma scale # not used if GoFishing dtree activated
   logbooks_agg$trip.duration.h     <-  20 # not really important if StopFishing dtree activated with EndOfTheDay
   logbooks_agg$multip.fuel.steaming<-  1
   logbooks_agg$multip.fuel.fishing <-  0
   logbooks_agg[logbooks_agg$LE_MET_rough %in% c('Dredge', 'Trawl', 'Seine'), 'multip.fuel.fishing'] <- 0.9
   logbooks_agg$multip.fuel.ret.port.fish <- 1.1
   logbooks_agg$multip.fuel.inactive<- 0.15
   logbooks_agg$weekEndStartDay     <-  5
   logbooks_agg$WeekEndEndDay       <-  6
   logbooks_agg$WorkHoursStart      <-  5
   logbooks_agg$WorkHoursEnd        <-  22
   
   logbooks_agg$vessel_range_km     <- 30
   logbooks_agg[logbooks_agg$LE_MET_rough %in% c('Dredge', 'Trawl', 'Seine'), 'vessel_range_km'] <- 50
   
   logbooks_agg$name_gis_file_for_fishing_effort_per_polygon <- paste(substr(as.character(logbooks_agg$VE_REF), 1,3), "_gis_feffort_", logbooks_agg$LE_MET, sep='')
   logbooks_agg$name_gis_layer_field                         <- "feffort"                     # giving releative  e.g. in 5 categories: 1 to 5 with 1 high occurence or absoulte effort ditribtion
   logbooks_agg$is_gis_layer_field_relative_numbers          <- FALSE                           # if relative effort categories (e.g. high to low) then xfold_gis_layer_field will be used to convert in absolute
   logbooks_agg$xfold_gis_layer_field                        <- "1"                            # giving relative importance of the 5 categories e.g. visting an area of cat 1 is 10000 times more probable than for cat 5
                             

   logbooks_agg$Region <- "DanishWaters"
   logbooks_agg$Harbor <- logbooks_agg$SI_HARB
   logbooks_agg$metier <- logbooks_agg$LE_MET
   logbooks_agg$vid    <- logbooks_agg$VE_REF
   logbooks_agg$"N. of vessels"  <- 1
   logbooks_agg$Crew <- NA
   logbooks_agg$mean_GT <- NA
   
   logbooks_agg$FIRM    <- paste("FIRM", 1:nrow(logbooks_agg))
   logbooks_agg$firm_id <- 1:nrow(logbooks_agg)
  
  # finally, export!
  nameobj           <- paste("vessels_specifications_per_harbour_metiers_",ctry,"_from_logbooks_only.csv",sep='')  #....and possibly per vid!
  
  logbooks_agg <- logbooks_agg[!is.na(logbooks_agg$SI_HARB),]
  
  #Region,Harbor,metier,N. of vessels,Crew,mean_LOA_m,mean_GT,mean_kW,hake_kg_h,sole_kg_h,redmullet_kg_h,mantis_kg_h,fishing speed knots,cruise speed knots,fuel cons h,ave storage fish kg,fuel tank liter,trip duration h,multip  fuel steaming,multip fuel fishing,multip fuel ret port fish,multip fuel inactive,range km fish ground,fuel_price_Euro/liter,weekEndStartDay,WeekEndEndDay,WorkHoursStart,WorkHoursEnd
  
   write.table(logbooks_agg, file.path(general$main.path.param.gis, "FISHERIES", nameobj), row.names=FALSE, sep=";", quote=FALSE)


   
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  # FINAL STEP, ONCE ALL COUNTRIES INFORMED THEN COMBINE ALL COUNTRIES
  countries <- c('DEN')
  logbooks   <- NULL
  for (ctry in countries){

     nameobj           <- paste("vessels_specifications_per_harbour_metiers_",ctry,"_from_logbooks_only.csv",sep='')  #....and possibly per vid!
     logbooks_this_ctry <- read.table(file.path(general$main.path.param.gis, "FISHERIES", nameobj), header=TRUE, sep=";")
  
     if(is.null(logbooks)){
        logbooks <- logbooks_this_ctry 
     }else{
  
        nm                          <- intersect(colnames(logbooks), colnames(logbooks_this_ctry))
        nm_not_in_logbooks           <- setdiff(colnames(logbooks_this_ctry), colnames(logbooks))
        nm_not_in_logbooks_this_ctry <- setdiff(colnames(logbooks), colnames(logbooks_this_ctry))

        logbooks <- rbind.data.frame(
                        logbooks[,nm],
                         logbooks_this_ctry[,nm]
                         )
     }
  }

  # keep only DNK vessels
 logbooks <- logbooks[c(grep("DEN", logbooks$VE_REF)),]
 
 # finally, export!
 nameobj <- "vessels_specifications_per_harbour_metiers_from_logbooks_only.csv"  #....and possibly per vid!
  
  #for example: Region,Harbor,metier,N. of vessels,Crew,mean_LOA_m,mean_GT,mean_kW,hake_kg_h,sole_kg_h,redmullet_kg_h,mantis_kg_h,fishing speed knots,cruise speed knots,fuel cons h,ave storage fish kg,fuel tank liter,trip duration h,multip fuel steaming,multip fuel fishing,multip fuel ret port fish,multip fuel inactive,range km fish ground,fuel_price_Euro/liter,weekEndStartDay,WeekEndEndDay,WorkHoursStart,WorkHoursEnd
  
 write.table(logbooks, file.path(general$main.path.param.gis, "FISHERIES", nameobj), row.names=FALSE, sep=";", quote=FALSE)

