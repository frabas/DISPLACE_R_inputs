
 general <- list()
      if(.Platform$OS.type == "windows") {
        general$application           <- "balticRTI" # ...or myfish
        general$main.path.param.gis   <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis", general$application)
       }


  
  #!#!#!#!#!#
  #!#!#!#!#!#
  # choose your country
  ctry <- "DEN"
  #!#!#!#!#!#
  #!#!#!#!#!#
  

  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  if(ctry=="DEN"){
  #load(file=file.path(general$main.path.param.gis, "FISHERIES", "all_merged_weight_DEN_2015.RData"))
  load(file=file.path(general$main.path.param.gis, "FISHERIES", "coupled_VMS_logbooks_DNK_2015.RData"))
  tacsatp_den           <- coupled_VMS_logbooks
  
  # look at the data format obtained from MergingVMS2Logbooks.R => head(coupled_VMS_logbooks,2)
  #   VE_REF  FT_REF VE_FLT     LE_MET_level6 LE_GEAR SI_LATI SI_LONG SI_SP SI_HE SI_STATE    SI_DATE SI_TIME SI_HARB LE_KG_COD LE_KG_CSH LE_KG_DAB LE_KG_ELE LE_KG_FLE LE_KG_HAD LE_KG_HER LE_KG_HKE LE_KG_HOM LE_KG_LEM LE_KG_MAC
  #DNK000001391 3386276 fleet1 GNS_DEF_>=157_0_0      GN   54.94 10.7087     0    76        2 13/02/2015   10:28    NA55        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA     NA
  #DNK000001391 3386276 fleet1 GNS_DEF_>=157_0_0      GN  55.024 10.7487   2.8     6        2 13/02/2015   11:28      NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA     NA
  #     LE_KG_MON LE_KG_MUS LE_KG_NEP LE_KG_NOP LE_KG_OYF LE_KG_PLE LE_KG_POK LE_KG_PRA LE_KG_SAN LE_KG_SOL LE_KG_SPR LE_KG_TUR LE_KG_WHB LE_KG_WHG LE_KG_WIT LE_EFF_VMS KW_HOURS flag
  #        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA          0        0    4
  #        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA         60      128    4

  tacsatp_den$SI_LONG   <- as.numeric(as.character(tacsatp_den$SI_LONG))
  tacsatp_den$SI_LATI   <- as.numeric(as.character(tacsatp_den$SI_LATI))
  tacsatp_den$SI_STATE  <- as.numeric(as.character(tacsatp_den$SI_STATE))
  tacsatp_den           <- tacsatp_den[!is.na(as.numeric(as.character(tacsatp_den$SI_LONG))) &  !is.na(as.numeric(as.character(tacsatp_den$SI_LATI))), ]

  # retrieve the missing info for LEN (vessel length) and kW (vessel engine power) from logbooks EFLALO
  load(file.path(general$main.path.param.gis, "FISHERIES",
           #paste("eflalo_","2015",".RData",sep='')))
           paste("logbooks_DNK_","2015",".RData",sep='')))
      logbooks  <- logbooks[grep("DNK", as.character(logbooks$VE_REF)),]

      x <- logbooks ; rm(logbooks); gc(reset=TRUE)

    x              <- subset(x,FT_REF != 0)
    vessel.length.per.vid <- x[!duplicated(x$VE_REF),c("VE_REF","VE_LEN")]
    tacsatp_den$VE_LEN       <- round(vessel.length.per.vid [match( tacsatp_den$VE_REF,vessel.length.per.vid$VE_REF), "VE_LEN"], 0) # map

    vessel.kw.per.vid <- x[!duplicated(x$VE_REF),c("VE_REF","VE_KW")]
    tacsatp_den$VE_KW       <- round(vessel.kw.per.vid [match( tacsatp_den$VE_REF,vessel.kw.per.vid$VE_REF), "VE_KW"], 0) # map



    tacsatp_den$nb_vessels <- 1 # a trick to retrieve the mean from the aggregate sum
    tacsatp_den$LE_EFF_VMS <- as.numeric(as.character(tacsatp_den$LE_EFF_VMS)) / 60 # convert in hour
    tacsatp_den$all_effort <- tacsatp_den$LE_EFF_VMS  # save...
    tacsatp_den[tacsatp_den$SI_STATE==2, "LE_EFF_VMS"] <- 0 # remove effort if non-fishing points
  
  tacsatp <- tacsatp_den
  }
  
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  if(ctry=="SWE"){
  #load(file=file.path(general$main.path.param.gis, "FISHERIES", "Displace2015_tacsat_swe_v2.RData"))
  load(file=file.path(general$main.path.param.gis, "FISHERIES", "coupled_VMS_logbooks_SWE_2015.RData"))
  tacsatp_swe           <- tacsat.swe
  tacsatp_swe$SI_LONG   <- as.numeric(as.character(tacsatp_swe$SI_LONG))
  tacsatp_swe$SI_LATI   <- as.numeric(as.character(tacsatp_swe$SI_LATI))
  tacsatp_swe$SI_STATE  <- as.numeric(as.character(tacsatp_swe$SI_STATE))
  
  tacsatp_swe$LE_MET_level6 <- tacsatp_swe$VE_MET
  
  format_date <- "%Y-%m-%d %H:%M:%S" 
  tacsatp_swe$SI_DATIM <- as.POSIXct( tacsatp_swe$SI_DATIM, tz='GMT',   format_date)

  # compute effort in min
  tacsatp_swe$LE_EFF_VMS <- abs(c(0, as.numeric( tacsatp_swe[-nrow( tacsatp_swe),"SI_DATIM"] -
                                         tacsatp_swe[-1,"SI_DATIM"], units="hours")))
  start.trip <- c(1,diff( tacsatp_swe[,"FT_REF"]))
  tacsatp_swe$all_effort <- tacsatp_swe$LE_EFF_VMS  # save...
  tacsatp_swe[start.trip!=0, "LE_EFF_VMS"] <- 0  # just correct for the trip change points

  tacsatp_swe$LE_EFF_VMS <- as.numeric(as.character(tacsatp_swe$LE_EFF_VMS))
  tacsatp_swe <- tacsatp_swe[!is.na(as.numeric(as.character(tacsatp_swe$SI_LONG))) &  !is.na(as.numeric(as.character(tacsatp_swe$SI_LATI))), ]
  tacsatp_swe$nb_vessels <- 1 # a trick to retrieve the mean from the aggregate sum

  tacsatp <- tacsatp_swe
  }
 
  
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  if(ctry=="DEU"){
  cat(paste("Formatting for this country: TO DO! \n"))
   ### CHRISTIAN:  TO DO ###
  # load(file=file.path(general$main.path.param.gis, "FISHERIES", "coupled_VMS_logbooks_DEU_2015.RData"))
  #tacsatp_deu           <- tacsat.swe
  #tacsatp_deu$SI_LONG   <- as.numeric(as.character(tacsatp_deu$SI_LONG))
  #tacsatp_deu$SI_LATI   <- as.numeric(as.character(tacsatp_deu$SI_LATI))
  #tacsatp_deu$SI_STATE  <- as.numeric(as.character(tacsatp_deu$SI_STATE))
  
  #tacsatp_swe$LE_MET_level6 <-  ...
  
  #format_date <- "%Y-%m-%d %H:%M:%S" 
  #tacsatp_deu$SI_DATIM <- as.POSIXct( tacsatp_deu$SI_DATIM, tz='GMT',   format_date)

  # compute effort in min
  #tacsatp_deu$LE_EFF_VMS <- abs(c(0, as.numeric( tacsatp_deu[-nrow( tacsatp_swe),"SI_DATIM"] -
  #                                       tacsatp_deu[-1,"SI_DATIM"], units="hours")))
  #start.trip <- c(1,diff( tacsatp_deu[,"FT_REF"]))
  #tacsatp_deu$all_effort <- tacsatp_deu$LE_EFF_VMS  # save...
  #tacsatp_deu[start.trip!=0, "LE_EFF_VMS"] <- 0  # just correct for the trip change points

  #tacsatp_deu$LE_EFF_VMS <- as.numeric(as.character(tacsatp_deu$LE_EFF_VMS))
  #tacsatp_deu <- tacsatp_deu[!is.na(as.numeric(as.character(tacsatp_deu$SI_LONG))) &  !is.na(as.numeric(as.character(tacsatp_deu$SI_LATI))), ]
  #tacsatp_deu$nb_vessels <- 1 # a trick to retrieve the mean from the aggregate sum
 
  tacsatp <- tacsatp_deu
  }
 
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  # assign the landing harbour to each ping of the trip
 
  #start.trip          <- c(1,diff( tacsatp[,"FT_REF"]))
  end.trip             <- c(diff( tacsatp[,"FT_REF"]), 1)
  tacsatp_end          <- tacsatp[end.trip!=0, ]
  land_harbour_of_trip <- tacsatp_end[!duplicated(tacsatp_end$FT_REF, tacsatp_end$SI_HARB), c('FT_REF', 'SI_HARB')]
  
 # then substitute....
  tacsatp$SI_HARB2 <-   tacsatp$FT_REF # init
  tacsatp$SI_HARB2 <-   land_harbour_of_trip [match(tacsatp$SI_HARB2, land_harbour_of_trip$FT_REF), 'SI_HARB']
  tacsatp$SI_HARB  <-   tacsatp$SI_HARB2
 
 
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   # make the port names consistent with the graph construction
   # by substituting the port names in raw input data with port names from the graph
   
 
  port_names <- read.table(file=file.path(general$main.path.param.gis, "GRAPH",
                                  paste("harbours.dat", sep='')), sep=";")

  all_ports_in_input <- levels(tacsatp$SI_HARB)
  all_ports_in_input <- all_ports_in_input[all_ports_in_input!="NA"]
   
  tacsatp$SI_HARB         <- factor(tacsatp$SI_HARB)
  coord_ports             <- tacsatp[!duplicated(tacsatp$SI_HARB), c('SI_LATI',   'SI_LONG', 'SI_HARB')] 
  coord_ports             <- coord_ports[!is.na(coord_ports$SI_HARB),]

  # brute force to link nodes to harbours (with euclidian distance)
  idx <- rep(0, nrow(coord_ports))
  for(i in 1:nrow(coord_ports)){
    idx[i] <- which.min ( sqrt( ((coord_ports[i, "SI_LONG"] -   port_names  [, "x"])^2) +  (((coord_ports[i, "SI_LATI"] -   port_names  [, "y"]))^2)) )
    print(i)
  }
  coord_ports$harbour <- row.names(port_names)[idx]
  
  # then, substitute...
  rownames (coord_ports)  <- coord_ports$SI_HARB
  levels(tacsatp$SI_HARB) <- coord_ports[levels(factor(tacsatp$SI_HARB)), "harbour"]
 

  
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   # relevant vessels only i.e. active in the area....
  
     # keep only the vessels fishnig in the western Baltic  (and kattegat because her.3a22, and East baltic because spr.2232)
    library(maptools)
    handmade            <- readShapePoly(file.path(general$main.path.param.gis, "MANAGEMENT", "wbaltic_wgs84"))  # build in ArcGIS 10.1
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
 
  # rename
  levels(tacsatp$LE_MET_rough) [levels(tacsatp$LE_MET_rough) %in% c("DRB", "FPO", "GNS", "LLD", "No_", "GTR", "LHP", "LLS", "MIS" )] <- "Passive"
  levels(tacsatp$LE_MET_rough) [levels(tacsatp$LE_MET_rough) %in% c("OTB","OTT",  "OTM", "PTB", "PTM" )]       <- "Trawl"
  levels(tacsatp$LE_MET_rough) [levels(tacsatp$LE_MET_rough) %in% c("PS_", "SDN", "SSC")]               <- "Seine"
    

  # keep level 6 and built an OTHER metier
  levels(tacsatp$LE_MET_level6)
  agg_per_met                 <- tapply(tacsatp[tacsatp$SI_STATE==1, ]$LE_EFF_VMS, list(tacsatp[tacsatp$SI_STATE==1, ]$LE_MET_level6), sum, na.rm=TRUE)
  agg_per_met_ordered         <- agg_per_met[order(agg_per_met, decreasing =TRUE)]
  agg_per_met_ordered_and_cum <- cumsum(agg_per_met_ordered/sum(agg_per_met_ordered, na.rm=TRUE)*100)
  met_to_keep                 <- names(agg_per_met_ordered_and_cum[agg_per_met_ordered_and_cum<90])   #  the ones that cumulatively cover 90% of the effort
  met_to_keep                 <- met_to_keep[!is.na(met_to_keep)]
 
  agg_per_met_and_vid                 <- tapply(tacsatp[tacsatp$SI_STATE==1, ]$LE_EFF_VMS, list( tacsatp[tacsatp$SI_STATE==1, ]$VE_REF, tacsatp[tacsatp$SI_STATE==1, ]$LE_MET_level6), sum, na.rm=TRUE)
  #agg_per_met_and_vid[!is.na(agg_per_met_and_vid[, "PTM_SPF_16-31_0_0"]),]   # check if some vessels are polyvalent demersal-pelagic
  
  levels(tacsatp$LE_MET_level6)[!levels(tacsatp$LE_MET_level6)%in% met_to_keep]  <- "other"
  levels(tacsatp$LE_MET_level6)[levels(tacsatp$LE_MET_level6)%in% "No_Matrix6"]  <- "other"
  
   # debug metier names for impossible symbols in file names
  levels(tacsatp$LE_MET_level6) <- gsub(">=", "o", levels(tacsatp$LE_MET_level6))  
  levels(tacsatp$LE_MET_level6) <- gsub("<",  "u", levels(tacsatp$LE_MET_level6))  
  levels(tacsatp$LE_MET_level6) <- gsub(">",  "o", levels(tacsatp$LE_MET_level6))  
  levels(tacsatp$LE_MET_level6) <- gsub("-",  "_", levels(tacsatp$LE_MET_level6))  
 
  

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
 nm <- names(tacsatp)
 idx.col.sp <- grep('LE_KG_', nm)
 idx.col.e  <- grep('LE_EFF_VMS', nm)
 idx.col.lg  <- grep('VE_LEN', nm)
 idx.col.kw  <- grep('VE_KW', nm)
 idx.col.nb  <- grep('nb_vessels', nm)
 idx.col <- c(idx.col.sp, idx.col.e, idx.col.lg, idx.col.kw, idx.col.nb)
 DT  <- data.table(tacsatp) # library data.table for fast grouping replacing aggregate()
 # AGGREGATE PER SPECIES ------> SUM
 eq1  <- c.listquote( paste ("sum(",nm[idx.col],",na.rm=TRUE)",sep="") )
 tacsatp_agg <- DT[,eval(eq1),by=list(VE_REF,SI_HARB, LE_MET_level6, LE_MET_rough)]
 tacsatp_agg <- data.frame(tacsatp_agg)
 colnames(tacsatp_agg) <- c("VE_REF","SI_HARB", "LE_MET","LE_MET_rough", paste(nm[idx.col.sp], sep=''), "LE_EFF_VMS", "VE_LEN", "VE_KW", "nb_vessels")
 tacsatp_agg <- tacsatp_agg[order(tacsatp_agg$LE_MET),] # order




  tacsatp_agg$VE_LEN <-     tacsatp_agg$VE_LEN / tacsatp_agg$nb_vessels
  tacsatp_agg$VE_KW <-     tacsatp_agg$VE_KW / tacsatp_agg$nb_vessels

  # then correct  (given it is individual data)
  tacsatp_agg$nb_vessels <- 1

 
  #but caution the true nb of vessels per occurence is
  #nbvessels.per.met.per.harb <-  aggregate(tacsatp$VE_REF, list(tacsatp$VE_REF, tacsatp$SI_HARB, tacsatp$LE_MET_level6), function(x) length(unique(x)), simplify = FALSE)
  #colnames(nbvessels.per.met.per.harb) <-  c("VE_REF",'SI_HARB', 'LE_MET', 'nbvessels')
  #tacsatp_agg                    <- merge(tacsatp_agg, nbvessels.per.met.per.harb) # map


  # compute kg_per_h cpues
  nm <- colnames(tacsatp_agg)
  tacsatp_agg <- tacsatp_agg[tacsatp_agg$LE_EFF_VMS!=0,]
  tacsatp_agg[, grep('LE_KG_', nm)]          <- tacsatp_agg[, grep('LE_KG_', nm)]   /   tacsatp_agg$LE_EFF_VMS
  colnames(tacsatp_agg)[grep('LE_KG_', nm)]  <- paste(gsub('LE_KG_', '', nm[grep('LE_KG_', nm)]), '_kg_h', sep='')
  
  # to do
  # add other features such as:
   # fishing speed knots	cruise speed knots	fuel cons h	ave storage fish kg	fuel tank liter	trip duration h	multip fuel steaming	multip fuel fishing	multip fuel ret port fish	multip fuel inactive	range km fish ground	
   # fuel_price_Euro/liter	weekEndStartDay	WeekEndEndDay	WorkHoursStart	WorkHoursEnd
   tacsatp_agg$mean_LOA_m          <-  tacsatp_agg$VE_LEN
   tacsatp_agg$mean_kW             <-  tacsatp_agg$VE_KW
   
   tacsatp_agg$"fishing speed knots" <- 4
   tacsatp_agg$"cruise speed knots"  <- 10
  
   # i.e. fuel cons
   table.fuelcons.per.engine       <-  read.table(file= file.path(general$main.path.param.gis, "FISHERIES", "IBM_datainput_engine_consumption.txt"), header=TRUE,sep="")
   linear.model                    <-  lm(calc_cons_L_per_hr_max_rpm~ kW2, data=table.fuelcons.per.engine)  # conso = a*Kw +b   # to guess its fuel consumption at maximal speed
   tacsatp_agg$fuel.cons.h         <-  predict(linear.model, newdata=data.frame(kW2=as.numeric(as.character(tacsatp_agg$VE_KW)))) # Liter per hour
  
   # i.e. fuel tank capacity
   tacsatp$fuel_cons_rate         <-  predict(linear.model, newdata=data.frame(kW2=as.numeric(as.character(tacsatp$VE_KW)))) # Liter per hour
   tacsatp$fuelcons               <- as.numeric(as.character(tacsatp$fuel_cons_rate)) * (as.numeric(as.character(tacsatp$all_effort))) # liter per hour
   DT                    <- data.table(tacsatp) 
   eq1                   <- c.listquote( paste ("sum(","fuelcons",", na.rm=TRUE)",sep="") )
   DT$VE_REF             <- as.factor(DT$VE_REF)
   x.agg                 <- DT[,eval(eq1),by=list(VE_REF,FT_REF)]
   x.agg                 <- data.frame( x.agg)
   colnames(x.agg)       <- c("VE_REF", "FT_REF", "totcons")
   tank.capacity.per.vid <- tapply(x.agg$totcons, x.agg$VE_REF, function(x) {x<- x[x<100000]; quantile(x, probs=0.9)})# {x<- x[x<100000]; quantile(x, probs=0.99)})
  
   cap                             <- tank.capacity.per.vid[!is.na(tank.capacity.per.vid)]
   vessel.length.per.vid           <- tacsatp[!duplicated(tacsatp$VE_REF), c("VE_REF","VE_LEN")]
   rownames(vessel.length.per.vid) <- vessel.length.per.vid [,"VE_REF"]
   dd                              <- cbind(cap=cap, VE_LEN=vessel.length.per.vid[names(cap),"VE_LEN"])
   #plot(dd [,'VE_LEN'], dd [,'cap'])
   nls1                            <- nls(cap~ b*VE_LEN^a, data=as.data.frame(dd), start=list(a=1, b=100))
   #points(dd [,'VE_LEN'], predict(nls1), col=4)
   tacsatp_agg$fuel.tank.liter     <-   round ( predict(nls1, tacsatp_agg) ) 
  

  
   # i.e. fish storage capacity
   DT         <- data.table(tacsatp)
   nm         <- colnames(tacsatp)
   idx.col    <- grep("KG", nm)
   eq1        <- c.listquote( paste ("sum(",nm[idx.col],",na.rm=TRUE)",sep="") )
   DT$VE_REF  <- as.factor(DT$VE_REF)
   x.agg      <- DT[,eval(eq1),by=list(VE_REF, FT_REF)]
   x.agg      <- data.frame(x.agg)
   x.agg$tot_landings              <- apply(x.agg[, -c(1:2)], 1, sum, na.rm=TRUE)   # kg
   carrying.capacity.per.vid       <- tapply(x.agg$tot_landings, x.agg$VE_REF, function(x) {; quantile(x, probs=0.9, na.rm=TRUE)})  
   cap                             <- carrying.capacity.per.vid[!is.na(carrying.capacity.per.vid)]
   vessel.length.per.vid           <- tacsatp[!duplicated(tacsatp$VE_REF), c("VE_REF","VE_LEN")]
   rownames(vessel.length.per.vid) <- vessel.length.per.vid [,"VE_REF"]
   dd                              <- cbind(cap=cap, VE_LEN=vessel.length.per.vid[names(cap),"VE_LEN"])
   #plot(dd [,'VE_LEN'], dd [,'cap'])
   nls2                            <- nls(cap~ b*VE_LEN^a, data=as.data.frame(dd), start=list(a=1, b=1000))
   #points(dd [,'VE_LEN'], predict(nls2), col=4)
   tacsatp_agg$ave.storage.fish.kg <-  round ( predict(nls2, tacsatp_agg) ) 
  
    
   tacsatp_agg$nb_pings_per_trip   <-  10
   tacsatp_agg$Gamma_shape         <-  0.4485   # Gamma shape # not used if GoFishing dtree activated
   tacsatp_agg$Gamma_scale         <-  336.7618 # Gamma scale # not used if GoFishing dtree activated
   tacsatp_agg$trip.duration.h     <-  20 # not really important if StopFishing dtree activated with EndOfTheDay
   tacsatp_agg$multip.fuel.steaming<-  1
   tacsatp_agg$multip.fuel.fishing <-  0
   tacsatp_agg[tacsatp_agg$LE_MET_rough %in% c('Dredge', 'Trawl', 'Seine'), 'multip.fuel.fishing'] <- 0.9
   tacsatp_agg$multip.fuel.ret.port.fish <- 1.1
   tacsatp_agg$multip.fuel.inactive<- 0.15
   tacsatp_agg$weekEndStartDay     <-  5
   tacsatp_agg$WeekEndEndDay       <-  6
   tacsatp_agg$WorkHoursStart      <-  5
   tacsatp_agg$WorkHoursEnd        <-  22
   
   tacsatp_agg$vessel_range_km     <- 30
   tacsatp_agg[tacsatp_agg$LE_MET_rough %in% c('Dredge', 'Trawl', 'Seine'), 'vessel_range_km'] <- 100
   
   tacsatp_agg$name_gis_file_for_fishing_effort_per_polygon <- paste(substr(as.character(tacsatp_agg$VE_REF), 1,3), "_gis_feffort_", tacsatp_agg$LE_MET, sep='')
   tacsatp_agg$name_gis_layer_field                         <- "feffort"                     # giving releative  e.g. in 5 categories: 1 to 5 with 1 high occurence or absoulte effort ditribtion
   tacsatp_agg$is_gis_layer_field_relative_numbers          <- FALSE                           # if relative effort categories (e.g. high to low) then xfold_gis_layer_field will be used to convert in absolute
   tacsatp_agg$xfold_gis_layer_field                        <- "1"                            # giving relative importance of the 5 categories e.g. visting an area of cat 1 is 10000 times more probable than for cat 5
                             

   tacsatp_agg$Region <- "wbaltic"
   tacsatp_agg$Harbor <- tacsatp_agg$SI_HARB
   tacsatp_agg$metier <- tacsatp_agg$LE_MET
   tacsatp_agg$vid    <- tacsatp_agg$VE_REF
   tacsatp_agg$"N. of vessels"  <- 1
   tacsatp_agg$Crew <- NA
   tacsatp_agg$mean_GT <- NA
   
 
  
  # finally, export!
  nameobj           <- paste("vessels_specifications_per_harbour_metiers_",ctry,".csv",sep='')  #....and possibly per vid!
  
  tacsatp_agg <- tacsatp_agg[!is.na(tacsatp_agg$SI_HARB),]
  
  #Region,Harbor,metier,N. of vessels,Crew,mean_LOA_m,mean_GT,mean_kW,hake_kg_h,sole_kg_h,redmullet_kg_h,mantis_kg_h,fishing speed knots,cruise speed knots,fuel cons h,ave storage fish kg,fuel tank liter,trip duration h,multip  fuel steaming,multip fuel fishing,multip fuel ret port fish,multip fuel inactive,range km fish ground,fuel_price_Euro/liter,weekEndStartDay,WeekEndEndDay,WorkHoursStart,WorkHoursEnd
  
   write.table(tacsatp_agg, file.path(general$main.path.param.gis, "FISHERIES", nameobj), row.names=FALSE, sep=";", quote=FALSE)


   
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  # FINAL STEP, ONCE ALL COUNTRIES INFORMED THEN COMBINE ALL COUNTRIES
  countries <- c('DEN', 'SWE', 'DEU')
  tacsatp   <- NULL
  for (ctry in countries){

     nameobj           <- paste("vessels_specifications_per_harbour_metiers_",ctry,".csv",sep='')  #....and possibly per vid!
     tacsatp_this_ctry <- read.table(file.path(general$main.path.param.gis, "FISHERIES", nameobj), header=TRUE, sep=";")
  
     if(is.null(tacsatp)){
        tacsatp <- tacsatp_this_ctry 
     }else{
  
        nm                          <- intersect(colnames(tacsatp), colnames(tacsatp_this_ctry))
        nm_not_in_tacsatp           <- setdiff(colnames(tacsatp_this_ctry), colnames(tacsatp))
        nm_not_in_tacsatp_this_ctry <- setdiff(colnames(tacsatp), colnames(tacsatp_this_ctry))

        tacsatp <- rbind.data.frame(
                        tacsatp[,nm],
                         tacsatp_this_ctry[,nm]
                         )
     }
  }

  # keep only DNK and DEU, and SWE vessels
 tacsatp <- tacsatp[c(grep("DEN", tacsatp$VE_REF), grep("DEU",tacsatp$VE_REF), grep("SWE", tacsatp$VE_REF)),]
 
 # finally, export!
 nameobj <- "vessels_specifications_per_harbour_metiers.csv"  #....and possibly per vid!
  
  #Region,Harbor,metier,N. of vessels,Crew,mean_LOA_m,mean_GT,mean_kW,hake_kg_h,sole_kg_h,redmullet_kg_h,mantis_kg_h,fishing speed knots,cruise speed knots,fuel cons h,ave storage fish kg,fuel tank liter,trip duration h,multip fuel steaming,multip fuel fishing,multip fuel ret port fish,multip fuel inactive,range km fish ground,fuel_price_Euro/liter,weekEndStartDay,WeekEndEndDay,WorkHoursStart,WorkHoursEnd
  
 write.table(tacsatp_agg, file.path(general$main.path.param.gis, "FISHERIES", nameobj), row.names=FALSE, sep=";", quote=FALSE)

