
FRANCOIS <- TRUE
HEINO    <- FALSE

                                           
#library(vmstools)  # 54
#library(help=vmstools)  # getting help...
#memory.limit(4000)
# => not in use anymore, use external R source instead
# to be independent of potential broken code from vmstools updates


# set your own path here:
if(FRANCOIS){
   main_path_data         <- file.path("C:","merging", "EflaloAndTacsat")
   main_path_ibm_param    <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_raw")  
   main_path_ibm_param_R  <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_R_inputs") 

   }
if(HEINO){
   main_path_data          <-  file.path("C:","merging", "EflaloAndTacsat","GermanEflaloTacsat")
   main_path_ibm_param     <- file.path("C:","displace-project.org","repository", "ibm_vessels_param")   
   main_path_ibm_param_R   <- file.path("C:","displace-project.org","repository", "ibm_vessels_param_R")   
  }
  
 
  
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  ### LOAD TACSAT & EFLALO DATA SETS ###
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  #y <- "2005"
  #y <- "2006"
  #y <- "2007"
  #y <- "2008"
  #y <- "2009"  
  #y <- "2010"
  y <- "2012"
  #y <- "2013"
  y <- "2014"
  y <- "2015"
  
  if(FRANCOIS){ 
                # ease the loading by creating RData objects.
                #tacsat <- read.table(file.path("C:","merging","EflaloAndTacsat", paste("tacsat2_",y,".csv",sep="")), header=TRUE, sep=",")
                #save(tacsat, file=file.path("C:","merging","EflaloAndTacsat", paste("tacsat",y,".RData",sep="")))
                #eflalo <- read.table(file.path("C:","merging","EflaloAndTacsat", paste("eflalo3_",y,".csv",sep="")), header=TRUE, sep=",")
                #save(eflalo, file=file.path("C:","merging","EflaloAndTacsat", paste("eflalo_",y,".RData",sep="")))
                
                load(file.path(main_path_data, paste("tacsat",y,".RData",sep='')) )
                load(file.path(main_path_data, paste("eflalo_",y,".RData",sep='')))
  }
  if(XXX){ 
        eflalo <-read.table(file=file.path(main_path_data, paste("ger_eflalo",y,".csv", sep="")), sep=",", header=TRUE)
        tacsat <-read.table(file=file.path(main_path_data, paste("TACSAT2_GER_", y, ".csv", sep="")), sep=",", header=TRUE)  
  }



 
  
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  ### LOAD A PORT DATA.FRAME         ###
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
   
   load(file.path(main_path_ibm_param, "myEUports.RData"))  # get EU_ports
 
  
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  ### DETECT POSITIONS IN HARBOUR    ###
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
   
 
  library(doBy) # need to order tacsat first (e.g. for Heino)
  tacsat$SI_DATE <- as.POSIXct(tacsat$SI_DATE, tz='GMT',   "%d/%m/%Y") #=> caution: becomes  "%Y-%m-%d"  !!
  tacsat$SI_TIME <-  format(strptime(  paste(tacsat$SI_TIME) , tz='GMT',  "%H:%M" ), "%H:%M")    
  tacsat <- orderBy(~VE_REF+SI_DATE+SI_TIME, data=tacsat)
  
  anf <- function(x) as.numeric(as.character(x))
  source(file.path( main_path_ibm_param_R, 'vmstools_pointInHarbour.r'))
  
  tacsat$SI_HARB <- NA
  tacsat$SI_HARB <- pointInHarbour(lon=anf(tacsat$SI_LONG), lat=anf(tacsat$SI_LATI), harbours=EU_ports, rowSize=30, returnNames=TRUE)
  inHarb <- tacsat$SI_HARB
  inHarb <- replace(inHarb, !is.na(inHarb), 1)
  inHarb <- replace(inHarb, is.na(inHarb), 0)
  inHarb <- as.numeric(inHarb)

  # assign a trip identifier
  tacsat$SI_FT <- 1 # init
  idx <- which(inHarb==0)
  tacsat[idx,"SI_FT"] <- cumsum(inHarb) [idx] # add a SI_FT index

  # keep 'out of harbour' points only
  # (but keep the departure point and the arrival point lying in the harbour)
  startTrip <- c(diff(tacsat[,"SI_FT"]), 0)
  endTrip   <- c(0, diff(tacsat[,"SI_FT"]))
  tacsat[which(startTrip>0),"SI_FT"]  <-  tacsat[which(startTrip>0)+1,"SI_FT"] # tricky here
  tacsat[which(endTrip<0),"SI_FT"]    <-  tacsat[which(endTrip<0)-1,"SI_FT"] # tricky here
  tacsat <- tacsat[which(inHarb==0 |  startTrip>0 |  endTrip<0),]
    
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  ### ASSIGN A FISHING STATE         ###
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  tacsat$SI_STATE <- 2 # init (1: fishing; 2: steaming)
  tacsat$SI_STATE [(tacsat$SI_SP>4 & tacsat$SI_SP<8)] <-1
    ## => fill in SI_STATE with a fake rule just to add some guesses
 
 
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  ### FEW ADJUSTEMENTS...            ###
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  eflalo$LE_MET_level6 <- eflalo$LE_MET
  eflalo <- eflalo[eflalo$LE_MET!="No_logbook6",]
  eflalo <- eflalo[eflalo$LE_MET!="No_logbook6_>10",]
  if(FRANCOIS) eflalo$VE_FLT<-"fleet1"



  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  ### VESSEL BY VESSEL MERGING       ###
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  
  
  
  if(FRANCOIS) namefolder <- file.path("C:","output", paste("merged_DEN_", y, sep=''))
  if(HEINO)    namefolder <- file.path("C:","output", paste("merged_DEU_", y, sep=''))
  
  vessels <- unique(eflalo$VE_REF)
  if(FRANCOIS) vessels <- vessels[ grep("DNK", vessels) ]
  if(HEINO)    vessels <- vessels[ grep("DEU", vessels) ]
  
  
  an <- function(x) as.numeric(as.character(x))
  af <- function(x) as.factor(as.character(x))
  ac <- function(x) as.character(x)
  source(file.path( main_path_ibm_param_R, 'vmstools_mergeEflalo2Pings.r'))
  source(file.path( main_path_ibm_param_R, 'vmstools_segmentTacsatSpeed.r'))
  source(file.path( main_path_ibm_param_R, 'vmstools_c.listquote.r'))
  source(file.path( main_path_ibm_param_R, 'vmstools_countPings.r'))
  library(data.table)
  
  mergeEflalo2PingsForDisplace (eflalo=eflalo, tacsat=tacsat,  vessels=vessels,
          general=list(output.path=namefolder,  visual.check = TRUE,
                visual.check=TRUE, detectFishing=TRUE, speed="segment", what.speed="calculated", conserve.all=TRUE))

  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  ### LOOK AT ONE GIVEN VESSEL       ###
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  if(FRANCOIS){
    load(file.path(namefolder, "merged_DNK000003612_2010.RData"))
    head(merged[,c("VE_REF", "FT_REF", "VE_FLT", "VE_KW", "LE_MET_level6", "LE_GEAR",
                 "SI_LATI", "SI_LONG", "SI_SP", "SI_HE", "SI_HARB","SI_STATE","SI_RECT","LE_EFF_VMS","flag", 
                 "SI_DATE","SI_TIME",
                 "LE_KG_COD","LE_KG_DAB")], 40)
   }

  
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  ### BUILD ONE BIG DATASET          ###
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  
  # ...selecting relevant species:
  spp <- c('COD', 'CSH', 'DAB', 'ELE', 'FLE', 'HAD', 'HER', 'HKE', 'HOM',
         'LEM', 'MAC', 'MON', 'MUS', 'NEP', 'NOP', 'OYF', 'PLE', 'POK', 'PRA', 'SAN',
           'SOL', 'SPR', 'TUR', 'WHB', 'WIT', 'WHG',
            'OTH')

                          
  vnames <- as.character(unique(tacsat$VE_REF))
   

  source(file.path( main_path_ibm_param_R, 'vmstools_bindAllMergedTables.r'))
  tmp <- bindAllMergedTables (vessels=vnames, a.year=y, species.to.keep=spp, 
             folder = file.path(namefolder), all.in.one.table=FALSE)
   
   
   ##=> all_merged_value_2012.RData and all_merged_weight_2012.RData are now created in the output folder...
   
   
 #---------------------------------------------#
 #------plot used harbours---------------------#
 #---------------------------------------------# 
if(FALSE){ 

load(file.path(namefolder, "all_merged_weight_2012.RData"))
used.ports.when.merging.proc <- as.character(unique(all.merged$SI_HARB))
used.ports.when.merging.proc <- used.ports.when.merging.proc [!used.ports.when.merging.proc=="NA"]
all(used.ports.when.merging.proc %in% EU_ports$Description ) # should be TRUE
used <- EU_ports[EU_ports$Description%in%used.ports.when.merging.proc,]
plot(used$lon, used$lat, col=4)
library(mapdata)
map("worldHires", add=TRUE) 



# Swedish data are not merged following the above procedure...anyway we need to inform the visited ports by Swedish vessels. 
# add on top of previous ones all the visited ports by swedish vessels (sent by Patrik J.)
SWEports           <- read.table(file=file.path(main_path_ibm_param,"SWE_harbour_o12.txt"), header=TRUE,sep="\t") 
SWEports           <- cbind(SWEports, Coordinates="XXXX", range=3)
colnames(SWEports) <- c('lon','lat', 'ISO.3.Country.Code', 'Description', 'LOCODE.Code', 'Coordinates', 'range' )  # rename
SWEports           <- SWEports[, c('ISO.3.Country.Code', 'LOCODE.Code', 'Description',  'Coordinates', 'lat','lon', 'range')]  # reorder columns


# save
if(HEINO) { used_ger <- used$Description; save(used_ger, file=file.path(main_path_ibm_param, "used_ports_ger_2012.RData")) }
if(FRANCOIS) { used_den <- used$Description; save(used_den, file=file.path(main_path_ibm_param, "used_ports_den_2012.RData")) }
if(PATRIK) { used_swe <- SWEports$Description; save(used_swe, file=file.path(main_path_ibm_param, "used_ports_swe_2012.RData")) }  # we keep them all for Sweden

 
} # end FALSE 
  

  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  ### SAVE PORTS                     ###
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###

  multi_countries <- TRUE
  if(multi_countries){
     load (file=file.path(main_path_ibm_param, "used_ports_swe_2012.RData"))
     load (file=file.path(main_path_ibm_param, "used_ports_ger_2012.RData"))
     load (file=file.path(main_path_ibm_param, "used_ports_den_2012.RData"))
     ports.to.keep <- EU_ports[ EU_ports$Description %in% unique(c(as.character(used_swe), as.character(used_ger), as.character(used_den))), ]  # search names among the EU list
     ports.to.keep <- rbind(ports.to.keep, SWEports[! (SWEports$Description  %in% ports.to.keep$Description), ])  # for swe, add all the other ports, i.e. those not already added
     ports.to.keep <- ports.to.keep [ !duplicated( ports.to.keep $Description),]  # keep only one record for one name... 
     # ...but potentially still redundant ports if countries give different names to the same port!
     # nevertheless it is important to keep all redundant ports on board as the names link back to the country specific logbooks
     save(ports.to.keep , file=file.path(main_path_ibm_param, "ports_to_keep_2012.RData")) 
  } else{
    load (file=file.path(main_path_ibm_param, "used_ports_den_2012.RData"))
    ports.to.keep <- EU_ports[ EU_ports$Description %in% unique(c(as.character(used_den))), ]
    save(ports.to.keep , file=file.path(main_path_ibm_param, "ports_to_keep_2012.RData")) 
    }
    

  # check
  plot(ports.to.keep$lon, ports.to.keep$lat, xlim=c(-10,20), ylim=c(50,70), col=4)
  map("worldHires", add=TRUE) 
  points(missing.ports.dnk$Lon, missing.ports.dnk$Lat, col=2, pch=16, cex=0.5)
  points(missing.ports.ger$Lon, missing.ports.ger$Lat, col=3, pch="+")
  points(ports.to.keep[ports.to.keep$Coordinates=='XXXX',]$lon, ports.to.keep[ports.to.keep$Coordinates=='XXXX',]$lat, xlim=c(-10,20), ylim=c(50,70), col=5, pch=16, cex=0.5)
                 
  
 # deprecated but useful...
 if(FALSE){
 # convert coordinates into UTM coordinates
 library(sp)
 library(rgdal)
 ports.to.keep <- ports.to.keep[ !is.na(ports.to.keep$lon) &  !is.na(ports.to.keep$lat),]
 SP <- SpatialPoints(cbind(as.numeric(as.character(ports.to.keep$lon)), as.numeric(as.character(ports.to.keep$lat))),
                       proj4string=CRS("+proj=longlat +datum=WGS84"))
 ports.to.keep <- cbind(ports.to.keep, spTransform(SP, CRS("+proj=utm  +ellps=intl +zone=32 +towgs84=-84,-107,-120,0,0,0,0,0")))    # convert to UTM

 colnames( ports.to.keep)[colnames( ports.to.keep) == "coords.x1" ] <- "UTM_Easting"
 colnames( ports.to.keep)[colnames( ports.to.keep) == "coords.x2" ] <- "UTM_Northing"
  # brute force to detect overlapping points (with euclidian distance)
 conn <- matrix(ncol=3)
 colnames(conn) <- c('idx1', 'idx2', 'dist')
 
 for(i in 1:nrow( ports.to.keep)){
    if(!i %in% conn[,'idx1']){
      distances  <- sqrt( (( ports.to.keep[i, "UTM_Easting"] -    ports.to.keep  [-i, "UTM_Easting"])^2) +  ((( ports.to.keep[i, "UTM_Northing"] -    ports.to.keep  [-i, "UTM_Northing"]))^2)) 
      conn       <- rbind(conn, cbind(i, which.min (distances), distances[which.min (distances)])) 
     }
    print(i)
 }
 
 # potential overlapping points
 pts <- conn[conn[,3]>0 & conn[,3]<500,]
 cbind.data.frame(ports.to.keep[pts[,'idx1'], ],ports.to.keep[pts[,'idx2'], ]) 
 } # end FALSE



  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  ### SWEDEN                         ###
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  # special treatment for Sweden
  # because pre-processed data (by Patrik 14 Feb 14)
  
  all.merged <- read.table(file=file.path("C:","merging","EflaloAndTacsat", "SwedishEflaloTacsat", "DISPLACE_SWE_140218.txt"), sep="\t", header=TRUE)
  colnames(all.merged)[colnames(all.merged) %in% "VE_MET"]  <- "LE_MET_level6"
  colnames(all.merged)[colnames(all.merged) %in% "VE_GEAR"] <- "LE_GEAR"
  all.merged$flag <- 1 
  format_date <- "%Y-%m-%d %H:%M:%S" 
  all.merged$SI_DATIM <- as.POSIXct( all.merged$SI_DATIM, tz='GMT',   format_date)

  # compute effort in min
  all.merged$LE_EFF_VMS <- abs(c(0, as.numeric( all.merged[-nrow( all.merged),"SI_DATIM"] -
                                         all.merged[-1,"SI_DATIM"], units="mins")))
  start.trip <- c(1,diff( all.merged[,"FT_REF"]))
  all.merged[start.trip!=0, "LE_EFF_VMS"] <- 0  # just correct for the trip change points

  save(all.merged, file="C:\\displace-project.org\\repository\\ibm_vessels_param\\merged_tables\\baltic_only\\all_merged_weight_SWE_2012.RData")



  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  ### PLOTTING ON A GRID             ###
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###

 if(FRANCOIS){ a.year <- '2009'

  # 1:  fishing effort in hours
  load(file.path(namefolder, paste("all_merged_weight_",a.year,".RData",sep='')))
  all.merged$SI_LONG <- anf(all.merged$SI_LONG ) ; all.merged$SI_LATI <- anf(all.merged$SI_LATI )
  all.merged$LE_MET <- all.merged$LE_MET_level6

  # assign a trip identifier
  all.merged$tripnum <- 0 # init
  all.merged[all.merged$SI_HARB!="NA", "tripnum"] <- 1
  all.merged[, "tripnum"]  <- cumsum( all.merged[, "tripnum"]  )
  # keep only fishing positions (and the first point in the harbour)
  all.merged <- all.merged[all.merged$SI_STATE==1 | !all.merged$SI_HARB=="NA",]
  all.merged$SI_LONG <- anf(all.merged$SI_LONG)
  all.merged$SI_LATI <- anf(all.merged$SI_LATI)
  all.merged[,"LE_EFF_VMS"] <- as.numeric(as.character(all.merged[,"LE_EFF_VMS"] ))
  all.merged[, "LE_EFF_VMS"] <- replace(all.merged[,"LE_EFF_VMS"], is.na(all.merged[, "LE_EFF_VMS"]) | all.merged[, "LE_EFF_VMS"] < 0, 0)
  all.merged$LE_EFF_VMS <-   an(all.merged$LE_EFF_VMS)/ 60 # in hours
  dnk_area1 <- vmsGridCreate(all.merged, nameVarToSum="LE_EFF_VMS",  numCats=10,  plotPoints =FALSE, legendtitle="fishing (hours)",
          colLand="darkolivegreen4",  addICESgrid=TRUE,
              nameLon="SI_LONG", nameLat="SI_LATI", cellsizeX =0.05, cellsizeY =0.05, we = -3.5,ea = 3.5, so = 57, no = 61,
               breaks0=c(0,2,4,6,12,24,32, 100000), legendncol=2,
                outGridFile= paste("dnk",a.year,"_area1.asc",sep='') )
  rect(1.005,59.32,1.20,59.37, border=4, lwd=1.7)
   #59.32 N to 59.37N and 1.00 E to 1.20E
   # save
  #savePlot(filename = paste("dnk",a.year,"_area1",sep=''), type ="jpeg")

  # 2: A ZOOM
  dnk_area1_zoom <- vmsGridCreate(all.merged, nameVarToSum="LE_EFF_VMS",  numCats=10,  plotPoints =FALSE, legendtitle="fishing (hours)",
          colLand="darkolivegreen4",  addICESgrid=TRUE,
              nameLon="SI_LONG", nameLat="SI_LATI", cellsizeX =0.05, cellsizeY =0.05, we = -2,ea = 2, so = 58, no = 60.5,
               breaks0=c(0,2,4,6,12,24,32, 100000), legendncol=2,
                outGridFile= paste("dnk",a.year,"_area1_zoom.asc",sep='') )
  rect(1.005,59.32,1.20,59.37, border=4, lwd=1.7)
  #savePlot(filename =  paste("dnk",a.year,"_area1_zoom",sep=''), type ="jpeg")

  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  ### PLOTTING THE VMS TRACKS        ###
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###

  # 3: PLOT THE TRACKS
  load(file.path(namefolder, paste("all_merged_weight_",a.year,".RData",sep='')))
  all.merged$SI_LONG <- anf(all.merged$SI_LONG ) ; all.merged$SI_LATI <- anf(all.merged$SI_LATI )
   # assign a trip identifier
  all.merged$tripnum <- 0 # init
  all.merged[all.merged$SI_HARB!="NA", "tripnum"] <- 1
  all.merged[, "tripnum"]  <- cumsum( all.merged[, "tripnum"]  )

  windows(6,6)
  plot(0,0, type="n", xlim=c(-1.5,1.5),ylim=c(58,60.5), ylab="Degree North", xlab="Degree West", axes=FALSE)
  idx <- point.in.polygon(point.x = anf(all.merged$SI_LONG), point.y = anf(all.merged$SI_LATI),
        pol.x = c(-1.5, 1.5, 1.5, -1.5), pol.y = c(58, 58, 60.5, 60.5)) > 0
  focus <- all.merged[idx,]
  lst <- split(focus, focus$tripnum, drop=TRUE)
  for(i in 1:length(lst)){
    x <- lst[[i]]
    x$SI_STATE <- replace (x$SI_STATE, x$SI_STATE==2, -1) # disable
    if(nrow(x)>0) segments(x$SI_LONG, x$SI_LATI,
                                   c(x$SI_LONG [-1], x$SI_LONG [length(x$SI_LONG)]) ,
                                       c(x$SI_LATI[-1],x$SI_LATI[length(x$SI_LATI)]), col=an(x$SI_STATE))
  }
  rect(1.005,59.32,1.20,59.37, border=4, lwd=2)
  axis(1)
  axis(2, las=2)
  box()
  map("worldHires", add = TRUE, col = "darkolivegreen4", fill = TRUE,
        bg = "white",  xlim=c(-1.5,1.5),ylim=c(58,60), regions = c("uk",
            "ireland", "france", "germany", "netherlands", "norway",
            "belgium", "spain", "luxembourg", "denmark", "sweden",
            "iceland", "portugal", "italy", "sicily", "ussr",
            "sardinia", "albania", "monaco", "turkey", "austria",
            "switzerland", "czechoslovakia", "finland", "libya",
            "hungary", "yugoslavia", "poland", "greece", "romania",
            "bulgaria", "slovakia", "morocco", "tunisia", "algeria",
            "egypt"))
  #savePlot(filename =  paste("dnk",a.year,"_area1_zoom_with_tracks",sep=''), type ="jpeg")




  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  ### PLOTTING IN GOOGLE MAP         ###
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###

  
  Grid2KML <- function(output.mat=output.mat, what.quantity = 'effort', kmlfile='vms.kml',imagefile='vms.png')  {
     #Takes the output from vmsGridCreate ie. when plotMap is set to FALSE.
      #output.mat[["fishing"]] <- log(output.mat[["fishing"]])
  dd <- output.mat@grid
  d1 <- dd@cells.dim[1]
  d2 <- dd@cells.dim[2]
  fishing <- output.mat@data$fishing
  mat <- matrix((fishing),byrow=T,ncol=d1,nrow=d2)
  mat <- t(mat[d2:1,])
  bbox <- output.mat@bbox
  xxx <- seq(bbox[1,1],bbox[1,2],length=d1)
  yyy <- seq(bbox[2,1],bbox[2,2],length=d2)
  rr <- range(mat[mat!='-Inf'],na.rm=T)
  labs<-seq(rr[1],rr[2],length=9)
  image(xxx,yyy,mat,zlim=c(rr[1],rr[2]),xlab="",ylab="",col=rainbow(9))
  gd <- list(x=xxx,y=yyy,z=mat)
  gd.1 <- as.SpatialGridDataFrame.im(as.im(gd))
  proj4string(gd.1) <- CRS("+proj=longlat +datum=WGS84")
  vms.kml <- GE_SpatialGrid(gd.1)
  tf <- tempfile()
  png(file=paste(tf,imagefile,sep=''), width=vms.kml$width, height=vms.kml$height, bg="transparent",res=576)
  par(mar=c(0,0,0,0), xaxs="i", yaxs="i",cex=.25)
  image(as.image.SpatialGridDataFrame(gd.1[1]), col=heat.colors(9),xlim=vms.kml$xlim, ylim=vms.kml$ylim)
  dev.off()
  kmlOverlay(vms.kml, kmlfile=paste(tf,kmlfile,sep=''), imagefile=paste(tf,imagefile,sep=''), name=what.quantity)

  legend(x='topleft', legend=as.character(labs), pch = 22,pt.bg=heat.colors(length(labs)),
  title=what.quantity, ncol=2,bg="transparent",pt.cex=1.5 )
  }

  # call it...
  dnk_area1_zoom <- vmsGridCreate(all.merged, plotMap=FALSE, nameVarToSum="LE_EFF_VMS",  numCats=10,  plotPoints =FALSE, legendtitle="fishing (hours)",
          colLand="darkolivegreen4",  addICESgrid=TRUE,
              nameLon="SI_LONG", nameLat="SI_LATI", cellsizeX =0.05, cellsizeY =0.05, we = -2,ea = 2, so = 58, no = 60.5,
               breaks0=c(0,2,4,6,12,24,32, 100000), legendncol=2,
                outGridFile= paste("dnk",a.year,"_area1_zoom.asc",sep='') )

  Grid2KML (dnk_area1_zoom)  
  
  
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  ### PLOTTING THE ORIGIN OF LANDINGS###
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  sp     <- "LE_EURO_COD"
  what   <- "value"
  a.unit <- "(EURO)"
  cellsizeX = 0.05
  cellsizeY = 0.05 
  we <- 9.8; ea <- 12.7; no <- 55.2; so <- 54;
  breaks0 <- c(0, 100, 100 * (2^1), 100 * (2^2), 100 * (2^3), 100 * (2^4), 100 * 
            (2^5), 100 * (2^6), 100 * (2^7), 100 * (2^8), 100 * 
            (2^9), 1e+07) 
                                  

  load(file.path(namefolder, paste("all_merged_value_",a.year,".RData",sep='')))
   
  df1 <- all.merged[all.merged$SI_STATE == 1, c("SI_LATI", "SI_LONG", sp)]
  df1$SI_LATI <- anf(df1$SI_LATI)
  df1$SI_LONG <- anf(df1$SI_LONG)
  df1[, sp] <- replace(df1[, sp], is.na(df1[, sp]) | df1[, sp] < 0, 0)
  vmsGridCreate(df1, nameVarToSum = sp, numCats = 10, plotPoints = FALSE, 
        legendtitle = paste("landings", what, a.unit, sep = " "), 
        colLand = "darkolivegreen4", addICESgrid = TRUE, nameLon = "SI_LONG", 
        nameLat = "SI_LATI", cellsizeX = cellsizeX, cellsizeY = cellsizeY, 
        we = we, ea = ea, no = no, so = so, breaks0 = breaks0, 
        legendncol = 2)
    title(sp)
 }  #end if(FRANCOIS)                      