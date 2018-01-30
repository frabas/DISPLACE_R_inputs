 
   # GENERAL SETTINGS

   # CAUTION: either using the already existing fishing_gear_selectivity_ogives_per_stock.csv file
   # or creating it from scratch from L50 hardcoded L50 parameters...

   args <- commandArgs(trailingOnly = TRUE)

   general <- list()

   if (length(args) < 2) {
     if(.Platform$OS.type == "windows") {
       general$application           <- "testexample" # ...or myfish
       general$main_path_gis         <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis", general$application)
       general$main.path.ibm         <- file.path("C:","Users","fbas","Documents","GitHub",paste("DISPLACE_input_", general$application, sep=''))
       general$igraph                <- 56  # caution: should be consistent with existing vessels already built upon a given graph
       do_plot                       <- TRUE
     }
  } else {
       general$application           <- args[1]
       general$main_path_gis         <- args[2]
       general$main.path.ibm         <- args[3]
       general$igraph                <- args[4]  # caution: should be consistent with existing vessels already built upon a given graph
       do_plot                       <- FALSE
  }
   cat(paste("START \n"))


 library(maptools)
 library(rgeos)
 library(raster)


 # OPTIONAL, OPTIONAL, OPTIONAL
 # the goal is to retrieve the coastal nodes to inform the shortPaths library building with them
 # in order to optimize the size of the .bin files

 # just an example....
 ices_areas             <- readShapePoly(file.path(general$main_path_gis, "MANAGEMENT", "ices_areas","ices_areas.shp"),
                                    proj4string= CRS("+proj=longlat +ellps=WGS84"))

 # e.g.
 ices_areas[ices_areas$ICES_area == "29",]

 # zone to include in the mask
 ices_areas$ICES_area
 zones    <- c("IIIa", "22", "23", "24", "25")
 a_shape <- ices_areas[ices_areas$ICES_area %in% zones,]
 



 # reduce the complexity of the shapefile
 a_smoothed_shape <- gSimplify(a_shape, tol=0.01, topologyPreserve=TRUE)
 sauv <- a_smoothed_shape

 area <- lapply(a_smoothed_shape@polygons, function(x) sapply(x@Polygons, function(y) y@area))
 mainPolys <- lapply(area, function(x) which(x > 0.001))

     for(i in 1:length(mainPolys)){
      if(length(mainPolys[[i]]) >= 1 && mainPolys[[i]][1] >= 1){
        a_smoothed_shape@polygons[[i]]@Polygons  <- a_smoothed_shape@polygons[[i]]@Polygons[mainPolys[[i]]]
        a_smoothed_shape@polygons[[i]]@plotOrder <- 1:length(a_smoothed_shape@polygons[[i]]@Polygons)
      }
    }

 a_smoothed_shape <- SpatialPolygonsDataFrame(a_smoothed_shape, data=data.frame(1:length(mainPolys)), match.ID = FALSE)

 writeSpatialShape(a_smoothed_shape, file.path(general$main_path_gis, "MANAGEMENT", "smoothed_shape.shp")
                                   )

 # reproject
  library(sp)
  library(rgdal)
  UTMzone  <- 32
  a_smoothed_shape_utm <- spTransform(a_smoothed_shape, CRS(paste("+proj=utm +zone=",UTMzone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep=''))) 
 
 
  
 # do a buffer
 dd <-sapply(slot(a_smoothed_shape_utm, "polygons"), function(x) lapply(slot(x, "Polygons"), function(x) x@coords)) # tricky there...

 buff.minus3nm_3nm <- list()
  plot(a_smoothed_shape_utm, xlim=c(300000, 700000), ylim=c(5750000, 7290000)) # utm 32
 for(iLand in 1:length(dd)){
  #if(a_smoothed_shape_utm@data$C_NAME[iLand]=="DENMARK") {
       p1 <- readWKT(paste("POLYGON((",paste(dd[[iLand]][[1]][,1], dd[[iLand]][[1]][,2], collapse=","),"))", sep=''))
       buff.minus3nm <- gBuffer(p1,width=-3*1853)
       buff.3nm <- gBuffer(p1,width=3*1853)
       
       #plot(buff.minus3nm, add=TRUE)
       #plot(buff.3nm, add=TRUE)
      
    # }


     # then build the buffer strips!
     buff.minus3nm_3nm[[iLand]] <- gDifference( buff.3nm, buff.minus3nm, byid=TRUE)
     projection(buff.minus3nm_3nm[[iLand]])        <-  CRS(paste("+proj=utm +zone=",UTMzone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep=''))
  
     plot (buff.minus3nm_3nm[[iLand]], add=TRUE )

  }

 



 # find out the graph points lying into the buffer zone

  # load the graph
  coord <- read.table(file=file.path(general$main_path_gis, "GRAPH",
             paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
  coord <- as.matrix(as.vector(coord))
  coord <- matrix(coord, ncol=3)
  coord <- cbind(coord, 1:nrow(coord))
  colnames(coord) <- c('x', 'y', 'harb', 'pt_graph')
  if(do_plot) plot(coord[,1], coord[,2])

  SP       <- SpatialPoints(cbind(as.numeric(as.character(coord[,'x'])), as.numeric(as.character(coord[,'y']))),
                       proj4string=CRS("+proj=longlat +ellps=WGS84"))
  UTMzone  <- 32
  coord <- cbind.data.frame(coord,
                 spTransform(SP,CRS(paste("+proj=utm +zone=",UTMzone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep=''))))    # convert to UTM
  
  spo                    <- SpatialPoints(coordinates(data.frame(x=as.numeric(as.character(coord[,"coords.x1"])),
                                             y=as.numeric(as.character(coord[,"coords.x2"])))))
  projection(spo)        <-  CRS(paste("+proj=utm +zone=",UTMzone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep=''))

  # loop
  relevant_nodes <- NULL
  for(iLand in 1: length(buff.minus3nm_3nm))
     {

     idx                    <- over(spo, buff.minus3nm_3nm[[iLand]])  
     relevant_nodes         <-  c(relevant_nodes, names(idx[!is.na(idx)]))
  }
   
   
  relevant_nodes <- unique(as.numeric(relevant_nodes))
  
  
  #check:
  plot(coord[, "coords.x1"], coord[,"coords.x2"], pch=".")
  points(coord[relevant_nodes, "coords.x1"], coord[relevant_nodes,"coords.x2"], pch=".", col=2)
  for(iLand in 1: length(buff.minus3nm_3nm)) plot(buff.minus3nm_3nm[[iLand]], add=TRUE)
  
  # export for c++ identifier
  relevant_nodes <- relevant_nodes -1 ##!!## CAUTION: C++ OFFSET BY 1 ##!!##!!##
  
  
  # export final result
  write.table(cbind.data.frame(type= "coastline", nodeids=relevant_nodes),
              file=file.path(general$main.path.ibm, "graphsspe", "idx_additional_relevant_nodes_in_building_shortPaths.dat"),
              col.names=TRUE, row.names=FALSE, sep=" ", quote=FALSE)
 
  #=> to supplement the fgrounds when creating the shortPaths library (optional)
  
  
  

