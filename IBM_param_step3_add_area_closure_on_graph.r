
## TO INCORPORE AREA-BASED MANAGMEENT, WE USE A TRICK:
## TO MODEL E.G. A CLOSURE
# AND A FORBIDDEN-CROSSED ZONE WE STILL NEED TO USE THE SAME GRAPH
## (because of the underlying population layers on each node)
## BUT WHAT WE CAN DO IS TO ALTER THE DISTANCE BETWEEN NODES
# SAY: 10000km SO THAT NODES ARE LIKE DISCONNECTED BECAUSE THE
## DIJSKTRA ALGO WILL NEVER USE THOSE CONNECTIONS


 general <-
        list(   main.path.ibm= file.path("C:","displace-project.org","repository","ibm_vessels_param"))
 idx_nodes_R_in_polygons <- NULL
 
 canadian_paper <- FALSE ; baltic_only <- TRUE
 
 
 if(canadian_paper){
    a_year                     <- 2010
    windmills                  <- FALSE
    windmills_and_NATURA2000   <- FALSE
    name_coord_file <- "coord41.dat"
    name_graph_file <- "graph41.dat"
    name_file       <- "nodes_in_polygons_a_graph41"
    define_from_polygons <- TRUE ; define_from_shapefile_polygons <- FALSE 
  }


 
 if(baltic_only){
    a_year                     <- 2012
    windmills                  <- FALSE
    windmills_and_NATURA2000   <- FALSE
    NATURA2000_only            <- TRUE
    if(windmills) {
       name_coord_file <- "coord111.dat"
       name_graph_file <- "graph111.dat"
       name_file       <- "nodes_in_polygons_a_graph111"
       }
    if(windmills_and_NATURA2000) {
       name_coord_file <- "coord112.dat"
       name_graph_file <- "graph112.dat"
       name_file       <- "nodes_in_polygons_a_graph112"
       }
    if(NATURA2000_only) {
       name_coord_file <- "coord113.dat"
       name_graph_file <- "graph113.dat"
       name_file       <- "nodes_in_polygons_a_graph113"
       }
    define_from_polygons <- TRUE ; define_from_shapefile_polygons <- TRUE 
  }







##--------------------------------------------------------------------------##
##-------------UTILS--------------------------------------------------------##
##--------------------------------------------------------------------------##
a.shortest.path.old.igraph <- function (from = 6, to = 1001, g=g, is.plot=FALSE, a.color=2) {
from <- from - 1; to <- to - 1 # CAUTION! idx from 0 in g (!)
path <- unlist(get.shortest.paths(g, from, to , weights= g[[9]][[4]]$dist.km)) # dijkstra´s algorithm in library(igraph)
x.path <- g[[9]][[3]]$x[path+1]
y.path <- g[[9]][[3]]$y[path+1]
if(is.plot){
  lines(x.path,y.path,col=a.color, lwd=1.6)
  points(x.path,y.path,col=a.color, pch=16, cex=0.6)
  # i.e start point is points(coord[from,1], coord[from,2], col=3,pch=16)
  # i.e stop point is points(coord[to,1], coord[to,2],col=3, pch=16)
  }
return(path+1) # index pts compatible with coord
}

a.shortest.path <- function (from = 6, to = 1001, g=g, is.plot=FALSE, a.color=2) {
path <- unlist(get.shortest.paths(g, from, to , weights= get.edge.attribute(g, "dist.km"))) # dijkstra´s algorithm in library(igraph)
x.path <-get.vertex.attribute(g, "x")[path]
y.path <- get.vertex.attribute(g, "y")[path]
if(is.plot){
  lines(x.path,y.path,col=a.color, lwd=1.6)
  points(x.path,y.path,col=a.color, pch=16, cex=0.6)
  # i.e start point is points(coord[from,1], coord[from,2], col=3,pch=16)
  # i.e stop point is points(coord[to,1], coord[to,2],col=3, pch=16)
  }
return(path) # index pts compatible with coord
}
 

#--------------------------- 
# loop over the SpatialPoly
detectCoordInPolygonsFromSH <- function (sh, coord, name_column="poly1"){

     coord <- eval(parse(text=paste("cbind(coord, ",name_column,"= 0)", sep='')))
     dd                   <- sapply(slot(sh, "polygons"), function(x) lapply(slot(x, "Polygons"), function(x) x@coords)) # tricky there...
     library(sp)
     for(iLand in 1:length(dd)){
      if(length(dd)>1){
       for(i in 1:length(dd[[iLand]])){
          print(iLand)
          # Points on land are 1 or 2 and not is 0
          er <- try({res   <- point.in.polygon(coord[,1],coord[,2],dd[[iLand]][[i]][,1],dd[[iLand]][[i]][,2])}, silent=TRUE)
          if(class(er)=="try-error") res   <- point.in.polygon(coord[,1],coord[,2],dd[[iLand]][[i]][,1],dd[[iLand]][[i]][,2])
          coord[which(res!=0), name_column] <- 1
       }
      } else{
          er <- try({res   <- point.in.polygon(coord[,1],coord[,2],dd[[1]][,1],dd[[1]][,2])}, silent=TRUE)
          if(class(er)=="try-error") res   <- point.in.polygon(coord[,1],coord[,2],dd[[1]][,1],dd[[1]][,2])
          coord[which(res!=0), name_column] <- 1
      
      } 
      
     }
 return(coord)
 }
 
##--------------------------------------------------------------------------##
##-------------LOAD THE GRAPH TO ALTER--------------------------------------##
##--------------------------------------------------------------------------##

## FILES FOR BUILDING A IGRAPH
#load(file.path(general$main.path,"igraph","4_graphibm.RData")) # built from the R code
#load(file.path(general$main.path.ibm,"igraph","8_graphibm.RData")) # built from the R code - canadian
load(file.path(general$main.path.ibm,"igraph","11_graphibm.RData")) # built from the R code - balticonly DEN, DEU, SWE

if(windmills)                name_plot <- "map_graph111_points.png"
if(windmills_and_NATURA2000) name_plot <- "map_graph112_points.png"
if(NATURA2000_only)          name_plot <- "map_graph113_points.png"

# check the effect on the same shortest paths
# RE-BUILD THE IGRAPH OBJECT IN R 
  png(filename=file.path(general$main.path.ibm,"igraph",
      # paste("map_graph41_points.png",sep='')),
      # paste("map_graph81_points.png",sep='')),
       paste(name_plot,sep='')),
                                   width = 1600, height = 1600, 
                                   units = "px", pointsize = 12,  res=300)
  #windows(5,5)
 
 #  a plot
 library(maptools)
 library(maps)
 library(mapdata)
 plot(coord[,1], coord[,2], pch=".", xlab="", ylab="", xlim=c(-8,23), ylim=c(52,65))
 ices_areas <- readShapeSpatial(file.path(general$main.path.ibm, "ices_areas","ices_areas"))
 plot(ices_areas, col="white",  add=TRUE)
 points(coord[,1], coord[,2], pch=".")
 box()
  map("worldHires", add=TRUE)
   library(maptools)
 sh1 <- readShapePoly(file.path(general$main.path.ibm,"shp","francois_EU"))
 plot(sh1,  col=grey(0.7), add=TRUE)

 library(igraph)
 #a.shortest.path  (from = 2, to = 3170, g=g, is.plot=TRUE, a.color=5)
#a.shortest.path  (from = 725, to = 2341, g=g, is.plot=TRUE, a.color=3)

##--------------------------------------------------------------------------##
##-------------DEFINE ZONES FOR CLOSURE-------------------------------------##
##--------------------------------------------------------------------------##

## 1.
if(define_from_polygons){

 if(canadian_paper){
   nb_areas <- 3
   assign("pol1", list(x=c(1,2,5), y=c(55,54,56)))# dogger bank
   assign("pol2", list(x=c(5.997407, 5.997407, 6.924934, 8.130718, 8.130718), 
           y=c(58.03101, 57.05588, 57.05588, 57.56666, 58.03101))) # demersal skaggerak/NS
   assign("pol3", list(x=c(14.09476, 14.09476, 15.93514, 15.93514), 
           y=c(55.43405,54.98306, 55.02406, 55.51604))) # Baltic Bornholm
 }
 if(baltic_only &&  (windmills_and_NATURA2000 || windmills)){
    nb_areas <- 7
    # plot polygons retrieved from the map 'windmills_OWP_Ostsee_30102013' (Leyre Goti) using locator()!!
    # sent by Leyre Goti 7 Jan14
    #windmills_OWP_Ostsee_30102013                      <<- readShapePoly(file.path(general$main.path.ibm,"shp","Windmills", "January_2014_ger", "OWP_Ostsee_30102013.shp"),
    #                               proj4string=CRS("+proj=utm  +ellps=intl +zone=32 +towgs84=-81,-89,-115,0,0,0,0,0"))
    #plot(windmills_OWP_Ostsee_30102013, add=TRUE, border=3)
     assign("pol1", list(x=c(11.41674, 11.53503, 11.55322, 11.60175, 11.56839),
            y=c(54.46500, 54.40737, 54.41344, 54.41647, 54.43770))) # ger  from locator()
     assign("pol2", list(x=c(11.38338, 11.41978, 11.42281, 11.38338),
            y=c(54.24056, 54.25573, 54.30729, 54.29516))) # ger from locator()
     assign("pol3", list(x=c(13.55495, 13.79758, 13.95226, 13.89160, 13.79758, 13.80061, 13.73692, 13.61561),
            y=c(54.83501, 54.77132, 54.76222, 54.90174, 54.89567, 54.80772, 54.80468, 54.85624))) # ger from locator()
     assign("pol4", list(x=c(13.98562, 14.04325, 14.15243, 14.17669, 14.02505),
            y=c( 54.92297, 54.76222, 54.75009, 54.80468, 54.92903))) # ger from locator()   
    # plot polygons retrieved from the map 'windmillmap1' using locator()!!
     assign("pol5", list(x=c( 15.37101, 14.73002, 15.00666, 14.98642, 15.18883, 15.78258, 15.50595, 15.39125),
            y=c( 54.58778, 54.52937, 54.49433, 54.42424, 54.33858, 54.44760, 54.51380, 54.52158))) # poland from locator()
     assign("pol6", list(x=c(  12.20245, 12.06557, 12.02824, 12.16512),
            y=c(  56.58406, 56.68640, 56.65911, 56.53972))) # swe kattegat, Stora middelgrund
     assign("pol7", list(x=c(  12.38911, 12.33311, 12.29578, 12.33311),
            y=c( 56.83308, 56.86719, 56.84672, 56.80238))) # swe kattegat, kattegat offshore skottarevprojektet
    # i.e. for Sweden and Germany, use from screen capture http://www.4coffshore.com/offshorewind/ Nov 2013
    #library(raster)
    #oth_windmills       <<- raster(file.path("C:","Users","fba","Dropbox","ibm_vessels_param","shp","Windmills", "windmillmap1.tif"))
    #newproj          <- "+proj=longlat +datum=WGS84"
    #oth_windmills  <<- projectRaster(oth_windmills, crs=newproj)
    ##plot( oth_windmills, xlim= c(10,13), ylim=c(54,55.5) )

 }
 
 
 ## CAUTION: MAKE SURE THE GRAPH IS NOT IMPLICTELY DISCONNECTED BY THE IMPLEMENTATION
 ## OF THESE AREA!!  E.G: CAUTION IN SKAGERRAK
 ## CAUTION: MAKE SURE THAT YOU DO NOT INCLUDE SOME PORTS!

 # test inclusion...
 library(sp)
 pts <- matrix(0, ncol=nb_areas, nrow=nrow(coord))
 for(pol in 1:nb_areas){
   pts[,pol] <- point.in.polygon(point.x=coord[,1], point.y=coord[,2],
                    pol.x=get(paste("pol",pol,sep=''))$x, pol.y=get(paste("pol",pol,sep=''))$y, mode.checked=FALSE)
   }

 # visual check
 dd <- apply(pts, 1, sum)
 points(coord[which(dd!=0 & coord[,3]==0),], col="green", pch=".", cex=1.0) # in polygons AND not a port
 polygon(pol3, border="red", lwd=2)



 for(pol in 1:nb_areas){
   idx_nodes_R_in_polygons   <- rbind( idx_nodes_R_in_polygons,
                                       cbind(pol, which(pts[,pol]!=0 & coord[,3]==0)) 
                                ) # for R and the graph object
   }
   
 idx_nodes_cpp_in_polygons <- idx_nodes_R_in_polygons # init
 idx_nodes_cpp_in_polygons[,2] <- idx_nodes_R_in_polygons[,2] -1 # offset cpp 

 colnames(idx_nodes_cpp_in_polygons) <- c("polygon", "node")

 }
 
 
## 2.
if(define_from_shapefile_polygons){
 library(maptools)
 library(rgdal)
 # planned nearshore windmills  # http://www.ens.dk/en/supply/renewable-energy/wind-power/offshore-wind-power/new-nearshore-wind-tenders
  windmills_KriegersFlak                      <<- readShapePoly(file.path(general$main.path.ibm,"shp","Windmills", "November_2013", "Havvindmoeller", "KriegersFlak_polygon.shp"),
                                   proj4string=CRS("+proj=utm  +ellps=intl +zone=32 +towgs84=-81,-89,-115,0,0,0,0,0"))
  windmills_KriegersFlak                      <<- spTransform(windmills_KriegersFlak, CRS("+proj=longlat +ellps=WGS84"))    # convert to longlat

  windmills_Kystmølle_zoner2012                      <<- readShapePoly(file.path(general$main.path.ibm,"shp","Windmills", "November_2013", "Havvindmoeller", "Kystmølle_zoner2012.shp"),
                                   proj4string=CRS("+proj=utm  +ellps=intl +zone=32 +towgs84=-81,-89,-115,0,0,0,0,0"))
  windmills_Kystmølle_zoner2012                      <<- spTransform(windmills_Kystmølle_zoner2012, CRS("+proj=longlat +ellps=WGS84"))    # convert to longlat

  windmills_U_omr_StoreMiddelgrund                      <<- readShapePoly(file.path(general$main.path.ibm,"shp","Windmills", "November_2013", "Havvindmoeller", "U_omr_StoreMiddelgrund.shp"),
                                   proj4string=CRS("+proj=utm  +ellps=intl +zone=32 +towgs84=-81,-89,-115,0,0,0,0,0"))
  windmills_U_omr_StoreMiddelgrund                      <<- spTransform(windmills_U_omr_StoreMiddelgrund, CRS("+proj=longlat +ellps=WGS84"))    # convert to longlat

  windmills_U_omr_Ronne                      <<- readShapePoly(file.path(general$main.path.ibm,"shp","Windmills", "November_2013", "Havvindmoeller", "U_omr_Ronne.shp"),
                                   proj4string=CRS("+proj=utm  +ellps=intl +zone=32 +towgs84=-81,-89,-115,0,0,0,0,0"))
  windmills_U_omr_Ronne                      <<- spTransform(windmills_U_omr_Ronne, CRS("+proj=longlat +ellps=WGS84"))    # convert to longlat

  windmills_U_omr_Ringkobing                      <<- readShapePoly(file.path(general$main.path.ibm,"shp","Windmills", "November_2013", "Havvindmoeller", "U_omr_Ringkobing.shp"),
                                   proj4string=CRS("+proj=utm  +ellps=intl +zone=32 +towgs84=-81,-89,-115,0,0,0,0,0"))
  windmills_U_omr_Ringkobing                      <<- spTransform(windmills_U_omr_Ringkobing, CRS("+proj=longlat +ellps=WGS84"))    # convert to longlat

  windmills_U_omr_Jammmerbugt                      <<- readShapePoly(file.path(general$main.path.ibm,"shp","Windmills", "November_2013", "Havvindmoeller", "U_omr_Jammmerbugt.shp"),
                                   proj4string=CRS("+proj=utm  +ellps=intl +zone=32 +towgs84=-81,-89,-115,0,0,0,0,0"))
  windmills_U_omr_Jammmerbugt                      <<- spTransform(windmills_U_omr_StoreMiddelgrund, CRS("+proj=longlat +ellps=WGS84"))    # convert to longlat
 
 
  # from Leyre Goti, further processed by Kerstin G. to restrict to western baltic
  # European Environment Agency http://www.eea.europa.eu/data-and-maps/data/natura-4/natura-2000-tabular-data-12-tables
  # NOTE THAT THESE AREAS ARE PROTECTED 'ON THE PAPER'
  # BUT NOTHING SPECIFY THAT THE FISHING ACTIVITIES ARE RESTRICTED INSIDE THEM...
  # BECAUSE THE USUAL (EU or NATIONAL) PROCEDURE IS TO CONDUCT AN IMPACT ASSESSEMENT FOR EACH OF THEM...WHICH IS NOT DONE YET!
  if(windmills_and_NATURA2000 || NATURA2000_only){
  NATURA2000          <<- readShapePoly(file.path(general$main.path,"shp", "Natura2000_end2012_rev1_9_15_53_60","Natura2000_end_2012_rev1_9_15_53_60_water_1.shp"), 
                                    proj4string= CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"))
  NATURA2000_proj       <<- spTransform(NATURA2000,CRS("+proj=longlat +ellps=WGS84")) # actually,  already in WGS
  } 
  
 
  
 
 plot(coord, xlim=c(8, 15), ylim=c(53,59), pch=".")
 plot(windmills_KriegersFlak, add=TRUE, border=2)
 plot(windmills_Kystmølle_zoner2012, add=TRUE, border=2)
 plot(windmills_U_omr_StoreMiddelgrund, add=TRUE, border=2)
 plot(windmills_U_omr_Ronne, add=TRUE, border=2)
 plot(windmills_U_omr_Ringkobing, add=TRUE, border=2)
 plot(windmills_U_omr_Jammmerbugt, add=TRUE, border=2)
 plot(NATURA2000_proj, add=TRUE, border=3)
 
 if(windmills_and_NATURA2000 || windmills){
  coord <- detectCoordInPolygonsFromSH (windmills_KriegersFlak, coord, name_column="windmills_KriegersFlak")
  coord <- detectCoordInPolygonsFromSH (windmills_Kystmølle_zoner2012, coord, name_column="windmills_Kystmølle_zoner2012")
  coord <- detectCoordInPolygonsFromSH (windmills_U_omr_StoreMiddelgrund, coord, name_column="windmills_U_omr_StoreMiddelgrund")
  coord <- detectCoordInPolygonsFromSH (windmills_U_omr_Ronne, coord, name_column="windmills_U_omr_Ronne")
  coord <- detectCoordInPolygonsFromSH (windmills_U_omr_Ringkobing, coord, name_column="windmills_U_omr_Ringkobing")
  coord <- detectCoordInPolygonsFromSH (windmills_U_omr_Jammmerbugt, coord, name_column="windmills_U_omr_Jammmerbugt")
 
 points(coord[coord[,'windmills_KriegersFlak']!=0 & coord[,3]==0, 'x'], coord[coord[,'windmills_KriegersFlak']!=0 & coord[,3]==0, 'y'], pch=".", col=3)
 points(coord[coord[,'windmills_Kystmølle_zoner2012']!=0 & coord[,3]==0, 'x'], coord[coord[,'windmills_Kystmølle_zoner2012']!=0 & coord[,3]==0, 'y'], pch=".", col=5)
 points(coord[coord[,'windmills_U_omr_StoreMiddelgrund']!=0 & coord[,3]==0, 'x'], coord[coord[,'windmills_U_omr_StoreMiddelgrund']!=0 & coord[,3]==0, 'y'], pch=".", col=6)
 points(coord[coord[,'windmills_U_omr_Ronne']!=0 & coord[,3]==0, 'x'], coord[coord[,'windmills_U_omr_Ronne']!=0 & coord[,3]==0, 'y'], pch=".", col=6)
 points(coord[coord[,'windmills_U_omr_Ringkobing']!=0 & coord[,3]==0, 'x'], coord[coord[,'windmills_U_omr_Ringkobing']!=0 & coord[,3]==0, 'y'], pch=".", col=6)
 points(coord[coord[,'windmills_U_omr_Jammmerbugt']!=0 & coord[,3]==0, 'x'], coord[coord[,'windmills_U_omr_Jammmerbugt']!=0 & coord[,3]==0, 'y'], pch=".", col=6)
 }
  if(windmills_and_NATURA2000 || NATURA2000_only){
  coord <- detectCoordInPolygonsFromSH (NATURA2000_proj, coord, name_column="Natura2000")

 points(coord[coord[,'Natura2000']!=0 & coord[,3]==0, 'x'], coord[coord[,'Natura2000']!=0 & coord[,3]==0, 'y'], pch="0", col=2)
}
 
 

 if(windmills_and_NATURA2000 || windmills){
  idx_nodes_R_in_polygons   <- rbind( idx_nodes_R_in_polygons,
                                        cbind(11, which(coord[,'windmills_KriegersFlak']!=0 & coord[,3]==0)),
                                        cbind(22, which(coord[,'windmills_Kystmølle_zoner2012']!=0 & coord[,3]==0)),
                                        cbind(33, which(coord[,'windmills_U_omr_StoreMiddelgrund']!=0 & coord[,3]==0)),
                                        cbind(44, which(coord[,'windmills_U_omr_Ronne']!=0 & coord[,3]==0)),
                                        cbind(55, which(coord[,'windmills_U_omr_Ringkobing']!=0 & coord[,3]==0)),
                                        cbind(66, which(coord[,'windmills_U_omr_Jammmerbugt']!=0 & coord[,3]==0))
                                        ) # for R and the graph object
 }
 if(windmills_and_NATURA2000 || NATURA2000_only){
  idx_nodes_R_in_polygons   <- rbind( idx_nodes_R_in_polygons,
                                        cbind(77, which(coord[,'Natura2000']!=0 & coord[,3]==0))
                                        ) # for R and the graph object
 }
 idx_nodes_cpp_in_polygons <- idx_nodes_R_in_polygons # init
 idx_nodes_cpp_in_polygons[,2] <- idx_nodes_R_in_polygons[,2] -1 # offset cpp 

 colnames(idx_nodes_cpp_in_polygons) <- c("polygon", "node")


}





##--------------------------------------------------------------------------##
##-------------CHANGE DISTANCE TO/FROM CONCERNED NODES TO 900---------------##
##--------------------------------------------------------------------------##
# add 900km of penalty is of course arbitrary...
init_graph <- graph # from load()
graph[ graph[,1] %in% idx_nodes_R_in_polygons[,2], 'dist.km'] <- 900 +  # penalty
                      graph[ graph[,1] %in% idx_nodes_R_in_polygons[,2], 'dist.km']
graph[ graph[,2] %in% idx_nodes_R_in_polygons[,2] & graph[,3] <900, 'dist.km'] <- 900 +  # penalty
                     graph[ graph[,2] %in% idx_nodes_R_in_polygons[,2] & graph[,3] <900, 'dist.km']
graph[, 'dist.km'] <- round(graph[, 'dist.km'],2) 

#=> so the vessel is likely to avoid the zone, 
# UNLESS the vessel actually target a node inside the zone (i.e. in fgrounds)
# if closure, then additional thing to do in the cpp code: put the freq_fgrounds very low for nodes in polygons i.e. 99% compliance


 library(igraph)
 vertices    <- data.frame(name=as.character(unique(graph[,1])), x=coord[,1], y=coord[,2]) #(**)
 edges        <- data.frame(from=c(graph[,1]),
                        to=c(graph[,2]),
                        dist.km=graph[,3])
 g_with_closures <- graph.data.frame(edges, directed=FALSE, vertices=vertices)
 a.shortest.path  (from = 2, to = 3170, g=g_with_closures, is.plot=TRUE, a.color=5)
 a.shortest.path  (from = 725, to = 2341, g=g_with_closures, is.plot=TRUE, a.color=3)
 a.shortest.path  (from = 725, to = 9800, g=g_with_closures, is.plot=TRUE, a.color=2)
 a.shortest.path  (from = 725, to = 9700, g=g_with_closures, is.plot=TRUE, a.color=4)

 #plot(1:100,1/(1+exp(0.2*((1:100)-20))))
 
 text(5.6,58.5, "B", cex=1.5)
 text(3.8,54.5, "A", cex=1.5)
 text(17,55.5, "C", cex=1.5)
 
  mtext(side=1, "Longitude", cex=1, adj=0.5, line=2)
    mtext(side=2, "Latitude", cex=1, adj=0.5, line=2)
    mtext(side=3, "(b)", cex=1.5, adj=0, line=1)


  #savePlot(filename=file.path(general$main.path,"igraph",
       #paste("map_graph41_points.jpeg",sep='')), type="jpeg")
  #     paste("map_graph81_points.jpeg",sep='')), type="jpeg")
  dev.off()
  
  
##--------------------------------------------------------------------------##
##-------------EXPORT A GRAPH BIS ------------------------------------------##
##--------------------------------------------------------------------------##


# export in txt format for use with C
# THIS WAY IS BETTER BECAUSE C IS ROW ORIENTED
write(signif(coord[, c(1:3)], 6),file=file.path(general$main.path.ibm,"igraph", name_coord_file), ncol=1)
nrow(coord)
 ncol(coord)
 # remnber that the idx.port is related to the one in harbour.list

# export in txt format for use with C
# THIS WAY IS BETTER BECAUSE C IS ROW ORIENTED
graph [,1:2] <- graph [,1:2] -1
write(signif(graph[, c(1:3)], 5),file=file.path(general$main.path.ibm,"igraph", name_graph_file), ncol=1)  # CONNECTION BETWEEN NODES + DISTANCE KM
nrow(graph)
 ncol(graph)


# list of harbour nodes
which(coord[,"idx.port"]!=0)-1


# distance between two points in long lat
distance <- function (x1, y1, x2, y2)
{
    p = 180/3.1415;
    Rearth = 6371.0;
    res = Rearth * acos(sin(y1/p)*sin(y2/p) + (cos(y1/p) * cos(y2/p)*cos(x1/p - x2/p)));
    return(res);
}
distance(2.561737, 55.61520, 2.707887,53.90234)

##--------------------------------------------------------------------------##
##-------------ALSO EXPORT THE CONCERNED GRAPH NODES -----------------------##
##--------------------------------------------------------------------------##

## copy/paste for quarters


write.table(idx_nodes_cpp_in_polygons,
           file=file.path(general$main.path.ibm, "igraph",
             paste(name_file, "_quarter1",".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)
  
write.table(idx_nodes_cpp_in_polygons,
           file=file.path(general$main.path.ibm, "igraph",
             paste(name_file,"_quarter2",".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)

write.table(idx_nodes_cpp_in_polygons,
           file=file.path(general$main.path.ibm, "igraph",
             paste(name_file,"_quarter3",".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)

write.table(idx_nodes_cpp_in_polygons,
           file=file.path(general$main.path.ibm, "igraph",
             paste(name_file,"_quarter4",".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)





