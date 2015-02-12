
## TO INCORPORE AREA-BASED MANAGMEENT, WE USE A TRICK:
## TO MODEL E.G. A CLOSURE
# AND A FORBIDDEN-CROSSED ZONE WE STILL NEED TO USE THE SAME GRAPH
## (because of the underlying population layers on each node)
## BUT WHAT WE CAN DO IS TO ALTER THE DISTANCE BETWEEN NODES
# SAY: 10000km SO THAT NODES ARE LIKE DISCONNECTED BECAUSE THE
## DIJSKTRA ALGO WILL NEVER USE THOSE CONNECTIONS


 general <-
        list(main.path.ibm= file.path("C:","Users", "fbas", "Documents", "GitHub", "DISPLACE_input"))
 
 name_coord_file <- "coord115.dat"
 name_graph_file <- "graph115.dat"
 name_file       <- "nodes_in_polygons_a_graph115"
 

  idx_nodes_R_in_polygons <- NULL
 





##--------------------------------------------------------------------------##
##-------------UTILS--------------------------------------------------------##
##--------------------------------------------------------------------------##

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
 
 
##--------------------------------------------------------------------------##
##-------------LOAD THE GRAPH TO ALTER--------------------------------------##
##--------------------------------------------------------------------------##

## FILES FOR BUILDING A IGRAPH
load(file.path(general$main.path,"graphsspe", "11_graphibm.RData")) # built from the R code - balticonly DEN, DEU, SWE

##--------------------------------------------------------------------------##
##-------------DEFINE ZONES FOR CLOSURE-------------------------------------##
##--------------------------------------------------------------------------##

 library(maptools)
 library(rgdal)
  Scenario1             <<- readShapePoly(file.path(general$main.path,"graphsspe", "shp", "Scenario1_Habita525.shp"), 
                                    proj4string= CRS("+proj=longlat +ellps=WGS84"))
  Scenario1_proj        <<- spTransform(Scenario1, CRS("+proj=longlat +ellps=WGS84")) # actually,  already in WGS
  
  Scenario1             <<- readShapePoly(file.path(general$main.path,"graphsspe", "shp", "Scenario1_Habita525.shp"), 
                                    proj4string= CRS("+proj=longlat +ellps=WGS84"))
  Scenario1_proj        <<- spTransform(Scenario1, CRS("+proj=longlat +ellps=WGS84")) # actually,  already in WGS

  #Scenario2             <<- readShapePoly(file.path(general$main.path,"graphsspe", "shp", "Scenario2_OffshoreWindFarms.shp"), 
  #                                  proj4string= CRS("+proj=longlat +ellps=WGS84"))
  #Scenario2_proj        <<- spTransform(Scenario2, CRS("+proj=longlat +ellps=WGS84")) # actually,  already in WGS
    
  
  #Habitats_WGS84             <<- readShapePoly(file.path("C:","Users","fbas","Documents","GitHub","Elance_studiofuga","Habitats_WGS84","Habitats_WGS84.shp"), 
  #                                  proj4string= CRS("+proj=longlat +ellps=WGS84"))
  #
  #Habitats_WGS84$landscape_code<-Habitats_WGS84$grid_code
  #writeSpatialShape(Habitats_WGS84, file.path("C:","Users","fbas","Documents","GitHub","Elance_studiofuga","Habitats_WGS84","code_for_landscapes_WGS84.shp")
  #                                  )
  
  #shapeB             <<- readShapePoly(file.path("C:","Users","fbas","Documents","GitHub","Elance_studiofuga","Shapefiles","Shapefiles","Ices_Area_Edited.shp"), 
  #                                  proj4string= CRS("+proj=longlat +ellps=WGS84"))
  #
  #shapeB$node_code<-shapeB$Code
  #writeSpatialShape(shapeB, file.path("C:","Users","fbas","Documents","GitHub","Elance_studiofuga","Shapefiles","Shapefiles","code_for_areas_WGS84.shp")
  #                                  )

  
 
 plot(coord, xlim=c(8, 15), ylim=c(53,59), pch=".")
 plot(Scenario1_proj, add=TRUE, border=2)
 
 
  coords                <- SpatialPoints(cbind(long=coord[,1],lati=coord[,2]),
                               proj4string=CRS("+proj=longlat +datum=WGS84"))

  names(Scenario1_proj) # return the name of the coding variable

  SP1      <- SpatialPolygons(Scenario1_proj@polygons, proj4string=CRS("+proj=longlat +datum=WGS84"))
 
  idx <- over(coords, SP1)
  coord <-  cbind.data.frame(coordinates(coords), coord[,3], Scenario1 = Scenario1_proj$Id[idx])

 
  points(coord[!is.na(coord[,'Scenario1']) & coord[,3]==0, 'long'], coord[!is.na(coord[,'Scenario1']) & coord[,3]==0, 'lati'], pch="0", col=2)


 idx_nodes_R_in_polygons   <- rbind( idx_nodes_R_in_polygons,
                                        cbind(77, which(!is.na(coord[,'Scenario1']) & coord[,3]==0))
                                        ) # for R and the graph object
 idx_nodes_cpp_in_polygons <- idx_nodes_R_in_polygons # init
 idx_nodes_cpp_in_polygons[,2] <- idx_nodes_R_in_polygons[,2] -1 # offset cpp 

 colnames(idx_nodes_cpp_in_polygons) <- c("polygon", "node")





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

 
  mtext(side=1, "Longitude", cex=1, adj=0.5, line=2)
  mtext(side=2, "Latitude", cex=1, adj=0.5, line=2)
  mtext(side=3, "(a)", cex=1.5, adj=0, line=1)


  dev.off()
  
  
##--------------------------------------------------------------------------##
##-------------EXPORT A GRAPH BIS ------------------------------------------##
##--------------------------------------------------------------------------##


# export in txt format for use with C
# THIS WAY IS BETTER BECAUSE C IS ROW ORIENTED
coord <- as.matrix (signif(coord[, c(1:3)], 6))
write(coord, file=file.path(general$main.path.ibm, "graphsspe", name_coord_file), ncol=1)
nrow(coord)
 ncol(coord)
 # remnber that the idx.port is related to the one in harbour.list

# export in txt format for use with C
# THIS WAY IS BETTER BECAUSE C IS ROW ORIENTED
graph [,1:2] <- graph [,1:2] -1
write(signif(graph[, c(1:3)], 5),file=file.path(general$main.path.ibm, "graphsspe", name_graph_file), ncol=1)  # CONNECTION BETWEEN NODES + DISTANCE KM
nrow(graph)
 ncol(graph)


##--------------------------------------------------------------------------##
##-------------ALSO EXPORT THE CONCERNED GRAPH NODES -----------------------##
##--------------------------------------------------------------------------##

## copy/paste for quarters


write.table(idx_nodes_cpp_in_polygons,
           file=file.path(general$main.path.ibm, "graphsspe",
             paste(name_file, "_quarter1",".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)
  
write.table(idx_nodes_cpp_in_polygons,
           file=file.path(general$main.path.ibm, "graphsspe",
             paste(name_file,"_quarter2",".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)

write.table(idx_nodes_cpp_in_polygons,
           file=file.path(general$main.path.ibm, "graphsspe",
             paste(name_file,"_quarter3",".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)

write.table(idx_nodes_cpp_in_polygons,
           file=file.path(general$main.path.ibm, "graphsspe",
             paste(name_file,"_quarter4",".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)





