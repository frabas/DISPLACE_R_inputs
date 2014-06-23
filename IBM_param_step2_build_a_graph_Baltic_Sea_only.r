

##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!FUNCTIONS FOR CREATING THE GRAPH!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

create_graph<-function(general){


 #load(file=file.path(general$main.path, "ports_to_keep_2010.RData")) # get port.to.keep built with IBM_param_step1.r 
 load(file=file.path(general$main_path_ibm_param, "ports_to_keep_2012.RData")) # get port.to.keep built with IBM_param_step1.r 
 EUports <- ports.to.keep[!is.na(ports.to.keep$lat),]

 EUports$NAVN     <- EUports$Description
 EUports$DEC_LONG <- EUports$lon
 EUports$DEC_LAT  <- EUports$lat
 
  # move few inland ports (or move them to force connection!)
 EUports[EUports$NAVN=="Aalborg","DEC_LONG"] <- EUports[EUports$NAVN=="Aalborg","DEC_LONG"] +0.3
 EUports[EUports$NAVN=="Nykøbing Mors",c("DEC_LONG","DEC_LAT")] <- EUports[EUports$NAVN=="Nykøbing Mors",c("DEC_LONG","DEC_LAT")] -0.6
 EUports[EUports$NAVN=="Brande",c("DEC_LONG","DEC_LAT")] <- EUports[EUports$NAVN=="Brande",c("DEC_LONG","DEC_LAT")] -0.6
 EUports[EUports$NAVN=="Flensburg",c("DEC_LONG", "DEC_LAT")] <- c(9.752067, 54.81383)
 EUports[EUports$NAVN=="NA50",c("DEC_LONG", "DEC_LAT")] <- c( 9.988302, 55.83189)
 EUports[EUports$NAVN=="Cuxhaven",c("DEC_LONG", "DEC_LAT")] <- c(  8.84225, 53.82392)
 EUports[EUports$NAVN=="NA42",c("DEC_LONG", "DEC_LAT")] <- c(  11.2537, 55.98798)
 EUports[EUports$NAVN=="Maasholm","DEC_LONG"] <- EUports[EUports$NAVN=="Maasholm","DEC_LONG"] +0.1
 EUports[EUports$NAVN=="Egernsund",c("DEC_LONG", "DEC_LAT")] <- c(  9.762419, 54.84012)

 
                    
 harbour.list  <-  EUports

 
 harbour.list$NAVN     <- factor(harbour.list$NAVN)
 harbour.list$idx.port <- harbour.list$NAVN
 harbour.list$idx.port <- 1:length(harbour.list$NAVN) # convert in numeric for soft use


 harbours <- harbour.list[,c("DEC_LONG","DEC_LAT","idx.port")] # longlat
 colnames(harbours) <- c("x","y","idx.port") # rename
 rownames(harbours) <- harbours$idx.port
 harbours <- as.matrix(harbours)



 # create the underlying igraph
 # (caution: remenber that length(ext.y)/2 need to be pair)
 
 do.triangular.grid <- function(dx, dy, xdep, xarr, ydep, yarr){
 # noisy triangle grid with dx and dy
 ext.x <- seq(xdep ,xarr ,by=2*dx)
 ext.x2 <- seq(xdep + dx/2, xarr + dx/2, by=2*dx)
 ext.y <- seq(ydep, yarr,by=dy)
 x <-  rep(c(ext.x,ext.x2), length(ext.y)/2)  # caution: length(ext.y)/2 need to be pair
 y  <- rep(c(ext.y), each=length(ext.x))
 x <- x+ rnorm(length(x),0,0.001)
 y <- y+ rnorm(length(y),0,0.001)
 return(cbind(x,y))
 }
 
 do.regular.grid <- function(dx, dy, xdep, xarr, ydep, yarr){
 # noisy regular grid with dx and dy
 ext.x <- seq(xdep ,xarr ,by=dx)
 ext.y <- seq(ydep, yarr, by=dy)
 x <-  rep(c(ext.x), length(ext.y)) 
 y  <- rep(c(ext.y), each=length(ext.x))
 x <- x+ rnorm(length(x),0,0.0001)
 y <- y+ rnorm(length(y),0,0.0001)
 return(cbind(x,y))
 }

 # various grid resolution centered on dk and western baltic
 # (1 degree longitude = 67.6 km at 57 degree North i.e. middle of North Sea, 
 #  while 1 degree latitude =111.2 km also at 57 degree North (but actually almost constant whatever the latitude)
 # caution: avoid bias: apply one resolution grain only per management area becausae of evenly distributed avai.
 #grid1.xy <- do.regular.grid(dx=2.218935,   dy=1.348921, -13.5, 25, 48,   67.0)  # rought grain: 150 by 150 km grid
 grid1.xy <- do.regular.grid(dx= 1.47929,   dy= 0.8992807, -13.5, 25, 48,   67.0)  # rought grain: 100 by 100 km grid
 grid2.xy <- do.regular.grid(dx= 0.03698225,   dy=0.02248201,   7,  25, 53,   60.1)  # finest grain: 2.5 by 2.5 km grid
 #grid3.xy <- do.regular.grid(dx=0.295858,   dy=0.1798561,   7.5,  16, 53,   60.1)  # medium grain: 30 by 30 km grid
 grid3.xy <- do.regular.grid(dx=0.1972387,   dy=0.1199041,   7.5,  16, 53,   60.1)  # medium grain: 20 by 20 km grid
 plot(grid1.xy[,1], grid1.xy[,2], pch=".")
 points(grid2.xy[,1], grid2.xy[,2], col=3, pch=".")
  library(mapdata)
  library(sp)
  map("worldHires", add=TRUE)


 idx_pts <- point.in.polygon(point.x = grid2.xy[,1], point.y = grid2.xy[,2],
        pol.x = c(12, 12, 11.94, 11.97, 12, 12, 15, 15, 14.78,
            14.2, 13.7, 12.81, 12.44), pol.y = c(55.3, 54.75,
            54.67, 54.56, 54.56, 53.5, 53.5, 55, 55.3, 55.4,
            55.5, 55.38, 55.33)) > 0  |
    #         point.in.polygon(point.x = grid2.xy[,1], point.y = grid2.xy[,2],
    #   pol.x = c(8.648336, 7.034822, 7.357525, 9.083985, 9.608377,
    #        10.22958, 10.689431, 11.084742, 11.617201, 12.068985,
    #        11.972174, 10.59262, 9.971417, 9.39862, 8.648336),
    #    pol.y = c(57.08073, 57.99182, 58.20964, 58.87187, 59.32015,
    #        59.86417, 59.99375, 59.8804, 58.96783, 58.0774, 57.4653, #sk
    #        57.74247, 57.50441, 57.10708, 57.08073)) > 0  | 
             point.in.polygon(point.x = grid2.xy[,1], point.y = grid2.xy[,2], #sd2224  
             pol.x = c(12.297, 12.13, 12.45, 12.81, 12.94, 13.21,
            12.5, 12.448), pol.y = c(56.13, 55.48, 55.31, 55.38,
            55.41, 55.71, 56.29, 56.305)) > 0 |
            point.in.polygon(point.x = grid2.xy[,1], point.y = grid2.xy[,2],
             pol.x = c(10.1, 10.75, 10.71, 11.58, 11.58, 11.99, 11.94,
            11.97, 12, 12, 9.3, 9.3), pol.y = c(56.6, 56.3, 56.15,
            55.9, 55.65, 55, 54.67, 54.56, 54.56, 53.75, 53.75,   #sd2224
            56.6)) > 0 |
            point.in.polygon(point.x = grid2.xy[,1], point.y = grid2.xy[,2],
              pol.x = c(10.59262, 11.97217, 12.15883, 12.70796, 13.12992,
            12.80622, 12.95073, 12.72185, 12.45127, 12.29556,
            12.13384, 11.99063, 11.58487, 11.58487, 11.63281,
            11.49492, 11.3094, 11.27652, 10.71374, 10.70218,  
            10.24553, 10.19351, 10.42472, 10.59262), pol.y = c(57.74247,
            57.4653, 57.48032, 56.94085, 56.46389, 56.36135,
            56.19091, 56.16918, 56.29535, 56.12728, 55.49119,
            55.28764, 55.63113, 55.91101, 55.90623, 55.94866,
            55.97965, 56.00988, 56.14253, 56.25853, 56.49587, # ka
            57.11107, 57.63566, 57.74247)) > 0  | 
      #          point.in.polygon(point.x = grid2.xy[,1], point.y = grid2.xy[,2],
     #         pol.x = c(14.63647, 15.36935, 15.50677, 14.95711), pol.y = c(55.17844, 54.83013, 55.35260, 55.52675)) > 0  |  # an insight in the baltic for SPR2232 
     #         pol.x =  c(15.51894, 14.30453, 14.03031, 14.97050), pol.y =  c(55.36256, 55.74772, 55.47261, 54.75730)) > 0  |  # an insight in the baltic for SPR2232 
                point.in.polygon(point.x = grid2.xy[,1], point.y = grid2.xy[,2],
              pol.x = c(19.03744, 20.40754, 19.18423, 18.10772
                    ), pol.y = c(56.20489, 58.02184, 58.12717, 56.15223)) > 0  |  # an insight in the baltic for SPR2232 
                point.in.polygon(point.x = grid2.xy[,1], point.y = grid2.xy[,2],
              pol.x = c(15.55243, 18.25490, 18.20909, 15.55243
                    ), pol.y = c(55.32770, 56.22335, 56.24823, 55.40233)) > 0 #| # an insight in the baltic for SPR2232
             #   point.in.polygon(point.x = grid2.xy[,1], point.y = grid2.xy[,2],
             # pol.x = c(10.61689, 11.66183, 12.05021), pol.y = c(57.73585, 57.92670, 57.41549)) > 0  # an insight in the sk
 grid2.xy <- grid2.xy[idx_pts,] # keep only those           
 points(grid2.xy[,1], grid2.xy[,2], col=3, pch=".")
 
 
 
 idx_pts <- point.in.polygon(point.x = grid1.xy[,1], point.y = grid1.xy[,2],
        pol.x = c(12, 12, 11.94, 11.97, 12, 12, 15, 15, 14.78,
            14.2, 13.7, 12.81, 12.44), pol.y = c(55.3, 54.75,
            54.67, 54.56, 54.56, 53.5, 53.5, 55, 55.3, 55.4,
            55.5, 55.38, 55.33)) > 0  | 
     #       point.in.polygon(point.x = grid1.xy[,1], point.y = grid1.xy[,2],
     #  pol.x = c(8.648336, 7.034822, 7.357525, 9.083985, 9.608377,
     #       10.22958, 10.689431, 11.084742, 11.617201, 12.068985,
     #       11.972174, 10.59262, 9.971417, 9.39862, 8.648336),
     #   pol.y = c(57.08073, 57.99182, 58.20964, 58.87187, 59.32015,
     #       59.86417, 59.99375, 59.8804, 58.96783, 58.0774, 57.4653, #SK
     #       57.74247, 57.50441, 57.10708, 57.08073)) > 0  | 
             point.in.polygon(point.x = grid1.xy[,1], point.y = grid1.xy[,2], #sd2224  
             pol.x = c(12.297, 12.13, 12.45, 12.81, 12.94, 13.21,
            12.5, 12.448), pol.y = c(56.13, 55.48, 55.31, 55.38,
            55.41, 55.71, 56.29, 56.305)) > 0 |
            point.in.polygon(point.x = grid1.xy[,1], point.y = grid1.xy[,2],
             pol.x = c(10.1, 10.75, 10.71, 11.58, 11.58, 11.99, 11.94,
            11.97, 12, 12, 9.3, 9.3), pol.y = c(56.6, 56.3, 56.15,
            55.9, 55.65, 55, 54.67, 54.56, 54.56, 53.75, 53.75,   #sd2224
            56.6)) > 0 |
            point.in.polygon(point.x = grid1.xy[,1], point.y = grid1.xy[,2],
              pol.x = c(10.59262, 11.97217, 12.15883, 12.70796, 13.12992,
            12.80622, 12.95073, 12.72185, 12.45127, 12.29556,
            12.13384, 11.99063, 11.58487, 11.58487, 11.63281,
            11.49492, 11.3094, 11.27652, 10.71374, 10.70218,  #kask
            10.24553, 10.19351, 10.42472, 10.59262), pol.y = c(57.74247,
            57.4653, 57.48032, 56.94085, 56.46389, 56.36135,
            56.19091, 56.16918, 56.29535, 56.12728, 55.49119,
            55.28764, 55.63113, 55.91101, 55.90623, 55.94866,
            55.97965, 56.00988, 56.14253, 56.25853, 56.49587,
            57.11107, 57.63566, 57.74247)) > 0   # kask

 grid1.xy <- grid1.xy[!idx_pts,] # clip on grid2

  idx_pts <- point.in.polygon(point.x = grid3.xy[,1], point.y = grid3.xy[,2],
        pol.x = c(12, 12, 11.94, 11.97, 12, 12, 15, 15, 14.78,
            14.2, 13.7, 12.81, 12.44), pol.y = c(55.3, 54.75,
            54.67, 54.56, 54.56, 53.5, 53.5, 55, 55.3, 55.4,
            55.5, 55.38, 55.33)) > 0  | 
   #         point.in.polygon(point.x = grid3.xy[,1], point.y = grid3.xy[,2],
   #    pol.x = c(8.648336, 7.034822, 7.357525, 9.083985, 9.608377,
   #         10.22958, 10.689431, 11.084742, 11.617201, 12.068985,
   #         11.972174, 10.59262, 9.971417, 9.39862, 8.648336),
   #     pol.y = c(57.08073, 57.99182, 58.20964, 58.87187, 59.32015,
   #         59.86417, 59.99375, 59.8804, 58.96783, 58.0774, 57.4653, #sk
   #         57.74247, 57.50441, 57.10708, 57.08073)) > 0  | 
             point.in.polygon(point.x = grid3.xy[,1], point.y = grid3.xy[,2], #sd2224  
             pol.x = c(12.297, 12.13, 12.45, 12.81, 12.94, 13.21,
            12.5, 12.448), pol.y = c(56.13, 55.48, 55.31, 55.38,
            55.41, 55.71, 56.29, 56.305)) > 0 |
            point.in.polygon(point.x = grid3.xy[,1], point.y = grid3.xy[,2],
             pol.x = c(10.1, 10.75, 10.71, 11.58, 11.58, 11.99, 11.94,
            11.97, 12, 12, 9.3, 9.3), pol.y = c(56.6, 56.3, 56.15,
            55.9, 55.65, 55, 54.67, 54.56, 54.56, 53.75, 53.75,   #sd2224
            56.6)) > 0 |
            point.in.polygon(point.x = grid3.xy[,1], point.y = grid3.xy[,2],
              pol.x = c(10.59262, 11.97217, 12.15883, 12.70796, 13.12992,
            12.80622, 12.95073, 12.72185, 12.45127, 12.29556,
            12.13384, 11.99063, 11.58487, 11.58487, 11.63281,
            11.49492, 11.3094, 11.27652, 10.71374, 10.70218,  #ka
            10.24553, 10.19351, 10.42472, 10.59262), pol.y = c(57.74247,
            57.4653, 57.48032, 56.94085, 56.46389, 56.36135,
            56.19091, 56.16918, 56.29535, 56.12728, 55.49119,
            55.28764, 55.63113, 55.91101, 55.90623, 55.94866,
            55.97965, 56.00988, 56.14253, 56.25853, 56.49587,
            57.11107, 57.63566, 57.74247)) > 0   # kask

 grid3.xy <- grid3.xy[!idx_pts,] # clip on grid2
 
 
 # check
 plot(grid1.xy[,1], grid1.xy[,2], pch=".")
 map("worldHires", add=TRUE)
 points(grid1.xy[,1], grid1.xy[,2], col=4, pch=".")
 points(grid2.xy[,1], grid2.xy[,2], col=4, pch=".")
 points(grid3.xy[,1], grid3.xy[,2], col=4, pch=".")

 # approve...
 x <- c(grid1.xy[,1], grid2.xy[,1], grid3.xy[,1])
 y <- c(grid1.xy[,2], grid2.xy[,2], grid3.xy[,2])


 coord <-matrix(0,ncol=3, nrow=length(x))
 coord[,1] <- x
 coord[,2] <- y
 coord[,3] <- 0 # 'at sea'
 colnames(coord) <- c("x","y","idx.port")

 
 # remove points on land...
 library(rgeos)
 library(maptools)
 sh1 <- readShapePoly(file.path(general$main_path_ibm_param,"shp","francois_EU"))
 dd <-sapply(slot(sh1, "polygons"), function(x) lapply(slot(x, "Polygons"), function(x) x@coords)) # tricky there...
 library(sp)
 for(iLand in 1:length(dd)){
     print(iLand)
     #if(sh1@data$C_NAME[iLand]=="DENMARK") dilate.with=0.02 else dilate.with=0.018
     #if(sh1@data$C_NAME[iLand]=="DENMARK") dilate.with=0.008 else dilate.with=0.008
     if(sh1@data$C_NAME[iLand]=="DENMARK") dilate.with=0.015 else dilate.with=0.008
          # Points on land are 1 or 2 and not is 0
          #res<- point.in.polygon(coord[,1],coord[,2],dd[[iLand]][[1]][,1],dd[[iLand]][[1]][,2])
          #replaced by:
          p1 <- readWKT(paste("POLYGON((",paste(dd[[iLand]][[1]][,1], dd[[iLand]][[1]][,2], collapse=","),"))", sep=''))
          buff <- gBuffer(p1,width=dilate.with)
          coords <- buff@polygons[[1]]@Polygons[[1]]@coords
          res<- point.in.polygon(coord[,1],coord[,2],coords[,1],coords[,2])
          #=> very small dilatation using the gBuffer facility in library(rgeos): to remove the useless points in Fjords
    
       coord <- coord[which(res==0),]
       map("worldHires",  xlim=c(min(grid1.xy[,1]),max(grid1.xy[,1])), ylim=c(min(grid1.xy[,2]),max(grid1.xy[,2])))
       points(coord[,"x"], coord[,"y"])
       }
 # remove artefact UK
 #rect(-2.5, 51.0, -4.5, 52.0, col=2)      
 coord <- coord[!(coord[,1]< -2.5 & coord[,1]> -4.5 & coord[,2]> 51.0 & coord[,2]< 52.0),]
 # remove artefact UK
 #rect(-7.123233, 51.52951, -2.237419, 56.88309, col=2)      
 coord <- coord[!(coord[,1]< -2.237419 & coord[,1]> -7.123233 & coord[,2]> 51.52951 & coord[,2]< 56.88309),]
 # remove artefact UK
 #rect( -3.25, 51.6, -5.6, 49.9)      
 coord <- coord[!(coord[,1]< -3.25 & coord[,1]> -5.6 & coord[,2]> 49.9 & coord[,2]< 51.6),]
 # remove artefact SWD
 #rect(13.7, 66.0, 28, 68.0, col=2)     
 coord <- coord[!(coord[,1]< 28 & coord[,1]> 13.7 & coord[,2]> 66 & coord[,2]< 68.0),]
 # remove artefact FRA
 #rect(-0.85, 49.82, -1.95, 47.88)     
 coord <- coord[!(coord[,1]< -0.85 & coord[,1]> -1.95 & coord[,2]> 47.88 & coord[,2]< 49.82),]
 
 # caution: add to coord some nodes to avoid link over land for denmark Esperg
 # coord <-  rbind(coord, cbind(c(8.254036, 8.104059, 8.196352, 9.696125 ,9.592294,  8.265572, 8.277109, 8.323256, 8.565527,  8.41555, 10.03069),
 #  c(55.33099, 55.12831, 54.77024, 55.09453, 55.12155, 53.93249, 54.06086, 54.16895, 54.18247, 53.86493, 54.79727), c(0,0,0,0,0, 0,0,0, 0, 0, 0)  ))
 
 
 dd <- all.merged[,"LE_KG_SPR"]/max(all.merged[,"LE_KG_SPR"],na.rm=TRUE)
points(an(all.merged$SI_LONG),an(all.merged$SI_LATI), cex=dd, col=2)
     
 # ...and save temporary object
 save(coord, file=file.path(general$main_path_ibm_param, "coord.Rdata"))


 
 # add harbour nodes to coord
 load(file=file.path(general$main_path_ibm_param, "coord.Rdata"))
 coord <-  coord[!is.na(coord[,"x"]),]

 
 coord <- as.matrix(coord)
 coord <- rbind(coord, harbours)
 coord[,"x"] <- as.numeric(coord[,"x"] )
 coord[,"y"] <- as.numeric(coord[,"y"] )

 # reorder to get increasing x i.e. longitude
 idx.for.reorder <-  order(coord[,"x"], decreasing=FALSE)
 coord <- coord [idx.for.reorder,]

 ## filter out area that will produce disconnected sub-graph
  idx_pts <- point.in.polygon(point.x = coord[,1], point.y = coord[,2],
          pol.x = c(13.65199, 14.89721, 14.83062, 13.65865),
           pol.y = c(  53.91045, 53.91818, 53.61273, 53.62820)) > 0 

    #check
    points(coord[idx_pts,1],coord[idx_pts,2], col=3, pch="0")
   
    # and remove...
    nodes_to_be_removed <- (1:nrow(coord))[idx_pts]
    coord <- coord[!idx_pts,] # remove nodes


 # graph-based Neighbours
 library(spdep)
 nbk <- 8
 map("worldHires", xlim=c(min(grid1.xy[,1]),max(grid1.xy[,1])), ylim=c(min(grid1.xy[,2]),max(grid1.xy[,2])))
 #plot(knn2nb(knearneigh(coord[,c("x","y")], k=nbk, longlat = TRUE),
 #                row.names=1:nrow(coord)) , coord[,c("x","y")], add=TRUE, cex=0.3,col=grey(0.5))
 graph.neightb <- knearneigh(coord[,c("x","y")], k=nbk, longlat = TRUE)
 # get distance
 dd <- nbdists(knn2nb(graph.neightb) , coord[,c("x","y")], longlat = TRUE)  # return distance in km
 #dd <- nbdists(nb , coord, longlat = TRUE)  # return distance in km

 # build the graph object compatible with igraph
 node      <- rep(1:nrow(graph.neightb$nn), each=nbk)
 neighbour <- matrix(t(graph.neightb$nn), ncol=1)
 dist.km   <- unlist(dd)
 graph     <- cbind(node, neighbour, dist.km)

  # plot the graph
 #plot(0,0, type="n", xlim=c(8,12), ylim=c(53.9,56.4))
 x1 <- coord[graph[,1],1]
 y1 <- coord[graph[,1],2]
 x2 <- coord[graph[,2],1]
 y2 <- coord[graph[,2],2]
 segments(x1,y1,x2,y2,col=2)

 ## find out in coord which points are lying in A POLYGON WE DEFINE  (do it manually: use dd <- locator())
 ## IN WHICH WE WILL DELETE ALL TOO LONG LINKS  
 idx_pts <- point.in.polygon(point.x = coord[,1], point.y = coord[,2],
   #     pol.x = c(10.598986, 10.335821,  9.847084,  7.967329,  8.806953, 13.493809, 14.897359, 13.919887, 13.042668, 11.614054, 10.812025, 10.686708),
        pol.x = c(10.598986, 10.335821,  9.847084,  8.967329,  9.806953, 13.493809, 14.897359, 13.919887, 13.042668, 11.614054, 10.812025, 10.686708),
        pol.y = c( 57.71963, 57.56211, 55.25656, 55.19212, 53.12285, 53.20877, 55.19928, 55.50716, 55.44988, 57.05374, 57.64087, 57.67667)) > 0 

   #check
   points(coord[idx_pts,1],coord[idx_pts,2], col=4, pch="0")
  
   
   ## filter out too long links for nodes in the fine grid
   graph_sauv <- graph
   pts_with_connections_to_check <- (1:nrow(coord)) [idx_pts]
   do_not_touch_this_one         <- NULL
   for(a_pt in pts_with_connections_to_check) {  # column 1
         connect.for.this.pt <- graph[graph[,1]==a_pt,] # all neighbours of this port
         if(length(connect.for.this.pt)>3){
          connect.for.this.pt <- connect.for.this.pt[connect.for.this.pt[,'dist.km'] < 3.6,] # keep the links less than 15km
           if(length(connect.for.this.pt)==0){ # prevent removing the node....
               connect.for.this.pt <- graph[graph[,1]==a_pt,] 
               connect.for.this.pt <- connect.for.this.pt[connect.for.this.pt[,'dist.km'] <= min(connect.for.this.pt[,'dist.km']),]           
               do_not_touch_this_one <- c(do_not_touch_this_one, a_pt)
              }
          graph <- graph[graph[,1]!=a_pt,]  # remove
          graph <- rbind (graph, connect.for.this.pt) # add
          }
   }
    for(a_pt in pts_with_connections_to_check) {  # column 2
         connect.for.this.pt <- graph[graph[,2]==a_pt,] # all neighbours of this port
         if(length(connect.for.this.pt)>3){
          connect.for.this.pt <- connect.for.this.pt[connect.for.this.pt[,'dist.km'] < 3.6 | (connect.for.this.pt[,1] %in% do_not_touch_this_one),] # keep the links less than 15km
           if(length(connect.for.this.pt)==0){ # prevent removing the node....
               connect.for.this.pt <- graph[graph[,2]==a_pt,] 
               connect.for.this.pt <- connect.for.this.pt[connect.for.this.pt[,'dist.km'] <= min(connect.for.this.pt[,'dist.km']) | (connect.for.this.pt[,1] %in% do_not_touch_this_one),]           
              }
          graph <- graph[graph[,2]!=a_pt,]  # remove
          graph <- rbind (graph, connect.for.this.pt) # add
          }
   }

   

if(FALSE){
convert2SpatialLines <- function(x1,y1,x2,y2) {
    xy = cbind(x1,y1,x2,y2, 1:length(x1))
    lines = Lines(apply(xy, 1, function(z) {
        Line(rbind(z[1:2], z[3:4], ID=z[5]))
    }), ID = 1)
    SpatialLines(list(lines))
}
 
 x1 <- coord[graph[,1],1]
 y1 <- coord[graph[,1],2]
 x2 <- coord[graph[,2],1]
 y2 <- coord[graph[,2],2]
 a_SpatialLines <- convert2SpatialLines (x1,y1,x2,y2) 
 res <-gIntersection(a_SpatialLines, sh1)  # library rgeos
} 


 if(FALSE){
 # reduce the number of neigbours for harbours
 # caution : when removing link it is possible to get
 # less than 2 neigbours for remaining port when port to port links exist
 idx.harbor.in.coord <-  as.numeric(which(coord[,"idx.port"]!=0))
 for(a.port in idx.harbor.in.coord) {
      # col 1
      connect.for.this.port <- graph[graph[,1]==a.port,] # all neighbours of this port
     if(length(connect.for.this.port)>=2*3){
         connect.for.this.port <- connect.for.this.port[order(connect.for.this.port[,"dist.km"]),][1:2,] # keep the 5 nearest neighbours
         graph <- graph[graph[,1]!=a.port,]  # remove
         graph <- rbind (graph, connect.for.this.port) # add
         }
      # col 2
      connect.for.this.port <- graph[graph[,2]==a.port,] # all neighbours of this port
      if(length(connect.for.this.port)>=2*3){
         connect.for.this.port <- connect.for.this.port[order(connect.for.this.port[,"dist.km"]),][1:2,] # keep the 5 nearest neighbours
         graph <- graph[graph[,2]!=a.port,]  # remove
         graph <- rbind (graph, connect.for.this.port) # add
         }
      }

     # restore the few ports which have been potentially lost when removing links...
     the.disappeared.ports.idx <- c(1: max(unique(graph[,1])))[! c(1: max(unique(graph[,1]))) %in% unique(graph[,1])]
     if(length(the.disappeared.ports.idx)>0){
     init.graph     <- cbind(node, neighbour, dist.km)
     this.unfortunate.port <- init.graph[init.graph[,1] %in% the.disappeared.ports.idx,]
     graph <- rbind (graph, this.unfortunate.port)
     }
   
   } # end FALSE


  # check that no node has been incidentelly removed
  # (occur when all links conducting to this node are removed while this node is lying outside the polygon where nodes are to be checked) 
  miss <- as.character(1:nrow(coord))[!as.character(1:nrow(coord)) %in% as.character(unique(graph[,1]))] 
  points(coord[as.numeric(miss),1],coord[as.numeric(miss),2], col=2, pch=16)
  graph <- rbind (graph, graph_sauv[graph_sauv[,"node"] %in% miss,])  # undo for this node
  
  
   
   graph <- graph[order(graph[,1]),] # VERY VERY ESSENTIAL (because see ** below)

 # build the igraph object
 library(igraph)
 vertices    <- data.frame(name=as.character(unique(graph[,1])), x=coord[,1], y=coord[,2]) #(**)
 edges        <- data.frame(from=c(graph[,1]),
                        to=c(graph[,2]),
                        dist.km=graph[,3])
 g <- graph.data.frame(edges, directed=FALSE, vertices=vertices)
 print(g, e=TRUE, v=TRUE)

 # plot the graph
 x1 <- coord[graph[,1],1]
 y1 <- coord[graph[,1],2]
 x2 <- coord[graph[,2],1]
 y2 <- coord[graph[,2],2]
 segments(x1,y1,x2,y2,col=3)

 # add harbours
 coord.ports <- coord[coord[,"idx.port"]!=0,]
 points(coord.ports[,1],coord.ports[,2], col=2, pch=16)



 # plot a ZOOM of the graph
 plot(0,0, type="n", xlim=c(8,12), ylim=c(53.9,56.4))
 #plot(0,0, type="n", xlim=c(6,16), ylim=c(53.7,56.5))
 #plot(0,0, type="n", xlim=c(8,15), ylim=c(53.7,56.5))
 #plot(0,0, type="n", xlim=c(8,15), ylim=c(53.7,58.5))
 x1 <- coord[graph[,1],1]
 y1 <- coord[graph[,1],2]
 x2 <- coord[graph[,2],1]
 y2 <- coord[graph[,2],2]
 segments(x1,y1,x2,y2,col=3)

 # add harbours
 coord.ports <- coord[coord[,"idx.port"]!=0,]
 points(coord.ports[,1],coord.ports[,2], col=2, pch=16)

 plot(sh1, add=TRUE)
 
return( list(g=g, graph=graph, coord=coord, harbour.list=harbour.list) )
}






    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
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




     #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 destB.f <- function(A, angleAB, dist.km){
   # CAUTION : LONGLAT
      Rearth = 6378.388 # in km
      # remind to convert from the degree to radian after input dividing by p
      # and from radian to degree before output multiplying by p
      p = 180/pi
       lat2 <-  asin(sin(A[,2]/p)*cos(dist.km/Rearth) + cos(A[,2]/p)*sin(dist.km/Rearth)*cos(angleAB/p))
       lon2 <-  A[,1]/p + atan2(sin(angleAB/p)*sin(dist.km/Rearth)*cos(A[,2]/p), cos(dist.km/Rearth)-sin(A[,2]/p)*sin(lat2))
       #lon2 <- ((lon2+pi)%%(2*pi)) -pi
        return(cbind(lon2*p,lat2*p))
        }
        
   #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 distAB.f <- function(A,B, .unit="km"){
 # CAUTION : LONGLAT
 # return the dist in km or nautical miles.
 # formula = cos (gamma) = sin LatA sin LatB + cos LatA cos LatB cos delta
 # if gamma in degree, knowing that 1 degree = 60 minutes = 60 nm, then gamma in degree * 60 = dist in nm
 # we can also use the radius R of the earth: the length of the arc AB is given by R*gamma with gamma in radians
        p = 180/pi;  Rearth = 6378.388 # in km
        nm <- 1852e-3 # 1852 m for 1 nm
        res <- switch(.unit,
           km = Rearth * acos(sin(A[,2]/p)*sin(B[,2]/p) + (cos(A[,2]/p) * cos(B[,2]/p)*cos(A[,1]/p - B[,1]/p))),
           nm = Rearth * acos(sin(A[,2]/p)*sin(B[,2]/p) + (cos(A[,2]/p) * cos(B[,1]/p)*cos(A[,1]/p - B[,1]/p))) / nm
           )
       names(res) <- NULL
        return(res)
        }

    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  bearingAB.f  <- function(A,B, .unit="km"){
    # CAUTION : LONGLAT
      p = 180/pi
         bearing <- atan2(   sin((B[,1]/p)-(A[,1]/p))*cos(B[,2]/p), cos(A[,2]/p)*sin(B[,2]/p)-sin(A[,2]/p)*cos(B[,2]/p)*cos((B[,1]/p)-(A[,1]/p)) )
        return(bearing*p)
        }
              


   #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
dist.paths <- function(from=1, to=c(100,200), g=g, coord=coord){
   short.path.dists <- rep(0, length(to))
   for(i in 1: length(to)){
     a.sh.path <- a.shortest.path  (from = from, to = to[i], g=g, is.plot=FALSE, a.color=4)
     mat <- coord[a.sh.path,]
     if(length(mat)>3){
         short.path.dists[i] <-  sum (distAB.f(A=mat, B=rbind(mat[-1,],mat[nrow(mat),]), .unit="km"  ))
       } else{short.path.dists[i] <- 0} # in that case, dep pt = arr pt!
     }

return(short.path.dists)
}


###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
# hereafter some R code for a tentative to build a grid starting from equidistant points i.e. from UTM
# but it is actually not required to bother so much with this since DISPLACE computes the great circle distances anyhow.

if(FALSE){
 library(maptools)
 sh_coastlines            <- readShapePoly(file.path("C:","Users","fba","Dropbox","ibm_vessels_param","shp","francois_EU"))

 grid_utm_33 <- expand.grid(
                            eastings=seq (300000, 700000, by=15000), # in meters, 15x15km
                            northings=seq (5750000, 7290000, by=15000) # in meters
                            )
 grid_utm_32 <- expand.grid(
                            eastings=seq (300000, 700000, by=15000), # in meters, 15x15km
                            northings=seq (5750000, 7290000, by=15000) # in meters
                            )
 grid_utm_31 <- expand.grid(
                            eastings=seq (300000, 700000, by=15000), # in meters, 15x15km
                            northings=seq (5750000, 7290000, by=15000) # in meters
                            )
 # remenber that by convention the false eastings is giving the central meridian of the zone at +500000
 # check http://geokov.com/education/utm.aspx
 
 library(sp)
 library(rgdal)
 SP_33           <- SpatialPoints(cbind(as.numeric(as.character(grid_utm_33$eastings)), as.numeric(as.character(grid_utm_33$northings))),
                       proj4string=CRS("+proj=utm  +ellps=intl +zone=33 +towgs84=-84,-107,-120,0,0,0,0,0"))
 SP_32           <- SpatialPoints(cbind(as.numeric(as.character(grid_utm_32$eastings)), as.numeric(as.character(grid_utm_32$northings))),
                       proj4string=CRS("+proj=utm  +ellps=intl +zone=32 +towgs84=-84,-107,-120,0,0,0,0,0"))
 SP_31           <- SpatialPoints(cbind(as.numeric(as.character(grid_utm_31$eastings)), as.numeric(as.character(grid_utm_31$northings))),
                       proj4string=CRS("+proj=utm  +ellps=intl +zone=31 +towgs84=-84,-107,-120,0,0,0,0,0"))
 grid_longlat_33 <- cbind(grid_utm_33,
                       spTransform(SP_33, CRS("+proj=longlat +datum=WGS84")))    # convert to longlat
 grid_longlat_32 <- cbind(grid_utm_32,
                       spTransform(SP_32, CRS("+proj=longlat +datum=WGS84")))    # convert to longlat
 grid_longlat_31 <- cbind(grid_utm_31,
                       spTransform(SP_31, CRS("+proj=longlat +datum=WGS84")))    # convert to longlat

 plot(grid_longlat_32$coords.x1, grid_longlat_32$coords.x2,
           , xlim=c(min(grid_longlat_31$coords.x1), max(grid_longlat_33$coords.x1)),  pch=16, cex=0.5 )
 points(grid_longlat_31$coords.x1, grid_longlat_31$coords.x2, pch=16, cex=0.5, col=2 )
 points(grid_longlat_33$coords.x1, grid_longlat_33$coords.x2, pch=16, cex=0.5, col=3 )
 plot(sh_coastlines, add=TRUE)
 
 } # end FALSE



###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################



if(FALSE){

general                        <- list()
general$lim.lat                <- c(49,70) # default
general$lim.long               <- c(-8.5,25) # default
general$main_path_ibm_param    <- file.path("C:","displace-project.org","repository","ibm_vessels_param")


res <- create_graph(general)
g=res$g; graph=res$graph; coord=res$coord; harbour.list=res$harbour.list


 # plot the graph
library(maps)
library(mapdata)
windows(15,15)
 map("worldHires", xlim=general$lim.long, ylim=general$lim.lat)
 #zoom: map("worldHires", xlim=c(10,15), ylim=c(53.5,56.2))
 #zoom: map("worldHires", xlim=c(9,13), ylim=c(53,56.2))
 #zoom: map("worldHires", xlim=c(5,14), ylim=c(53,56))
#zoom:  map("worldHires", xlim=c(7,15), ylim=c(53,58))

 x1 <- coord[graph[,1],1]
 y1 <- coord[graph[,1],2]
 x2 <- coord[graph[,2],1]
 y2 <- coord[graph[,2],2]
 segments(x1,y1,x2,y2,col=3)
 plot(sh1, add=TRUE)  # better for plotting the western baltic sea coastline!
 
 
 
 # add harbours
 coord.ports <- coord[coord[,"idx.port"]!=0,]
 points(coord.ports[,1],coord.ports[,2], col=2, pch=16)

 # a shortest path
  a.sh.path <- a.shortest.path  (from = 10, to = 4956, g=g, is.plot=TRUE, a.color=6)
  a.sh.path <- a.shortest.path  (from = 10, to = 4955, g=g, is.plot=TRUE, a.color=5)


# save a graph (do not erase previous ones i.e. increment 'num')
 lst <- list.files(path =  file.path(general$main_path_ibm_param, "igraph"), pattern = "graphibm")
 num <- max(as.numeric(unlist(lapply(strsplit (lst, split="_"), function(x) x[1]))))+1
 save('g','graph','coord', 'harbour.list', file = file.path(general$main_path_ibm_param, "igraph",
         paste(num,"_graphibm.RData",sep="")))



## FILES FOR BUILDING A IGRAPH
#load(file.path(general$main_path_ibm_param,"igraph","6_graphibm.RData")) # built from the R code
#load(file.path(general$main_path_ibm_param,"igraph","8_graphibm.RData")) # built from the R code
load(file.path(general$main_path_ibm_param,"igraph","11_graphibm.RData")) # built from the R code

 # save a plot
 windows(5,5)
 plot(coord[,1], coord[,2], pch=".", xlab="Longitude", ylab="Latitude", xlim=c(-8,23), ylim=c(52,65))
 ices_areas <- readShapeSpatial(file.path(general$main_path_ibm_param,"ices_areas", "ices_areas"))
 plot(ices_areas, col="white",  add=TRUE)
 points(coord[,1], coord[,2], pch=".")
 box()
 #map("worldHires", add=TRUE)
  a.sh.path <- a.shortest.path  (from = 100, to = 4956, g=g, is.plot=TRUE, a.color=6)
  a.sh.path <- a.shortest.path  (from = 100, to = 4955, g=g, is.plot=TRUE, a.color=5)
 savePlot(filename=file.path(general$main_path_ibm_param,"igraph",
       paste("map_graph11_points.jpeg",sep='')), type="jpeg")
       #paste("map_graph8_points.jpeg",sep='')), type="jpeg")
 #      paste("map_graph6_points.jpeg",sep='')), type="jpeg")
 ## zoom in..... 
 windows(5,5)
 plot(coord[,1], coord[,2], pch=".", xlab="Longitude", ylab="Latitude", xlim=c(9.5,15), ylim=c(53.5,56.2))
 ices_areas <- readShapeSpatial("C:\\Users\\fba\\Dropbox\\ibm_vessels_param\\ices_areas\\ices_areas")
 plot(ices_areas, col="white",  add=TRUE)
 points(coord[,1], coord[,2], pch=".")
 box()
 #map("worldHires", add=TRUE)
 # a.sh.path <- a.shortest.path  (from = 100, to = 4956, g=g, is.plot=TRUE, a.color=6)
 # a.sh.path <- a.shortest.path  (from = 100, to = 4955, g=g, is.plot=TRUE, a.color=5)
  a.sh.path <- a.shortest.path  (from = 1000, to = 12500, g=g, is.plot=TRUE, a.color=6)
  a.sh.path <- a.shortest.path  (from = 100, to = 4500, g=g, is.plot=TRUE, a.color=5)
 savePlot(filename=file.path(general$main.path,"igraph",
       paste("map_graph11_points_zoom_in.jpeg",sep='')), type="jpeg")
       #paste("map_graph8_points_zoom_in.jpeg",sep='')), type="jpeg")
       #paste("map_graph6_points_zoom_in.jpeg",sep='')), type="jpeg")

 
 
# using ggmap
library(ggplot2)
coord.ports <- coord[coord[,"idx.port"]!=0,]
coord.ports <- data.frame(coord.ports)
coord.ports$color <- factor(sample(c(1,2), size=nrow(coord.ports),replace=TRUE)) # fake just to test
require(ggmap)
require(mapproj)
map.center <- geocode("copenhaguen")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), maprange = TRUE, source="google", zoom=7)
SHmap <- qmap(maprange, source="google", zoom=7)

SHmap + geom_point(
  aes(x=x, y=y, colour= color), data=coord.ports) +
  scale_colour_manual(values=c("1"="dark blue","2"="orange"))+
  labs(colour=c("port type"))
SHmap + geom_point(
  aes(x=x, y=y), data=coord.ports)
points(8,56, col=1)
segments(7.5, 56, 8.5, 56)
segments(8, 55.5, 8, 56.5)
savePlot(filename=file.path(general$main_path_ibm_param,"igraph",
       paste("map_ggmap_port_balticonly.jpeg",sep='')), type="jpeg")

 
 
# RE-BUILD THE IGRAPH OBJECT IN R TO SHOW WHAT IS NECESSARY AS INPUT
 library(igraph)
 vertices    <- data.frame(name=as.character(unique(graph[,1])), x=coord[,1], y=coord[,2]) #(**)
 edges        <- data.frame(from=c(graph[,1]),
                        to=c(graph[,2]),
                        dist.km=graph[,3])
 g <- graph.data.frame(edges, directed=FALSE, vertices=vertices)
 print(g, e=TRUE, v=TRUE)


# export in graphml format for use with C
#write.graph(g, file=file.path("C:","ibm_vessels_param","1_graphibm"), format="graphml")

# export in txt format for use with C
#write.table(signif(coord, 6), file=file.path(general$main.path,"igraph","coord3.dat"),
#    row.names=FALSE, col.names=FALSE, sep=" ")


# export in txt format for use with C
# THIS WAY IS BETTER BECAUSE C IS ROW ORIENTED
#write(signif(coord, 6),file=file.path(general$main_path_ibm_param,"igraph", "coord6.dat"), ncol=1)
#write(signif(coord, 6),file=file.path(general$main_path_ibm_param,"igraph", "coord8.dat"), ncol=1)
write(signif(coord, 6),file=file.path(general$main_path_ibm_param,"igraph", "coord11.dat"), ncol=1)
nrow(coord)
 ncol(coord)
 # remnber that the idx.port is related to the one in harbour.list

# export in txt format for use with C
# THIS WAY IS BETTER BECAUSE C IS ROW ORIENTED
graph [,1:2] <- graph [,1:2] -1
#write(signif(graph, 5),file=file.path(general$main_path_ibm_param,"igraph","graph6.dat"), ncol=1)  # CONNECTION BETWEEN NODES + DISTANCE KM
#write(signif(graph, 5),file=file.path(general$main_path_ibm_param,"igraph","graph8.dat"), ncol=1)  # CONNECTION BETWEEN NODES + DISTANCE KM
write(signif(graph, 6),file=file.path(general$main_path_ibm_param,"igraph","graph11.dat"), ncol=1)  # CONNECTION BETWEEN NODES + DISTANCE KM
##=> CAUTION with signif!!! if at 5 then no more than 99999 nodes.
nrow(graph)
 ncol(graph)

# plot: compare version R vs version cpp
plot(0,0,xlim=c(-10,20),ylim=c(40,70),type="n")
a.shortest.path (from = 20, to = 200, g=g, is.plot=TRUE, a.color=2) # WITH IGRAPH
 [1]  20  19  92 119 150 214 287 366 368 438 445 450 514 536 516 490 581 598 624 557 532 504 535
[24] 457 429 417 437 362 390 327 342 267 258 194 211 239 260 200


a.path <- c(20, 19, 92, 172, 247, 266, 346, 366, 368, 438, 445, 468, 450, 514, 536,
516 ,490, 478 ,449 ,481, 464, 467, 455, 479, 457, 429, 417 ,385,
326 ,296 ,274 ,257 ,198 ,162 ,149 ,138 ,155,217,200)   # WITH THE HOME MADE C++ VERSION
points(coord[a.path, 1], coord[a.path,2], col=1)
for(i in 1:(length(a.path)-1)) segments(coord[a.path[i], 1], coord[a.path[i],2], coord[a.path[i+1], 1], coord[a.path[i+1],2], col=1)

# list of harbour nodes
which(coord[,"idx.port"]!=0)-1

##--------------------------------------------------------------------------##
##-------------ALSO EXPORT A CSV FOR VARIUOS USE OTHER THAN C++----- -------##
##--------------------------------------------------------------------------##
#write.table(signif(coord, 6),file=file.path(general$main_path_ibm_param,"igraph", "coord6.csv"),
#                 col.names=TRUE, row.names=FALSE)
#write.table(signif(coord, 6),file=file.path(general$main_path_ibm_param,"igraph", "coord8.csv"),
#                 col.names=TRUE, row.names=FALSE)
write.table(signif(coord, 6),file=file.path(general$main_path_ibm_param,"igraph", "coord11.csv"),
                 col.names=TRUE, row.names=FALSE)

##--------------------------------------------------------------------------##
##-------------ALSO EXPORT THE DEFAULT NODES IN POLYGONS GRAPH NODES -------##
##--------------------------------------------------------------------------##


idx_nodes_cpp_in_polygons <- matrix("", ncol=2)
colnames(idx_nodes_cpp_in_polygons) <- c("polygon", "node")

#=> just generate empty files at this stage for this graph...
# => see IBM_param_step2bis.r

## copy/paste for quarters

write.table(idx_nodes_cpp_in_polygons,
           file=file.path(general$main_path_ibm_param, "igraph",
            # paste("nodes_in_polygons_a_graph8_quarter1",".dat",sep='')),
             paste("nodes_in_polygons_a_graph11_quarter1",".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)
  
write.table(idx_nodes_cpp_in_polygons,
           file=file.path(general$main_path_ibm_param, "igraph",
             # paste("nodes_in_polygons_a_graph8_quarter2",".dat",sep='')),
             paste("nodes_in_polygons_a_graph11_quarter2",".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)

write.table(idx_nodes_cpp_in_polygons,
           file=file.path(general$main_path_ibm_param, "igraph",
             # paste("nodes_in_polygons_a_graph8_quarter3",".dat",sep='')),
             paste("nodes_in_polygons_a_graph11_quarter3",".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)

write.table(idx_nodes_cpp_in_polygons,
           file=file.path(general$main_path_ibm_param, "igraph",
             #paste("nodes_in_polygons_a_graph8_quarter4",".dat",sep='')),
             paste("nodes_in_polygons_a_graph11_quarter4",".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)




} # end False