
## IBM parametrisation
## Francois Bastardie (DTU-Aqua) 
## outputs: mainly .dat files to be used for the IBM simulations

PATRIK    <- TRUE # SWE 
HEINO     <- FALSE # DEU
FRANCOIS  <- FALSE  # DEN



 # GENERAL SETTINGS
 if(FRANCOIS) {
  general <- list()
  general$main.path      <- file.path("C:","displace-project.org","repository", "ibm_vessels_param")
  general$main.path.code <- file.path("C:","displace-project.org","repository", "ibm_vessels_param_R")

  #general$igraph               <- 4 # for the Canadian paper
  #general$case_study           <- "canadian_paper"
  #general$case_study_countries <- c("DNK", "GER") # for the Canadian paper
  #general$a.year                <- "2010"
  #general$a.country             <- "DEN"      
   
  general$igraph                <- 11
  general$case_study            <- "baltic_only"
  general$case_study_countries  <- c("DEN", "SWE", "DEU")    # for the Baltic only
  general$a.year                <- "2012"
  if(FRANCOIS) general$a.country <- "DEN"
  if(HEINO)    general$a.country <- "DEU"
  if(PATRIK)   general$a.country <- "SWE"

  # mkdir
     dir.create(path=file.path(general$main.path, "merged_tables", general$case_study), 
                      showWarnings = TRUE, recursive = TRUE, mode = "0777") 
   
  }
 
  
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!UTILS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 
 library(maps)
 
  ICESarea2 <-
function (tacsat, string = TRUE)
{
    library(sp)
    ICES.area <- rep(NA, dim(tacsat)[1])
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(8.648336, 7.034822, 7.357525, 9.083985, 9.608377,
            10.22958, 10.689431, 11.084742, 11.617201, 12.068985,
            11.972174, 10.59262, 9.971417, 9.39862, 8.648336),
        pol.y = c(57.08073, 57.99182, 58.20964, 58.87187, 59.32015,
            59.86417, 59.99375, 59.8804, 58.96783, 58.0774, 57.4653,
            57.74247, 57.50441, 57.10708, 57.08073)) > 0] <- ifelse(string, 'kask', '0')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(10.59262, 11.97217, 12.15883, 12.70796, 13.12992,
            12.80622, 12.95073, 12.72185, 12.45127, 12.29556,
            12.13384, 11.99063, 11.58487, 11.58487, 11.63281,
            11.49492, 11.3094, 11.27652, 10.71374, 10.70218,
            10.24553, 10.19351, 10.42472, 10.59262), pol.y = c(57.74247,
            57.4653, 57.48032, 56.94085, 56.46389, 56.36135,
            56.19091, 56.16918, 56.29535, 56.12728, 55.49119,
            55.28764, 55.63113, 55.91101, 55.90623, 55.94866,
            55.97965, 56.00988, 56.14253, 56.25853, 56.49587,
            57.11107, 57.63566, 57.74247)) > 0] <-ifelse(string,'kask', '0')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(-4, 7, 8, 7, 7, 7.906163, -4, -4.6, -4.6, -4),
        pol.y = c(62, 62, 61.5, 60, 58, 57.5, 57.5, 57.3, 58.2,
            58.4)) > 0] <- ifelse(string,'nsea', '1')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(7.906163, 8.6488368, 8.5, 10.3, 10.3, 9.2,
            9.2, 7.11, -1, -4, -1.78), pol.y = c(57.5, 57.08073,
            57, 57.3, 57, 56.2, 52.7, 53.5, 53.5, 56.1, 57.5)) >
        0] <- ifelse(string,'nsea', '1')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(0, 0.5, 7.5, 7.5), pol.y = c(53.5, 51, 51,
            53.5)) > 0] <- ifelse(string,'nsea', '1')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(12, 12, 11.94, 11.97, 12, 12, 15, 15, 14.78,
            14.2, 13.7, 12.81, 12.44), pol.y = c(55.3, 54.75,
            54.67, 54.56, 54.56, 53.5, 53.5, 55, 55.3, 55.4,
            55.5, 55.38, 55.33)) > 0] <- ifelse(string,'2224', '2')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(14.2, 14.78, 15, 15, 18, 18, 14.2), pol.y = c(55.4,
            55.3, 55, 53, 53, 56.5, 56.5)) > 0] <- ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(18, 18, 22, 22), pol.y = c(56.5, 53.5, 53.5,
            56.5)) > 0] <- ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(18, 18, 18.32, 18.32, 19, 19, 16, 16), pol.y = c(56.5,
            57, 57, 57.5, 57.925, 59.762, 59.762, 56.5)) > 0] <- ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(19, 19, 18.45, 18.3, 18, 18, 21.5, 21.72, 21.98,
            22.17, 22.24, 21.93), pol.y = c(58.5, 57.9, 57.58,
            57, 57, 56.5, 56.5, 57.57, 57.97, 58.04, 58.15,
            58.5)) > 0] <- ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(21.5, 21.72, 21.98, 22.17, 22.24, 22.24, 23,
            25, 25), pol.y = c(56.5, 57.57, 57.97, 58.04, 58.15,
            58.35, 58.5, 58.5, 56.5)) > 0] <- ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(19, 17.975, 21.6, 21.8, 23.325, 23.325, 23.191,
            23, 23, 23.5, 23.6, 24, 23.692, 22.5, 22.1, 21.92,
            19), pol.y = c(59.762, 60.5, 60.5, 60.7, 60.5, 59.965,
            59.867, 59.827, 59, 59, 59.05, 58.75, 59.5, 59.5,
            58.35, 58.5, 58.5)) > 0] <- ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(16.5, 16.5, 19.7, 19.7, 22.6, 21.4), pol.y = c(60.5,
            63.7, 63.7, 63.5, 63.5, 60.5)) > 0] <- ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(19.7, 19.7, 25.7, 25.7, 19.7), pol.y = c(63.7,
            63.5, 63.5, 67, 67)) > 0] <-ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(23.325, 23.325, 23.191, 23, 23, 30.5, 30.5),
        pol.y = c(60.5, 59.965, 59.867, 59.827, 59, 59, 60.5)) >
        0] <- ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(12.297, 12.13, 12.45, 12.81, 12.94, 13.21,
            12.5, 12.448), pol.y = c(56.13, 55.48, 55.31, 55.38,
            55.41, 55.71, 56.29, 56.305)) > 0] <- ifelse(string,'2224', '2')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(10.1, 10.75, 10.71, 11.58, 11.58, 11.99, 11.94,
            11.97, 12, 12, 9.3, 9.3), pol.y = c(56.6, 56.3, 56.15,
            55.9, 55.65, 55, 54.67, 54.56, 54.56, 53.75, 53.75,
            56.6)) > 0] <- ifelse(string,'2224', '2')
    return(ICES.area)
}

 ICESrectangle <- function (dF) 
{
    rectChar1n2 <- as.integer(2 * (dF[, "SI_LATI"] - 35.5))
    rectChar3 <- ifelse(dF[, "SI_LONG"] <= -40, "A", ifelse(dF[, 
        "SI_LONG"] <= -30, "B", ifelse(dF[, "SI_LONG"] <= -20, 
        "C", ifelse(dF[, "SI_LONG"] <= -10, "D", ifelse(dF[, 
            "SI_LONG"] <= 0, "E", ifelse(dF[, "SI_LONG"] <= 10, 
            "F", ifelse(dF[, "SI_LONG"] <= 20, "G", ifelse(dF[, 
                "SI_LONG"] <= 30, "H", "I"))))))))
    rectChar4 <- as.integer(dF[, "SI_LONG"]%%10)
    rectID <- paste(rectChar1n2, rectChar3, rectChar4, sep = "")
    return(rectID)
}
 
 
 ICESrectangle2LonLat <- function (statsq, midpoint = F) {
    part1 <- substr(statsq, 1, 2)
    part2 <- substr(statsq, 3, 4)
    labels <- 0:90
    latlabels <- ifelse(labels < 10, paste("0", labels, sep = ""),
        as.character(labels))
    latvalues <- seq(35.5, 80.5, 0.5) + 0.25
    lonlabels <- paste(rep(LETTERS[2:8], rep(10, 7)), rep(0:9,
        7), sep = "")
    lonvalues <- (-40:29) + 0.5
    indx <- match(part1, latlabels)
    lat <- latvalues[indx]
    indx <- match(part2, lonlabels)
    lon <- lonvalues[indx]
    if (any(is.na(lat)) | any(is.na(lon)))
        warning("Some stat squares have not been recognised.")
    if (midpoint == F) {
        lat <- lat - 0.25
        lon <- lon - 0.5
    }
    return(data.frame(SI_LATI = lat, SI_LONG = lon))
   }

 if(TRUE){

 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!! THE IGRAPH !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

 if(FRANCOIS) {
 
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
 ## FILES FOR BUILDING A IGRAPH

 
  general <- list()
  general$lim.lat   <- c(49,65.25) # default
  general$lim.long  <-  c(-8.5,22) # default
  general$main.path <- file.path("C:","displace-project.org","repository", "ibm_vessels_param")
  general$main.path.code <- file.path("C:","displace-project.org","repository", "ibm_vessels_param_R")
  #general$igraph   <- 4 # for the canadian paper
  #general$a.year    <- "2010" # for the canadian paper
  general$igraph    <- 11 # for Baltic only
  general$a.year    <- "2012" # for Baltic only
  if(FRANCOIS) general$a.country <- "DEN"
  if(HEINO)    general$a.country <- "DEU"
  if(PATRIK)   general$a.country <- "SWE"
  general$case_study <- "baltic_only"
  
  
 load(file.path(general$main.path, "igraph", paste(general$igraph, "_graphibm.RData",sep=''))) # built from the R code

 # plot the graph
 library(mapdata)
 map("worldHires", xlim=general$lim.long, ylim=general$lim.lat)
 x1 <- coord[graph[,1],1]
 y1 <- coord[graph[,1],2]
 x2 <- coord[graph[,2],1]
 y2 <- coord[graph[,2],2]
 segments(x1,y1,x2,y2,col=3)

 # add harbours
 coord.ports <- coord[coord[,"idx.port"]!=0,]
 points(coord.ports[,1],coord.ports[,2], col=2, pch=16)



# create a file with node/code area for area-based management...
 code_area           <- data.frame(coord)
 colnames(code_area) <- c("SI_LONG","SI_LATI","code_area")
 code_area$code_area <- ICESarea2(code_area, string=FALSE)
 code_area[is.na(code_area[,3]), 3] <- 10 # open sea
 # check
 points(code_area[,1],code_area[,2], col=code_area[,3], pch=16)
 code_area[,1] <- as.numeric(as.character(code_area[,1]))
 code_area[,2] <- as.numeric(as.character(code_area[,2]))
 code_area[,3] <- as.numeric(as.character(code_area[,3]))
 if(FRANCOIS) write.table(signif(c(as.matrix(code_area)), 4),file=file.path(general$main.path,"igraph", paste("code_area_for_graph",general$igraph,"_points.dat",sep="")), row.names=FALSE, quote=FALSE)


 # create a file with node/ICES square for area-based management...
 code_square             <- data.frame(coord)
 colnames(code_square)   <- c("SI_LONG","SI_LATI","code_square")
 code_square$code_square <- ICESrectangle(code_square)
 code_square[is.na(code_square[,3]), 3] <- '99A9' # not found
 code_square[, 1:2]      <- signif(code_square[, 1:2],4) 
 if(FRANCOIS) write.table(code_square,file=file.path(general$main.path,"igraph", paste("code_square_for_graph",general$igraph,"_points.dat",sep="")), row.names=FALSE, quote=FALSE)

 # a shortest path
 library(igraph)
 a.sh.path <- a.shortest.path  (from = 50, to = 2500, g=g, is.plot=TRUE, a.color=4)

} # end FRANCOIS


 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!! THE MERGED TABLE!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 load(file.path(general$main.path,"merged_tables", general$case_study,
           paste("all_merged_weight_",general$a.country,"_",general$a.year,".RData",sep='')))
  x <- all.merged ; rm(all.merged); gc(reset=TRUE)

  # add the landing harbour
  endTrip     <- c(0,diff(x[,"FT_REF"]))
  table.end   <- x[which(endTrip!=0)-1, c("FT_REF", "SI_HARB")]
  x$land_harb <- table.end$SI_HARB [match(x$FT_REF, table.end$FT_REF, nomatch =1)]

  x.fishing <- x[x$SI_STATE==1 & x$flag!=4 & x$flag!=5,]
  #=> KEEP ONLY FISHNIG PINGS AND WITH LANDINGS I:E: NOT THE RSIDUAL EFFORT (FLAG 4)

  x.inharb <- x[x$SI_STATE!=1 & x$SI_HARB!="NA" ,]

  x.inharb <- x.inharb[!is.na(x.inharb$SI_STATE),] 


 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!FIND THE CLOSED IGRAPH NODE FOR EACH FISHING PING!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

  # load the graph
  load(file.path(general$main.path, "igraph", paste(general$igraph, "_graphibm.RData",sep=''))) # built from the R code

  
  # caution: remove the ports to be sure to have no fishnig ground in ports
  # because otherwise it is a too much mess in the movement algorithm
  # but take care of index node (idx in coord)
    coord <- cbind(coord, idx=1:nrow(coord))  ## CAUTION: start at 1
    coord.fgrounds  <-  coord[coord[,'idx.port']==0,]
    coord.harbours  <-  coord[coord[,'idx.port']!=0,]



    #memory.limit(4000)
    library(spatstat)
    an <- function(x)  as.numeric(as.character(x))
    # for fishing pings
    x.fishing.ppp    <- ppp(x=an(x.fishing$SI_LONG), y=an(x.fishing$SI_LATI),
                         xrange=range(an(x.fishing$SI_LONG)), yrange=range(an(x.fishing$SI_LATI)))
    graph.ppp <- ppp(x=coord.fgrounds[,"x"], y=coord.fgrounds[,"y"],
                   xrange=range(coord.fgrounds[,"x"]), yrange=range(coord.fgrounds[,"y"]))
    X <- x.fishing.ppp #[sample(1:x.ppp$n)[1:10] ]
    Y <- graph.ppp #[sample(1:graph.ppp$n)[1:10] ]
    unloadNamespace("igraph") # CAUTION:to avoid conflicts with the spatstat library e.g. diameter()
    N <- nncross (X=X, Y=Y)$which # caution: just euclidean distance on coord
    # visual check
    if(FALSE){
      plot(superimpose(X=X, Y=Y), main="nncross", cols=c("red","blue"))
      arrows(X$x, X$y, Y[N]$x, Y[N]$y, length=0.15)
      }
    # add
    ping.fgrounds <- cbind(x.fishing, pt_graph= coord.fgrounds[N, 'idx'])

   # check one pt
   #points(an(x.fishing$SI_LONG[10]),an(x.fishing$SI_LATI[10]),col=4,pch=16)
   #points(an(coord.fgrounds[ x.fishing$pt_graph[10] ,"x"]),an(coord.fgrounds[x.fishing$pt_graph[10],"y"]),col=1,pch=16)

   # nb of visits per fishing ping over seasons
   #pt_graph_occurrence <- tapply(ping.fgrounds$pt_graph, paste(ping.fgrounds$VE_REF, ping.fgrounds$year.quarter, sep="-"), table)

   # check wihch nodes are actually used
   # map("worldHires", xlim=general$lim.long, ylim=general$lim.lat)
   # points(coord[ping.fgrounds$pt_graph,1], coord[ping.fgrounds$pt_graph,2])

    # for "in harbour" pings: find the node of the graph for each harbour
    x.inharb$idx_port <- harbour.list$idx.port [match(x.inharb$land_harb, harbour.list$NAVN) ]  # translate name into idx_port from coord
    x.inharb$pt_graph <- coord.harbours [,'idx'] [ match(x.inharb$idx_port, coord.harbours[,'idx.port']) ] #...then translate idx_port into idx_node
    ping.harbours <- x.inharb
    
    # remove few records for which an harbour has not been found...
    # likely to occur for the few ports removed afterward i.e. within step2 e.g. aalborg 
    # this should be check to do not remove any vessels here!
    ping.harbours <- ping.harbours [!is.na(ping.harbours$pt_graph),]
    
    #  check
    any(ping.fgrounds$pt_graph %in% ping.harbours$pt_graph) #=> should return FALSE

    # remove the No_Matrix6 metier and records with no weight (because remenber that not all species has been kept when aggregating the merging table)...
    ping.fgrounds   <- ping.fgrounds[ping.fgrounds$LE_MET_level6!= "No_Matrix6",]
    ping.fgrounds   <- ping.fgrounds[ping.fgrounds$LE_MET_level6!= "NA",]
    idx_non0_weight <- apply(ping.fgrounds[,grep('KG',colnames(ping.fgrounds))], 1, sum, na.rm=TRUE) !=0  # because maybe the relvant bunch of species for this record has not been kept 
    ping.fgrounds   <- ping.fgrounds[idx_non0_weight,] # keep only records with some weight for some species

   
    ## debug to avoid 0/0
    ping.fgrounds <- ping.fgrounds[ping.fgrounds$LE_EFF_VMS!=0,]
  
    # remove foreign vessels!!
    if(general$a.country=="DEN") a.country <- "DNK"
    if(general$a.country=="DEU") a.country <- "DEU"
    if(general$a.country=="SWE") a.country <- "SWE"
    ping.fgrounds <- ping.fgrounds[grep(a.country, ping.fgrounds$VE_REF),]
    ping.fgrounds$VE_REF <- factor(ping.fgrounds$VE_REF)

    #*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
    #*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
    # SUBSET FOR CASE STUDY RELEVANT VESSELS
    if(general$case_study=="canadian_paper"){
     # keep all vessels on board....
    vid_this_case_study <- as.character(unique(ping.fgrounds$VE_REF))
    }
    if(general$case_study=="baltic_only"){
     # keep only the vessels fishnig in the western Baltic  (and kattegat because her.3a22, and East baltic because spr.2232)
    library(maptools)
    in_west_baltic_kattegat_and_east_baltic <- 
       point.in.polygon(point.x = an(ping.fgrounds$SI_LONG), point.y = an(ping.fgrounds$SI_LATI),
        pol.x = c(12.297, 12.13, 12.45, 12.81, 12.94, 13.21,
            12.5, 12.448), pol.y = c(56.13, 55.48, 55.31, 55.38,
            55.41, 55.71, 56.29, 56.305)) > 0 |  # sd23
       point.in.polygon(point.x = an(ping.fgrounds$SI_LONG), point.y = an(ping.fgrounds$SI_LATI),
        pol.x = c(10.1, 10.75, 10.71, 11.58, 11.58, 11.99, 11.94,
            11.97, 12, 12, 9.3, 9.3), pol.y = c(56.6, 56.3, 56.15,
            55.9, 55.65, 55, 54.67, 54.56, 54.56, 53.75, 53.75,
            56.6)) > 0  | # sd 22
       point.in.polygon(point.x = an(ping.fgrounds$SI_LONG), point.y = an(ping.fgrounds$SI_LATI),
          pol.x = c(12, 12, 11.94, 11.97, 12, 12, 15, 15, 14.78,
            14.2, 13.7, 12.81, 12.44), pol.y = c(55.3, 54.75,
            54.67, 54.56, 54.56, 53.5, 53.5, 55, 55.3, 55.4,
            55.5, 55.38, 55.33)) > 0 | # sd24
       point.in.polygon(point.x = ping.fgrounds$SI_LONG, point.y = ping.fgrounds$SI_LATI,
          pol.x = c(10.59262, 11.97217, 12.15883, 12.70796, 13.12992,
            12.80622, 12.95073, 12.72185, 12.45127, 12.29556,
            12.13384, 11.99063, 11.58487, 11.58487, 11.63281,
            11.49492, 11.3094, 11.27652, 10.71374, 10.70218,
            10.24553, 10.19351, 10.42472, 10.59262), pol.y = c(57.74247,
            57.4653, 57.48032, 56.94085, 56.46389, 56.36135,
            56.19091, 56.16918, 56.29535, 56.12728, 55.49119,
            55.28764, 55.63113, 55.91101, 55.90623, 55.94866,
            55.97965, 56.00988, 56.14253, 56.25853, 56.49587,
            57.11107, 57.63566, 57.74247)) >0  | #2532
       point.in.polygon(point.x = ping.fgrounds$SI_LONG, point.y = ping.fgrounds$SI_LATI,
          pol.x = c(14.2, 14.78, 15, 15, 18, 18, 14.2), pol.y = c(55.4,
            55.3, 55, 53, 53, 56.5, 56.5)) > 0 |  #2532
       point.in.polygon(point.x = ping.fgrounds$SI_LONG, point.y = ping.fgrounds$SI_LATI,
          pol.x = c(18, 18, 22, 22), pol.y = c(56.5, 53.5, 53.5,
            56.5)) > 0 | # 2232
       point.in.polygon(point.x = ping.fgrounds$SI_LONG, point.y = ping.fgrounds$SI_LATI,
          pol.x = c(18, 18, 18.32, 18.32, 19, 19, 16, 16), pol.y = c(56.5,
            57, 57, 57.5, 57.925, 59.762, 59.762, 56.5)) > 0 | # 2532
       point.in.polygon(point.x = ping.fgrounds$SI_LONG, point.y = ping.fgrounds$SI_LATI,
          pol.x = c(19, 19, 18.45, 18.3, 18, 18, 21.5, 21.72, 21.98,
            22.17, 22.24, 21.93), pol.y = c(58.5, 57.9, 57.58,
            57, 57, 56.5, 56.5, 57.57, 57.97, 58.04, 58.15,
            58.5)) > 0 | # 2532
       point.in.polygon(point.x = ping.fgrounds$SI_LONG, point.y = ping.fgrounds$SI_LATI,
          pol.x = c(21.5, 21.72, 21.98, 22.17, 22.24, 22.24, 23,
            25, 25), pol.y = c(56.5, 57.57, 57.97, 58.04, 58.15,
            58.35, 58.5, 58.5, 56.5)) > 0 | # 2532
       point.in.polygon(point.x = ping.fgrounds$SI_LONG, point.y = ping.fgrounds$SI_LATI,
          pol.x = c(19, 17.975, 21.6, 21.8, 23.325, 23.325, 23.191,
            23, 23, 23.5, 23.6, 24, 23.692, 22.5, 22.1, 21.92,
            19), pol.y = c(59.762, 60.5, 60.5, 60.7, 60.5, 59.965,
            59.867, 59.827, 59, 59, 59.05, 58.75, 59.5, 59.5,
            58.35, 58.5, 58.5)) > 0 | # 2532
       point.in.polygon(point.x = ping.fgrounds$SI_LONG, point.y = ping.fgrounds$SI_LATI,
          pol.x = c(16.5, 16.5, 19.7, 19.7, 22.6, 21.4), pol.y = c(60.5,
            63.7, 63.7, 63.5, 63.5, 60.5)) > 0 | # 2532
       point.in.polygon(point.x = ping.fgrounds$SI_LONG, point.y = ping.fgrounds$SI_LATI,
          pol.x = c(19.7, 19.7, 25.7, 25.7, 19.7), pol.y = c(63.7,
            63.5, 63.5, 67, 67)) > 0 | # 2532
       point.in.polygon(point.x = ping.fgrounds$SI_LONG, point.y = ping.fgrounds$SI_LATI,
          pol.x = c(23.325, 23.325, 23.191, 23, 23, 30.5, 30.5),
          pol.y = c(60.5, 59.965, 59.867, 59.827, 59, 59, 60.5)) > 0 | # 2532
       point.in.polygon(point.x = ping.fgrounds$SI_LONG, point.y = ping.fgrounds$SI_LATI,
          pol.x = c(10.59262, 11.97217, 12.15883, 12.70796, 13.12992,
            12.80622, 12.95073, 12.72185, 12.45127, 12.29556,
            12.13384, 11.99063, 11.58487, 11.58487, 11.63281,
            11.49492, 11.3094, 11.27652, 10.71374, 10.70218,
            10.24553, 10.19351, 10.42472, 10.59262), pol.y = c(57.74247,
            57.4653, 57.48032, 56.94085, 56.46389, 56.36135,
            56.19091, 56.16918, 56.29535, 56.12728, 55.49119,
            55.28764, 55.63113, 55.91101, 55.90623, 55.94866,
            55.97965, 56.00988, 56.14253, 56.25853, 56.49587,
            57.11107, 57.63566, 57.74247)) > 0 # kattegat

    
    # check wihch nodes are actually used
    #map("worldHires", xlim=general$lim.long, ylim=general$lim.lat)
    #points(an(ping.fgrounds$SI_LONG[in_west_baltic]), an(ping.fgrounds$SI_LATI[in_west_baltic]), col=2)

    # then subset here...
    vid_this_case_study <- as.character(unique(ping.fgrounds$VE_REF[in_west_baltic_kattegat_and_east_baltic]))
    cat(paste(length(vid_this_case_study), " vessels over ", length(unique(ping.fgrounds$VE_REF)), " in total" , "\n"))
    ping.fgrounds      <- ping.fgrounds[ping.fgrounds$VE_REF %in% vid_this_case_study,]
    ping.harbours      <- ping.harbours[ping.harbours$VE_REF %in% vid_this_case_study,]
    }  # end case_study
    
    
   
    #*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
    #*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
    
    
      save(ping.fgrounds,  file=file.path(general$main.path, "merged_tables", general$case_study,
             paste("ping.fgrounds.",general$a.country,".",general$a.year,".igraph",general$igraph,".RData", sep='')))
      cat(paste("save 'ping.fgrounds', this year...OK\n\n",sep=""))
      save(ping.harbours,  file=file.path(general$main.path, "merged_tables", general$case_study,
             paste("ping.harbours.",general$a.country,".",general$a.year,".igraph",general$igraph,".RData", sep='')))
     cat(paste("save 'ping.harbours', this year...OK\n\n",sep=""))
      save(vid_this_case_study,  file=file.path(general$main.path, "merged_tables", general$case_study,
             paste("vid_this_case_study_",general$a.country,".",general$a.year,".igraph",general$igraph,".RData", sep='')))
      cat(paste("save 'vid_this_case_study', this year...OK\n\n",sep=""))
 
   # for info: percent of fishing points not included in the graph rectangle...
   #glat <- c(49,65) # range graph
   #glong <-  c(-2.5,21) # range graph
   #res.lat <- findInterval(as.numeric(as.character(x$SI_LATI)), glat)
   #res.long <- findInterval(as.numeric(as.character(x$SI_LONG)), glong)
   #a.res  <- res.long + res.lat # if !=2 then not in the rectangle defined by the graph
   #length(which(a.res!=2))*100/603607 # percent of fishing point not included in the graph rectangle...
   #table(x[which(a.res!=2),"VE_FLT"]) # which fleet-segemnt is concerned...

} # end FALSE

 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!AGGREGATE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
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


 # load the combined graph with the "merged" table
 load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.fgrounds.",general$a.country,".",general$a.year,".igraph",
                general$igraph,".RData",sep='')))
 load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.harbours.",general$a.country,".",general$a.year,".igraph",
                general$igraph,".RData",sep='')))
 

 x <- ping.fgrounds  # ONLY USE THE FGROUNDS

 # add quarters, and then semesters
 x$quarter <- quarters(as.POSIXct(x$SI_DATE, tz="GMT"))
 x$semester <- factor(x$quarter)
 levels(x$semester) <- c(1,1,2,2)

 #...and slightly modify the name of metiers
 x$LE_MET_level6 <- factor(x$LE_MET_level6)
 x$LE_MET_level6 <- as.factor(unlist(lapply(strsplit( as.character(x$LE_MET_level6), split="_"),
                                  function(x) paste(x[1],'_',x[2],sep=''))))   # remove mesh size and useless selective grid info 

 metier_names <- cbind(levels(x$LE_MET_level6), 0:(length(levels(x$LE_MET_level6))-1))
 save(metier_names,  file=file.path(general$main.path, "merged_tables", general$case_study, ## SAVE 
                     paste("metier_names.",general$a.country,".",general$a.year,".igraph",
                      general$igraph,".RData", sep='')))
 write.table(metier_names, file.path(general$main.path, "metiersspe",paste("metier_names_",general$a.country,"_",general$a.year,".txt",sep='')),
               quote=FALSE, col.names=FALSE, row.names=FALSE)
 
 #levels(x$LE_MET_level6) <-
 #   paste("met", 0:(length(levels(x$LE_MET_level6))-1), "_", levels(x$LE_MET_level6), "_",general$a.country, sep='')
 # NEED JUST INTEGERS! for c++, replaced by:
 levels(x$LE_MET_level6) <- 0:(length(levels(x$LE_MET_level6))-1)

 # then, aggregate...
 nm           <- names(x)
 idx.col.w    <- grep('KG', nm) # index columns with species weight
 idx.col.e    <- grep('LE_EFF_VMS', nm)
 x$LE_EFF_VMS <- as.numeric(as.character(x$LE_EFF_VMS))
 idx.col      <- c(idx.col.w, idx.col.e)

 library(data.table)
 DT  <- data.table(x) # library data.table for fast grouping replacing aggregate()
 # AGGREGATE WEIGHT AND EFFORT PER SPECIES
 eq1             <- c.listquote( paste ("sum(",nm[idx.col],",na.rm=TRUE)",sep="") )
 DT$pt_graph     <- as.integer(DT$pt_graph)
 DT$VE_REF       <- as.factor(DT$VE_REF)
 x.agg           <- DT[,eval(eq1),by=list(VE_REF, LE_MET_level6, semester, pt_graph)]        
 x.agg           <- data.frame( x.agg)
 colnames(x.agg) <- c("VE_REF", "LE_MET_level6", "semester", "pt_graph", nm[idx.col])


  # save
  save(x.agg,  file=file.path(general$main.path, "merged_tables", general$case_study,
             paste("x.agg.",general$a.country,".",general$a.year,".igraph",
              general$igraph,".RData", sep='')))

 
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## PLAN B: second way: get the vessel-specific cpue per node per species 
 ## (to be used as in Bastardie et al 2010)
 ## will be used for stocks for which we do not have N_pops because not assessed by ICES
 ## => implicit populations vs. explicit ones
 load(file.path(general$main.path, "igraph", paste(general$igraph, "_graphibm.RData",sep=''))) # built from the R code
 coord_pt_graph <- coord[x$pt_graph,] # replace coord of vms point by the coord of the graph node before finding out the stock area
 
 #x$SI_LONG <- as.numeric(as.character(coord_pt_graph[,'x'])) 
 #x$SI_LATI <- as.numeric(as.character(coord_pt_graph[,'y'])) 
 #x$area   <- ICESarea2(x, string=TRUE) # utils
 
 # find out areas
 source(file=file.path(general$main.path.code,"IBM_param_utils_longlat_to_ICESareas.r"))
 x$x      <- as.numeric(as.character(coord_pt_graph[,'x'])) 
 x$y      <- as.numeric(as.character(coord_pt_graph[,'y'])) 
 x$area   <- longlat_to_ICESareas(x)
 x[is.na(x$area) | !(x$area %in% c('22', '23', '24', '25', '26', '27', '28-1', '28-2', '29', '30', '31', '32',
                         'IIIan', 'IIIas') ), 'area'] <- 'nsea' # if out of range, assign all these nodes to North Sea stocks...
 x[(x$area %in% c('22', '23', '24')), 'area'] <- '2224'
 x[(x$area %in% c('25', '26', '27', '28-1', '28-2', '29','30', '31', '32')), 'area'] <- '2532'
 
  ## check
 if(FRANCOIS){ x[x$VE_REF=="DNK000005491" & x$LE_KG_SPR!=0 & !is.na(x$LE_KG_SPR), ]
  x[x$VE_REF=="DNK000006040" & x$LE_KG_COD!=0 & !is.na(x$LE_KG_COD), ]
  }
  ####### PLAN B-1 ##########
  ####### PLAN B-1 ##########
  ####### PLAN B-1 ##########
  ### THE CPUE ARE LIKELY TO BE UNDERESTIMATED BECAUSE AN AVERAGE ON NODES...
  ## (in addition the cpues here are not accounting for possible underlying metiers.....)
  ## PRELIMINARY RESULTS SHOWED THAT INDEED THIS DOES NOT SEEM ENOUGH TO 
  ## DESCRIBE THE SHAPE OF THE DISTRIBUTION OF CPUEs (i.e. skewed with long tail)...
  ## SO IT COULD BE BETTER TO FIT A GAMMA DISTRIBUTION INSTEAD (ON EACH NODE)
  nm            <- names(x)
  idx.col.w     <- grep('KG', nm) # index columns with species weight
  tmp           <- x[,idx.col.w]
  colnames(tmp) <- gsub('KG', 'cpue', colnames(tmp))  
  x             <- cbind (x, tmp / (x$LE_EFF_VMS/60)) # kghour
  gamma.nll     <- function(par,data) -sum(dgamma(data,shape=par[1],scale=par[2],log=T))
  cpues         <- list(NULL)
  filen         <- file.path(general$main.path, "merged_tables",  general$case_study,
                              paste("cpues_gamma.", general$a.country, ".", general$a.year, ".igraph",
                               general$igraph,".txt", sep=''))
  write.table("", append=FALSE, file=filen, row.names=FALSE, col.names=FALSE, quote=FALSE)
  for (sp in colnames(tmp)){
     d <- x[, c('VE_REF','quarter','pt_graph','area',sp)]
     cat(paste("\n"))
     cat(paste(sp,"\n"))
     for(vid in unique(d$VE_REF)){
        cat(paste("."))
        for (a.quarter in c("Q1","Q2","Q3","Q4")){
           dd <- d[d$VE_REF==vid & d$quarter== a.quarter, ]
           for (a.pt_graph in unique(dd$pt_graph)){
            ddd   <- dd[dd$pt_graph== a.pt_graph,]
            dddd  <- ddd[, sp]
            area  <- ddd[1,"area"] 
            dddd  <- dddd[dddd>1]
            if(length(dddd)!=0){
              er <-  try(opt <- optim(c(1,1),gamma.nll,data=dddd,method='BFGS') , silent=TRUE)
             if(class(er)=="try-error"){
                opt <- list(par=c(1,mean(dddd)))  # best guess
             }
               # check 
         #      par(mfrow=c(1,2))
         #      hist(dddd)
         #      hist(rgamma(100, shape=opt$par[1], scale = opt$par[2]) )
         # browser()
            }   else{opt <- list(par=c(0,0))}
           # save (line by line to avoid out of memory)
           write.table(data.frame(VE_REF=vid, quarter=a.quarter, pt_graph= a.pt_graph, area= area, stock= sp, 
                                   shape=round(opt$par[1],4), scale = round(opt$par[2],4)),
                        file = filen, append = TRUE, quote = FALSE, sep = " ",
                         row.names=FALSE, col.names=FALSE)
   }}} } # be (really) patient.....
  
 ## Then,
 ## R:   produce two .dat files, one for each gamma parameter
 ## C++: draw cpue on nodes from gamma distribution
  cpues_gamma           <- read.table(filen, sep=" ")
  colnames(cpues_gamma) <- c('VE_REF','quarter','pt_graph','area','species','shape','scale')
   ####------
  # keep only the relevant stocks
  # (not creating any duplicates here because pt_graph is unique....)
  cpues_gamma$stock <- paste( gsub("LE_cpue_", "", cpues_gamma$species), ".", cpues_gamma$area, sep='') 
  # correct names for special cases (those across management areas)
  cpues_gamma[cpues_gamma$stock %in% c("COD.IIIan"), "stock"] <- 'COD.nsea'
  cpues_gamma[cpues_gamma$stock %in% c("COD.IIIas"), "stock"] <- 'COD.kat'
  cpues_gamma[cpues_gamma$stock %in% c("HAD.IIIan"), "stock"] <- 'HAD.nsea'
  cpues_gamma[cpues_gamma$stock %in% c("HER.IIIan", "HER.IIIas", "HER.2224"), "stock"] <- 'HER.3a22'
  cpues_gamma[cpues_gamma$stock %in% c("SPR.2224", "SPR.2532"), "stock"] <- 'SPR.2232'
  cpues_gamma[cpues_gamma$stock %in% c("PLE.2224", "PLE.2532"), "stock"] <- 'PLE.2232'
  cpues_gamma[cpues_gamma$stock %in% c("FLE.2224", "FLE.2532"), "stock"] <- 'FLE.2232'
  cpues_gamma[cpues_gamma$stock %in% c("TUR.2224", "TUR.2532"), "stock"] <- 'TUR.2232'
  cpues_gamma[cpues_gamma$stock %in% c("DAB.2224", "DAB.2532"), "stock"] <- 'DAB.2232'
  cpues_gamma[grep('IIIa', cpues_gamma$stock), "stock"] <- # for all other species, correct kask 
     paste( gsub("LE_cpue_", "",cpues_gamma[grep('IIIa', cpues_gamma$stock),'species']), ".kask", sep='')
  
  # subset for relevant populations
  pop_names                   <- read.table(file.path(general$main.path, "popsspe",paste("pop_names_",general$case_study,".txt", sep='')))  ## CAUTION: circularity for pop_names 
  cpues_gamma                 <- cpues_gamma[cpues_gamma$stock %in% pop_names[,1],] # keep simulated stocks only 
  cpues_gamma$stock           <- factor(cpues_gamma$stock)
  levels(cpues_gamma$stock )  <- pop_names[,2][ match(levels(cpues_gamma$stock), as.character(pop_names[,1]))] # map the name to integer
  cpues_gamma$mapped_stk_code <- as.numeric(as.character(cpues_gamma$stock))
  library(doBy)
  cpues_gamma                 <- orderBy(~VE_REF, data=cpues_gamma) # library(doBy) # order from 0 to nbstock
  cpues_gamma                 <- cpues_gamma[,c("VE_REF",  "quarter", "pt_graph", "mapped_stk_code", "shape", "scale")]

  ## merge with all combi to get all pop informed even if cpue at 0.(required fro Cpp multimap)
  ## (tricky to get all combi because need to exclude pt_graph because no need to complete for all combi of nodes!)
  all.combi           <- cpues_gamma [!duplicated(cpues_gamma  [,c('VE_REF','quarter','pt_graph')]), c('VE_REF','quarter','pt_graph')]
  all.combi           <- merge(all.combi,  pop_names[,2],  all=TRUE)
  colnames(all.combi) <- c('VE_REF','quarter','pt_graph', 'mapped_stk_code') 
  cpues_gamma         <- merge(all.combi, cpues_gamma , all=TRUE)
  cpues_gamma[is.na( cpues_gamma$shape ), 'shape' ]  <- 0   # replace NA  by 0
  cpues_gamma[is.na( cpues_gamma$scale ), 'scale' ]  <- 0   # replace NA  by 0

 
  ## order
  library(doBy) 
  cpues_gamma <- orderBy(~VE_REF+quarter+pt_graph+mapped_stk_code , data=cpues_gamma)# order from 0 to nb of pops for the cpp multimap to be in the right order...
  
   ####-------
  # save .dat files
   for (a.quarter in c("Q1","Q2","Q3","Q4")){
      cpues_gamma.Q          <- cpues_gamma[cpues_gamma$quarter==a.quarter,]
      cpues_gamma.Q$VE_REF   <- factor( cpues_gamma.Q$VE_REF )
      cpues_gamma.Q$pt_graph <- cpues_gamma.Q$pt_graph - 1 ##!!! OFFSET FOR C++ !!!##
      
      for(vid in unique(cpues_gamma$VE_REF)){
        cpues_gamma.Q.vid                              <- cpues_gamma.Q[cpues_gamma.Q$VE_REF==vid,]
        vesselspe_gshape_cpue_per_stk_on_nodes_quarter <- cpues_gamma.Q.vid[,c('pt_graph', 'shape')]
        vesselspe_gscale_cpue_per_stk_on_nodes_quarter <- cpues_gamma.Q.vid[,c('pt_graph', 'scale')]
        # vessel spe .dat file
        write.table(round(vesselspe_gshape_cpue_per_stk_on_nodes_quarter, 2),
          file=file.path(general$main.path, "vesselsspe",
           paste(vid, "_gshape_cpue_per_stk_on_nodes_quarter",gsub("Q","",a.quarter),".dat",sep='')),
            col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)
        # vessel spe .dat file
        write.table(round(vesselspe_gscale_cpue_per_stk_on_nodes_quarter),
          file=file.path(general$main.path, "vesselsspe",
           paste(vid, "_gscale_cpue_per_stk_on_nodes_quarter",gsub("Q","",a.quarter),".dat",sep='')),
            col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)
      }
   }

   ## check
if(FRANCOIS){  cpues_gamma[cpues_gamma$VE_REF=="DNK000006040" & cpues_gamma$mapped_stk_code=="11", ]
  cpues_gamma[cpues_gamma$VE_REF=="DNK000007161", ]
 }
if(TANJA){ cpues_gamma[cpues_gamma$VE_REF=="GER000001079" & cpues_gamma$mapped_stk_code=="11", ]
 cpues_gamma[cpues_gamma$VE_REF=="GER000001079", ]
  }
 
 
 
   
  ####### PLAN B-2 ##########
  ####### PLAN B-2 ##########
  ####### PLAN B-2 ##########
 # the 'average' way: aggregate weight and effort before computing cpue
 library(data.table)
 nm           <- names(x)
 idx.col.w    <- grep('KG', nm) # index columns with species weight
 idx.col.e    <- grep('EFF', nm) # index columns with species weight
 idx.col      <- c(idx.col.w,idx.col.e ) # index columns with species weight
 DT           <- data.table(x) # library data.table for fast grouping replacing aggregate()
 # AGGREGATE WEIGHT AND EFFORT PER SPECIES
 eq1              <- c.listquote( paste ("sum(",nm[idx.col],",na.rm=TRUE)",sep="") )
 x.agg2           <- DT[,eval(eq1),by=list(VE_REF, quarter, pt_graph, area)]
 x.agg2           <- data.frame( x.agg2)
 colnames(x.agg2) <- c("VE_REF",  "quarter", "pt_graph","area",nm[idx.col])
  
 # check 
 x.agg2[x.agg2$VE_REF=="DNK000007161" ,]
  
 ## CPUE COMPUTATION + RESHAPING (WIDE TO LONG FORMAT)
 nm                      <- names(x.agg2)
 idx.col.w               <- grep('KG', nm) # index columns with species weight
 # compute cpue kg per hour
 x.agg2[,idx.col.w]      <- x.agg2[,idx.col.w] / (x.agg2$LE_EFF_VMS/60) 
 # remove no longer used col
 x.agg2                  <- x.agg2[, !colnames(x.agg2) %in% "LE_EFF_VMS"] 
 # reshape
 x.agg2$id               <- paste(x.agg2$VE_REF, '.', x.agg2$quarter, '.', x.agg2$pt_graph, '.', x.agg2$area, sep='')
 x.agg2.long             <- reshape(x.agg2, direction="long", ids="id",
                              times=nm[idx.col.w], timevar="species", 
                               v.names="LE_KG_", varying=5:(ncol(x.agg2)-1))  # be patient....
 x.agg2.long             <- x.agg2.long[,c("VE_REF",  "quarter", "pt_graph", "area", "species", "LE_KG_")]
 rownames(x.agg2.long)   <- NULL
 colnames(x.agg2.long)   <- c("VE_REF",  "quarter", "pt_graph", "area", "species", "cpue_kghour")


 ####------
  # keep only the relevant stocks
  x.agg2.long$stock  <- paste( gsub("LE_KG_", "",x.agg2.long$species), ".", x.agg2.long$area, sep='') 
  # correct names for special cases (those across management areas)
  x.agg2.long[x.agg2.long$stock %in% c("COD.IIIan"), "stock"] <- 'COD.nsea'
  x.agg2.long[x.agg2.long$stock %in% c("COD.IIIas"), "stock"] <- 'COD.kat'
  x.agg2.long[x.agg2.long$stock %in% c("HAD.IIIan"), "stock"] <- 'HAD.nsea'
  x.agg2.long[x.agg2.long$stock %in% c("HER.IIIan", "HER.IIIas", "HER.2224"), "stock"] <- 'HER.3a22'
  x.agg2.long[x.agg2.long$stock %in% c("SPR.2224", "SPR.2532"), "stock"] <- 'SPR.2232'
  x.agg2.long[x.agg2.long$stock %in% c("PLE.2224", "PLE.2532"), "stock"] <- 'PLE.2232'
  x.agg2.long[x.agg2.long$stock %in% c("FLE.2224", "FLE.2532"), "stock"] <- 'FLE.2232'
  x.agg2.long[x.agg2.long$stock %in% c("TUR.2224", "TUR.2532"), "stock"] <- 'TUR.2232'
  x.agg2.long[x.agg2.long$stock %in% c("DAB.2224", "DAB.2532"), "stock"] <- 'DAB.2232'
  x.agg2.long[grep('IIIa', x.agg2.long$stock), "stock"] <- # for all other species, correct kask 
     paste( gsub("LE_KG_", "",x.agg2.long[grep('IIIa', x.agg2.long$stock),'species']), ".kask", sep='')
  
  # subset for relevant populations
   pop_names                  <- read.table(file.path(general$main.path, "popsspe",paste("pop_names_",general$case_study,".txt", sep='')))
  x.agg2.long                 <- x.agg2.long[x.agg2.long$stock %in% pop_names[,1],] # keep simulated stocks only 
  x.agg2.long$stock           <- factor(x.agg2.long$stock)
  levels(x.agg2.long$stock )  <- pop_names[,2][ match(levels(x.agg2.long$stock), as.character(pop_names[,1]))] # map the name to integer
  x.agg2.long$mapped_stk_code <- as.numeric(as.character(x.agg2.long$stock))
  x.agg2.long                 <- orderBy(~VE_REF, data=x.agg2.long) # library(doBy) # order from 0 to nbstock

  x.agg2.long                 <- x.agg2.long[,c("VE_REF",  "quarter", "pt_graph", "mapped_stk_code", "cpue_kghour")]
  x.agg2.long                 <- x.agg2.long[!is.na(x.agg2.long$mapped_stk_code),] # remove NA stocks
  
  ## check
  dd<-x.agg2.long[x.agg2.long$VE_REF=="DNK000039090" & x.agg2.long$mapped_stk_code=="1","cpue_kghour"]
  dd<-x.agg2.long[x.agg2.long$VE_REF=="DNK000007161" ,"cpue_kghour"]

  
  ## clean up  (e.g. inf and NaN comes from division by 0 when LE_EFF_VMS at 0 for some few cases....)
  x.agg2.long[is.na(x.agg2.long$cpue_kghour), "cpue_kghour"]       <- 0 
  x.agg2.long[is.infinite(x.agg2.long$cpue_kghour), "cpue_kghour"] <- 0 

  
  ## merge with all combi to get all pop informed even if cpue at 0.(required fro Cpp multimap)
  ## (tricky to get all combi because need to exclude pt_graph because no need to complete for all combi of nodes!)
  all.combi             <- x.agg2.long[!duplicated( x.agg2.long [,c('VE_REF','quarter','pt_graph')]), c('VE_REF','quarter','pt_graph')]
  all.combi             <- merge(all.combi,   pop_names[,2] , all=TRUE)
  colnames(all.combi)   <- c('VE_REF','quarter','pt_graph', 'mapped_stk_code') 
  x.agg2.long           <- merge(all.combi, x.agg2.long , all=TRUE)
  x.agg2.long[is.na( x.agg2.long$cpue_kghour ), 'cpue_kghour' ]  <- 0   # replace NA cpue by 0

  ## order
  library(doBy) 
  x.agg2.long <- orderBy(~VE_REF+quarter+pt_graph+mapped_stk_code , data=x.agg2.long)# order from 0 to nb of pops for the cpp multimap to be in the right order...
  
   ####-------
  # save .dat files
   for (a.quarter in c("Q1","Q2","Q3","Q4")){
      x.agg.Q           <- x.agg2.long[x.agg2.long$quarter==a.quarter,]
      x.agg.Q$VE_REF    <- factor( x.agg.Q$VE_REF )
      x.agg.Q$pt_graph  <-  x.agg.Q$pt_graph - 1 ##!!! OFFSET FOR C++ !!!##
      
      for(vid in unique(x.agg2.long$VE_REF)){
        x.agg.Q.vid <- x.agg.Q[x.agg.Q$VE_REF==vid,]
        vesselspe_cpue_per_stk_on_nodes_quarter <- x.agg.Q.vid[,c('pt_graph', 'cpue_kghour')]
        # vessel spe .dat file
        write.table(round(vesselspe_cpue_per_stk_on_nodes_quarter),
          file=file.path(general$main.path, "vesselsspe",
           paste(vid, "_cpue_per_stk_on_nodes_quarter",gsub("Q","",a.quarter),".dat",sep='')),
            col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)
      }
   }

 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!MERGE WITH AVAI!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!AND DO THE GLMs!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!PER POP AND SEMESTER!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

 if(FRANCOIS){
   if(general$case_study=="canadian_paper"){
      year      <- "2010" 
      years     <- "2005_2010" 
      method    <- "maximum"
      threshold <- "50"
      }
   if(general$case_study=="baltic_only"){
      #years     <- "2010" 
      #method    <- "inverse"
      #threshold <- "50"
      year      <- "2012" 
      years     <- "2008_2012" 
      method    <- "inverse"
      threshold <- "25"
      }

  # load avai object built using get_spatial_avai_keys_on_igraph_nodes_from_surveys_per_size_group.r
  load(file.path(general$main.path, "avai",
        paste("lst_avai_igraph",general$igraph,"_",year,"_",method,"_",threshold,".RData",sep=""))) ##!! e.g. 2010 !!##
  lst.avai1      <- lst.avai
 
  # load avai object built using get_spatial_avai_keys_on_igraph_nodes_from_surveys_per_size_group.r
  load(file.path(general$main.path, "avai",
        paste("lst_avai_igraph",general$igraph,"_",years,"_",method,"_",threshold,".RData",sep=""))) ##!!e.g.  2005-2010 !!##
  lst.avai2     <- lst.avai
 
  #lst.avai  <-  lst.avai1 # one year... 
  lst.avai  <-  lst.avai2 # ...or a period.
 
  
  
  # load for Denmark 
  if("DEN" %in% general$case_study_countries){
     load(file.path(general$main.path,"merged_tables", general$case_study, paste("x.agg.DEN.",year,".igraph",general$igraph,".RData",sep='')))
     x_agg_den <- x.agg ; x_agg_den$country <- "DEN"
     load(file.path(general$main.path,"merged_tables", general$case_study, paste("metier_names.DEN.",year,".igraph",general$igraph,".RData",sep='')))
     metier_names_den           <- metier_names  
     colnames(metier_names_den) <- c("met", "idx")
      combined_met_names <-  data.frame(metier_names_den)
     # save
     save(combined_met_names,  file=file.path(general$main.path, "merged_tables", general$case_study, ## SAVE 
             paste("combined_met_names.", general$a.year,".igraph",
              general$igraph,".RData", sep='')))
     if(FRANCOIS) write.table(combined_met_names[,c(1,2)], 
                 paste(general$main.path, "\\metiersspe\\combined_met_names.txt",sep=''),
                  quote=FALSE, col.names=FALSE, row.names=FALSE)
     }
     
     
  # load for Germany 
  if("DEU" %in% general$case_study_countries){
     load(file.path(general$main.path,"merged_tables", general$case_study, paste("x.agg.DEU.",year,".igraph",general$igraph,".RData",sep='')))
     x_agg_deu <- x.agg ; x_agg_deu$country <- "DEU" 
     load(file.path(general$main.path,"merged_tables", general$case_study, paste("metier_names.DEU.",year,".igraph",general$igraph,".RData",sep='')))
     metier_names_deu <- metier_names  ; colnames(metier_names_deu) <- c("met", "idx")
     combined_met_names <-  data.frame(metier_names_deu)
    # save
     save(combined_met_names,  file=file.path(general$main.path, "merged_tables", general$case_study, ## SAVE 
             paste("combined_met_names.", general$a.year,".igraph",
              general$igraph,".RData", sep='')))
     if(FRANCOIS) write.table(combined_met_names[,c(1,2)], 
                 paste(general$main.path, "\\metiersspe\\combined_met_names.txt",sep=''),
                  quote=FALSE, col.names=FALSE, row.names=FALSE)
    }
    
 
  # load for SWE 
  if("SWE" %in% general$case_study_countries){
     load(file.path(general$main.path,"merged_tables", general$case_study, paste("x.agg.SWE.",year,".igraph",general$igraph,".RData",sep='')))
     x_agg_swe <- x.agg ; x_agg_swe$country <- "SWE" 
     load(file.path(general$main.path,"merged_tables", general$case_study, paste("metier_names.SWE.",year,".igraph",general$igraph,".RData",sep='')))
     metier_names_swe <- metier_names  ; colnames(metier_names_swe) <- c("met", "idx")
     combined_met_names <-  data.frame(metier_names_swe)
    # save
     save(combined_met_names,  file=file.path(general$main.path, "merged_tables", general$case_study, ## SAVE 
             paste("combined_met_names.", general$a.year,".igraph",
              general$igraph,".RData", sep='')))
     if(FRANCOIS) write.table(combined_met_names[,c(1,2)], 
                 paste(general$main.path, "\\metiersspe\\combined_met_names.txt",sep=''),
                  quote=FALSE, col.names=FALSE, row.names=FALSE)
    }
 
 
  if("DEN" %in% general$case_study_countries &&
      "DEU" %in% general$case_study_countries &&
        "SWE" %in% general$case_study_countries  ){
     # rename the metiers because of the combination of dnk and ger
     combined_met_names        <- merge(metier_names_den, metier_names_deu, by="met", all=TRUE, suffixes = c(".den",".deu"))
     combined_met_names        <- merge(combined_met_names, metier_names_swe, by="met", all=TRUE)
     colnames(combined_met_names) <- c('met', 'idx.den', 'idx.deu', 'idx.swe')
     combined_met_names$idx    <- 0:(nrow(combined_met_names)-1) # dont forget the c++ offset...
     # rename for den
     levels(x_agg_den$LE_MET_level6) <- 
        combined_met_names$idx[ match(levels(x_agg_den$LE_MET_level6), as.character(combined_met_names$idx.den))] 
     # rename for deu
     levels(x_agg_deu$LE_MET_level6) <-
       combined_met_names$idx[ match(levels(x_agg_deu$LE_MET_level6), as.character(combined_met_names$idx.deu))] 
     # rename for swe
     levels(x_agg_swe$LE_MET_level6) <-
       combined_met_names$idx[ match(levels(x_agg_swe$LE_MET_level6), as.character(combined_met_names$idx.swe))] 
     # save
     save(combined_met_names,  file=file.path(general$main.path, "merged_tables", general$case_study, ## SAVE 
             paste("combined_met_names.", general$a.year,".igraph",
              general$igraph,".RData", sep='')))
     if(FRANCOIS) write.table(combined_met_names[,c('met','idx')], 
                 paste(general$main.path, "\\metiersspe\\combined_met_names.txt",sep=''),
                  quote=FALSE, col.names=FALSE, row.names=FALSE)
 
    
     # rbind Germany, Denmark and Sweden
     combined_columns   <- unique(c(names(x_agg_den),names(x_agg_deu),names(x_agg_swe)))
     idx_col            <- grep("KG", combined_columns)
     combined_columns   <- c(combined_columns[!(1:length(combined_columns)) %in% idx_col], sort(combined_columns[idx_col])) 
     x_agg_den[,combined_columns[is.na(match(combined_columns,names(x_agg_den)))]] <- NA
     x_agg_deu[,combined_columns[is.na(match(combined_columns,names(x_agg_deu)))]] <- NA
     x_agg_swe[,combined_columns[is.na(match(combined_columns,names(x_agg_swe)))]] <- NA
     x_agg_den          <- x_agg_den[, combined_columns] # order column names
     x_agg_deu          <- x_agg_deu[, combined_columns] # order column names
     x_agg_swe          <- x_agg_swe[, combined_columns] # order column names
     x.agg              <- rbind(x_agg_den, x_agg_deu, x_agg_swe) # rbind
   } else{
    x.agg  <- x_agg_den
   }# end combine


 
  
   ## SELECTIVITY ###################
  # by default, create a fake selectivity ogive i.e. all at 1 (and not species-specific...)
  sel <- NULL
  for (met in unique(combined_met_names$idx) ) {
 
    the_met <- as.character(combined_met_names[combined_met_names[, 'idx']==met, 'met']) 
    
    clupeid <- FALSE  ; gadoid <- FALSE; trawl <- FALSE ; gillnet <- FALSE
    #if (length (grep("16", the_met))!=0 || length (grep("SPF", the_met))!=0)  clupeid <- TRUE
    if (length (grep("SPF", the_met))!=0)  clupeid <- TRUE
    if ((length (grep("DEF", the_met))!=0 || length (grep("MCD", the_met))!=0 || length (grep("CRU", the_met))!=0 || length (grep("FWS", the_met))!=0) && clupeid==FALSE) 
                    {trawl <- TRUE; gillnet <- FALSE ; gadoid <- TRUE}
    if (length (grep("GNS", the_met))!=0 && clupeid==FALSE)  {trawl <- FALSE; gillnet <- TRUE ; gadoid <- TRUE}
 
    #### TO DO : PER METIER 
    L50 <- 1   # default: will generate ogive at 1
    L75 <- 1.1  # default: will generate ogive at 1
    if(clupeid){
     L50         <- 16 # 36mm trawl, Suuronen and millar 1992  
     L75         <- 18 # 36mm trawl, Suuronen and millar 1992 
    }
    if(gadoid && trawl){
     L50         <- 38
     L75         <- 41
    }   
    if(gadoid && gillnet ){
     L50         <- 44 # gillnet Madsen 2007  
     L75         <- 46 # gillnet Madsen 2007 
    }
    cat(paste('the_met is ', the_met, ' then clupeid is ', 
                 clupeid,', gadoid is ',gadoid, 
                  ', trawl is ', trawl, ', gillnet is ',gillnet, '\n', sep=''))
    
    if(general$case_study=="canadian_paper"){
        a_size_group_bin_in_cm <- 10
        mid                    <- 5
        }
    if(general$case_study=="baltic_only"){
        a_size_group_bin_in_cm <- 5
        mid                    <- 2.5
        }
    l           <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,1000) *a_size_group_bin_in_cm  # vector of 14 length groups of eg 10 cm bin or 5 cm   
    length.fish <-  l + mid # i.e. mid size in cm
    equ.sel     <- paste("1/(1+exp(S1-S2*length.fish))")  # cf. Rasmus paper
    S1          <- L50*log(3) / (L75 - L50)      # L75=L50+(1/2*SR)
    S2          <-  S1/L50
    # eval(parse("",text=equ.sel)) # a sigmoid....
    ogive              <- rep(met, 14) 
    sel <-  rbind (sel, cbind(ogive, eval(parse("",text=equ.sel))  )  )  # ...assuming 14 szgroup bins
  }
  colnames(sel) <- c("met_name","ogive")
 
 
 
  # save the .dat file
  if(FRANCOIS) {  
            write.table(sel,
               file=file.path(general$main.path, "metiersspe",
                 paste("metier_selectivity_ogives.dat",sep='')),
                   col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)
       }


  #-------------------------------
  #-------------------------------
  # keep only DEN (caution: DNK in VE_REF), DEU and SWE vessels
  x.agg <- x.agg[c(grep("DNK", x.agg$VE_REF), grep("DEU", x.agg$VE_REF), grep("SWE", x.agg$VE_REF)),]
   
  all.betas.vid    <- NULL  # VE_REF mean estimate
  all.betas.met    <- NULL  # LE_MET_level6 mean estimate 
  all.betas.vid.se <- NULL  # VE_REF standard error
  all.betas.met.se <- NULL  # LE_MET_level6 standard error

  range_szgroup <- c(0,2,3,5,7)   # canadian journal - size group to be included in the glm e.g 0: 0-10cm, 1: 10-20cm, etc.
  the_selected_szgroups <- NULL
 
 
  for(rg in range_szgroup){
    assign(paste('all.betas' ,rg, sep=''),  NULL)   # init
    assign(paste('all.betas.se' ,rg, sep=''),  NULL)   # init
    }
    
  res1 <- NULL  # goodness of fit glm1
  res2 <- NULL   # goodness of fit glm2
  #----------------------------------------------
  # subset the x.agg to keep the distribution area of
  # the relevant pop and during the relevant semester only
  nm        <-  names(lst.avai)  # names of stocks
  for (i in 1: length(lst.avai)){  # PER STOCK
     nm2 <- names(lst.avai)[[i]]  # name of a given stock
     for (j in 1: length(lst.avai[[i]])){  # PER SEMESTER
       nm3        <- names(lst.avai[[i]])
       cat(paste("stock ",    nm2,    "\n"))
       cat(paste("semester ", nm3[j], "\n"))
       avai       <- lst.avai[[i]][[j]]
       #if(nm2 %in% c("SOL.kask", "SOL.2224")) { avai  <- lst.avai.2005.2010 [[i]][[j]] } # special case for SOL.kask and SOL.2224 not inofrmed in 2010 (weird) 
       name.sp    <- substr(nm2, 1,3) # FAO sp code
       a_pop      <- paste("LE_KG_", name.sp, sep='')
       if(a_pop %in% colnames(x.agg) && name.sp!="MZZ"){
            xx <- x.agg[
                       x.agg$pt_graph %in% avai[,4] &
                         x.agg$semester==as.numeric(nm3[[j]]),  # subset according to pt_graph and semester
                          c("VE_REF","LE_MET_level6", "LE_EFF_VMS", "semester", "pt_graph", a_pop)
                        ]  # subset the pop
       if(name.sp=="SAN") thrhold <- 200000 else  thrhold <- 5000
  
 
       # Instead of making the code very complicated and furthermore refactor the c++ side, 
       # herafter an easy and DANGEROUS shortcut (dangerous because confusing...) as a way for changing the szgroups used for the catch rate equation....
       # i.e. select the 5 most relevant ones for this species.
       # e.g. try to contrast the spatial avai data for small vs. larger fish (i.e. >minimum landing size)
       selected_szgroups <-  range_szgroup #default
       if(nm2 %in% "COD.2224") selected_szgroups <-   c(2,6,7,8,9)  
       if(nm2 %in% "SPR.2232") selected_szgroups <-   c(0,1,2,3,4)  
       if(nm2 %in% "HER.3a22") selected_szgroups <-   c(2,3,4,5,6) 
       avai[,paste(name.sp,'.nb_indiv.',range_szgroup, sep='')] <- avai[,paste(name.sp,'.nb_indiv.', selected_szgroups, sep='')] ## CAUTION HERE!  
       if(nm3[j]==1) the_selected_szgroups <- rbind(the_selected_szgroups, cbind.data.frame(nm2, selected_szgroups))   # store for later use....

 
    
       if( table(xx[,a_pop]!=0)["TRUE"] >5  && # on more than 5 nodes
             !all(is.na(avai[,-c(1:4)]))   &&  # avai informed
               sum(xx[,a_pop], na.rm=TRUE)> thrhold ){  # at least 10 tons landed this semester!
  
       
        
    
       # map avai according to pt_graph
       xx$idx           <- match( xx[,"pt_graph"], avai[,4])  # avai[,4] is the pt_graph
       xx               <- xx[!is.na(xx$idx),]
       col.avai.to.keep <- paste (name.sp,  paste('.nb_indiv.',range_szgroup, sep=''), sep='')
       xx               <- cbind(xx, avai[xx$idx, col.avai.to.keep]) #=> keep only some sz group bins...
       xx[,colnames(xx) %in% col.avai.to.keep] <- xx[,colnames(xx) %in% col.avai.to.keep]*1000
       # ==> !!CAUTION!! change of scale here i.e. *1000 to improve the glm.

       # debug in case of not informed avai for the range of szgroup chosen...
       for (a.col in col.avai.to.keep) xx[,a.col] <- replace( xx[,a.col],  is.na(xx[,a.col]), 0)

       # make the col names generic by removing the FAO code...
       colnames(xx)[colnames(xx) %in% col.avai.to.keep] <-  paste('nb_indiv.',range_szgroup, sep='')

       ## STRONG ASSUMPTION (just to test the effect of some cpue on pt_graph with 0 availability)
       #idx <- apply(xx[,c('nb_indiv.2', 'nb_indiv.3', 'nb_indiv.4')], 1, sum, na.rm=T)==0
       #xx[idx,a_pop] <- 0 # remove the catches!

       # EXPERT KNOWLEDGE: remove records if 0 landings
       idx  <- which(xx[,a_pop]<=0)
       xx   <- xx[-idx, ] ## EXPERT KNOWLEDGE


       # compute cpue kg per hour
       xx$cpue <- ceiling( xx[,a_pop] / (xx$LE_EFF_VMS/60) )
       # EXPERT KNOWLEDGE: remove records if avai say 0 on this node
       idx <- which(apply(xx[,paste('nb_indiv.',range_szgroup, sep='')], 1, sum, na.rm=TRUE) <=0)  ## EXPERT KNOWLEDGE
       if(length(idx)!=0){ 
                 cat(paste("percent records with avai at 0: ", round(nrow(xx[idx, ])/ nrow(xx[, ])*100),"%\n",sep=''))
                 # avai at 0 while some landings is explained by insufficient coverage of the bits and ibts surveys (in 2010)
                 # (or too small threshold distance for interpolation...) and should be removed to make the glm significant...
                 # in the particualr case of the SPR-2232, this leads to also remove an entire metier! so need to be avoided: 
                # if(nm2=="SPR.2232") xx[idx, 'nb_indiv.0'] <- 1 else xx <- xx[-idx, ] # to avoid removing a metier (met 34)!
                xx <- xx[-idx, ]
                 ##if(nm2=="SPR.2232") browser()
                  }
 
       if(nrow(xx)>5){  
       # clean up
       xx <- xx[! (is.na(xx$cpue) | is.infinite(xx$cpue)),]
       for(a.col in  paste('nb_indiv.',range_szgroup, sep='') ) xx <- xx[!is.na(xx[,a.col]),]

       # EXPERT KNOWLEDGE: clean up unrealistic cpue for DEMERSAL spp
       if(!name.sp %in% c("HER","SPR","NOP","MAC", "MUS", "OYF")) xx <- xx[xx$cpue<5000,] # 3000 kg a hour
  
       # EXPERT KNOWLEDGE: a lot of variables for a very ugly outcome response cpue? filter out low cpue for these species... 
       #if(name.sp %in% c("HER","SPR","NOP","MAC")) xx <- xx[xx$cpue>5,] # 5 kg a hour
        xx <- xx[xx$cpue>1,] # 1 kg a hour

       if(nrow(xx)>2){
       # get the vessel effect, the metier effect and the avai effect for this species
       # but first, decide on the metier of ref 
       # ( e.g. arbitrary take the metier with highest landings for this species given the area)
       ref.metier        <- ""
       sum_this_sp       <- tapply(xx[,paste("LE_KG_",name.sp,sep='')], xx$LE_MET_level6, sum, na.rm=T)
       sum_this_sp       <- sum_this_sp[!is.na(sum_this_sp)]
       ref.metier        <- names(which.max(round(sum_this_sp[order(sum_this_sp)])))
       cat(paste("ref metier is taken ", combined_met_names[combined_met_names$idx==ref.metier,'met'],"\n"))
       if(length(sum_this_sp[sum_this_sp>1]) <= 1){
          # only one metier for some species, so need to contrast the glm by fake landings for a closer metier
          #    if(name.sp %in% c('CSH','SOL', 'MON','HKE', 'HAD')){  xx$LE_MET_level6 <- as.character(xx$LE_MET_level6) ;  xx$LE_MET_level6[1] <- "17";  xx$LE_MET_level6 <- factor( xx$LE_MET_level6)} #fake, to contrast
          #    if(name.sp %in% c('SAN')){        xx$LE_MET_level6 <- as.character(xx$LE_MET_level6) ;  xx$LE_MET_level6[1] <- "51" ;  xx$LE_MET_level6 <- factor( xx$LE_MET_level6)} #fake, to contrast
              if(name.sp %in% c('MUS','OYF')){  xx$LE_MET_level6 <- as.character(xx$LE_MET_level6) ;  xx$LE_MET_level6[1] <-  "8";   xx$LE_MET_level6 <- factor( xx$LE_MET_level6)} #fake, to contrast
              if(name.sp %in% c('PRA','CSH')){        xx$LE_MET_level6 <- as.character(xx$LE_MET_level6) ;  xx$LE_MET_level6[1] <-  "9";   xx$LE_MET_level6 <- factor( xx$LE_MET_level6)} #fake, to contrast
       print("need for contrast!!!!")
       }
       if(nm2=="PRA.kask") { xx$VE_REF <- as.character(xx$VE_REF) ;  xx$VE_REF[1] <-  "SWE_39";   xx$VE_REF <- factor( xx$VE_REF) } # fake to contrast!
       #if(nm2=="SOL.nsea") { xx$VE_REF <- as.character(xx$VE_REF) ;  xx$VE_REF[1] <-  "DNK000011752";   xx$VE_REF <- factor( xx$VE_REF) } # fake to contrast!
       #if(nm2=="MUS.kask") { xx$VE_REF <- as.character(xx$VE_REF) ;  xx$VE_REF[1] <-  "DNK000033712";   xx$VE_REF <- factor( xx$VE_REF) } # fake to contrast!
  
       
    
       if(length(ref.metier)==0) ref.metier <- levels(factor(xx$LE_MET_level6))[1] # if unusual fleet for this sp
       xx$LE_MET_level6     <-  relevel(factor(xx$LE_MET_level6), ref=ref.metier)

       # choose the reference VE_REF with median cpue (removing 0 cpue before)
       ref.vid       <-  ""
       ref.vid       <-  xx[as.numeric(xx[,a_pop]) >=   median(xx[xx [,a_pop] !=0,a_pop],na.rm=T) , "VE_REF"][1]
       xx$VE_REF     <-  relevel(factor(xx$VE_REF), ref=as.character(ref.vid))


       # refactorize
       xx$VE_REF        <- as.factor(xx$VE_REF)
       xx$LE_MET_level6 <- as.factor( xx$LE_MET_level6)


      ## VISUALIZE THE SPATIAL AVAI
      if(FALSE){
       a_xx <- xx[!duplicated(xx$pt_graph),]
       xy <- coord[a_xx$pt_graph, 1:2]
       a_col <- "nb_indiv.5"   # for a given szgroup
       a_sum <- sum(a_xx[,a_col])
       plot(xy[,1], xy[,2], pch=16, cex=sqrt((a_xx[,a_col])/a_sum)*30, xlim=c(10,15), ylim=c(54,56))
       a_xx <- xx[xx$LE_MET_level6=="17",] # color for a given metier to see the spatial coverage...
       xy <- coord[a_xx$pt_graph, 1:2]
       points(xy[,1], xy[,2], pch=16, cex=sqrt((a_xx[,a_col])/a_sum)*30, col=2)
       # CAUTION: it is likely that the spatial avai is lacking of contrast 
         # 1- when the ad hoc inverse-distance used to interpolate is not optimal (taking the "maximum" seems actually perform better)
         # 2- when a given vessel metier occur in a limited spatial coverage where the avai is likely to be almost equal!!
       # => as the point 2 is likely then main part of the effect on the catch rates is actually explained by the vessel-metier combination and not by the avai.....
       }

     
     
      # multiply avai with selectivity ogive for the catch equation model
      # multiply avai with absolute number at szgroup for the catch equation model
      # Log(E(CPUE (s,VES,MET,n))) =  beta(s,VES)+ gamma(s,MET)  + sum(z in szgroups) delta(s,z)×SEL(MET,s,z)  × RAV(n,s,z)     × N(s,z)
      # load
       sel <- read.table(file.path(general$main.path, "metiersspe",
                 paste("metier_selectivity_ogives.dat",sep='')), sep= ' ', header=TRUE)
      
       sel <- cbind.data.frame(sel, szgroup=rep(0:13, length=nrow(sel)))  # 14 szgroups
       sel <- sel[sel$szgroup %in%   selected_szgroups,]
       sel <- cbind.data.frame(sel, szgroup_alias=rep(range_szgroup, length=nrow(sel)))  # caution here: keep the 5 szgroup and relabel as agreed for avai in c++
       for(ii in 1:nrow(xx)){ # brute force (to avoid mistakes...)
          xx[ii, paste("nb_indiv.",range_szgroup, sep='')] <- xx[ii, paste("nb_indiv.",range_szgroup, sep='')] * sel[ sel$met_name==xx[ii, "LE_MET_level6"], "ogive"]                                                          }
       
       
       
        
      ### DO A GLM ###
      # to relate cpue to N (a proxy of N) also accounting for vessel and metier effect.
     
     #library(pscl)
      # glm3  <- zeroinfl(cpue ~ VE_REF + LE_MET_level6   | 1 , data=xx, dist = "negbin")
      # to run on xx with 0s still present!  but optim fails....
 
 
      # start with a poisson glm   
      glm1  <- glm(
                   as.formula(paste("(ceiling(cpue)) ~ VE_REF + LE_MET_level6 +",
                               paste('nb_indiv.',range_szgroup, sep='', collapse="+"), "-1"  )),
                   family=poisson, # offset=log(LE_EFF_VMS),
                    data=xx, x=TRUE 
                   )
      # then do a negative binomial glm because poisson likely to does not properly fit due to overdispersed response   
      library(MASS) # for glm.nb()
       er <- try({glm.nb(
                   as.formula(paste("(ceiling(cpue)) ~ VE_REF + LE_MET_level6 +",
                               paste('nb_indiv.',range_szgroup, sep='', collapse="+"), "-1" )),
                    data=xx, x=TRUE, control=list(epsilon=1e-05, maxit=100, trace=5), init.theta=2, 
                   )  # careful intepretation of the significance needed here because no intercept
      }, silent=TRUE)
      if(class(er)!="try-error"){
      glm2 <- er
      }  else{
      glm2 <- glm1   # but use the poisson one in case the glm.nb does not converge!
      }
        
     
     
     #if(nm2=="NOP.nsea") browser()

     #glm2 <- glm.nb(
     #              as.formula(paste("(ceiling(cpue)) ~ VE_REF -1" )),
     #               data=xx, x=TRUE, control=list(epsilon=1e-05, maxit=100, trace=5), init.theta=2, 
     #              )
                   

       #http://www.unc.edu/courses/2006spring/ecol/145/001/docs/lectures/lecture27.htm
      
      
      #The Poisson regression results indicated that WSSTA has a significant relationship to prevalence. The estimated coefficient of WSSTA is 0.268846. Since our link function is a log, this coefficient reflects changes in the log mean prevalence. The reported model is: log ? = 2.198 + 0.269*WSSTA. To understand the effect of WSSTA on the original scale we need to exponentiate this equation.
      # mu= exp(2.198+0.269*WSSTA)=exp(2.198)exp(0.269*WSSTA)=exp(2.198)exp(0.269)^WSSTA=exp(2.198)*1.308^WSSTA 
      # Thus we see that each one unit increase in WSSTA multiplies the predicted mean prevalence by 1.3

      #Unfortunately we can also see from the output that this model does not fit. 
      #For a Poisson model the residual deviance can be used as a goodness of fit statistic. 
      #The reported residual deviance is 2716.8 on 45 degrees of freedom. For a good-fitting model the ratio of the residual deviance to its degrees of freedom should be approximately 1.
      #Since the ratio here is far greater than 1, we conclude the data are overdispersed. 
      #This is additional evidence that a negative binomial error distribution would be a more appropriate choice here.
      
      
      #Observe that the AIC has decreased tremendously, from 2914.9 in the Poisson model to 413.98 in the negative binomial model. 
      #Interestingly there has been little change in the parameter estimates. 
      # Thus our interpretation of the effect of WSSTA on the mean prevalence is roughly the same as it was for Poisson model.
      
      #=> poisson regression i.e. mixing categorical to continuous variables
                        # e.g. nb_indiv.1 + nb_indiv.2 + nb_indiv.3 are actually proxies for N
       vars <- c("VE_REF","LE_MET_level6",  paste('nb_indiv.',range_szgroup, sep='') )


       ## by hand:
       ##beta <- coefficients(glm2)
       ##beta[is.na(beta)] <- 0
       ##eta <- as.numeric(glm2$x %*% beta )
       ##mu <- exp(eta)
       ##all.equal(mu, predict(glm2, type = "response"))

       #Pearson goodness of fit (should be different from 0 in case the model fits)
       # We test for goodness-of-fit of the model with a chi-square test based on the residual deviance and degrees of freedom.
       # The GOF test indicates that the Poisson model fits the data if p > 0.05.
       res1 <- rbind(res1,c(nm2, nm3[j], pchisq(glm1$deviance, glm1$df.resid, lower=FALSE))) # i.e. 1- pchisq(glm1$deviance, glm1$df.resid, lower=TRUE)
       pearson.nb<-residuals(object = glm2, type="pearson")
       res2 <- rbind(res2, c(stock=nm2,
                             semester=nm3[j], 
                             p_from_deviance_residuals=pchisq(summary(glm2)$deviance,summary(glm2)$df.residual, lower=FALSE),
                             p_from_pearson_residuals  =pchisq(sum(pearson.nb^2),summary(glm2)$df.residual, lower=FALSE) 
                               ))
      #The information on deviance is also provided. We can use the residual deviance to perform a goodness of fit test for the overall model. The residual deviance is the difference between the deviance of the current model and the maximum deviance of the ideal model where the predicted values are identical to the observed. Therefore, if the residual difference is small enough, the goodness of fit test will not be significant, indicating that the model fits the data. We conclude that the model fits reasonably well because the goodness-of-fit chi-squared test is not statistically significant. If the test had been statistically significant, it would indicate that the data do not fit the model well. In that situation, we may try to determine if there are omitted predictor variables, if our linearity assumption holds and/or if there is an issue of over-dispersion. 
      with(glm1, cbind(res.deviance = deviance, df = df.residual, p = pchisq(deviance, 
          df.residual, lower.tail = FALSE)))
       
  
            
   
   # get the decomposition
    pred <- predict(glm2,type="terms") # caution: in terms of cpue => not the betas
    #check exp( 1.411228-0.1094064+1.172626) = response 1
     
    # get back the Beta factors one by one on the right scale => e.g. exp(metier effect)
    ## SHOULD WE KEEP THE SIGNIFICANT ONES ONLY AND THEN PUT THE OTHERS TO 0??
     smry.hat         <- summary(glm2)$coefficients[,"Estimate"]
     smry.hat.se      <- summary(glm2)$coefficients[,"Std. Error"]
      for (a.var in vars){
       idx1 <-  which(regexpr(a.var, names(smry.hat))==1) # idx to keep only the setofvessels estimates
       idx2 <-  which(regexpr(a.var, names(smry.hat.se))==1) # idx to keep only the setofvessels estimates
       ref  <-  paste(a.var, levels(xx[,a.var])[1],sep="")  # get the reference level for this factor
       if(!a.var %in%  paste('nb_indiv.',range_szgroup, sep='')  ) {
           smry.var.hat           <-  c(smry.hat[idx1], 0)
           smry.var.hat.se        <-  c(smry.hat.se[idx2], 0)
           names(smry.var.hat)    <-  c(names(smry.hat)[idx1], ref)
           names(smry.var.hat.se) <-  c(names(smry.hat.se)[idx2], ref)
           beta.                  <-  smry.var.hat
           beta.se.               <-  smry.var.hat.se
           an_order               <-  order( beta. , decreasing=TRUE)
           beta.                  <-  beta. [an_order]
           beta.se.               <-  beta.se. [an_order]
           nms                    <-  names( beta. )
           nms.se                 <-  names( beta.se. )
           beta.                  <-  matrix( beta. )
           beta.se.               <-  matrix( beta.se. )
           rownames( beta. )      <-  sub(a.var, "", nms)
           rownames( beta.se. )   <-  sub(a.var, "", nms.se)
           if(length(beta.)==0) {  beta. <- 0 }
           if(length(beta.se.)==0) {  beta.se. <- 0 }
       } else{
       beta. <- smry.hat[idx1]
       beta.se. <- smry.hat.se[idx2]
       }
       assign( paste("beta.",a.var, sep=''), beta. )
       assign( paste("beta.se.",a.var, sep=''), beta.se. )
      }

  
    # then map on xx and predict response to compare with observed data
    idx                      <- which(!xx$LE_MET_level6 %in%   rownames(beta.LE_MET_level6))
    if(length(idx)>0) xx     <- xx[- idx ,]   ## caution: some levels removed becase singularities
    xx$beta.VE_REF           <- beta.VE_REF[as.character(xx$VE_REF), 1]
    xx$beta.se.VE_REF        <- beta.se.VE_REF[as.character(xx$VE_REF), 1]
    xx$beta.LE_MET_level6    <- beta.LE_MET_level6[as.character(xx$LE_MET_level6), 1]
    xx$beta.se.LE_MET_level6 <- beta.se.LE_MET_level6[as.character(xx$LE_MET_level6), 1]
    ## NO intecept...xx$beta.intercept <- summary(glm1)$coefficients[1]
    for(rg in range_szgroup){
       a.name <- paste('beta.nb_indiv.',rg,sep='')
       if(length(get(a.name))>0) xx[,a.name] <- get(a.name)     else xx[,a.name] <- 0
       a.name <- paste('beta.se.nb_indiv.',rg,sep='')
       if(length(get(a.name))>0) xx[,a.name] <- get(a.name)     else xx[,a.name] <- 0
    }


    xx$response <-  exp(xx$beta.VE_REF*1 +              # canadian paper
                            xx$beta.LE_MET_level6*1 +
                              xx$beta.nb_indiv.0*xx$nb_indiv.0 +
                                xx$beta.nb_indiv.2*xx$nb_indiv.2 +
                                  xx$beta.nb_indiv.3*xx$nb_indiv.3  +
                                   xx$beta.nb_indiv.5*xx$nb_indiv.5  +
                                     xx$beta.nb_indiv.7*xx$nb_indiv.7)
    
    
    
    # should be the same as returned by:
    xx$response1 <- predict(glm1, xx, type="response")   # poisson
    xx$response2 <- predict(glm2, xx, type="response")   # negative binomial
    xx$se2       <- predict(glm2, xx, type="response", se.fit=TRUE)$se.fit   # standart error negative binomial
 #browser()
    

     ## then , fit a normal distrubtion on the residuals....
     ## (should be reused for stochastic catch rate in the c++ code)
    
     #  residus <- round(xx$response1 - xx$cpue)
     #  norm.nll <- function(par,data) -sum(dnorm(data, par[1],par[2],log=T))
     #  opt <- optim(c(1,1), norm.nll,data=residus, method='BFGS')
     #  print(rnorm(100, mean=opt$par[1], sd = opt$par[2]) )
    
 
    
 #     lnorm.nll <- function(par,data) {
 # sum(dlnorm(data, par[1],par[2],log=T))
 #   }
 #       opt <- optim(c(0.1,0.1), lnorm.nll,data=abs(residus)+1,control=list(fnscale=-1),hessian=T)
 #       print(rlnorm(100, mean=opt$par[1], sd = opt$par[2]) )


    # plot
    graphics.off()
    for(met in levels(xx$LE_MET_level6)) {  # plot per metier
    print(met)
       xxx <- xx[xx$LE_MET_level6==met,]   # keep only the start of the curve
       if(nrow(xxx)>0){
          plot(xxx$cpue, xxx$response1, ylim=c(0,15000), xlim=c(0,15000))
          coeffs1 <- coefficients(lm(xxx$response1~xxx$cpue))
          if(!is.na(coeffs1[1]) && !is.na(coeffs1[2]) ) abline(a=coeffs1[1], b=coeffs1[2])
         points(xxx$cpue, xxx$response2, col=2)
         coeffs2 <- coefficients(lm(xxx$response2~xxx$cpue))
         if(!is.na(coeffs2[1]) && !is.na(coeffs2[2]) ) abline(a=coeffs2[1], b=coeffs2[2], col=2)
          abline(a=0, b=1, lty=2)
          title(nm2)
          }
   # browser()
  #anova(glm1,test = "Chisq")
   
    }
 #browser()
    # plot all
   plot(xx$cpue, xx$response1, ylim=range(xx$cpue), xlim=range(xx$cpue))
          coeffs <- coefficients(lm(xx$response1~xx$cpue))
          if(!is.na(coeffs[1]) && !is.na(coeffs[2]) ) abline(a=coeffs[1], b=coeffs[2])
          abline(a=0, b=1, lty=2)
          title(nm2)

    #=> in case of a perfect model, the two lines should overlap...
    # save the plot
    savePlot(filename=file.path(general$main.path,"jpeg_glm",
       paste("cpue_vs_response_from_glm_on_",nm2,"_semester", nm3[j],
         "_",years,"_",method, "_", threshold, ".jpeg",sep='')), type="jpeg")


    # The betas coeffs should then be used in the IBM catch equation
    # i.e. catch= exp(beta1*vid + beta2*metier +beta3*szgroup2*1000 + beta4*szgroup3*1000 + beta5*szgroup4*1000)
    # neglecting the intercept (which should be 0 in the ideal case anyway)
    # Would be ideal to also draw the predictions in the c++ code using the confidence intervals around the estimates from mean+/-2*beta.se.
    betas.vid            <- xx[!duplicated(data.frame(xx$VE_REF, xx$beta.VE_REF)), c("VE_REF", "beta.VE_REF")]
    betas.vid            <- cbind(betas.vid,  pop=nm2, semester=nm3[j] ) # add the pop name + semester
    all.betas.vid        <- rbind(all.betas.vid, betas.vid)
    all.betas.vid[,2]    <- round(all.betas.vid[,2], 5)
    betas.vid.se         <- xx[!duplicated(data.frame(xx$VE_REF, xx$beta.se.VE_REF)), c("VE_REF", "beta.se.VE_REF")]
    betas.vid.se         <- cbind(betas.vid.se,  pop=nm2, semester=nm3[j] ) # add the pop name + semester
    all.betas.vid.se     <- rbind(all.betas.vid.se, betas.vid.se)
    all.betas.vid.se[,2] <- round(all.betas.vid.se[,2], 5)
    betas.met            <- xx[!duplicated(data.frame(xx$LE_MET_level6, xx$beta.LE_MET_level6)), c("LE_MET_level6", "beta.LE_MET_level6")]
    betas.met            <- cbind(betas.met,  pop=nm2, semester=nm3[j] ) # add the pop name + semester
    all.betas.met        <- rbind(all.betas.met, betas.met)
    all.betas.met[,2]    <- round(all.betas.met[,2], 5)
    betas.met.se         <- xx[!duplicated(data.frame(xx$LE_MET_level6, xx$beta.se.LE_MET_level6)), c("LE_MET_level6", "beta.se.LE_MET_level6")]
    betas.met.se         <- cbind(betas.met.se,  pop=nm2, semester=nm3[j] ) # add the pop name + semester
    all.betas.met.se     <- rbind(all.betas.met.se, betas.met.se)
    all.betas.met.se[,2] <- round(all.betas.met.se[,2], 5)

    # particular cases because continuous variables...
    for (rg in range_szgroup){
      # mean estimate
      betas         <- data.frame(beta.nb_indiv=xx[1, paste('beta.nb_indiv.',rg,sep='')], pop=nm2, semester= nm3[j] ) # add the pop name  + semester
      all.betas     <- get(paste('all.betas',rg,sep=''))
      all.betas     <- rbind(all.betas, betas)
      all.betas[,1] <- round(all.betas[,1], 5)
      assign (paste('all.betas',rg,sep=''), all.betas) # send back...
      # se
      betas.se         <- data.frame(beta.nb_indiv=xx[1, paste('beta.se.nb_indiv.',rg,sep='')], pop=nm2, semester= nm3[j] ) # add the pop name  + semester
      all.betas.se     <- get(paste('all.betas.se',rg,sep=''))
      all.betas.se     <- rbind(all.betas.se, betas.se)
      all.betas.se[,1] <- round(all.betas.se[,1], 5)
      assign (paste('all.betas.se',rg,sep=''), all.betas.se) # send back...
      }
      
    } else{ cat("this stock has been filtered out...\n")}
    } else{ cat("this stock has been filtered out...\n")}
    } else{ cat("this stock is irrelevant here...\n")}
    } else{ cat("this stock is not found in the merged table...\n")}
    }
  
  } # end for loop
   #----------------------------------------------

   # the goodness of fit table 
   res2 <- as.data.frame(res2)
   res2 <- cbind.data.frame(res2, "significance (p>0.05)"=NA)
   res2 <- cbind.data.frame(res2, "significance (p>0.05)"=NA)
   res2 <- cbind.data.frame(res2, "significance (p>0.05)"=NA)
   res2 <- cbind.data.frame(res2, "p"=NA)
   res2[as.numeric(as.character(res2[,3])) >0.05, 5] <- "signif."
   res2[as.numeric(as.character(res2[,4])) >0.05, 6] <- "signif."
   res2[ !(is.na(res2[,5]) & is.na(res2[,6]) ) , 7] <- "s."
   res2[ is.na(res2[,7]) , 7] <- "n.s."
   res2[  , 8] <- round( as.numeric( apply(res2, 1, function (x) { max(c(as.numeric(as.character(x[3])), as.numeric(as.character(x[4]))), na.rm=TRUE)}) ) , 3)
 

    # subset for relevant populations
   pop.to.keep <- c(   "COD.nsea",  "COD.kat",    "COD.2224", "COD.2532",
                       "HER.nsea",      "HER.3a22",           "HER.2532",
                       "SPR.nsea",   "SPR.kask",                          "SPR.2232",
                       "NEP.nsea",   "NEP.kask",
                       "SOL.nsea",   "SOL.kask",  "SOL.2224",
                                     "LEM.kask",
                       "PLE.nsea",   "PLE.kask",                          "PLE.2232",
                                                                          "FLE.2232",
                       "HAD.nsea",  
                       "HOM.nsea", 
                       "MON.nsea", 
                       "NOP.nsea",
                       "HKE.nsea",
                       "DAB.nsea",                                        "DAB.2232",
                                                                          "TUR.2232", 
                       "CSH.nsea",
                       "PRA.nsea",   "PRA.kask",
                       "SAN.nsea",   
                       "POK.nsea",
                       "MAC.nsea",
                                     "WHG.kask",  "WHG.2224",
                                     "MUS.kask",
                                                  "MUS.2224"
                      )

    
    #other.pops <- pop.to.keep[!pop.to.keep%in%all.betas.vid$pop]
    
    all.betas.vid     <- all.betas.vid[all.betas.vid$pop %in%  pop.to.keep,]
    all.betas.met     <- all.betas.met[all.betas.met$pop %in%  pop.to.keep,]
    all.betas.vid$pop <- factor(all.betas.vid$pop)
    all.betas.met$pop <- factor(all.betas.met$pop)
    all.betas.vid.se     <- all.betas.vid.se[all.betas.vid.se$pop %in%  pop.to.keep,]
    all.betas.met.se     <- all.betas.met.se[all.betas.met.se$pop %in%  pop.to.keep,]
    all.betas.vid.se$pop <- factor(all.betas.vid.se$pop)
    all.betas.met.se$pop <- factor(all.betas.met.se$pop)
    
    #replace NA by 0
    all.betas.vid       <- replace(all.betas.vid, is.na(all.betas.vid), 0)
    all.betas.met       <- replace(all.betas.met, is.na(all.betas.met), 0)
    all.betas.vid.se    <- replace(all.betas.vid.se, is.na(all.betas.vid.se), 0)
    all.betas.met.se    <- replace(all.betas.met.se, is.na(all.betas.met.se), 0)
    ## merge with all combi to get all pop informed even if no betas...(required fro Cpp)
    all.combi           <- expand.grid(VE_REF=levels(factor(all.betas.vid$VE_REF)),
                                pop=levels(factor(all.betas.vid$pop)),
                                 semester=levels(factor(all.betas.vid$semester)))
    all.betas.vid       <- merge(all.combi, all.betas.vid, all=TRUE)
    all.betas.vid.se    <- merge(all.combi, all.betas.vid.se, all=TRUE)
    all.combi           <- expand.grid(LE_MET_level6=levels(factor(all.betas.met$LE_MET_level6)),
                                pop=levels(factor(all.betas.met$pop)),
                                  semester=levels(factor(all.betas.met$semester)))
    all.betas.met       <- merge(all.combi, all.betas.met, all=TRUE)
    all.betas.met.se    <- merge(all.combi, all.betas.met.se, all=TRUE)
    # order to get some multimap for Cpp
    #   i.e. vessel/beta per pop i.e metier/beta per pop
    #   i.e. avai_szgroup2/beta per pop, avai_szgroup3/beta per pop, avai_szgroup4/beta per pop
    library(doBy)
    all.betas.vid       <- orderBy(~semester+VE_REF+pop, data=all.betas.vid)
    all.betas.vid.se    <- orderBy(~semester+VE_REF+pop, data=all.betas.vid.se)
    all.betas.met       <- orderBy(~semester+LE_MET_level6+pop, data=all.betas.met)
    all.betas.met.se    <- orderBy(~semester+LE_MET_level6+pop, data=all.betas.met.se)

    # ...do the same for the continous variables
    for (rg in range_szgroup){
      # mean estimate
      all.betas         <- get(paste('all.betas',rg,sep=''))
      all.betas         <- all.betas[all.betas$pop %in%  pop.to.keep,]
      all.betas         <- replace(all.betas, is.na(all.betas), 0)
      all.betas$pop     <- factor(all.betas$pop)
      all.betas         <- orderBy(~semester+pop, data=all.betas)
     assign (paste('all.betas',rg,sep=''), all.betas) # send back...
      # se
      all.betas.se      <- get(paste('all.betas.se',rg,sep=''))
      all.betas.se      <- all.betas.se[all.betas.se$pop %in%  pop.to.keep,]
      all.betas.se      <- replace(all.betas.se, is.na(all.betas.se), 0)
      all.betas.se$pop  <- factor(all.betas.se$pop)
      all.betas.se      <- orderBy(~semester+pop, data=all.betas.se)
     assign (paste('all.betas.se',rg,sep=''), all.betas.se) # send back...
    }

    ## convert string name into integer name to speed up the c++
    # mean estimates
    all.betas.vid$pop <- factor(all.betas.vid$pop) 
    all.betas.met$pop <- factor(all.betas.met$pop) 
    for (obj_name in paste('all.betas', range_szgroup,sep='')){obj <- get(obj_name); obj$pop <- factor(obj$pop); assign (obj_name, obj, .GlobalEnv)}
    pop_names         <- cbind(levels(all.betas.vid$pop), 0:(length(levels(all.betas.vid$pop))-1))
    # se
    all.betas.vid.se$pop <- factor(all.betas.vid.se$pop) 
    all.betas.met.se$pop <- factor(all.betas.met.se$pop) 
    for (obj_name in paste('all.betas.se', range_szgroup,sep='')){obj <- get(obj_name); obj$pop <- factor(obj$pop); assign (obj_name, obj, .GlobalEnv)}
    if(FRANCOIS ){  #!#!#! CAUTION   #!#!#!
       write.table(pop_names, file.path(general$main.path,"popsspe",paste("pop_names_",general$case_study,".txt",sep="")), quote=FALSE, col.names=FALSE, row.names=FALSE)
       # cution: name of species can change across the different case studies
       # because some species can be filtered out...e.g. less than 10 tons, etc.
       }
    levels(all.betas.vid$pop)  <-  0:(length(levels(all.betas.vid$pop))-1) # change level names
    levels(all.betas.met$pop)  <-  0:(length(levels(all.betas.vid$pop))-1) # change level names
    levels(all.betas.vid.se$pop)  <-  0:(length(levels(all.betas.vid.se$pop))-1) # change level names
    levels(all.betas.met.se$pop)  <-  0:(length(levels(all.betas.vid.se$pop))-1) # change level names
    for (obj_name in paste('all.betas', range_szgroup,sep='')){obj <- get(obj_name); levels(obj$pop)   <-  0:(length(levels(obj$pop))-1); assign (obj_name, obj, .GlobalEnv)} # change level names
    for (obj_name in paste('all.betas.se', range_szgroup,sep='')){obj <- get(obj_name); levels(obj$pop)   <-  0:(length(levels(obj$pop))-1); assign (obj_name, obj, .GlobalEnv)} # change level names
    
    
    ## check that we do not have lost metiers along the process....
    ## if yes, then add the metier with NA (..should have a very little impact) 
    dd<- combined_met_names[!combined_met_names$idx %in%  unique(as.character(all.betas.met$LE_MET_level6)),]
    if(nrow(dd)!=0){
    cat(paste('lost metier: ',dd$idx,  "\n", sep=''))
    all.betas.met$LE_MET_level6  <- as.character(all.betas.met$LE_MET_level6)
    all.betas.met                <- rbind.data.frame(all.betas.met, expand.grid(LE_MET_level6=dd$idx, pop=0: (length(unique(all.betas.met$pop))-1), semester=1:2, beta.LE_MET_level6=NA ))
    all.betas.met$LE_MET_level6  <- as.factor(all.betas.met$LE_MET_level6)
    all.betas.met.se$LE_MET_level6  <- as.character(all.betas.met.se$LE_MET_level6)
    all.betas.met.se                <- rbind.data.frame(all.betas.met.se, expand.grid(LE_MET_level6=dd$idx, pop=0: (length(unique(all.betas.met$pop))-1), semester=1:2, beta.se.LE_MET_level6=NA ))
    all.betas.met.se$LE_MET_level6  <- as.factor(all.betas.met.se$LE_MET_level6)
    }

    ## check that we do not have lost some vessels along the process....
    dd <- x.agg[!x.agg$VE_REF %in%  unique(as.character(all.betas.vid$VE_REF)),]
    if(nrow(dd)!=0){
     cat("CHECK LOST VESSELS!")
     all.betas.vid$VE_REF  <- as.character(all.betas.vid$VE_REF)   
     all.betas.vid         <- rbind.data.frame(all.betas.vid, expand.grid(VE_REF=dd$VE_REF, pop=0: (length(unique(all.betas.met$pop))-1), semester=1:2, beta.VE_REF=NA ))
     all.betas.vid$VE_REF  <- as.factor(all.betas.vid$VE_REF)   
     }
     
    # order
    all.betas.vid     <- orderBy(~semester+VE_REF+pop, data=all.betas.vid)
    all.betas.met     <- orderBy(~semester+LE_MET_level6+pop, data=all.betas.met)
    all.betas.vid.se     <- orderBy(~semester+VE_REF+pop, data=all.betas.vid.se)
    all.betas.met.se     <- orderBy(~semester+LE_MET_level6+pop, data=all.betas.met.se)

    
    # save and export temporarly 
    save(list=c('all.betas.vid','all.betas.met', paste('all.betas', range_szgroup,sep=''),
                'all.betas.vid.se','all.betas.met.se', paste('all.betas.se', range_szgroup,sep='')),
              file=file.path(general$main.path, "popsspe", paste("betas", "_DEN_DEU_SWE","_INTEGER_",
                                  general$case_study ,".RData", sep='')))

      
    # the goodness of fit table
    res2 <- res2[res2[,1]%in%pop.to.keep,]
    write.table(res2,
                   file=file.path(general$main.path, "popsspe",
                       paste("goodness_of_fit_table.csv",sep=';')),
                         quote = FALSE, sep=" ", col.names=FALSE, row.names=FALSE)
      
    # the selected groups in the catch rate equation
    the_selected_szgroups       <- the_selected_szgroups[the_selected_szgroups[,1]%in%pop.to.keep,]
    pop_names                   <- read.table(file.path(general$main.path, "popsspe",paste("pop_names_",general$case_study,".txt", sep='')))  ## CAUTION: circularity for pop_names 
    #pop_names                   <- read.table(file.path("C:","Users","fba","Dropbox","ibm","Cpp","ibm_vessels", "popsspe_final",paste("pop_names_",general$case_study,".txt", sep='')))  ## CAUTION: circularity for pop_names 
    the_selected_szgroups$nm2   <- factor(the_selected_szgroups$nm2)
    levels(the_selected_szgroups$nm2)  <- pop_names[,2][ match(levels(the_selected_szgroups$nm2), as.character(pop_names[,1]))] # map the name to integer

    write.table(the_selected_szgroups,
                   file=file.path(general$main.path,"popsspe",
                       paste("the_selected_szgroups.dat",sep=' ')),
                         quote = FALSE, sep=" ", col.names=TRUE, row.names=FALSE)
   

     # then, export PER SEMESTER:
    for(semester in c(1,2)){

      ## VESSEL SPE----------
      # export betas specific to the vessel given this pop
      # mean estimates
      all.betas.vid <-  replace(all.betas.vid, is.na(all.betas.vid), 0)
      all.betas.vid <-  all.betas.vid[order(all.betas.vid$VE_REF,all.betas.vid$semester, all.betas.vid$pop),] # order
          write.table(all.betas.vid[all.betas.vid$semester==semester, c('VE_REF','beta.VE_REF')],
                   file=file.path(general$main.path,"vesselsspe",
                       paste("vesselsspe_betas_semester",semester,".dat",sep='')),
                         quote = FALSE, sep=" ", col.names=TRUE, row.names=FALSE)
      # s.e.
      all.betas.vid.se <-  replace(all.betas.vid.se, is.na(all.betas.vid.se), 0)
      all.betas.vid.se <-  all.betas.vid.se[order(all.betas.vid.se$VE_REF,all.betas.vid.se$semester, all.betas.vid.se$pop),] # order
          write.table(all.betas.vid.se[all.betas.vid.se$semester==semester, c('VE_REF','beta.se.VE_REF')],
                   file=file.path(general$main.path,"vesselsspe",
                       paste("vesselsspe_betas_se_semester",semester,".dat",sep='')),
                         quote = FALSE, sep=" ", col.names=TRUE, row.names=FALSE)
   
      
    
      
      
      
      ## METIER SPE----------
      # export betas specific to the metier given this pop
      # mean estimates
      all.betas.met <- replace(all.betas.met, is.na(all.betas.met), -20)
        #  better to replace NA by an arbitrary value (eg -20 because exp(-20) close to 0)
        # so that no catch when the metier is with NA cpue (because no observed landings for this metier this species)
        # otherwise: risk of unrealistic cacth when eg the beta_vid is very high for certain vessel...eg MAC.nsea and DNK000012028
        write.table(all.betas.met[all.betas.met$semester==semester, c('LE_MET_level6','beta.LE_MET_level6')],
                    file=file.path(general$main.path, "metiersspe",
                      paste("metierspe_betas_semester",semester,".dat",sep='')),
                         quote = FALSE, sep=" ", col.names=TRUE, row.names=FALSE)
      # s.e.
      all.betas.met.se <- replace(all.betas.met.se, is.na(all.betas.met.se), 0)
        write.table(all.betas.met.se[all.betas.met.se$semester==semester, c('LE_MET_level6','beta.se.LE_MET_level6')],
                    file=file.path(general$main.path, "metiersspe",
                      paste("metierspe_betas_se_semester",semester,".dat",sep='')),
                         quote = FALSE, sep=" ", col.names=TRUE, row.names=FALSE)
      
     
      ## POP SPE----------
      for(rg in range_szgroup){
        # export betas specific to the avai szgroup given this pop (caution: remenber the scaling i.e *1000)
        # mean estimates
        all.betas <- get(paste('all.betas',rg,sep='')) 
        all.betas <- replace(all.betas, is.na(all.betas), 0)
           write.table(all.betas[all.betas$semester==semester, c('pop','beta.nb_indiv')],
                    file=file.path(general$main.path, "popsspe",
                       paste("avai",rg,"_betas_semester",semester,".dat",sep='')),
                         quote = FALSE, sep=" ", col.names=TRUE, row.names=FALSE)
        # s.e.
        all.betas.se <- get(paste('all.betas.se',rg,sep='')) 
        all.betas.se <- replace(all.betas.se, is.na(all.betas.se), 0)
           write.table(all.betas.se[all.betas.se$semester==semester, c('pop','beta.nb_indiv')],
                    file=file.path(general$main.path, "popsspe",
                       paste("avai",rg,"_betas_se_semester",semester,".dat",sep='')),
                         quote = FALSE, sep=" ", col.names=TRUE, row.names=FALSE)
       }
      }
    
    
} # end FRANCOIS    


 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!GET THE FILES FOR AVAI ON NODE        !!!!!!!!!!!!!!!!!!!!##
 ##!!!! e.g. pop0spe_avai_szgroup_nodes_semester1.dat !!!!!!!!!!##
 ##!!!!!and list of node with presence           !!!!!!!!!!!!!!!##
 ##!!!!!!!!!lst_idx_nodes_per_pop.dat     !!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

## POP SPE
if(FRANCOIS){
  
  if(general$case_study=="canadian_paper"){
      year      <- "2010" 
      years     <- "2005_2010" 
      method    <- "maximum"
      threshold <- "50"
      }
   if(general$case_study=="baltic_only"){
      year      <- "2012" 
      years     <- "2008_2012" 
      method    <- "inverse"
      threshold <- "25"
      }


   
 # load avai built using get_spatial_avai_keys_on_igraph_nodes_from_surveys_per_size_group.r
  load(file.path(general$main.path, "avai",
        paste("lst_avai_igraph",general$igraph,"_",years,"_",method,"_",threshold,".RData",sep=""))) ##!! 2012 !!##
                       # HERE WE use lst_avai_2008_2012.RData instead of 2012 only.

 # load outputs from glm  to get the pop to keep
 load(file.path(general$main.path, "popsspe", paste("betas_DEN_DEU_SWE_INTEGER_",general$case_study,".RData", sep='')) )
 pops      <- as.character(unique(all.betas0$pop))
 pop_names <- read.table(file.path(general$main.path, "popsspe",paste("pop_names_",general$case_study,".txt", sep='')))

 # load shape file
 library(sp)
 library(maptools)
 sh1 <- readShapePoly(file.path(general$main.path,"shp","francois_EU"))

 
 #-----------
 call_make_avai_files <- 
    function (lst.avai, add_stochastic_variation=FALSE, num="", general=general){

  # save in .dat files
 idx_node_semester1 <- NULL
 idx_node_semester2 <- NULL
 for (sp in 1: length(lst.avai)){
  nm <- names(lst.avai)
  if(nm[sp] %in% pop_names[,1]) for (j in 1: length(lst.avai[[sp]])){
    avai <- lst.avai[[sp]][[j]]
    nm2 <- names(lst.avai[[sp]])
    options(scipen=999) # suppress the scientific notation
    avai         <- signif(avai,6)
    avai         <- replace(avai, is.na(avai),0)
    avai[,4]     <- avai[,4] -1 ## CAUTION for Cpp: idx_node must start at 0
    idx_with_0s  <- which (apply(avai[,-c(1:4)], 1, sum)==0)
    # plot
    for(szgroup in 0:7){
        if(any(  avai[,grep(paste('nb_indiv.',szgroup,sep=''), colnames(avai))[1]  ] !=0 )){
          
          if(add_stochastic_variation==FALSE){ 
           library(PBSmapping)  # addBubbles()
           plot(avai[,1],avai[,2], xlab="Longitude", ylab="Latitude", col=2, type="n")
           addBubbles(data.frame(EID=1:nrow(avai), X=avai[,1], Y=avai[,2], Z=avai[,grep(paste('nb_indiv.',szgroup,sep=''), colnames(avai))[1] ]),
                   legend.pos="bottomleft", legend.type=c("nested"), 
                        legend.title= paste(nm[sp], " availability", szgroup, " semest.", j), legend.cex=0.8, symbol.zero="+")
          plot(sh1, add=TRUE)
          savePlot(file.path(general$main.path,"popsspe", "jpeg_avai",
                paste("presence_area_",nm[sp],"_sz",szgroup, " semest.", j,".jpeg",sep='')), type="jpeg")
           } else{
             ## add a multivariate lognormal error on availability key
             ## to enjoy a stcohastic variation in spatial distribution.
             ## call it several time and store stochastic output files somewhere so that we do not have to bother
             ## doing it in c++ i.e. we´ll just pick up a file randomly from c++ afterward
             avai_per_szgroup_per_quarter <-  avai[,grep(paste('nb_indiv.',szgroup,sep=''), colnames(avai))[1]]

             #note that the stochastic variation can only add on node where avai!=0
             # i.e. cannot create some new zones for the distrubtion of the stocks!!
             # more elaborated process needed if required e.g. erosion/dilatation knowing neighbouring nodes, etc. 
             # eg add a fake epsilon to enable avai on 0 nodes as well,
             # but only those node that are on the frontiers of the current distrib:
             # those can be identified using the "graph" object i.e. all the links with avai!=0 in dep and avai==0 in arr  (dilation order 1)
             
             for(i in 1: nrow(avai)) {points(as.numeric(as.character(avai[i,"x"])), as.numeric(as.character(avai[i,"y"])), col=2) ; for(j in 1: 1000000) {} }
             
             library(compositions)
             corr <- diag(length(avai_per_szgroup_per_quarter))
             corr[lower.tri(corr)] <- 1 # i.e. correlated
             corr[upper.tri(corr)] <- 1  # i.e. correlated
             diag(corr ) <-1.2 # corr*sd    ## HERE WE ARE.....
             stochast_avai <- as.vector(rlnorm.rplus(1,log(avai_per_szgroup_per_quarter),corr)  )
             stochast_avai <-  stochast_avai/sum(stochast_avai) # rescale the avai key to 1
             # a plot to check the magnitude of the change: 
             # drawns <- rlnorm.rplus(1,log(avai_per_szgroup_per_quarter),corr)
             #plot(avai_per_szgroup_per_quarter, as.vector(drawns)/  sum(as.vector(drawns)) )  
             #abline(a=0,b=1 )  
             
  
             # then, replace back:
             avai[,grep(paste('nb_indiv.',szgroup,sep=''), colnames(avai))] <- stochast_avai 
           
           }
         }
    }
    # then remove node if pop not present...
    if(length(idx_with_0s)!=0) avai         <- avai[-idx_with_0s, ]  # remove null avai
    
    # brute force to fill in the 0s i.e. for the size groups where the avai is not informed
    sp_name                                <- strsplit(nm[sp], split="\\.")[[1]][1]
    the_avai                               <-  avai[,-c(1:4)]
    szgroups_with_all_0_avai               <-  which(apply(the_avai, 2, sum)==0)-1 
    szgroups_with_some_avai                <-  which(apply(the_avai, 2, sum)!=0)-1
    closer_szgroup                         <-  sapply(szgroups_with_all_0_avai, function (x) {  which.min(abs(x-szgroups_with_some_avai))    })
    if(length(szgroups_with_all_0_avai)!=0)
       the_avai[, szgroups_with_all_0_avai+1] <-  the_avai[, paste(sp_name,".nb_indiv.",closer_szgroup, sep='') ]
   
    # then, reshape for c++
    full_avai2               <- cbind(rep(as.numeric(as.character(avai[,4])), each=14), c(t(the_avai)))   # CAUTION 14 szgroup
  
  
   
  
  
    colnames(full_avai2) <- c("idx_node","avai")
    if(add_stochastic_variation==TRUE){
       is_stoch <- paste("_",as.character(num),sep='') ; stoch_folder <- "stochast_avai"
    } else{
       is_stoch <- ""; stoch_folder <- "static_avai"
       }
    write.table(full_avai2, file = file.path(general$main.path,"popsspe", stoch_folder,
                            paste(pop_names[ pop_names[,1]==nm[sp], 2], ## caution: convert string name into integer
                              "spe_full_avai_szgroup_nodes_semester",nm2[j],is_stoch,".dat",sep="")),
                               row.names = FALSE, col.names = TRUE, quote=FALSE )
                               ##=> pop0spe_full_avai_szgroup_nodes_semester1 => a c++ multimap

    ## change 6sep11: for speedup c++: get rid of the szgroups that are actually not used for the do_catch caluclation
    ## so that the multimap for availability will be smaller!
    ## cf. also range_szgroup for the glm poisson model
    
    the_selected_szgroups  <- read.table(file=file.path(general$main.path,"popsspe", paste("the_selected_szgroups.dat",sep=' ')), header=TRUE)
    idx                    <- the_selected_szgroups [ the_selected_szgroups[,1]== nm[sp] , "selected_szgroups"]
    if(length(idx)==0) idx <- c(0,2,3,5,7) # default
 
    # select the 5 relevant szgroup for this stock  (to feed the catch rate equation only)
    sp_name      <- strsplit(nm[sp], split="\\.")[[1]][1]
    avai2        <- cbind(rep(as.numeric(as.character(avai[,4])), each=5), c(t(avai[, paste(sp_name,".nb_indiv.", idx, sep="") ])))   # CAUTION 5 szgroups ONLY i.e. coded 0,2,3,5,7 in c++ but note that could correpond to other sz according to the_selected_szgroups table!!!
  
    
    colnames(avai2) <- c("idx_node","avai")
    write.table(avai2, file = file.path(general$main.path,"popsspe",  stoch_folder,
                            paste(pop_names[ pop_names[,1]==nm[sp], 2], ## caution: convert string name into integer
                              "spe_avai_szgroup_nodes_semester",nm2[j],is_stoch,".dat",sep="")),
                               row.names = FALSE, col.names = TRUE, quote=FALSE )
                               ##=> pop0spe_avai_szgroup_nodes_semester1 => a c++ multimap

  
    
    # inform list of presence nodes
    assign( paste("idx_node_semester",nm2[j],sep=''),
               rbind(get(paste("idx_node_semester",nm2[j],sep='')), cbind(pop_names[ pop_names[,1]==nm[sp], 2], avai[,4]))
        ) # bind each time PER SEMESTER
 
  
 }}

   # export lst_idx_nodes_per_pop_quarterXX.dat
  for(se in c('semester1', 'semester2') ){   
     idx_node <- get(paste("idx_node_",se,sep=''))
     colnames(idx_node) <- c("stock", "idx_node")
     write.table(idx_node, file = file.path(general$main.path,"popsspe", stoch_folder,
                            paste("lst_idx_nodes_per_pop_",se,is_stoch,".dat",sep='')),
                               row.names = FALSE, col.names = TRUE, quote=FALSE )
     }
     
     
  return()
  }
  
  # calls------
   # baseline
   cat("if it still doesn't exist, 'static_avai' folder is created in ",
                      file.path(general$main.path,"popsspe","\n"))
   dir.create(file.path(general$main.path,"popsspe","static_avai"), 
                      showWarnings = TRUE, recursive = TRUE, mode = "0777") 
   call_make_avai_files(lst.avai, add_stochastic_variation=FALSE, num="", general=general)
  
   # add a multivariate lognormal error
   cat("if it still doesn't exist, 'stochast_avai' folder is created in ",
                      file.path(general$main.path,"popsspe","\n"))
   dir.create(file.path(general$main.path,"popsspe","stochast_avai"), 
                      showWarnings = TRUE, recursive = TRUE, mode = "0777")
   n<-50
   increment <- sprintf("%03d", 1:n) 
   for(i in 1:n)  call_make_avai_files(lst.avai, add_stochastic_variation=TRUE, num=increment[i], general=general) 

 
 }  # end if FRANCOIS
 
 
 
  
 
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!LOAD & COMPUTE FOR OBTAINING!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## vesselsspe_fgrounds_quarter[xx].dat          !!!!!!!!!!!!!!!##
 ## vesselsspe_freq_fgrounds_quarter[xx].dat    !!!!!!!!!!!!!!!!##
 ## vesselsspe_harbours_quarter[xx].dat         !!!!!!!!!!!!!!!!##
 ## vesselsspe_freq_harbours_quarter[xx].dat     !!!!!!!!!!!!!!!##
 ## DNK00[xx]_possible_metiers_quarter2.dat     !!!!!!!!!!!!!!!!##
 ## DNK00[xx]_freq_possible_metiers_quarter[xx].dat !!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##


 # load the graph
 load(file.path(general$main.path, "igraph", paste(general$igraph,  "_graphibm.RData",sep=''))) # built from the R code
 coord            <-  cbind(coord, idx=1:nrow(coord))  ## CAUTION: start at 1
 coord.fgrounds   <-  coord[coord[,'idx.port']==0,]
 coord.harbours   <-  coord[coord[,'idx.port']!=0,]



 # load the combined graph with the "merged" table for DNK
 load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.fgrounds.","DEN",".",general$a.year,".igraph",general$igraph,".RData",sep='')))
 ping.fgrounds.den  <- ping.fgrounds
 load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.harbours.","DEN",".",general$a.year,".igraph",general$igraph,".RData",sep='')))
 ping.harbours.den  <- ping.harbours
 

 if("DEN" %in% general$case_study_countries &&
      "DEU" %in% general$case_study_countries &&
       "SWE" %in% general$case_study_countries ){

 # load the combined graph with the "merged" table for DEU
 load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.fgrounds.","DEU",".",general$a.year,".igraph",general$igraph,".RData",sep='')))
 ping.fgrounds.deu  <- ping.fgrounds[, !colnames(ping.fgrounds) %in% c("QUARTER")] 
 load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.harbours.","DEU",".",general$a.year,".igraph",general$igraph,".RData",sep='')))
 ping.harbours.deu  <- ping.harbours
 ping.harbours.deu <- ping.harbours[, !colnames(ping.harbours) %in% c("QUARTER")] 


  # load the combined graph with the "merged" table for SWE
 load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.fgrounds.","SWE",".",general$a.year,".igraph",general$igraph,".RData",sep='')))
 ping.fgrounds.swe  <- ping.fgrounds[, !colnames(ping.fgrounds) %in% c("QUARTER")] 
 load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.harbours.","SWE",".",general$a.year,".igraph",general$igraph,".RData",sep='')))
 ping.harbours.swe  <- ping.harbours
 ping.harbours.swe <- ping.harbours[, !colnames(ping.harbours) %in% c("QUARTER")] 

 # small adjustement for SWE data
 ping.fgrounds.swe$VE_FLT   <- NA
 ping.fgrounds.swe$SI_SP    <- NA
 ping.fgrounds.swe$SI_HE    <- NA
 ping.fgrounds.swe$KW_HOURS <- NA
 ping.harbours.swe$VE_FLT   <- NA
 ping.harbours.swe$SI_SP    <- NA
 ping.harbours.swe$SI_HE    <- NA
 ping.harbours.swe$KW_HOURS <- NA
 ping.harbours.swe$KW_HOURS <- NA
 

  # combined DNK and GER (removing useless info on kg landing)
  ping.fgrounds <- rbind(
                        ping.fgrounds.den[,-grep("KG", colnames(ping.fgrounds.den))],
                         ping.fgrounds.deu[,-grep("KG", colnames(ping.fgrounds.deu))], 
                          ping.fgrounds.swe[, colnames(ping.fgrounds.den[,-grep("KG", colnames(ping.fgrounds.den))])] 
                         )
  # keep only DNK and DEU, and SWE vessels
  ping.fgrounds <- ping.fgrounds[c(grep("DNK", ping.fgrounds$VE_REF), grep("DEU", ping.fgrounds$VE_REF), grep("SWE", ping.fgrounds$VE_REF)),]

  # combined DNK, DEU, and SWE (removing useless info on kg landing)
  ping.harbours <- rbind(
                        ping.harbours.den[,-grep("KG", colnames(ping.harbours.den))],
                         ping.harbours.deu[,-grep("KG", colnames(ping.harbours.deu))], 
                          ping.harbours.swe[,colnames(ping.harbours.den[,-grep("KG", colnames(ping.harbours.den))])]
                         )
  # keep only DNK and DEU, and SWE vessels
  ping.harbours <- ping.harbours[c(grep("DNK", ping.harbours$VE_REF), grep("DEU", ping.harbours$VE_REF), grep("SWE", ping.harbours$VE_REF)),]


  # save the combined datasets
  save(ping.fgrounds,  file=file.path(general$main.path, "merged_tables", general$case_study,
             paste("ping.fgrounds.","DEN.DEU.SWE",".",general$a.year,".igraph",general$igraph,".RData", sep='')))
      cat(paste("save the combined ping.fgrounds', this year...OK\n\n",sep=""))
  save(ping.harbours,  file=file.path(general$main.path, "merged_tables", general$case_study,
             paste("ping.harbours.","DEN.DEU.SWE",".",general$a.year,".igraph",general$igraph,".RData", sep='')))
     cat(paste("save the combined 'ping.harbours', this year...OK\n\n",sep=""))
 
 } else{ # end combine countries
 ping.fgrounds <- ping.fgrounds.dnk
 ping.harbours <- ping.harbours.dnk
 }
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!AGGREGATE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
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



 # add quarters, and then semesters
 ping.fgrounds$quarter          <- quarters(as.POSIXct(ping.fgrounds$SI_DATE, tz="GMT"))
 ping.fgrounds$semester         <- factor(ping.fgrounds$quarter)
 levels(ping.fgrounds$semester) <- c(1,1,2,2)

 # remove the No_Matrix6 metier...
 ping.fgrounds               <- ping.fgrounds[ping.fgrounds$LE_MET_level6!= "No_Matrix6",]
 ping.fgrounds               <- ping.fgrounds[ping.fgrounds$LE_MET_level6!= "NA",]
 ping.fgrounds$LE_MET_level6 <- factor(ping.fgrounds$LE_MET_level6)
 #...and slightly modify the name of metiers
 #levels(ping.fgrounds$LE_MET_level6) <-
 #   paste("met", 0:(length(levels(ping.fgrounds$LE_MET_level6))-1), "_", levels(ping.fgrounds$LE_MET_level6), "_",general$a.country, sep='')
 # NEED JUST INTEGERS!, replaced by:
  ping.fgrounds$LE_MET_char          <- ping.fgrounds$LE_MET_level6
  load(file.path(general$main.path, "merged_tables", general$case_study, 
        paste("combined_met_names.",general$a.year,".igraph", general$igraph,".RData", sep='')))    # get 'combined_met_names'
  ping.fgrounds$LE_MET_level6         <- unlist(lapply(strsplit(as.character(ping.fgrounds$LE_MET_level6), split="_"), function(x) paste(x[1],'_',x[2],sep='')))   # remove mesh size and useless selective grid info 
  # rename metiers into INTEGER metiers
  ping.fgrounds$LE_MET_level6         <- factor(ping.fgrounds$LE_MET_level6)
  levels(ping.fgrounds$LE_MET_level6) <- combined_met_names$idx[ match(levels(ping.fgrounds$LE_MET_level6), as.character(combined_met_names$met))] 
  

 # (optional) load outputs from glm and then remove here potential few records without any info
 # from avai to make the input .dat files very robust for Cpp...
 load(file.path(general$main.path, "popsspe", paste("betas_DEN_DEU_SWE_INTEGER_",general$case_study ,".RData", sep='')) )
 ping.fgrounds                <- ping.fgrounds[ping.fgrounds$VE_REF %in% unique(all.betas.vid$VE_REF),]
 ping.fgrounds                <- ping.fgrounds[ping.fgrounds$LE_MET_level6 %in% unique(all.betas.met$LE_MET_level6),]
 ping.fgrounds$VE_REF         <- factor(ping.fgrounds$VE_REF)
 ping.fgrounds$LE_MET_level6  <- factor(ping.fgrounds$LE_MET_level6)


 
 

 
 ## DISCARDS ###################
 # create discards ogive e.g. all at 0 (and not species-specific...)
 dis <- NULL
 for (met in levels(ping.fgrounds$LE_MET_level6) ) {
 
    the_met <- combined_met_names[combined_met_names[, 'idx']==met, 'met']

    L50         <- 5  # fake but conservative.
    L75         <- 10 # fake but conservative.    
    if(general$case_study=="canadian_paper"){
       a_size_group_bin_in_cm <- 10
       mid                    <- 5
       }
    if(general$case_study=="baltic_only"){
       a_size_group_bin_in_cm <- 5
       mid                    <- 2.5
       }
    l           <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,1000) *a_size_group_bin_in_cm  # vector of 14 length groups of eg 10 cm bin or 5 cm   
    length.fish <-  l + mid # i.e. mid size in cm
    equ.dis     <- paste("1- (1/(1+exp(S1-S2*length.fish)))")  # cf. Rasmus paper
    S1          <- L50*log(3) / (L75 - L50)      # L75=L50+(1/2*SR)
    S2          <-  S1/L50
 
    ogive              <- rep(met, 14) 
    dis <-  rbind (dis, cbind(ogive, round(eval(parse("",text=equ.dis)),3)  )      )    # ...assuming 14 szgroup bins
 }
 colnames(dis) <- c("met_name","ogive")
 #=> parameterize as the proportion discarded per szgroup.
 # CAUTION THIS IS VERY SENSISTIVE AS THE CATCHES ARE INFERED FROM THE MODELED LANDINGS AND THIS OGIVE......
 
 
 # save the .dat file
 if(FRANCOIS) {  
            write.table(dis,
               file=file.path(general$main.path, "metiersspe",
                 paste("metier_discards_ogives.dat",sep='')),
                   col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)
       }



 # add quarters, and then semesters
 ping.harbours$quarter          <- quarters(as.POSIXct(ping.harbours$SI_DATE, tz="GMT"))
 ping.harbours$semester         <- factor(ping.harbours$quarter)
 levels(ping.harbours$semester) <- c(1,1,2,2)

 # then, aggregate...
 # FGROUNDS: AGGREGATE PER VE_REF
 nm                       <- names(ping.fgrounds)
 idx.col.e                <- grep('LE_EFF_VMS', nm)
 ping.fgrounds$LE_EFF_VMS <- as.numeric(as.character(ping.fgrounds$LE_EFF_VMS))
 idx.col                  <- c(idx.col.e)

 library(data.table)
 DT              <- data.table(ping.fgrounds) # library data.table for fast grouping replacing aggregate()
 eq1             <- c.listquote( paste ("sum(",nm[idx.col],",na.rm=TRUE)",sep="") )
 x.agg           <- DT[,eval(eq1), by=list(VE_REF, quarter, pt_graph)]
 x.agg           <- data.frame( x.agg)
 colnames(x.agg) <- c("VE_REF", "quarter", "pt_graph",nm[idx.col])


# AGGREGATE PER VE_REF, PER METIER
 nm                       <- names(ping.fgrounds)
 idx.col.e                <- grep('LE_EFF_VMS', nm)
 ping.fgrounds$LE_EFF_VMS <- as.numeric(as.character(ping.fgrounds$LE_EFF_VMS))
 idx.col                  <- c(idx.col.e)

 library(data.table)
 DT                       <- data.table(ping.fgrounds) # library data.table for fast grouping replacing aggregate()
 eq1                      <- c.listquote( paste ("sum(",nm[idx.col],",na.rm=TRUE)",sep="") )
 x.agg.met                <- DT[,eval(eq1),by=list(VE_REF, LE_MET_level6, quarter, pt_graph)]
 x.agg.met                <- data.frame( x.agg.met)
 colnames(x.agg.met)      <- c("VE_REF", "LE_MET_level6", "quarter", "pt_graph", nm[idx.col])

 ####-------
 for (a.quarter in c("Q1","Q2","Q3","Q4")){

    #-----------
    # vesselsspe_fgrounds_quarter[xx].dat
    # vesselsspe_freq_fgrounds_quarter[xx].dat
    x.agg.Q        <- x.agg[x.agg$quarter==a.quarter,]
    x.agg.Q$VE_REF <- factor( x.agg.Q$VE_REF )
    tot            <- tapply(x.agg.Q$LE_EFF_VMS, x.agg.Q$VE_REF, sum, na.rm=TRUE  )
    x.agg.Q$tot    <- tot[match(x.agg.Q$VE_REF, names(tot))] # map
    x.agg.Q$freq   <- round(x.agg.Q$LE_EFF_VMS /  x.agg.Q$tot,4)

    # save .dat files
    x.agg.Q$pt_graph <-  x.agg.Q$pt_graph - 1 ##!!! OFFSET FOR C++ !!!##
        vesselsspe_fgrounds_quarter <- x.agg.Q[,c('VE_REF','pt_graph')]
        write.table(vesselsspe_fgrounds_quarter,
            file=file.path(general$main.path, "vesselsspe",
              paste("vesselsspe_fgrounds_quarter",gsub("Q","",a.quarter),".dat",sep='')),
                  col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)
        vesselsspe_freq_fgrounds_quarter <- x.agg.Q[,c('VE_REF','freq')]
        write.table(vesselsspe_freq_fgrounds_quarter,
          file=file.path(general$main.path, "vesselsspe",
            paste("vesselsspe_freq_fgrounds_quarter",gsub("Q","",a.quarter),".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)
    #-----------



    #-----------
    # vesselsspe_harbours_quarter[xx].dat
    # vesselsspe_freq_harbours_quarter[xx].dat
    h.Q         <- ping.harbours[ping.harbours$quarter==a.quarter, c("VE_REF","pt_graph")]
    h.Q         <- h.Q[!is.na(h.Q$pt_graph),]
    h.Q$VE_REF  <- factor(h.Q$VE_REF )
    h.Q         <- h.Q[!duplicated(h.Q),]
    nb          <- tapply(h.Q$pt_graph, h.Q$VE_REF , length)
    h.Q$nb      <-  nb[match(h.Q$VE_REF, names(nb))] # map
    h.Q$freq    <- round(1 / h.Q$nb , 2)

    # save .dat files
        h.Q$pt_graph <-  h.Q$pt_graph - 1 ##!!! FOR C++ !!!##
        vesselsspe_harbours_quarter <-h.Q[,c('VE_REF','pt_graph')]
        write.table(vesselsspe_harbours_quarter,
            file=file.path(general$main.path, "vesselsspe",
              paste("vesselsspe_harbours_quarter",gsub("Q","",a.quarter),".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)
        vesselsspe_freq_harbours_quarter <- h.Q[,c('VE_REF','freq')]
        write.table(vesselsspe_freq_harbours_quarter,
            file=file.path(general$main.path, "vesselsspe",
              paste("vesselsspe_freq_harbours_quarter",gsub("Q","",a.quarter),".dat",sep='')),
                col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)
   
        # check for NAs
        dd<- vesselsspe_harbours_quarter [is.na(vesselsspe_harbours_quarter[,c('pt_graph')]) , ]
        if(nrow(dd)!=0) {print ("NAs in vesselsspe_harbours_quarter"); browser()}
        dd<- vesselsspe_freq_harbours_quarter [is.na(vesselsspe_freq_harbours_quarter[,c('freq')]) , ]
        if(nrow(dd)!=0) {print ("NAs in vesselsspe_freq_possible_metiers_quarter"); browser()}
   #----------



   #-----------
   # DNK00[xx]_possible_metiers_quarter2.dat
   # DNK00[xx]_freq_possible_metiers_quarter[xx].dat
    x.agg.met.Q         <-  x.agg.met[ x.agg.met$quarter==a.quarter,]
    x.agg.met.Q$VE_REF  <- factor(x.agg.met.Q$VE_REF)
    for(vid in unique(x.agg.met$VE_REF)){
       x.agg.met.Q.vid                <-   x.agg.met.Q[  x.agg.met.Q$VE_REF %in% vid,]
       x.agg.met.Q.vid$VE_REF         <- factor(x.agg.met.Q.vid$VE_REF)
       x.agg.met.Q.vid$pt_graph       <- factor( x.agg.met.Q.vid$pt_graph )
       x.agg.met.Q.vid$LE_MET_level6  <- factor(x.agg.met.Q.vid$LE_MET_level6)
       tot                            <- tapply( x.agg.met.Q.vid$LE_EFF_VMS, list( x.agg.met.Q.vid$pt_graph) , sum, na.rm=TRUE)
       x.agg.met.Q.vid$tot            <- tot[match(x.agg.met.Q.vid$pt_graph, names(tot))] # map
       x.agg.met.Q.vid$freq           <-  round( x.agg.met.Q.vid$LE_EFF_VMS /x.agg.met.Q.vid$tot ,4)

       # save .dat files (vessel-spe .dat files)
       x.agg.met.Q.vid$pt_graph            <- as.numeric(as.character(x.agg.met.Q.vid$pt_graph)) - 1 ##!!! FOR C++ !!!##
       vesselsspe_possible_metiers_quarter <- x.agg.met.Q.vid[,c('pt_graph', 'LE_MET_level6')]

       write.table(vesselsspe_possible_metiers_quarter,
           file=file.path(general$main.path, "vesselsspe",
             paste(vid,"_possible_metiers_quarter",gsub("Q","",a.quarter),".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)
       vesselsspe_freq_possible_metiers_quarter <- x.agg.met.Q.vid[,c('pt_graph', 'freq')]
       write.table(vesselsspe_freq_possible_metiers_quarter,
           file=file.path(general$main.path, "vesselsspe",
             paste(vid,"_freq_possible_metiers_quarter",gsub("Q","",a.quarter),".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)

      # check for NAs
      dd<- vesselsspe_possible_metiers_quarter [is.na(vesselsspe_possible_metiers_quarter[,c('LE_MET_level6')]) , ]
        if(nrow(dd)!=0) {print ("NAs in vesselsspe_possible_metiers_quarter"); browser()}
      dd <- vesselsspe_freq_possible_metiers_quarter [is.na(vesselsspe_freq_possible_metiers_quarter[,c('freq')]) , ]
        if(nrow(dd)!=0){print ("NAs in vesselsspe_freq_possible_metiers_quarter"); browser()}
   
     } # end vid
     #----------

 } # end a.quarter




 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!LOAD & COMPUTE FOR OBTAINING!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## vesselsspe_features_quarter[xx].dat          !!!!!!!!!!!!!!!##
 ## with VE_REF| speed| fuel. cons. rate|         !!!!!!!!!!!!!!##
 ## vessel length| carry cap.| tank cap.| nbpingspertrip|   !!!!##
 ## gamma par1| gamma par2  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

  
  general$a.country <- 'DEN'
  #general$a.country <- 'DEU'
  #general$a.country <- 'SWE'

  features <- NULL

  load(file.path(general$main.path,"merged_tables", general$case_study,
           paste("all_merged_weight_",general$a.country,"_",general$a.year,".RData",sep='')))
  x <- all.merged ; rm(all.merged); gc(reset=TRUE)
  
  # ensure the FT_REF to be converted in numeric later on
  x$FT_REF <- factor(x$FT_REF)
  levels(x$FT_REF ) <- 1:length(x$FT_REF)

  # debug to get consistency between c++ list of vesselids and c++ [DNK000]_possible_metiers files
  # because possible vessels have been removed earlier e.g. those targetting OYE or MUS exclusively...
  load(file.path (general$main.path,"merged_tables", general$case_study, 
                   paste("ping.fgrounds.",general$a.country,".",general$a.year,".igraph",general$igraph,".RData", sep="")))
  load(file.path(general$main.path, "popsspe", paste("betas_DEN_DEU_SWE_INTEGER_",general$case_study ,".RData", sep='')) )
  ping.fgrounds                <- ping.fgrounds[ping.fgrounds$VE_REF %in% unique(all.betas.vid$VE_REF),]
  ping.fgrounds$VE_REF         <- factor(ping.fgrounds$VE_REF)
  vessels.still.there          <- unique(as.character(ping.fgrounds$VE_REF))
  x                            <- x[x$VE_REF %in% vessels.still.there,]

  # add quarters, and then semesters
  x$quarter <- quarters(as.POSIXct(x$SI_DATE, tz="GMT"))

  ## GAMMA PARAMETERS-----------------
  # get the interval time between trips
  x$tmp  <- c(0,diff( as.numeric(as.character(x$FT_REF))))
  x$tmp2 <- c(diff( as.numeric(as.character(x$FT_REF))),0)
  x      <-  x[x$tmp!=0 |  x$tmp2!=0,]
  ctime  <- strptime(  paste(x$SI_DATE, x$SI_TIME) ,  "%e/%m/%Y %H:%M" )
  x      <- cbind.data.frame(x, date.in.R=ctime)
  x$effort.mins <-
                 abs(c(0, as.numeric(x[-nrow(x),"date.in.R"] -
                           x[-1,"date.in.R"], units="mins")))
   x$time <- x$effort.mins /60
   x$a.diff <- c(0,diff( as.numeric(as.character(x$FT_REF))))
   x1 <- x[x$a.diff!=0 &  !is.na(x$a.diff),]
   time.interval.with.prev.trip.hours <- x1[, c("VE_REF", "FT_REF", "time", "quarter") ]
   x2 <- x[x$a.diff==0 &  !is.na(x$a.diff),]
   trip.duration.hours <- x2[, c("VE_REF", "FT_REF", "time", "quarter") ]

   # fit a gamma law for bwtrip
   # make the average for trip duration (will be used to detect vessel with daily trips)
   for(vid in unique(time.interval.with.prev.trip.hours$VE_REF)){
       print(vid)
       timeint.this.v             <- time.interval.with.prev.trip.hours[time.interval.with.prev.trip.hours$VE_REF==vid,]
       trip.duration.hours.this.v <- trip.duration.hours[trip.duration.hours$VE_REF==vid,]
       for (a.quarter in c("Q1","Q2","Q3","Q4")){
            # 1- between trip interval in hours 
            samples <- timeint.this.v[timeint.this.v$quarter==a.quarter,'time']
            samples <- samples[!is.na(samples)]
            #print(samples)
            samples <- samples[samples<3000 & samples>0]  # interval >125 days impossible.
            gamma.nll <- function(par,data) -sum(dgamma(data,shape=par[1],scale=par[2],log=T))
            opt <- optim(c(1,1),gamma.nll,data=samples,method='BFGS')
            #print(rgamma(100, shape=opt$par[1], scale = opt$par[2]) )
            # collect...
            if (round(opt$par[2],4)==1) opt$par[2] <- 100 # CAUTION: handcrafted correction to avoid too numerous trips if bad optim
      
            # 2- trip duration in hours
            dd <-  trip.duration.hours.this.v[ trip.duration.hours.this.v$quarter==a.quarter,'time']
            dd <- dd[dd!=0]
            av.trip.duration <- mean(dd, na.rm=TRUE)
            if(is.na(av.trip.duration)) av.trip.duration <- 1000

      
       features <- rbind.data.frame(features,
               data.frame(VE_REF=vid, quarter=a.quarter, shape=round(opt$par[1],4), scale = round(opt$par[2],4),
                              av.trip.duration=round(av.trip.duration,0)))      
       }
    }

  
  ## VESSEL (MAX) SPEED IN KNOTS-----------------
  load(file.path(general$main.path,"merged_tables", general$case_study,
           paste("all_merged_weight_",general$a.country,"_",general$a.year,".RData",sep='')))
  x <- all.merged ; rm(all.merged); gc(reset=TRUE)

  # add quarters, and then semesters
  x$quarter <- quarters(as.POSIXct(x$SI_DATE, tz="GMT"))

  # great circle distance
  `distance` <-
     function(lon,lat,lonRef,latRef){
                    x1 <- lon
                    y1 <- lat
                    x2 <- lonRef
                    y2 <- latRef

                    pd <- pi/180

                    a1<- sin(((y2-y1)*pd)/2)
                    a2<- cos(y1*pd)
                    a3<- cos(y2*pd)
                    a4<- sin(((x2-x1)*pd)/2)
                    a <- a1*a1+a2*a3*a4*a4

                                      c <- 2*atan2(sqrt(a),sqrt(1-a));
                                      R <- 6371;
                                      dx1 <- R*c
     return(dx1)
     }

     an <- function(x) as.numeric(as.character(x))
     last <- nrow(x)
     dists <- distance(an(x$SI_LATI[-last]), an(x$SI_LONG[-last]), an(x$SI_LATI[-1]), an(x$SI_LONG[-1]))
     x$dist <- c(0, dists[-last])
     x$knots <- (x$dist/1.852) / (an(x$LE_EFF_VMS)/60)
     max.speed.per.vid <- tapply(x$knots, x$VE_REF,
                  function(x) {x <- x[!is.infinite(x) & !is.na(x) & x<20] ; quantile(x, probs=c(0.99))})
                  #=> remove outliers included...
     # collect...
     features <- cbind.data.frame(features, speed=0)
     features$speed <- round(max.speed.per.vid [as.character(features$VE_REF)],2) # map
     
    
    # VESSEL LENGTH-------------------
    if(general$a.country=="DEN") load(file.path("C:","merging", "EflaloAndTacsat",
           paste("eflalo3_",general$a.year,".RData",sep='')))
    if(general$a.country=="DEU") {
       load(file.path("C:","merging", "EflaloAndTacsat",  "GermanEflaloTacsat",
           paste("ger_eflalo",general$a.year,".RData",sep='')))
       eflalo$VE_LEN <- as.numeric(as.character(eflalo$VE_LEN)) /100 # convert in meters...
       }
    if(general$a.country=="SWE") {
       load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.fgrounds.",general$a.country,".",general$a.year,".igraph",general$igraph,".RData",sep='')))
       eflalo <- ping.fgrounds ; eflalo$LE_EFF <- eflalo$LE_EFF_VMS
      }
    x <- eflalo ; rm(eflalo); gc(reset=TRUE)
  
    x              <- subset(x,FT_REF != 0)
  
    features <- cbind.data.frame(features, length=0)
    vessel.length.per.vid <- x[!duplicated(x$VE_REF),c("VE_REF","VE_LEN")]
    features$length <- round(vessel.length.per.vid [match( features$VE_REF,vessel.length.per.vid$VE_REF), "VE_LEN"], 0) # map
 
   
    # FUEL CONSUMPTION RATE IN LITERS PER HOUR & TANK CAPACITY IN LITERS-------------------
    if(general$a.country=="DEN") load(file.path("C:","merging", "EflaloAndTacsat",
           paste("eflalo3_",general$a.year,".RData",sep='')))
    if(general$a.country=="DEU") {
       load(file.path("C:","merging", "EflaloAndTacsat",  "GermanEflaloTacsat",
           paste("ger_eflalo",general$a.year,".RData",sep='')))
       eflalo$VE_LEN <- as.numeric(as.character(eflalo$VE_LEN)) /100 # convert in meters...
       }
    if(general$a.country=="SWE") {
     load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.fgrounds.",general$a.country,".",general$a.year,".igraph",general$igraph,".RData",sep='')))
       eflalo <- ping.fgrounds ; eflalo$LE_EFF <- eflalo$LE_EFF_VMS /60
       }
    x <- eflalo ; rm(eflalo); gc(reset=TRUE)
   
  
     # compute effort
   x$SI_DATIM     <- as.POSIXct(paste(x$FT_DDAT,  x$FT_DTIME,   sep=" "), tz="GMT", format="%d/%m/%Y  %H:%M")
   x              <- subset(x,FT_REF != 0)
   x$ID           <- paste(x$VE_REF, x$FT_REF,sep="_")
   library(doBy)
   x              <- orderBy(~VE_REF+SI_DATIM+FT_REF, data=x)
   x$SI_DATIM2    <- as.POSIXct(paste(x$FT_LDAT,  x$FT_LTIME,    sep=" "), tz="GMT", format="%d/%m/%Y  %H:%M")
   an                  <- function(x) as.numeric(as.character(x))
   if(!"LE_EFF" %in% colnames(x)) x$LE_EFF       <- an(difftime(x$SI_DATIM2, x$SI_DATIM, units="hours"))

   table.fuelcons.per.engine <-
     read.table(file= file.path(general$main.path, "IBM_datainput_engine_consumption.txt"), header=TRUE,sep="")        
   linear.model <- lm(calc_cons_L_per_hr_max_rpm~ kW2, data=table.fuelcons.per.engine)  # conso = a*Kw +b
    #=> so, it is assumed here that knowing the kW of the vessel is enought
    # to guess its fuel consumption at maximal speed

      
    
   x$fuel_cons_rate <- predict(linear.model, newdata=data.frame(kW2=an(x$VE_KW))) # Liter per hour
   fuel.cons.rate.per.vid <-
       tapply(x$fuel_cons_rate, x$VE_REF, function(x) {x <- x[!is.na(x)] ; x[1]})
    # collect...
    features <- cbind.data.frame(features, fuelconsrate=0)
    features$fuelconsrate <- round(fuel.cons.rate.per.vid [as.character(features$VE_REF)],3) # map

  
  
    # compute fuel cons (vessel specific)
    # (assuming equal consumption between fishing and steaming phases)
    an <- function(x) as.numeric(as.character(x))
    x$fuelcons       <- as.numeric(as.character(x$fuel_cons_rate)) * as.numeric(as.character(x$LE_EFF)) # liter per hour

    # caution, need to divide the fuel cons between records when several logbook events!!!!
    if(!general$a.country=="SWE"){
    dd                        <- table(x$ID)
    x$nb_records         <- factor(x$ID) # init
    levels(x$nb_records) <- dd[levels(x$nb_records)]   # more than 1 if change of day or ICES rect.
    x$fuelcons          <-  as.numeric(as.character(x$fuelcons)) / as.numeric(as.character(x$nb_records)) 
    }
    


    # compute the tank cap
    library(data.table)
    DT  <- data.table(x) # library data.table for fast grouping replacing aggregate()
    eq1  <- c.listquote( paste ("sum(","fuelcons",", na.rm=TRUE)",sep="") )
    DT$VE_REF<-as.factor(DT$VE_REF)
    x.agg <- DT[,eval(eq1),by=list(VE_REF,FT_REF)]
    x.agg <- data.frame( x.agg)
    colnames(x.agg) <- c("VE_REF", "FT_REF", "totcons")
    tank.capacity.per.vid <-
        tapply(x.agg$totcons, x.agg$VE_REF, function(x) {x<- x[x<100000]; quantile(x, probs=0.9)})# {x<- x[x<100000]; quantile(x, probs=0.99)})
        #=> remove outliers included...
        tank.capacity.per.vid[is.na(tank.capacity.per.vid)] <- mean(tank.capacity.per.vid, na.rm=TRUE) # a quantile chosen to deal with outliers....
    
    # as the fuel consumed is likely to be dependent on the particular fishing behaviour of the vessel then go one step further and
    # model the tank capacity eg according to vessel length
    dd <- cbind(cap=tank.capacity.per.vid[vessel.length.per.vid$VE_REF], vessel.length.per.vid)
    plot(dd$VE_LEN, dd$cap)
    nls1 <- nls(cap~ b*VE_LEN^a, data=dd, start=list(a=5, b=100))
    points(dd$VE_LEN, predict(nls1), col=4) 
    #=> this is OK...
     
    dd <- dd[complete.cases(dd) & dd$cap!=0,]
    dd$log_cap <- log(dd$cap)
    dd$log_length <- log(dd$VE_LEN)
    plot( dd$log_length, dd$log_cap)
    #=> an exponential model seems good and actually better than the nls fit for larger vessels...but not for smaller ones!
    
    pred <- predict(lm(log_cap~log_length, data=dd), dd)
    abline(lm(log_cap~log_length, data=dd))
    plot(dd$VE_LEN, dd$cap, xlim=c(0,50), ylim=c(0,100000))
    points( dd$VE_LEN, exp(pred), pch="*")

    # collect...
    features <- cbind.data.frame(features, tank_capacity_max=0)
    features$tank_capacity_max <- round(tank.capacity.per.vid [as.character(features$VE_REF)], 0) # map from eflalo for selected vessels 
    
    # apply the exp or the nls model
    features$log_length          <- log(features$length) 
    features$tank_capacity_model_log <- round( exp( predict(lm(log_cap~log_length, data=dd), features) ) ) # if the exp model
    features$VE_LEN              <- features$length 
    features$tank_capacity_model_nls <- round( predict(nls1, features) )  # if the nls model

   
   
    # CARRYING CAPACITY IN KG---------------------------------------------------------------------
    if(general$a.country=="DEN") load(file.path("C:","merging", "EflaloAndTacsat",
           paste("eflalo3_",general$a.year,".RData",sep='')))
    if(general$a.country=="DEU") {
       load(file.path("C:","merging", "EflaloAndTacsat",  "GermanEflaloTacsat",
           paste("ger_eflalo",general$a.year,".RData",sep='')))
       eflalo$VE_LEN <- as.numeric(as.character(eflalo$VE_LEN)) /100 # convert in meters...
       }
    if(general$a.country=="SWE") {
      load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.fgrounds.",general$a.country,".",general$a.year,".igraph",general$igraph,".RData",sep='')))
       eflalo <- ping.fgrounds ; eflalo$LE_EFF <- eflalo$LE_EFF_VMS /60
      }
    x <- eflalo ; rm(eflalo); gc(reset=TRUE)
  
     
    library(data.table)
    DT  <- data.table(x) # library data.table for fast grouping replacing aggregate()
    nm <- colnames(x)
    idx.col <- grep("KG", nm)
    eq1  <- c.listquote( paste ("sum(",nm[idx.col],",na.rm=TRUE)",sep="") )
    DT$VE_REF<-as.factor(DT$VE_REF)
    x.agg <- DT[,eval(eq1),by=list(VE_REF, FT_REF)]
    x.agg <- data.frame( x.agg)
    x.agg$tot_landings <- apply(x.agg[, -c(1:2)], 1, sum, na.rm=TRUE)   # kg

    carrying.capacity.per.vid <-
        tapply(x.agg$tot_landings, x.agg$VE_REF, function(x) {; quantile(x, probs=0.9)})   # {; quantile(x, probs=0.99)})
         #=> remove outliers included...also assume that this is independent of the fuel tank capacitiy
         # this is also implicitely releated to underlying quotas...
         # but it is the best we can do so far to estimate this capacity!
    # collect...
    features <- cbind.data.frame(features, carrying_capacity=0)
    features$carrying_capacity <- round(carrying.capacity.per.vid[as.character(features$VE_REF)], 0) # map
  
      
    # as the carrying capacitiy of fish  is likely to be dependent on the particular fishing behaviour of the vessel then go one step further and
    # model the  capacity eg according to vessel length
    dd <- cbind(cap=carrying.capacity.per.vid[vessel.length.per.vid$VE_REF], vessel.length.per.vid)
    plot(dd$VE_LEN, dd$cap)
    nls2 <- nls(cap~ b*VE_LEN^a, data=dd, start=list(a=1, b=1000))
    points(dd$VE_LEN, predict(nls2), col=4) 
    #=> this is OK...
     
    # collect...
    features <- cbind.data.frame(features, carrying_capacity_max=0)
    features$carrying_capacity_max <- round(carrying.capacity.per.vid [as.character(features$VE_REF)], 0) # map from eflalo for selected vessels 
    
    # the nls model
    features$VE_LEN              <- features$length 
    features$carrying_capacity_model_nls <- round ( predict(nls2, features) )  # if the nls model

   
     



    # NB PINGS PER TRIP-------------------
  # load the combined graph with the "merged" table i.e. the ping.fgrounds object
    load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.fgrounds.",general$a.country,".",general$a.year,".igraph",general$igraph,".RData",sep='')))
    x<- ping.fgrounds
    x$SI_STATE <- as.numeric(as.character(x$SI_STATE))

    library(data.table)
    DT  <- data.table(x) # library data.table for fast grouping replacing aggregate()
    eq1  <- c.listquote( paste ("length(unique(","pt_graph","))",sep="") )
    DT$VE_REF<-as.factor(DT$VE_REF)
    x.agg <- DT[,eval(eq1),by=list(VE_REF,FT_REF)]
    x.agg <- data.frame( x.agg)
    colnames(x.agg) <- c("VE_REF", "FT_REF", "nb_visits_of_different_fgrounds")
    nb.pings.per.trip.per.vid <-
        tapply(x.agg$nb_visits_of_different_fgrounds, x.agg$VE_REF, function(x) {; max(x, na.rm=TRUE)})
         #=>sometimes, no fpings for some vessels i.e. steaming only...then set to 3
    nb.pings.per.trip.per.vid <-  replace(nb.pings.per.trip.per.vid, is.na(nb.pings.per.trip.per.vid), 3)
    # collect...
    features <- cbind.data.frame(features, nb_pings_per_trip=0)
    features$nb_pings_per_trip <- round(nb.pings.per.trip.per.vid[as.character(features$VE_REF)], 0) # map


    ## check for NA on the German side
    head(features)

   # order columns and save per quarter----------------------
   for (a.quarter in c("Q1","Q2","Q3","Q4")){

    features_a_quarter <-
          features[features$quarter==a.quarter,
           c('VE_REF', 'speed', 'fuelconsrate', 'length', 'carrying_capacity_model_nls', 
                'tank_capacity_model_nls', 'nb_pings_per_trip', 'shape', 'scale', 'av.trip.duration')]

    # keep only country spe vessels
    #features_a_quarter <-     features_a_quarter[substr(   features_a_quarter$VE_REF,1,3) %in% general$a.country,]

    if(FRANCOIS) { do.append<-FALSE}
    if(!FRANCOIS)    { do.append<-TRUE}
      write.table(features_a_quarter,
           file=file.path(general$main.path, "vesselsspe",
             paste("vesselsspe_features_quarter", gsub("Q","",a.quarter),".dat",sep='')),
               col.names=FALSE,  row.names=FALSE, sep= '|', quote=FALSE, append=do.append)

  }                     
  
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!LOAD & COMPUTE FOR OBTAINING!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## vesselsspe_percent_tacs_quarter[xx].dat      !!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

  general$a.country <- 'DEN'
  #general$a.country <- 'DEU'
  #general$a.country <- 'SWE'
  
  load(file.path(general$main.path,"merged_tables", general$case_study,
           paste("all_merged_weight_",general$a.country,"_",general$a.year,".RData",sep='')))
  x <- all.merged ; rm(all.merged); gc(reset=TRUE)

  # debug to get consistency between c++ list of vesselids and c++ [DNK000]_possible_metiers files
  # because possible vessels have been removed earlier e.g. those targetting OYE or MUS exclusively...
  load(file.path (general$main.path,"merged_tables", general$case_study, 
                   paste("ping.fgrounds.",general$a.country ,".",general$a.year,".igraph",general$igraph,".RData", sep="")))
  load(file.path(general$main.path, "popsspe", paste("betas_DEN_DEU_SWE_INTEGER_",general$case_study ,".RData", sep='')) )
  ping.fgrounds                <- ping.fgrounds[ping.fgrounds$VE_REF %in% unique(all.betas.vid$VE_REF),]
  ping.fgrounds$VE_REF         <- factor(ping.fgrounds$VE_REF)
  vessels.still.there          <- unique(as.character(ping.fgrounds$VE_REF))
  x                            <- x[x$VE_REF %in% vessels.still.there,]
 
  # find out areas
  source(file=file.path(general$main.path.code,"IBM_param_utils_longlat_to_ICESareas.r"))
  x$x      <- as.numeric(as.character(x$SI_LONG)) 
  x$y      <- as.numeric(as.character(x$SI_LATI)) 
  x$area   <- longlat_to_ICESareas(x)
  x[is.na(x$area) | !(x$area %in% c('22', '23', '24', '25', '26', '27', '28-1', '28-2', '29', '30', '31', '32',
                         'IIIan', 'IIIas') ), 'area'] <- 'nsea' # if out of range, assign all these nodes to North Sea stocks...
  x[(x$area %in% c('22', '23', '24')), 'area'] <- '2224'
  x[(x$area %in% c('25', '26', '27', '28-1', '28-2', '29','30', '31', '32')), 'area'] <- '2532'
 
 
  # find total land per species & reshape
  totland            <- aggregate(x[,grep("LE_KG", colnames(x))], list(x$area), sum, na.rm=TRUE)
  totland_long       <- reshape(totland, direction="long",
                              times=colnames(totland)[grep("LE_KG", colnames(totland))], timevar="species", 
                               v.names="LE_KG_tot", varying=2:(ncol(totland)))  # be patient....
  totland_long$stock <- paste( gsub("LE_KG_", "", totland_long$species), ".", totland_long$Group.1, sep='') 
  
  # correct names for special cases (those across management areas)
  totland_long[totland_long$stock %in% c("COD.IIIan"), "stock"] <- 'COD.nsea'
  totland_long[totland_long$stock %in% c("COD.IIIas"), "stock"] <- 'COD.kat'
  totland_long[totland_long$stock %in% c("HAD.IIIan"), "stock"] <- 'HAD.nsea'
  totland_long[totland_long$stock %in% c("HER.IIIan", "HER.IIIas", "HER.2224"), "stock"] <- 'HER.3a22'
  totland_long[totland_long$stock %in% c("SPR.2224", "SPR.2532"), "stock"] <- 'SPR.2232'
  totland_long[totland_long$stock %in% c("PLE.2224", "PLE.2532"), "stock"] <- 'PLE.2232'
  totland_long[totland_long$stock %in% c("FLE.2224", "FLE.2532"), "stock"] <- 'FLE.2232'
  totland_long[totland_long$stock %in% c("TUR.2224", "TUR.2532"), "stock"] <- 'TUR.2232'
  totland_long[totland_long$stock %in% c("DAB.2224", "DAB.2532"), "stock"] <- 'DAB.2232'
  totland_long[grep('IIIa', totland_long$stock), "stock"] <- # for all other species, correct kask 
     paste( gsub("LE_KG_", "",totland_long[grep('IIIa', totland_long$stock),'species']), ".kask", sep='')

  # then aggregate again for correctness
  totland_agg <- aggregate(totland_long$LE_KG_tot, list(totland_long$stock), sum, na.rm=TRUE)
  colnames(totland_agg) <- c("stock", "totland")
  
  if(general$a.country=="DEN")  totland_agg_den <- cbind.data.frame(totland_agg, ctry="DNK")
  if(general$a.country=="DEU")  totland_agg_deu <- cbind.data.frame(totland_agg, ctry="DEU")
  if(general$a.country=="SWE")  totland_agg_swe <- cbind.data.frame(totland_agg, ctry="SWE")
  
  
  # find total land per vessel & reshape
  vessland <- aggregate(x[,grep("LE_KG", colnames(x))], list(x$area, x$VE_REF), sum, na.rm=TRUE)
  vessland$id               <- paste(vessland$Group.1, '.', vessland$Group.2, sep='')
  vessland_long     <- reshape(vessland, direction="long", ids="id",
                              times=colnames(vessland)[grep("LE_KG", colnames(vessland))], timevar="species", 
                               v.names="LE_KG_", varying=3:(ncol(vessland)-1))  # be patient....
  vessland_long$stock <- paste( gsub("LE_KG_", "", vessland_long$species), ".", vessland_long$Group.1, sep='') 
  
  # correct names for special cases (those across management areas)
  vessland_long[vessland_long$stock %in% c("COD.IIIan"), "stock"] <- 'COD.nsea'
  vessland_long[vessland_long$stock %in% c("COD.IIIas"), "stock"] <- 'COD.kat'
  vessland_long[vessland_long$stock %in% c("HAD.IIIan"), "stock"] <- 'HAD.nsea'
  vessland_long[vessland_long$stock %in% c("HER.IIIan", "HER.IIIas", "HER.2224"), "stock"] <- 'HER.3a22'
  vessland_long[vessland_long$stock %in% c("SPR.2224", "SPR.2532"), "stock"] <- 'SPR.2232'
  vessland_long[vessland_long$stock %in% c("PLE.2224", "PLE.2532"), "stock"] <- 'PLE.2232'
  vessland_long[vessland_long$stock %in% c("FLE.2224", "FLE.2532"), "stock"] <- 'FLE.2232'
  vessland_long[vessland_long$stock %in% c("TUR.2224", "TUR.2532"), "stock"] <- 'TUR.2232'
  vessland_long[vessland_long$stock %in% c("DAB.2224", "DAB.2532"), "stock"] <- 'DAB.2232'
  vessland_long[grep('IIIa', vessland_long$stock), "stock"] <- # for all other species, correct kask 
     paste( gsub("LE_KG_", "",vessland_long[grep('IIIa', vessland_long$stock),'species']), ".kask", sep='')

  # then aggregate again for correctness
  vessland_agg <- aggregate(vessland_long$LE_KG_, list(vessland_long$Group.2, vessland_long$stock), sum, na.rm=TRUE)
  colnames(vessland_agg) <- c("vid", "stock", "vessland")

  
  # merge & divide to get a percentage...
  vessland_totland <-  merge(vessland_agg, totland_agg, all=TRUE)
  vessland_totland$percent <- vessland_totland$vessland / vessland_totland$totland *100


  # keep relevant species and order according to pop_name
  pop_names                             <- read.table(file.path(general$main.path, "popsspe", paste("pop_names_", general$case_study,".txt", sep='')))
  vessland_totland$stock                      <- factor(vessland_totland$stock)
  levels(vessland_totland$stock )       <- pop_names[,2][ match(levels(vessland_totland$stock), as.character(pop_names[,1]))] # map the name to integer
  vessland_totland$mapped_stk_code      <- as.numeric(as.character(vessland_totland$stock))
  vessland_totland                      <- vessland_totland[!is.na(vessland_totland$mapped_stk_code),] # remove NA stocks

  library(doBy)
  vessland_totland                 <- orderBy(~vid+mapped_stk_code, data=vessland_totland) # library(doBy) # order from 0 to nbstock

  
  ## merge with all combi to get all pop informed even if percent at 0.(required fro Cpp multimap)
  all.combi             <- expand.grid(vid=unique(vessland_totland[, c('vid')]), 
                                       mapped_stk_code=pop_names[,2] )
  vessland_totland           <- merge(all.combi, vessland_totland , all=TRUE)
  vessland_totland[is.na( vessland_totland$percent ), 'percent' ]  <- 0   # replace NA  by 0

 
  vessland_totland[, 'percent' ]  <- round(vessland_totland[, 'percent' ], 4)


  if(FRANCOIS) { do.append<-FALSE}
  if(!FRANCOIS)    { do.append<-TRUE}
 
  # copy/paste the same for semester because we don´t care right now
  semester <- "semester1"
  write.table(vessland_totland[, c('vid', 'percent')], # for c++ multimap<vid,percent> with stock implicit
           file=file.path(general$main.path, "vesselsspe",
             paste("vesselsspe_percent_tacs_per_pop_",semester,".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE, append=do.append)

  semester <- "semester2"
  write.table(vessland_totland[, c('vid', 'percent')], # for c++ multimap<vid,percent> with stock implicit
           file=file.path(general$main.path, "vesselsspe",
             paste("vesselsspe_percent_tacs_per_pop_", semester,".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE, append=do.append)

  


  ## -------------------------------------------------------
  ## get aslo a relative key for the share between countries
  relative_stability <- rbind.data.frame(totland_agg_den,
                                         totland_agg_deu,
                                         totland_agg_swe)
  
  
  # correct names for special cases (those across management areas)
  relative_stability[relative_stability$stock %in% c("COD.IIIan"), "stock"] <- 'COD.nsea'
  relative_stability[relative_stability$stock %in% c("COD.IIIas"), "stock"] <- 'COD.kat'
  relative_stability[relative_stability$stock %in% c("HAD.IIIan"), "stock"] <- 'HAD.nsea'
  relative_stability[relative_stability$stock %in% c("HER.IIIan", "HER.IIIas", "HER.2224"), "stock"] <- 'HER.3a22'
  relative_stability[relative_stability$stock %in% c("SPR.2224", "SPR.2532"), "stock"] <- 'SPR.2232'
  relative_stability[relative_stability$stock %in% c("PLE.2224", "PLE.2532"), "stock"] <- 'PLE.2232'
  relative_stability[relative_stability$stock %in% c("FLE.2224", "FLE.2532"), "stock"] <- 'FLE.2232'
  relative_stability[relative_stability$stock %in% c("TUR.2224", "TUR.2532"), "stock"] <- 'TUR.2232'
  relative_stability[relative_stability$stock %in% c("DAB.2224", "DAB.2532"), "stock"] <- 'DAB.2232'
  relative_stability[grep('IIIa', relative_stability$stock), "stock"] <- # for all other species, correct kask 
     paste( relative_stability[grep('IIIa', relative_stability$stock),'stock'], ".kask", sep='')

  
  
  # then aggregate again for correctness
  relative_stability <- aggregate(relative_stability$totland, list(relative_stability$ctry, relative_stability$stock), sum, na.rm=TRUE)
  colnames(relative_stability) <- c("ctry", "stock", "totland")

  
  totland <- as.matrix(tapply(relative_stability$totland, relative_stability$stock, sum))
 
  relative_stability <- cbind.data.frame (relative_stability, totland_all=totland[as.character(relative_stability$stock),])

  relative_stability$percent <- round(relative_stability$totland/relative_stability$totland_all*100,3) 

  # keep relevant species and order according to pop_name
  pop_names                             <- read.table(file.path(general$main.path, "popsspe", paste("pop_names_", general$case_study,".txt", sep='')))
  relative_stability$stock                      <- factor(relative_stability$stock)
  levels(relative_stability$stock )       <- pop_names[,2][ match(levels(relative_stability$stock), as.character(pop_names[,1]))] # map the name to integer
  relative_stability$mapped_stk_code      <- as.numeric(as.character(relative_stability$stock))
  relative_stability                      <- relative_stability[!is.na(relative_stability$mapped_stk_code),] # remove NA stocks

  library(doBy)
  relative_stability                 <- orderBy(~ctry+mapped_stk_code, data=relative_stability) # library(doBy) # order from 0 to nbstock


   
  ## merge with all combi to get all pop informed even if percent at 0.(required fro Cpp multimap)
  all.combi             <- expand.grid(ctry=unique(relative_stability[, c('ctry')]), 
                                       mapped_stk_code=pop_names[,2] )
  relative_stability           <- merge(all.combi, relative_stability , all=TRUE)
  relative_stability[is.na( relative_stability$percent ), 'percent' ]  <- 0   # replace NA  by 0

   
  for (pop in unique(relative_stability$mapped_stk_code)){
     # copy/paste the same for semester because we don´t care right now
     semester <- "semester1"
     write.table(relative_stability[relative_stability$mapped_stk_code==pop, c('ctry', 'percent')], # for c++ multimap<vid,percent> with stock implicit
           file=file.path(general$main.path, "popsspe",
             paste(pop,"ctrysspe_relative_stability_",semester,".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE, append=FALSE)

     semester <- "semester2"
     write.table(relative_stability[relative_stability$mapped_stk_code==pop, c('ctry', 'percent')], # for c++ multimap<vid,percent> with stock implicit
           file=file.path(general$main.path, "popsspe",
             paste(pop,"ctrysspe_relative_stability_", semester,".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE, append=FALSE)

 }

     
    