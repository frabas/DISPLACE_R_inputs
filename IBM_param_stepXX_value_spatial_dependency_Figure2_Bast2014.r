

 # GENERAL SETTINGS
  general <- list()
  general$main.path <- file.path("C:","displace-project.org","repository","ibm_vessels_param")
  general$main.path.ibm <- file.path("C:","displace-project.org","repository","ibm_vessels_param")

  #general$igraph               <- 4 # for the Canadian paper
  #general$case_study           <- "canadian_paper"
  #general$case_study_countries <- c("DNK", "GER") # for the Canadian paper
  #general$a.year                <- "2010"
  #general$a.country             <- "DNK"

  #general$igraph                <- 6
  #general$case_study            <- "baltic_only"
  #general$case_study_countries  <- "DNK" # for the Baltic only
  #general$a.year                <- "2010"
  #general$a.country             <- "DNK"

  general$igraph                <- 11
  general$case_study            <- "baltic_only"
  general$case_study_countries  <- "DNK" # for the Baltic only
  general$a.year                <- "2012"
  
  # mkdir
     dir.create(path=file.path(general$main.path, "merged_tables", general$case_study),
                      showWarnings = TRUE, recursive = TRUE, mode = "0777")


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




 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!UTILS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

  #general$a.country             <- "DEN"
  general$a.country             <- "DEU"
  load(file.path(general$main.path,"merged_tables", general$case_study,
           paste("all_merged_value_",general$a.country,"_",general$a.year,".RData",sep='')))
   x <- all.merged ; rm(all.merged); gc(reset=TRUE) ## CAUTION: POTENTIALLY NOT ALL VESSELS HAVE BEEN KEPT IN THIS DATASET e.g. for BalticOnly
   # then careful with the mapping revenue interpretation
   # e.g. Balticonly for the criteria was: keep only the vessels fishnig in the western Baltic  (and kattegat because her.3a22, and East baltic because spr.2232)


  # add the landing harbour
  endTrip     <- c(0,diff(x[,"FT_REF"]))
  table.end   <- x[which(endTrip!=0)-1, c("FT_REF", "SI_HARB")]
  x$land_harb <- table.end$SI_HARB [match(x$FT_REF, table.end$FT_REF, nomatch =1)]

  x.fishing <- x[x$SI_STATE==1 & x$flag!=4 & x$flag!=5,]
  #=> KEEP ONLY FISHNIG PINGS AND WITH LANDINGS I:E: NOT THE RSIDUAL EFFORT (FLAG 4)

  x.inharb <- x[x$SI_STATE!=1 & x$SI_HARB!="NA" ,]



 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!FIND THE CLOSED IGRAPH NODE FOR EACH FISHING PING!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

  if(FALSE){
  # load the graph
  load(file.path(general$main.path, "igraph", paste(general$igraph, "_graphibm.RData",sep=''))) # built from the R code


  # caution: remove the ports to be sure to have no fishnig ground in ports
  # because otherwise it is a too much mess in the movement algorithm
  # but take care of index node (idx in coord)
    coord <- cbind(coord, idx=1:nrow(coord))  ## CAUTION: start at 1
    coord.fgrounds  <-  coord[coord[,'idx.port']==0,]
   } else{

  ## HERE WE START FROM THE IBM GRAPH,
  ## BUT WE COULD START FROM WHATEVER THE GRID AS WELL..........
  ## e.g. from
   coord.fgrounds <- expand.grid(x=seq(8,15.5,by=0.02), y=seq(53.5, 58, by=0.02))     # 0.02 degree  is about 1.5 km at this latitude?
   # remove points on land...
 library(rgeos)
 library(maptools)
 sh1 <- readShapePoly(file.path(general$main.path,"shp","francois_EU"))
 dd <-sapply(slot(sh1, "polygons"), function(x) lapply(slot(x, "Polygons"), function(x) x@coords)) # tricky there...
 library(sp)
 for(iLand in 1:length(dd)){
     print(iLand)
     if(sh1@data$C_NAME[iLand] %in% c("DENMARK", "GERMANY", "SWEDEN", "POLAND")){
       dilate.with=0.01
          # Points on land are 1 or 2 and not is 0
          p1     <- readWKT(paste("POLYGON((",paste(dd[[iLand]][[1]][,1], dd[[iLand]][[1]][,2], collapse=","),"))", sep=''))
          buff   <- gBuffer(p1,width=dilate.with)
          coords <- buff@polygons[[1]]@Polygons[[1]]@coords
          res    <- point.in.polygon(coord.fgrounds[,1],coord.fgrounds[,2],coords[,1],coords[,2])
          #=> very small dilatation using the gBuffer facility in library(rgeos): to remove the useless points in Fjords
       coord.fgrounds  <- coord.fgrounds [which(res==0),]
       }
   }
  plot( coord.fgrounds[,'x'],  coord.fgrounds[, 'y'], pch=".")
  coord.fgrounds <- cbind(coord.fgrounds, idx=1:nrow(coord.fgrounds))  ## CAUTION: start at 1



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
    N <- nncross (X=X, Y=Y)$which # caution: just euclidean distance on coord   ## CAUTION: broken for R version 3.0.2 
    # visual check
    if(FALSE){
      plot(superimpose(X=X, Y=Y), main="nncross", cols=c("red","blue"))
      arrows(X$x, X$y, Y[N]$x, Y[N]$y, length=0.15)
      }
    # add
    ping.fgrounds <- cbind(x.fishing, pt_graph= coord.fgrounds[N, 'idx'])

   # check one pt
   #points(an(x.fishing$SI_LONG[1000]),an(x.fishing$SI_LATI[1000]),col=4,pch=16)
   #points(an(coord.fgrounds[ ping.fgrounds$pt_graph[1000] ,"x"]),an(coord.fgrounds[ping.fgrounds$pt_graph[1000],"y"]),col=1,pch=16)

   # nb of visits per fishing ping over seasons
   #pt_graph_occurrence <- tapply(ping.fgrounds$pt_graph, paste(ping.fgrounds$VE_REF, ping.fgrounds$year.quarter, sep="-"), table)

   # check wihch nodes are actually used
   # map("worldHires", xlim=general$lim.long, ylim=general$lim.lat)
   # points(coord[ping.fgrounds$pt_graph,1], coord[ping.fgrounds$pt_graph,2])


    # remove the No_Matrix6 metier and records with no weight (because remenber that not all species has been kept when aggregating the merging table)...
    ping.fgrounds   <- ping.fgrounds[ping.fgrounds$LE_MET_level6!= "No_Matrix6",]
    ping.fgrounds   <- ping.fgrounds[ping.fgrounds$LE_MET_level6!= "NA",]
    idx_non0_value <- apply(ping.fgrounds[,grep('EURO',colnames(ping.fgrounds))], 1, sum, na.rm=TRUE) !=0  # because maybe the relvant bunch of species for this record has not been kept
    ping.fgrounds   <- ping.fgrounds[idx_non0_value,] # keep only records with some weight for some species


    ## debug to avoid 0/0
    ping.fgrounds <- ping.fgrounds[ping.fgrounds$LE_EFF_VMS!=0,]

    # remove foreign vessels!!
    ping.fgrounds <- ping.fgrounds[grep(general$a.country, ping.fgrounds$VE_REF),]
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
    ping.fgrounds            <- ping.fgrounds[ping.fgrounds$VE_REF %in% vid_this_case_study,]
    ping.fgrounds$VE_REF     <- factor(ping.fgrounds$VE_REF)
    }  # end case_study



    #*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
    #*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#



      #save(ping.fgrounds,  file=file.path(general$main.path, "merged_tables", general$case_study,
      #       paste("ping.fgrounds.value.",general$a.country,".",general$a.year,".igraph",general$igraph,".RData", sep='')))
      save(ping.fgrounds, coord.fgrounds,  file=file.path(general$main.path, "merged_tables", general$case_study,
             paste("ping.fgrounds.value.",general$a.country,".",general$a.year,".igraph","a_fake_grid",".RData", sep='')))
      cat(paste("save 'ping.fgrounds.value', this year...OK\n\n",sep=""))



    #*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
    #*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#




 ##-------------------------------------------
 ##-----------SHAPE FILES---------------------
 ##--------AND ADD OTHER PLANNED AREAS--------
 ##-------------------------------------------
 ##-------------------------------------------

 library(maptools)
 library(rgdal)

  sh_coastlines                  <<- readShapePoly(file.path(general$main.path,"shp","francois_EU"))

  #ruter                      <<- readShapeLines(file.path("C:","Users","fba","Dropbox","ibm_vessels_param","shp","ShippingRoutes", "ruter.shp"))


   #library(raster)
   #kort       <- raster(file.path("Y:","Dynamisk","GEOdata","BasicLayers","SeaCharts","Denmark","update_2013_09_18_with_frames","kort103.tif"))    # probably need an update of rgdal here....
   #newproj          <- "+proj=longlat +datum=WGS84"
   #kort_proj  <- projectRaster(kort, crs=newproj)



  # What has been used for the WMF graph definition
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


 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!GET A PERCENTAGE OF LANDING VALUE!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!PER VESSEL PER NODE!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

    if(FALSE){
    load(file.path(general$main.path,"merged_tables", general$case_study,
            paste("ping.fgrounds.value.",general$a.country,".",general$a.year,".",general$igraph,".RData",sep='')))
    load(file.path(general$main.path, "igraph", paste(general$igraph, "_graphibm.RData",sep=''))) # built from the R code

    } else{
     load(file.path(general$main.path,"merged_tables", general$case_study,"ping.fgrounds.value.DNK.2012.igrapha_fake_grid.RData"))
     coord <- coord.fgrounds
    }



    ping.fgrounds <- cbind.data.frame(ping.fgrounds,
                          totvalue= apply(ping.fgrounds[,grep('EURO',colnames(ping.fgrounds))], 1, sum, na.rm=TRUE) )


    totvalue_per_node <- aggregate(ping.fgrounds$totvalue, list(ping.fgrounds$VE_REF, ping.fgrounds$pt_graph), sum )
    colnames(totvalue_per_node) <- c("VE_REF", "pt_graph", "totvalue")
    
    
    compute_spatial_dependency <- function(totvalue_per_node, percent_threshold=90){
     dd <- lapply(split(totvalue_per_node, f=totvalue_per_node$VE_REF),
       function(x){
         x$percent                                      <- x$totvalue / sum(x$totvalue) *100 # percentage per node
         x                                              <- x[order(x$percent, decreasing=TRUE),]
         x$cumpercent                                   <- cumsum(x$percent)
         x[,paste("thres.",percent_threshold, sep="")]  <- as.numeric(x$cumpercent <  percent_threshold)
        x
       })
     totvalue_per_node_and_percentage <- do.call('rbind', dd)
     return(totvalue_per_node_and_percentage)
     }

   totvalue_per_node <- compute_spatial_dependency(totvalue_per_node, percent_threshold=10)
   totvalue_per_node <- compute_spatial_dependency(totvalue_per_node, percent_threshold=20)
   totvalue_per_node <- compute_spatial_dependency(totvalue_per_node, percent_threshold=30)
   totvalue_per_node <- compute_spatial_dependency(totvalue_per_node, percent_threshold=40)
   totvalue_per_node <- compute_spatial_dependency(totvalue_per_node, percent_threshold=50)
   totvalue_per_node <- compute_spatial_dependency(totvalue_per_node, percent_threshold=60)
   totvalue_per_node <- compute_spatial_dependency(totvalue_per_node, percent_threshold=70)
   totvalue_per_node <- compute_spatial_dependency(totvalue_per_node, percent_threshold=80)
   totvalue_per_node <- compute_spatial_dependency(totvalue_per_node, percent_threshold=90)

   rownames(totvalue_per_node) <- NULL
   







  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!PLOT FISHERY SPATIAL DEPENDENCY IN!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!A WINDFARM WORLD!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ## PLOT (ALL VESSELS BUT PLOT CELLS AS FAR AS THEY REPRENSENT SOME x% DEPENDENCY FOR AT LEAST ONE VESSEL...i.e. this is the max)
   the_colours <-  heat.colors(10)
   plot(coord[,'x'], coord [,'y'], pch=".", xlim=c(9,14.5), ylim=c(53.5,56.5), col=the_colours[10])
   plot(coord[,'x'], coord [,'y'], pch=".", xlim=c(9,15), ylim=c(53.5,57.5), col=the_colours[10], xlab="Longitude", ylab="Latitude")
   legend("topright", title="Dependency on revenue (%)", legend=c("10","20","30","40","50","60","70","80","90","100"), fill= heat.colors(10), bty="n")
   
   plot(bathymetry,add=TRUE, col=grey(0.8), lwd=0.1)

   this_totvalue_per_node.90 <-  totvalue_per_node[totvalue_per_node$thres.90==1,]
   this_totvalue_per_node.90$x <- coord[this_totvalue_per_node.90$pt_graph, "x"]
   this_totvalue_per_node.90$y <- coord[this_totvalue_per_node.90$pt_graph, "y"]
   points(this_totvalue_per_node.90$x, this_totvalue_per_node.90$y, pch=".", col=the_colours[9], cex=3 )



   this_totvalue_per_node.80 <-  totvalue_per_node[totvalue_per_node$thres.80==1,]
   this_totvalue_per_node.80$x <- coord[this_totvalue_per_node.80$pt_graph, "x"]
   this_totvalue_per_node.80$y <- coord[this_totvalue_per_node.80$pt_graph, "y"]
   points(this_totvalue_per_node.80$x, this_totvalue_per_node.80$y, pch=".", col=the_colours[8], cex=3 )


   this_totvalue_per_node.70 <-  totvalue_per_node[totvalue_per_node$thres.70==1,]
   this_totvalue_per_node.70$x <- coord[this_totvalue_per_node.70$pt_graph, "x"]
   this_totvalue_per_node.70$y <- coord[this_totvalue_per_node.70$pt_graph, "y"]
   points(this_totvalue_per_node.70$x, this_totvalue_per_node.70$y, pch=".", col=the_colours[7], cex=3 )


   this_totvalue_per_node.60 <-  totvalue_per_node[totvalue_per_node$thres.60==1,]
   this_totvalue_per_node.60$x <- coord[this_totvalue_per_node.60$pt_graph, "x"]
   this_totvalue_per_node.60$y <- coord[this_totvalue_per_node.60$pt_graph, "y"]
   points(this_totvalue_per_node.60$x, this_totvalue_per_node.60$y, pch=".", col=the_colours[6], cex=3 )

   this_totvalue_per_node.50 <-  totvalue_per_node[totvalue_per_node$thres.50==1,]
   this_totvalue_per_node.50$x <- coord[this_totvalue_per_node.50$pt_graph, "x"]
   this_totvalue_per_node.50$y <- coord[this_totvalue_per_node.50$pt_graph, "y"]
   points(this_totvalue_per_node.50$x, this_totvalue_per_node.50$y, pch=".", col=the_colours[5], cex=3 )

   this_totvalue_per_node.40 <-  totvalue_per_node[totvalue_per_node$thres.40==1,]
   this_totvalue_per_node.40$x <- coord[this_totvalue_per_node.40$pt_graph, "x"]
   this_totvalue_per_node.40$y <- coord[this_totvalue_per_node.40$pt_graph, "y"]
   points(this_totvalue_per_node.40$x, this_totvalue_per_node.40$y, pch=".", col=the_colours[4], cex=3 )

    this_totvalue_per_node.30 <-  totvalue_per_node[totvalue_per_node$thres.30==1,]
   this_totvalue_per_node.30$x <- coord[this_totvalue_per_node.30$pt_graph, "x"]
   this_totvalue_per_node.30$y <- coord[this_totvalue_per_node.30$pt_graph, "y"]
   points(this_totvalue_per_node.30$x, this_totvalue_per_node.30$y, pch=".", col=the_colours[3], cex=3 )

   this_totvalue_per_node.20 <-  totvalue_per_node[totvalue_per_node$thres.20==1,]
   this_totvalue_per_node.20$x <- coord[this_totvalue_per_node.20$pt_graph, "x"]
   this_totvalue_per_node.20$y <- coord[this_totvalue_per_node.20$pt_graph, "y"]
   points(this_totvalue_per_node.20$x, this_totvalue_per_node.20$y, pch=".", col=the_colours[2], cex=3 )

  this_totvalue_per_node.10 <-  totvalue_per_node[totvalue_per_node$thres.10==1,]
   this_totvalue_per_node.10$x <- coord[this_totvalue_per_node.10$pt_graph, "x"]
   this_totvalue_per_node.10$y <- coord[this_totvalue_per_node.10$pt_graph, "y"]
   points(this_totvalue_per_node.10$x, this_totvalue_per_node.10$y, pch=".", col=the_colours[1], cex=3 )

   plot(sh_coastlines, add=TRUE)

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
 

  plot(windmills_KriegersFlak,add=TRUE, border="red", lwd=2)
  plot(windmills_Kystmølle_zoner2012,add=TRUE, border="red", lwd=2)
  plot(windmills_U_omr_StoreMiddelgrund,add=TRUE, border="red", lwd=2)
  plot(windmills_U_omr_Ronne,add=TRUE, border="red", lwd=2)
  plot(windmills_U_omr_Ringkobing,add=TRUE, border="red", lwd=2)
  plot(windmills_U_omr_Jammmerbugt,add=TRUE, border="red", lwd=2)

  
    # plot polygons retrieved from the map 
    polygon(pol1, border="blue", lwd=2) 
    polygon(pol2, border="blue", lwd=2) 
    polygon(pol3, border="blue", lwd=2) 
    polygon(pol4, border="blue", lwd=2) 
    polygon(pol5, border="blue", lwd=2) 
    polygon(pol6, border="blue", lwd=2) 
    polygon(pol7, border="blue", lwd=2) 
  
    box()
  
     savePlot(filename=file.path(general$main.path,"merged_tables", general$case_study,
       paste("map_windmills_in_spatial_dependency_context.png",sep='')), type="png")

     # Kattegat cod box
   area1 <<- as.matrix(read.table(file=file.path(general$main.path,"shp", "KattegatBox","Lukket_omr_1.txt"),sep="", header=TRUE) )
   area2 <<- as.matrix(read.table(file=file.path(general$main.path,"shp", "KattegatBox","Lukket_omr_2.txt"),sep="", header=TRUE) )
   area3 <<- as.matrix(read.table(file=file.path(general$main.path,"shp", "KattegatBox","Lukket_omr_3.txt"),sep="", header=TRUE) )
   polygon(area1[,1],area1[,2],border=1, lty=2) ; polygon(area2[,1],area2[,2],border=2, lty=2) ; polygon(area3[,1],area3[,2], border=3, lty=2)

   
   
   # existing windmills in 2013
   Windmills_oct_2013_offshore    <<- readShapePoly(file.path(general$main.path,"shp","Windmills", "November_2013", "Windmoelleparker_polygoner_sept_2013.shp"),
                                    proj4string=CRS("+proj=utm  +ellps=intl +zone=32 +towgs84=-81,-89,-115,0,0,0,0,0"))
   Windmills_oct_2013_offshore                   <- spTransform(Windmills_oct_2013_offshore, CRS("+proj=longlat +ellps=WGS84"))    # convert to longlat



add_wind_mills_layer <- function(){

    # existing
    plot(Windmills_oct_2013_offshore, add=TRUE, border="purple", lwd=2)

    # planned
    plot(windmills_KriegersFlak,add=TRUE, border="purple", lwd=2)
    plot(windmills_Kystmølle_zoner2012,add=TRUE, border="purple", lwd=2)
    plot(windmills_U_omr_StoreMiddelgrund,add=TRUE, border="purple", lwd=2)
    plot(windmills_U_omr_Ronne,add=TRUE, border="purple", lwd=2)
    plot(windmills_U_omr_Ringkobing,add=TRUE, border="purple", lwd=2)
    plot(windmills_U_omr_Jammmerbugt,add=TRUE, border="purple", lwd=2)

  
    # plot polygons retrieved from the map 
    polygon(pol1, border="purple", lwd=2) 
    polygon(pol2, border="purple", lwd=2) 
    polygon(pol3, border="purple", lwd=2) 
    polygon(pol4, border="purple", lwd=2) 
    polygon(pol5, border="purple", lwd=2) 
    polygon(pol6, border="purple", lwd=2) 
    polygon(pol7, border="purple", lwd=2) 
  
  return()
   }


  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## build a spatial dependency index  0 to 1 with 1 corresponding to the cell(s) visited by all vessels
 ## and weighted according to the importance of the visiting vessels in terms of revenue

   x        <-  totvalue_per_node[totvalue_per_node$thres.90==1,]
   x$VE_REF <- as.factor(x$VE_REF)

   # index 1
   revenue_per_vessel                                  <- tapply(x$totvalue, list(x$VE_REF), sum)
   names_vessels_per_cell_this_threshold               <- tapply(x$VE_REF, list(x$pt_graph), function(x)as.character(x))
   tot_revenue_of_the_vessels_visiting_this_cell       <- lapply(names_vessels_per_cell_this_threshold, function(x) sum(revenue_per_vessel[x], na.rm=TRUE))
   index1_importance_of_vessels                        <- unlist(tot_revenue_of_the_vessels_visiting_this_cell) / sum(revenue_per_vessel, na.rm=TRUE) *1000

   # index 2
   revenue_per_cell                                    <- tapply(x$totvalue, list(x$pt_graph), sum)
   index2_contribution_of_the_cell                     <- unlist(revenue_per_cell) / sum(revenue_per_vessel, na.rm=TRUE) *1000

   # index 3
   index3_dependency_to_the_cell                       <- unlist(revenue_per_cell) /  unlist(tot_revenue_of_the_vessels_visiting_this_cell) *1000

   # note that index2 = index1 * index3
   # note that sum( index2_per_cell [,1])=1000 => per mille
   
   # categorize to ease a plot
   index1_per_cell        <- cbind(index1_importance_of_vessels, coord[as.numeric(names(index1_importance_of_vessels)),c('x', 'y')])
   index1_per_cell$bins  <- cut(index1_per_cell[,1],
                                 breaks=quantile(index1_per_cell[,1], probs=c(0,.05,0.1,0.15,.2,0.25,.3,0.35,.4,.45,.5,.55,.6,.65,.7,.75,.8,.85,.9,0.95,1)))
   index2_per_cell        <- cbind(index2_contribution_of_the_cell, coord[as.numeric(names(index2_contribution_of_the_cell)),c('x', 'y')])
   index2_per_cell$bins  <- cut(index2_per_cell[,1],
                                 breaks=quantile(index2_per_cell[,1], probs=c(0,.05,0.1,0.15,.2,0.25,.3,0.35,.4,.45,.5,.55,.6,.65,.7,.75,.8,.85,.9,0.95,1)))
   index3_per_cell        <- cbind(index3_dependency_to_the_cell, coord[as.numeric(names(index3_dependency_to_the_cell)),c('x', 'y')])
   index3_per_cell$bins  <- cut(index3_per_cell[,1],
                                 breaks=quantile(index3_per_cell[,1], probs=c(0,.05,0.1,0.15,.2,0.25,.3,0.35,.4,.45,.5,.55,.6,.65,.7,.75,.8,.85,.9,0.95,1)))

   Satellite.Palette <-colorRampPalette(c("blue3","cyan","aquamarine","yellow","orange","red"))


   # high resolution plot
   tiff(filename=file.path(general$main.path,"igraph",
       paste("map_a_spatial_dependency.tiff",sep='')),
                                   width = 2600, height = 1100,
                                   units = "px", pointsize = 12,  res=300)

   #windows(12,4)
   par(mfrow=c(1,3))

   plot(index2_per_cell[,'x'],
           index2_per_cell [,'y'],
           pch=".", xlim=c(9,15), ylim=c(53.5,57.5), xlab="Longitude", ylab="Latitude", cex.lab=1.5, axes=FALSE,
           col=Satellite.Palette(21)[index2_per_cell[,"bins"]],
           cex=3.5)
   axis(1, cex.axis=2); axis(2, las=2, cex.axis=2)
   #plot( sh_coastlines, add=TRUE,  col=grey(0.7))
    map("worldHires", add=TRUE, fill=TRUE,  col=grey(0.7))
   box()
   mtext("(a)", side=3, line=1, adj=0, cex=1.5)
   add_wind_mills_layer()

   plot(index1_per_cell[,'x'],
           index1_per_cell [,'y'],
           pch=".", xlim=c(9,15), ylim=c(53.5,57.5),  xlab="Longitude", ylab="Latitude", cex.lab=1.5, axes=FALSE,
           col=Satellite.Palette(21)[index1_per_cell[,"bins"]],
           cex=3.5)
   #    legend("topright", "spatial density index (Quantiles)", fill=Satellite.Palette(11), legend=c(0,.05,0.1,0.15,.2,0.25,.3,0.35,.4,.45,.5,.55,.6,.65,.7,.75,.8,.85,.9,0.95,1))
   axis(1, cex.axis=2); axis(2, las=2, cex.axis=2)
   plot( sh_coastlines, add=TRUE, col=grey(0.7))
   box()
   mtext("(b)", side=3, line=1, adj=0, cex=1.5)
   add_wind_mills_layer()

   plot(index3_per_cell[,'x'],
           index3_per_cell [,'y'],
           pch=".", xlim=c(9,15), ylim=c(53.5,57.5),  xlab="Longitude", ylab="Latitude", cex.lab=1.5, axes=FALSE,
           col=Satellite.Palette(21)[index3_per_cell[,"bins"]],
           cex=3.5)
   axis(1, cex.axis=2); axis(2, las=2, cex.axis=2)
   plot( sh_coastlines, add=TRUE, col=grey(0.7))
   box()
   mtext("(c)", side=3, line=1, adj=0, cex=1.5)
   add_wind_mills_layer()

   dev.off()


  # same but with planned windmill farms sites
   # high resolution plot
   tiff(filename=file.path(general$main.path,"igraph",
       paste("map_a_spatial_dependency_and_planned_windmillfarms_sites_24Sept14.tiff",sep='')),
                                   width = 3000, height = 1800,
                                   units = "px", pointsize = 12,  res=300)

   #windows(12,4)
   par(mfrow=c(1,2))
     par(xpd = F)
 
   plot(index1_per_cell[,'x'],
           index1_per_cell [,'y'],
           pch=".", xlim=c(9,15), ylim=c(53.5,57.5),  xlab="Longitude °E", ylab="Latitude °N", cex.lab=1.5, axes=FALSE,
           col=Satellite.Palette(21)[index1_per_cell[,"bins"]],
           cex=1.7)
   #    legend("topright", "spatial density index (Quantiles)", fill=Satellite.Palette(11), legend=c(0,.05,0.1,0.15,.2,0.25,.3,0.35,.4,.45,.5,.55,.6,.65,.7,.75,.8,.85,.9,0.95,1))
   axis(1, cex.axis=2); axis(2, las=2, cex.axis=2)
   #plot( sh_coastlines, add=TRUE, col=grey(0.7))
   map("worldHires", add=TRUE, fill=TRUE,  col=grey(0.7))
   library(SDMTools)
   x = c(8.9, 9, 9, 8.9)
   y = c(54.0, 56.4, 56.4, 54.0)     
   ## CAUTION FACTOR 1000 in the indx calculation...see above                                              
   legend.gradient (cbind(x = x , y = y ), cols=Satellite.Palette(21), title = "",  limits = c(expression(bold(paste(I[1],"<0.0001"))), expression(bold(paste(I[1],">0.004")))), cex=0.75, col="black")

   box()
   mtext("(a)", side=3, line=1, adj=0, cex=1.5)
   add_wind_mills_layer()

   par(xpd = FALSE)
   plot(index3_per_cell[,'x'],
           index3_per_cell [,'y'],
           pch=".", xlim=c(9,15), ylim=c(53.5,57.5),   xlab="Longitude °E", ylab="Latitude °N", cex.lab=1.5, axes=FALSE,
           col=Satellite.Palette(21)[index3_per_cell[,"bins"]],
           cex=1.7)
   axis(1, cex.axis=2); axis(2, las=2, cex.axis=2)
  # plot( sh_coastlines, add=TRUE, col=grey(0.7))
     map("worldHires", add=TRUE, fill=TRUE, col=grey(0.7))
   box()
   mtext("(b)", side=3, line=1, adj=0, cex=1.5)
   add_wind_mills_layer()
   #legend("topright", legend=levels(index3_per_cell[,"bins"]), fill=Satellite.Palette(21), bty="n", cex=0.5, border=NA)
   library(SDMTools)
   x = c(8.9, 9, 9, 8.9)
   y = c(54.0, 56.4, 56.4, 54.0)                                                   
   ## CAUTION FACTOR 1000 in the indx calculation...see above                                              
   legend.gradient (cbind(x = x , y = y ), cols=Satellite.Palette(21), title = "",  limits = c(expression(bold(paste(I[2],"<0.0005"))), expression(bold(paste(I[2],">0.02")))), cex=0.75, col="black")
 
   dev.off()

   # quantify what is included in the polygon
  plot(windmills_KriegersFlak,add=TRUE, border="red", lwd=2)
  plot(windmills_Kystmølle_zoner2012,add=TRUE, border="red", lwd=2)
  plot(windmills_U_omr_StoreMiddelgrund,add=TRUE, border="red", lwd=2)
  plot(windmills_U_omr_Ronne,add=TRUE, border="red", lwd=2)
  plot(windmills_U_omr_Ringkobing,add=TRUE, border="red", lwd=2)
  plot(windmills_U_omr_Jammmerbugt,add=TRUE, border="red", lwd=2)

  
    # plot polygons retrieved from the map 
    polygon(pol1, border="blue", lwd=2) 
    polygon(pol2, border="blue", lwd=2) 
    polygon(pol3, border="blue", lwd=2) 
    polygon(pol4, border="blue", lwd=2) 
    polygon(pol5, border="blue", lwd=2) 
    polygon(pol6, border="blue", lwd=2) 
    polygon(pol7, border="blue", lwd=2) 

    
    # NATURA 2000 special case
    NATURA2000          <<- readShapePoly(file.path(general$main.path,"shp", "Natura2000_end2012_rev1_9_15_53_60","Natura2000_end_2012_rev1_9_15_53_60_water_1.shp"), 
                                    proj4string= CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"))
    NATURA2000_proj       <<- spTransform(NATURA2000,CRS("+proj=longlat +ellps=WGS84")) # actually,  already in WGS
 
    
    # unwrap the complex polygon object
    the_names <- NULL
    for(i in 1: length(NATURA2000_proj@polygons)){
    for(j in 1: length(NATURA2000_proj@polygons[[i]]@Polygons)){
     assign(paste("a_poly_", i,"_",j, sep=''),
              NATURA2000_proj@polygons[[i]]@Polygons[[j]]@coords
              )
     the_names <- c(the_names, paste("a_poly_", i,"_",j, sep=''))         
              
    } }
    
    
   
   ## BUILD A TABLE WITH PERCENTAGE EFFORT PER POLYGON
   polygons <- c("windmills_KriegersFlak", "windmills_Kystmølle_zoner2012", 
                    "windmills_U_omr_StoreMiddelgrund", "windmills_U_omr_Ronne", 
                     "windmills_U_omr_Ringkobing", "windmills_U_omr_Jammmerbugt",
                      "pol1", "pol2", "pol3", "pol4", "pol5", "pol6", "pol7", the_names)
   percent_included_per_index <- matrix(0, nrow=length(polygons), ncol=2)
   colnames(percent_included_per_index) <- c("idx_a","idx_b")
   rownames(percent_included_per_index) <- polygons 
   count <-0
   for(pol in polygons){
      count <- count+1
      a_poly <- get(pol)
      if(class(a_poly)=="SpatialPolygonsDataFrame"){
         dd     <- a_poly@polygons[[1]]@Polygons[[1]]@coords
         dd     <- list(x=dd[,1], y=dd[,2]) 
      } else{
         if(!is.list(a_poly)) a_poly <- list(x=a_poly[,1], y=a_poly[,2]) 
         dd     <- a_poly
         }
         
    
      index1_per_cell$inclusion <-  point.in.polygon( index1_per_cell[,'x'], index1_per_cell[,'y'],dd$x,dd$y)
      included <- index1_per_cell[ index1_per_cell$inclusion!=0, ]
      points(included[,"x"], included[, "y"], col="black", pch=".")
      percent_included_per_index [count,"idx_a"]  <- sum(included[,1] ) / sum(index1_per_cell[,1]) * 100# included per %

      index3_per_cell$inclusion <-  point.in.polygon( index3_per_cell[,'x'], index3_per_cell[,'y'],dd$x,dd$y)
      included <- index3_per_cell[ index3_per_cell$inclusion!=0, ]
      points(included[,"x"], included[, "y"], col="black", pch=".")
      percent_included_per_index [count,"idx_b"]  <- sum(included[,1] ) / sum(index3_per_cell[,1]) * 100# included per %      
      }
      
    # do the total...
    apply(percent_included_per_index, 2, sum)  
    # explore...
    head(percent_included_per_index[percent_included_per_index[,1]>1,])
    polygon(x=a_poly_194_1[,1], y=a_poly_194_1[,2], border="green")
    polygon(x=a_poly_220_1[,1], y=a_poly_220_1[,2], border="green", lwd=2)


   
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##


     # plot the revenue and compare
     # to see if  areas with low revenue as such but money made by vessels which depends very much on the area!!.... and vice versa

      plot(coord[,'x'], coord [,'y'], pch=".", xlim=c(9,15), ylim=c(53.5,57.5), col="white", xlab="Longitude", ylab="Latitude")
   legend("topright", title="Quantile revenue", legend=c(0,.2,.4,.6,.8,1), fill=  rev(heat.colors(6)), bty="n")

   plot(bathymetry,add=TRUE, col=grey(0.8), lwd=0.1)

    totvalue_per_node$class_value <- cut(totvalue_per_node$totvalue, breaks= quantile(totvalue_per_node$totvalue, probs=c(0,.2,.4,.6,.8,1)))
     levels(totvalue_per_node$class_value) <-   rev(heat.colors(6))
     points(coord[totvalue_per_node$pt_graph, 'x'], coord[totvalue_per_node$pt_graph, 'y'], pch=".", col=as.character(totvalue_per_node$class_value), cex=3 )
   plot(sh_coastlines, add=TRUE)


  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!EVALUATE A POLYGON IN TERMS OF !!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!! % IMPACTED REVENUE PER VESSEL!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

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
          er <- try({res   <- point.in.polygon(coord[,'x'],coord[,'y'],dd[[iLand]][[i]][,1],dd[[iLand]][[i]][,2])}, silent=TRUE)
          if(class(er)=="try-error") res   <- point.in.polygon(coord[,'x'],coord[,'y'],dd[[iLand]][[i]][,1],dd[[iLand]][[i]][,2])
          coord[which(res!=0), name_column] <- 1
       }
      } else{
          er <- try({res   <- point.in.polygon(coord[,'x'],coord[,'y'],dd[[1]][,1],dd[[1]][,2])}, silent=TRUE)
          if(class(er)=="try-error") res   <- point.in.polygon(coord[,'x'],coord[,'y'],dd[[1]][,1],dd[[1]][,2])
          coord[which(res!=0), name_column] <- 1

      }

     }
 return(coord)
 }




 totvalue_per_node$x <- coord[totvalue_per_node$pt_graph, 'x']
 totvalue_per_node$y <- coord[totvalue_per_node$pt_graph, 'y']
 totvalue_per_node <- detectCoordInPolygonsFromSH (sh=windmills, coord=totvalue_per_node, name_column="windmills")   # test inclusion
 totvalue_per_node <- detectCoordInPolygonsFromSH (sh=windmills_Anhold, coord=totvalue_per_node, name_column="windmills_Anhold")   # test inclusion

 totvalue_per_node[totvalue_per_node$windmills!=0 | totvalue_per_node$windmills_Anhold!=0 ,]
 
 
 

 library(sp)
 nb_areas <- 6
 pts <- matrix(0, ncol=nb_areas, nrow=nrow(totvalue_per_node))
 pts[,1] <- point.in.polygon(point.x=totvalue_per_node[,'x'], point.y=totvalue_per_node[,'y'],
                    pol.x=c(13.31605, 12.75543, 12.61191, 12.75543, 12.71058, 12.87204),
                     pol.y=c(55.01882, 55.20517, 55.13529, 55.01364, 54.90235, 54.84282), mode.checked=FALSE)
 pts[,2] <- point.in.polygon(point.x=totvalue_per_node[,'x'], point.y=totvalue_per_node[,'y'],
                    pol.x=c(11.56242, 11.44132, 11.51308, 11.57139),
                     pol.y=c(54.44941, 54.46494, 54.42611, 54.41576), mode.checked=FALSE)
 pts[,3] <- point.in.polygon(point.x=totvalue_per_node[,'x'], point.y=totvalue_per_node[,'y'],
                    pol.x=c(13.59861, 13.88565, 13.96189),
                     pol.y=c(54.83764, 54.90753, 54.78070), mode.checked=FALSE)
 pts[,4] <- point.in.polygon(point.x=totvalue_per_node[,'x'], point.y=totvalue_per_node[,'y'],
                    pol.x=c( 15.37101, 14.73002, 15.00666, 14.98642, 15.18883, 15.78258, 15.50595, 15.39125),
                       pol.y=c( 54.58778, 54.52937, 54.49433, 54.42424, 54.33858, 54.44760, 54.51380, 54.52158), mode.checked=FALSE)
 pts[,5] <- point.in.polygon(point.x=totvalue_per_node[,'x'], point.y=totvalue_per_node[,'y'],
                    pol.x=c( 12.20245, 12.06557, 12.02824, 12.16512),
                       pol.y=c( 56.58406, 56.68640, 56.65911, 56.53972), mode.checked=FALSE)
 pts[,6] <- point.in.polygon(point.x=totvalue_per_node[,'x'], point.y=totvalue_per_node[,'y'],
                    pol.x=c(12.38911, 12.33311, 12.29578, 12.33311),
                       pol.y=c( 56.83308, 56.86719, 56.84672, 56.80238), mode.checked=FALSE)
 dd <- apply(pts, 1, sum)
 idx_nodes_R_in_polygons <-NULL
 for(i in 1: 6){
    if(nrow(cbind(1, which(pts[,i]!=0)))>1) idx_nodes_R_in_polygons   <- rbind(
                                                                           idx_nodes_R_in_polygons,
                                                                           cbind(1, which(pts[,i]!=0))
                                                                           ) # for R and the graph object
 }
 


 totvalue_per_node$windmills[idx_nodes_R_in_polygons] <- 1
 #points(totvalue_per_node[totvalue_per_node$windmills!=0,c('x', 'y')], col="green")
 
  ##-------------------------------------------
 ##-------------------------------------------

 # then, evaluate the percentage per vessel in the inclusion
 #....




    