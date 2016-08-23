## IBM parametrisation
## Francois Bastardie (DTU-Aqua)
## outputs: mainly .dat files to be used for the IBM simulations

 # GENERAL SETTINGS
  general <- list()
  general$main.path             <- file.path("C:", "Users", "fbas", "Documents", "GitHub", "DISPLACE_input_raw")
  general$main.path.code        <- file.path("C:", "Users", "fbas", "Documents", "GitHub", "DISPLACE_R_inputs")
  general$main_path_input       <- file.path("C:", "Users", "fbas", "Documents", "GitHub", "DISPLACE_input")

  general$igraph                <- 11
  general$case_study            <- "baltic_only"
  general$case_study_countries  <- c("DEN", "SWE", "DEU")    # for the Baltic only
  general$a.year                <- "2012"
  general$a.country             <- "DEN"
  #general$a.country             <- "DEU"
  #general$a.country             <- "SWE"


  general$igraph                <- 56
  general$case_study            <- "myfish"
  general$case_study_countries  <- c("DEN")    # for the Baltic only
  general$a.year                <- "2012"
  general$a.country             <- "DEN"
 

  # mkdir
  dir.create(path=file.path(general$main.path, "merged_tables", general$case_study),
                      showWarnings = TRUE, recursive = TRUE, mode = "0777")



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

  # potential inputs to DISPLACE_GUI
  write.table(x.fishing, file=file.path(general$main.path,"merged_tables", general$case_study,
           paste("all_merged_weight_on_fishing_pings_",general$a.country,"_",general$a.year,".txt",sep='')), sep=";", quote=FALSE, row.names=FALSE, col.names=TRUE)
  write.table(x.inharb, file=file.path(general$main.path,"merged_tables", general$case_study,
           paste("all_merged_pings_in_harbours_",general$a.country,"_",general$a.year,".txt",sep='')), sep=";", quote=FALSE, row.names=FALSE, col.names=TRUE)



 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!FIND THE CLOSED IGRAPH NODE FOR EACH FISHING PING!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

  # load the graph
  #load(file.path(general$main.path, "igraph", paste(general$igraph, "_graphibm.RData",sep=''))) # built from the R code
  coord <- read.table(file=file.path(general$main_path_input, "graphsspe", paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
  coord <- as.matrix(as.vector(coord))
  coord <- matrix(coord, ncol=3)
  colnames(coord) <- c('x', 'y', 'idx.port')
  #plot(coord[,1], coord[,2])


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
    harbour_list      <- read.table(file=file.path(general$main.path, "harbours.dat"), sep=";")
    x.inharb$idx_port <- harbour_list$idx.port [match(x.inharb$land_harb, rownames(harbour_list)) ]  # translate name into idx_port from coord
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




    if(general$case_study=="myfish"){
        # keep all vessels on board (for ensuring further use of the same parameterisation)....
        # but remenber we are only interested at looking to Baltic cod fisheries in the outcomes
        # so we will need to get rid of irrelevant vessels in the simulator output files before tabulating/plotting
    vid_this_case_study <- as.character(unique(ping.fgrounds$VE_REF))
    }  # end case_study


   ####-------
   ####-------
   ####-------
   ####-------
    save(ping.fgrounds,  file=file.path(general$main.path, "merged_tables", general$case_study,
             paste("ping.fgrounds.",general$a.country,".",general$a.year,".igraph",general$igraph,".RData", sep='')))
      cat(paste("save 'ping.fgrounds', this year...OK\n\n",sep=""))
    save(ping.harbours,  file=file.path(general$main.path, "merged_tables", general$case_study,
             paste("ping.harbours.",general$a.country,".",general$a.year,".igraph",general$igraph,".RData", sep='')))
      cat(paste("save 'ping.harbours', this year...OK\n\n",sep=""))
    save(vid_this_case_study,  file=file.path(general$main.path, "merged_tables", general$case_study,
             paste("vid_this_case_study_",general$a.country,".",general$a.year,".igraph",general$igraph,".RData", sep='')))
      cat(paste("save 'vid_this_case_study', this year...OK\n\n",sep=""))



   #...and slightly modify the name of metiers and save metier names
   ping.fgrounds$LE_MET_level6 <- factor(ping.fgrounds$LE_MET_level6)
   ping.fgrounds$LE_MET_level6 <- as.factor(unlist(lapply(strsplit( as.character(ping.fgrounds$LE_MET_level6), split="_"),
                                  function(x) paste(x[1],'_',x[2],sep=''))))   # remove mesh size and useless selective grid info

   metier_names <- cbind(levels(ping.fgrounds$LE_MET_level6), 0:(length(levels(ping.fgrounds$LE_MET_level6))-1))
   save(metier_names,  file=file.path(general$main.path, "merged_tables", general$case_study, ## SAVE
                     paste("metier_names.",general$a.country,".",general$a.year,".igraph",
                      general$igraph,".RData", sep='')))
   write.table(metier_names, file.path(general$main.path, "metiersspe", paste("metier_names_",general$a.country,"_",general$a.year,".txt", sep='')),
               quote=FALSE, col.names=FALSE, row.names=FALSE)

   