  # GENERAL SETTINGS

   args <- commandArgs(trailingOnly = TRUE)

   general <- list()

   if (length(args) < 2) {
     if(.Platform$OS.type == "windows") {
       general$application           <- "balticRTI" # ...or myfish
       general$main_path_gis         <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis", general$application)
       general$main.path.ibm         <- file.path("C:","Users","fbas","Documents","GitHub",paste("DISPLACE_input_", general$application, sep=''))
       general$igraph                <- 56  # caution: should be consistent with existing objects already built upon a given graph
       do_plot                       <- TRUE 
     }
  } else {
       general$application           <- args[1]
       general$main_path_gis         <- args[2]
       general$main.path.ibm         <- args[3]
       general$igraph                <- args[4]  # caution: should be consistent with existing objects already built upon a given graph
       do_plot                       <- FALSE 
  }
  cat(paste("START\n"))
  

  dir.create(file.path(general$main.path.ibm, paste("fishfarmsspe_", general$application, sep='')))

  
   # read
   fishfarms_features   <-  read.table(file.path(general$main_path_gis,"FISHFARMS", "fishfarms_features.csv"), sep=";", header=TRUE)
  cat(paste("Read fishfarms specs...done\n"))

   # find the nearest graph node
   cat(paste("Finding neighbours....\n"))
   library(spatstat)
   an <- function(x)  as.numeric(as.character(x))
   coord_f            <- coord[coord[,'harb']==0,]  
    X                 <- ppp(x=an(fishfarms_features$long), y=an(fishfarms_features$lat),
                         xrange=range(an(fishfarms_features$long)), yrange=range(an(fishfarms_features$lat)))
    Y                 <- ppp(x=coord_f[,"x"], y=coord_f[,"y"],
                                xrange=range(coord_f[,"x"]), yrange=range(coord_f[,"y"]))
    N                 <- nncross (X=X, Y=Y)$which # caution: just euclidean distance on coord
    fishfarms_features <- cbind(fishfarms_features, pt_graph= N)

  
  some_fishfarms_features           <- fishfarms_features[,c('pt_graph', 'size_km2')] 
  colnames(some_fishfarms_features) <- c('idx_node', 'size_km2')
  
  
  # write
  write.table(some_fishfarms_features,   
            file=file.path(general$main.path.ibm, paste("fishfarmsspe_", general$application, sep=''), 
              paste("size_per_farm.dat",sep='')),
                  col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE, append=FALSE)
  cat(paste("Write fishfarms-related files...done\n"))



  cat(paste("..........done\n"))
