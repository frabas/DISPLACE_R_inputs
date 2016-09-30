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
  

  dir.create(file.path(general$main.path.ibm, paste("shipsspe_", general$application, sep='')))


  # read
   shipsspe_features   <-  read.table(file.path(general$main_path_gis,"SHIPPING", "shipsspe_features.csv"), sep=";", header=TRUE)
   shipsspe_lanes_lat  <-  read.table(file.path(general$main_path_gis,"SHIPPING", "shipsspe_lanes_lat.csv"), sep=";", header=TRUE)
   shipsspe_lanes_lon  <-  read.table(file.path(general$main_path_gis,"SHIPPING", "shipsspe_lanes_lon.csv"), sep=";" , header=TRUE)
   cat(paste("Read shipping specs...done\n"))

  # write
  write.table(shipsspe_features,   
            file=file.path(general$main.path.ibm, paste("shipsspe_", general$application, sep=''), 
              paste("shipsspe_features.dat",sep='')),
                  col.names=FALSE,  row.names=FALSE, sep= '|', quote=FALSE, append=FALSE)
  write.table(shipsspe_lanes_lat,   
            file=file.path(general$main.path.ibm, paste("shipsspe_", general$application, sep=''), 
              paste("shipsspe_lanes_lat.dat",sep='')),
                  col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE, append=FALSE)
  write.table(shipsspe_lanes_lon,   
            file=file.path(general$main.path.ibm, paste("shipsspe_", general$application, sep=''), 
              paste("shipsspe_lanes_lon.dat",sep='')),
                  col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE, append=FALSE)
   cat(paste("Write shipping-related files...done\n"))




  cat(paste("..........done\n"))
