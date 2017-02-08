  # GENERAL SETTINGS

   args <- commandArgs(trailingOnly = TRUE)

   general <- list()

   if (length(args) < 2) {
     if(.Platform$OS.type == "windows") {
       general$application           <- "testexample" # ...or myfish
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
  

  dir.create(file.path(general$main.path.ibm, paste("firmsspe_", general$application, sep='')))




  # read
  firms_features   <-  read.table(file.path(general$main_path_gis,"FIRMS", "firms_features.csv"), sep=";", header=TRUE)
  cat(paste("Read firm specs...done\n"))

  
  some_firms_features <- firms_features
  colnames(some_firms_features) <- c('firms_id', 'firms_name', 'nb_vessels',  'long',   'lat')

  
  # write
  write.table(some_firms_features,   
            file=file.path(general$main.path.ibm, paste("firmsspe_", general$application, sep=''), 
              paste("firms_specs.dat",sep='')),
                  col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE, append=FALSE)
  cat(paste("Write firms-related files...done\n"))



  cat(paste("..........done\n"))
