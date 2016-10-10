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
  

  dir.create(file.path(general$main.path.ibm, paste("harboursspe_", general$application, sep='')))


  ### build quick and dirty fake harbour files

  port_names <- read.table(file=file.path(general$main_path_gis,  "GRAPH",
                                  paste("harbours.dat", sep='')), sep=";", row.names=NULL, header=TRUE)
  cat(paste("Read port names...done\n"))
 
  # a quick check of consistent port_names file
  max_idx <- max(port_names$idx_port)
  c(1:max_idx)[ ! c(1:max_idx) %in% port_names$idx.port ]  # should return integer(0)


 #load
  coord <- read.table(file=file.path(general$main_path_gis, "GRAPH", paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
  dd    <- coord
  coord <- as.matrix(as.vector(coord))
  coord <- matrix(coord, ncol=3)
  colnames(coord) <- c('x', 'y', 'harb')
  if(do_plot) plot(coord[,1], coord[,2])

  graph <- read.table(file=file.path(general$main_path_gis, "GRAPH", paste("graph", general$igraph, ".dat", sep=""))) # build from the c++ gui
  graph <- as.matrix(as.vector(graph))
  graph <- matrix(graph, ncol=3)
  if(do_plot) segments(coord[graph[,1]+1,1], coord[graph[,1]+1,2], coord[graph[,2]+1,1], coord[graph[,2]+1,2], col=4) # CAUTION: +1, because c++ to R
  
  cat(paste("Read graph...done\n"))



  ##-------------
  dep <-  ((nrow(dd)/3)+1) -nrow(port_names)
  idx <-  dep : (nrow(dd)/3)   # all idx nodes for ports

  idx <- idx - 1 ##!!## CAUTION OFFSET BY 1 in C++ ##!!##
 
 
  ##------------------------
  
  # stock names
  spp_table <-  read.table(file=file.path(general$main_path_gis, "POPULATIONS",
                                    paste("pop_names_",general$application,".txt",sep='')), header=TRUE)
  spp                        <- as.character(spp_table$spp)

  cat(paste("Read stock names...done\n"))
 
  
  ##------------------------
  # obtain a c++ multimap with stock / price for cat
  prices_per_szcat <-  read.table(file=file.path(general$main_path_gis, "POPULATIONS", paste("Stock_prices_data.csv")), sep=';',
              header=TRUE)
  #head(prices_per_szcat)
  #stock small medium large  
 #COD.2532   0.5      1     2 
 #COD.2224   0.5      1     2 
 #FLE.2223   0.5      1     2 
 #FLE.2425   0.5      1     2 
 #PLE.2123   0.5      1     2 
 #PLE.2432   0.5      1     2
 cat(paste("Read stock prices per commercial category...done\n"))


for (i in idx){
 prices_per_species_per_cat <- prices_per_szcat [, 2:4]  # price in euro per kilo for three size (small, medium, and large) 
              # => ......default
 
 if(i==i) prices_per_species_per_cat <- prices_per_species_per_cat
 
 
 write.table(cbind(stock= rep(0:(length(spp)-1), each=3), price_per_cat= c(t(as.matrix(prices_per_species_per_cat))) ), 
                file= file.path(general$main.path.ibm, paste("harboursspe_", general$application, sep=''), paste(i,"_quarter1_each_species_per_cat.dat",sep="")), row.names=FALSE, quote=FALSE)
 write.table(cbind(stock= rep(0:(length(spp)-1), each=3), price_per_cat= c(t(as.matrix(prices_per_species_per_cat))) ), 
                file= file.path(general$main.path.ibm, paste("harboursspe_", general$application, sep=''), paste(i,"_quarter2_each_species_per_cat.dat",sep="")), row.names=FALSE, quote=FALSE)
 write.table(cbind(stock= rep(0:(length(spp)-1), each=3), price_per_cat= c(t(as.matrix(prices_per_species_per_cat))) ), 
                file= file.path(general$main.path.ibm, paste("harboursspe_", general$application, sep=''), paste(i,"_quarter3_each_species_per_cat.dat",sep="")), row.names=FALSE, quote=FALSE)
 write.table(cbind(stock= rep(0:(length(spp)-1), each=3), price_per_cat= c(t(as.matrix(prices_per_species_per_cat))) ), 
                file= file.path(general$main.path.ibm, paste("harboursspe_", general$application, sep=''), paste(i,"_quarter4_each_species_per_cat.dat",sep="")), row.names=FALSE, quote=FALSE)
 
 cat(paste("Write ", i,"_quarterXX_each_species_per_cat.dat in /harboursspe...done\n"))

}
                                 

 write.table(cbind(node=idx,  name=sapply(rownames(port_names), function (x) paste(unlist(strsplit(x, " ")), collapse="_"))    ),
    file= file.path(general$main.path.ibm, paste("harboursspe_", general$application, sep=''),  "names_harbours.dat"), row.names=FALSE, col.names=TRUE, quote=FALSE)

 cat(paste("Write names_harbours.dat in /harboursspe...done\n"))


  cat(paste("..........done\n"))
