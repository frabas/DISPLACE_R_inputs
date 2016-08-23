
 a_case <- "balticRTI"
 
  # GENERAL SETTINGS
  if(a_case=="balticRTI"){
     general <- list()
     general$application           <- "balticRTI" # ...or myfish
     general$igraph                <- 56
     general$main.path.param       <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_raw")
     general$main.path.param.gis   <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis", general$application)
     general$main.path.ibm         <- file.path("C:","Users","fbas","Documents","GitHub", paste("DISPLACE_input_" , general$application, sep=""))
      
     general$case_study_countries  <- c("DEN", "SWE", "DEU")   
     general$a.year                <- "2015"
     }
     

  dir.create(file.path(general$main.path.ibm, paste("harboursspe_", general$application, sep='')))


  ### build quick and dirty fake harbour files

  port_names <- read.table(file=file.path(general$main.path.param.gis,  "GRAPH",
                                  paste("harbours.dat", sep='')), sep=";")
 

 #load
  coord <- read.table(file=file.path(general$main.path.param.gis, "GRAPH", paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
  dd    <- coord
  coord <- as.matrix(as.vector(coord))
  coord <- matrix(coord, ncol=3)
  colnames(coord) <- c('x', 'y', 'harb')
  plot(coord[,1], coord[,2])


 
  graph <- read.table(file=file.path(general$main.path.param.gis, "GRAPH", paste("graph", general$igraph, ".dat", sep=""))) # build from the c++ gui
  graph <- as.matrix(as.vector(graph))
  graph <- matrix(graph, ncol=3)
  segments(coord[graph[,1]+1,1], coord[graph[,1]+1,2], coord[graph[,2]+1,1], coord[graph[,2]+1,2], col=4) # CAUTION: +1, because c++ to R



  ##-------------
  dep <-  ((nrow(dd)/3)+1) -nrow(port_names)
  idx <-  dep : (nrow(dd)/3)   # all idx nodes for ports

  idx <- idx - 1 ##!!## CAUTION OFFSET BY 1 in C++ ##!!##
 
 
  ##------------------------
  
  # stock names
  spp_table <-  read.table(file=file.path(general$main.path.param.gis, "POPULATIONS", paste("pop_names_",general$application ,".txt",sep='')),
              header=TRUE)
  spp                        <- as.character(spp_table$spp)

  
  
  ##------------------------
  # obtain a c++ multimap with stock / price for cat
  prices_per_szcat <-  read.table(file=file.path(general$main.path.param.gis, "POPULATIONS", paste("DISPLACE_datainput_prices_per_category.csv")), sep=',',
              header=TRUE)



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
 
}
                                 

 write.table(cbind(idx,  sapply(rownames(port_names), function (x) paste(unlist(strsplit(x, " ")), collapse="_"))    ),
    file= file.path(general$main.path.ibm, paste("harboursspe_", general$application, sep=''),  "names_harbours.dat"), row.names=FALSE, col.names=FALSE, quote=FALSE)

