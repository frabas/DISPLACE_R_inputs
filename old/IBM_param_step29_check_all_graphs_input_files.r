   
  # GENERAL SETTINGS
  general <- list()
  general$main.path      <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_raw")
  general$main.path.code <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_R_inputs")
    

   
   
   
   
   
  # :::::::::::::TO DO!!
  correct to remove the x at the begining of the file for
  code_area_for_graph_points_graph






  ## check for NAs in .dat files in harboursspe
     lst_files <- list.files (file.path(general$main.path, "harboursspe"))
     for (fi in lst_files){
        er <- try(dd <- read.table(file=file.path(general$main.path,"harboursspe", fi), header=FALSE), silent=TRUE)
        if(class(er)=="try-error"){
         print(fi)
         print(er[[1]])
        }else{
         #print(head(dd,2))
         if( any(is.na(dd)) ) cat(paste("NAs found in ",fi,"......\n"))
         if( any(dd=="Inf") ) cat(paste("Inf found in ",fi,"......\n"))
         }
     }

  
  
    graph <- read.table("C:\\displace-project.org\\repository\\ibm_vessels_input\\graphsspe\\graph111.dat")
    graph <- as.matrix(graph)
    graph <- matrix(graph, ncol=3)
    dd <- unique(c(graph[graph[,3]>900, c(1,2)])) # should be nodes in polygons
    dd
    
    
    nodes_in_polygons <- read.table("C:\\displace-project.org\\repository\\ibm_vessels_input\\graphsspe\\nodes_in_polygons_a_graph111_quarter1.dat", header=TRUE)
    dd2 <-  nodes_in_polygons[,2]
    
    nodes_in_polygons[nodes_in_polygons[,1]=="11",2] %in% dd

   
    coord111 <- read.table("C:\\displace-project.org\\repository\\ibm_vessels_input\\graphsspe\\coord111.dat", header=TRUE)
      coord111 <- as.matrix(coord111)
    coord111 <- matrix(coord111, ncol=3)
     ddd<- coord111[dd,]
     points(ddd[,1],ddd[,2], pch=".")
     ddd2<- coord111[dd2,]
     points(ddd2[,1],ddd2[,2], pch=".", col="2")
     
     
    
    # a posteriori correction for graph111 !!!
    idx <- (graph[,1] %in% nodes_in_polygons[,2] | graph[,2] %in% nodes_in_polygons[,2]) & graph[,3]<900
    graph[idx,]
    graph[idx, 3] <-  graph[idx, 3]+900 
    graph[idx,]
    graph_c <- matrix(graph, ncol=1) 
    write.table(graph_c, file="C:\\displace-project.org\\repository\\ibm_vessels_input\\graphsspe\\graph111.dat", row.names=FALSE, col.names=FALSE)
   
     
   
    graph <- read.table("C:\\displace-project.org\\repository\\ibm_vessels_input\\graphsspe\\graph112.dat")
    graph <- as.matrix(graph)
    graph <- matrix(graph, ncol=3)
    dd <- unique(c(graph[graph[,3]>900, c(1,2)])) # should be nodes in polygons
    dd
    
    
    nodes_in_polygons <- read.table("C:\\displace-project.org\\repository\\ibm_vessels_input\\graphsspe\\nodes_in_polygons_a_graph112_quarter1.dat", header=TRUE)
    nodes_in_polygons[,2]
    
    nodes_in_polygons[nodes_in_polygons[,1]=="11",2] %in% dd

