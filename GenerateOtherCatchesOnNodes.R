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
       general$igraph                <- args[4]  # caution: should be consistent with existing vessels already built upon a given graph
       do_plot                       <- FALSE
  }
   cat(paste("START \n"))



   
   # caution fleet sce
   fleetsce <-  data.frame(sce=1, namesce=c('baseline'))
   
    write.table(fleetsce, quote=FALSE,
                 file=file.path(general$main.path.ibm, paste("multiplier_for_fleetsce", general$application,".dat",sep='')), append=FALSE,
                   row.names=FALSE, col.names=TRUE)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# load
catches_in_tons <- read.table(file.path(general$main_path_gis, "OTHERS", "othercatchespercountry.csv"), sep=";", header=TRUE)  # in tons
   cat(paste("Read the specs othercatchespercountry.csv\n"))

popnames <- as.character(catches_in_tons$POP)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


lst_layers <- list.files (file.path(general$main_path_gis, "POPULATIONS", "SpatialLayers"))  


  for(sce in fleetsce$sce){
  for (popid in 1: length(catches_in_tons$POP)){
 
     a_file <-  lst_layers [ grep(paste("contour",popid-1,"_large", sep=''), lst_layers) ]
     a_file <-  a_file[grep(".shp", a_file)]
     name_gis_layer_field <- "GRIDCODE"
     
     # get the spatial distribution of the stock that will obviously constraint where the depletion can actually occur
     library(maptools)
     handmade_WGS84            <- readShapePoly(file.path(general$main_path_gis, "POPULATIONS", "SpatialLayers", a_file ) , proj4string=CRS("+proj=longlat +datum=WGS84"))  # build in ArcGIS 10.1
 
     #load the locations of the graph
     coord <- read.table(file=file.path(general$main_path_gis, "GRAPH", paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
     coord <- as.matrix(as.vector(coord))
     coord <- matrix(coord, ncol=3)
     colnames(coord) <- c('x', 'y', 'harb')
     if(do_plot) plot(coord[,1], coord[,2])
     coord <- cbind(coord, pt_graph=1:nrow(coord))
 
  
     graph <- read.table(file=file.path(general$main_path_gis, "GRAPH", paste("graph", general$igraph, ".dat", sep=""))) # build from the c++ gui
     graph <- as.matrix(as.vector(graph))
     graph <- matrix(graph, ncol=3)
     if(do_plot) segments(coord[graph[,1]+1,1], coord[graph[,1]+1,2], coord[graph[,2]+1,1], coord[graph[,2]+1,2], col=4) # CAUTION: +1, because c++ to R
    
     
     # test coord for polygon inclusion
     spo                          <- SpatialPoints(coordinates(data.frame(CELL_LONG=coord[,1],
                                             CELL_LATI=coord[,2])))
     proj4string(spo)              <-  CRS("+proj=longlat +datum=WGS84")
     idx                          <- over(spo, handmade_WGS84, returnList=FALSE)  # idx in handmade_WGS84_reduced
     coord                        <- cbind(coord, idx[,name_gis_layer_field])
     colnames(coord)[ncol(coord)] <- name_gis_layer_field

     coord <- coord[!is.na(coord [,name_gis_layer_field]),]
 
 
    # if there are some locations included then....
    if(nrow(coord)!=0){

       # retrieve the 'other' landings...
       total_catches_this_year_in_kg <- sum(catches_in_tons[catches_in_tons$POP==popnames[popid], -c(1:2)], na.rm=TRUE)
    
       # ...and dispatch (just an assumption, one can do otherwise provided the data format is respected)
       coord <- cbind(coord, landings= total_catches_this_year_in_kg/nrow(coord)/12) # e.g. catches are evenly dispatched among the relevant nodes and given 12 months

       # save .dat files
       coord[,'pt_graph'] <-  as.numeric(as.character(coord[,'pt_graph'])) - 1 ##!!! OFFSET FOR C++ !!!##    because R to c++
       library(doBy)
       coord<- orderBy(~pt_graph, data=coord)
    } else{
       coord <- matrix(c(0,0),ncol=2,nrow=2, dimnames=list(c(0,1),c('pt_graph', 'landings')))
    }


    # save and export to multimap c++ for this pop
    # assuming for the testexample that the same amount in kg is depleted each month on the same location
   for (month in c("1", "2", "3","4", "5", "6", "7", "8", "9", "10", "11", "12")){

              # save for a multimap in c++  pt_graph / landings kg per month (to be used every months)
            # to fill in the pop attribute
        write.table(round(coord[, c('pt_graph', 'landings')]),
               file= file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=''),
                   paste((popid)-1, 'spe_stecf_oth_land_per_month_per_node_month', month, "_fleetsce",sce,".dat", sep='')), 
                 row.names=FALSE, col.names=TRUE, quote=FALSE)

        cat(paste("Write ", (popid)-1, 'spe_stecf_oth_land_per_month_per_node_month', month, ".dat\n", sep=''))


      }


    } # end popid
    } # end sce

  cat(paste(".....done \n"))
