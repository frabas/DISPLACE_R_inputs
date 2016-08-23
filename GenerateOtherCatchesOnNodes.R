

# some args for the bunch of vessels to be created....
 # GENERAL SETTINGS
   general <- list()
   if(.Platform$OS.type == "windows") {
     general$main.path             <- file.path("C:","DISPLACE_outputs")
     general$application           <- "balticRTI" # ...or myfish
     general$igraph                <- 56
     general$main.path.param       <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_raw")
     general$main.path.param.gis   <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis", general$application)
     general$main.path.ibm         <- file.path("C:","Users","fbas","Documents","GitHub", paste("DISPLACE_input_" , general$application, sep=""))
   }


  dir.create(file.path(general$main.path.ibm, paste("popsspe_", general$application, sep='')))



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# load
catches_in_tons <- read.table(file.path(general$main.path.param.gis, "OTHERS", "othercatchespercountry.csv"), sep=",", header=TRUE)  # in tons

popnames <- as.character(catches_in_tons$POP)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


lst_layers <- list.files (file.path(general$main.path.param.gis, "POPULATIONS", "SpatialLayers"))  


for (popid in 1: length(catches_in_tons$POP)){
 
     a_file <-  lst_layers [ grep(paste("contour",popid-1,"_large", sep=''), lst_layers) ]
     a_file <-  a_file[grep(".shp", a_file)]
     name_gis_layer_field <- "GRIDCODE"
     
     library(maptools)
     handmade_WGS84            <- readShapePoly(file.path(general$main.path.param.gis, "POPULATIONS", "SpatialLayers", a_file ) , proj4string=CRS("+proj=longlat +datum=WGS84"))  # build in ArcGIS 10.1
 
     #load
     coord <- read.table(file=file.path(general$main.path.param.gis, "GRAPH", paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
     coord <- as.matrix(as.vector(coord))
     coord <- matrix(coord, ncol=3)
     colnames(coord) <- c('x', 'y', 'harb')
     plot(coord[,1], coord[,2])
     coord <- cbind(coord, pt_graph=1:nrow(coord))
 
  
     graph <- read.table(file=file.path(general$main.path.param.gis, "GRAPH", paste("graph", general$igraph, ".dat", sep=""))) # build from the c++ gui
     graph <- as.matrix(as.vector(graph))
     graph <- matrix(graph, ncol=3)
     segments(coord[graph[,1]+1,1], coord[graph[,1]+1,2], coord[graph[,2]+1,1], coord[graph[,2]+1,2], col=4) # CAUTION: +1, because c++ to R
    
     
     # test coord for polygon inclusion
     spo                          <- SpatialPoints(coordinates(data.frame(CELL_LONG=coord[,1],
                                             CELL_LATI=coord[,2])))
     projection(spo)              <-  CRS("+proj=longlat +datum=WGS84")
     idx                          <- over(spo, handmade_WGS84, returnList=FALSE)  # idx in handmade_WGS84_reduced
     coord                        <- cbind(coord, idx[,name_gis_layer_field])
     colnames(coord)[ncol(coord)] <- name_gis_layer_field

     coord <- coord[!is.na(coord [,name_gis_layer_field]),]
 
 
    if(nrow(coord)!=0){

       total_catches_this_year_in_kg <- sum(catches_in_tons[catches_in_tons$POP==popnames[popid], -c(1:2)], na.rm=TRUE)
    
       # dispatch
       coord <- cbind(coord, landings= total_catches_this_year_in_kg/nrow(coord)/12) # catches are evenly dispatched among the relevant nodes and given 12 months

       # save .dat files
       coord[,'pt_graph'] <-  as.numeric(as.character(coord[,'pt_graph'])) - 1 ##!!! OFFSET FOR C++ !!!##
       library(doBy)
       coord<- orderBy(~pt_graph, data=coord)
    } else{
       coord <- matrix(c(0,0),ncol=2,nrow=2, dimnames=list(c(0,1),c('pt_graph', 'landings')))
    }


    # save and export to multimap c++ for this pop
   for (semester in c("1", "2")){

              # save for a multimap in c++  pt_graph / landings kg per month (to be used every months)
            # to fill in the pop attribute
        write.table(round(coord[, c('pt_graph', 'landings')]),
               file= file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=''),
                  paste((popid)-1, 'spe_stecf_oth_land_per_month_per_node_semester', semester, ".dat", sep='')),
                 row.names=FALSE, col.names=TRUE, quote=FALSE)



      }


    } # end popid
