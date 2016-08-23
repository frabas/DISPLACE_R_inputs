   # GENERAL SETTINGS
   general <- list()
   if(.Platform$OS.type == "windows") {
     general$main.path             <- file.path("C:","DISPLACE_outputs")
     general$application           <- "adriatic" # ...or myfish
     general$main.path.param       <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_raw")
     general$main.path.param.gis   <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis", general$application)
     general$main.path.ibm         <- file.path("C:","Users","fbas","Documents","GitHub", paste("DISPLACE_input_" , general$application, sep=""))
   }




     general$igraph <- 2

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-----utils---------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
 # loop over the SpatialPoly

 # loop over the SpatialPoly
 detectingCoordInPolygonsFromSH <- function (sh, coord, name_column="poly"){

     coord <- eval(parse(text=paste("cbind(coord, ",name_column,"= 0)", sep='')))
     dd                   <- sapply(slot(sh, "polygons"), function(x) lapply(slot(x, "Polygons"), function(x) x@coords)) # tricky there...

     ids <- sapply(slot(sh, "polygons"),  function (x) x@ID)

     library(sp)
     for(iLand in 1:length(dd)){
      if(length(dd)>1){
       for(i in 1:length(dd[[iLand]])){
          print(iLand)
          # Points on land are 1 or 2 and not is 0
          er <- try({res   <- point.in.polygon(coord[,1],coord[,2],dd[[iLand]][[i]][,1],dd[[iLand]][[i]][,2])}, silent=TRUE)
          if(class(er)=="try-error") res   <- point.in.polygon(coord[,1],coord[,2],dd[[iLand]][,1],dd[[iLand]][,2])
          coord[which(res!=0), name_column] <- ids[iLand]
       }
      } else{
          er <- try({res   <- point.in.polygon(coord[,1],coord[,2],dd[[1]][,1],dd[[1]][,2])}, silent=TRUE)
          if(class(er)=="try-error") res   <- point.in.polygon(coord[,1],coord[,2],dd[[1]][,1],dd[[1]][,2])
          coord[which(res!=0), name_column] <- ids[iLand]

      }

     }
 return(coord)
 }

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# adriatic specific info
popids <- 1:4
popnames <- c("Hake_medium", "Sole_medium", "Mullet_medium", "Spottailmantis_medium")
country <- "Croatia"

catches_in_tons <- read.table(file.path(general$main.path.param.gis, "OTHERS", "otherscatchespercountry.txt"), sep=";", header=TRUE)  # in tons

for (popid in popids){

     # pop specific info
     name_gis_file_for_total_abundance_per_polygon <-   paste(popnames[popid],"_", country, sep='')
     name_gis_layer_field <- "DEPTH"
     total_catches_this_year_in_kg <- catches_in_tons[catches_in_tons$pop==popnames[popid], country] *1000 # convert in kg




     # load the graph
     #load(file.path(general$main.path.igraph, paste(general$igraph, "_graphibm.RData",sep=''))) # built from the R code
     coord <- read.table(file=file.path(general$main.path.ibm, "graphsspe",
             paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
     coord <- as.matrix(as.vector(coord))
     coord <- matrix(coord, ncol=3)
     coord <- cbind(coord, 1:nrow(coord))
     colnames(coord) <- c('x', 'y', 'harb', 'pt_graph')
     plot(coord[,1], coord[,2])

     graph <- read.table(file=file.path(general$main.path.ibm, "graphsspe",
           paste("graph", general$igraph, ".dat", sep=""))) # build from the c++ gui
     graph <- as.matrix(as.vector(graph))
     graph <- matrix(graph, ncol=3)
     segments(coord[graph[,1]+1,1], coord[graph[,1]+1,2], coord[graph[,2]+1,1], coord[graph[,2]+1,2], col=4) # CAUTION: +1, because c++ to R



     # Overlay the shp to the graph
     library(maptools)
     handmade_WGS84            <- readShapePoly(file.path(general$main.path.param.gis, "OTHERS", name_gis_file_for_total_abundance_per_polygon) ,
                                                      proj4string=CRS("+proj=longlat +datum=WGS84"))  # ArcGIS 10.1

    if(FALSE){ # in case not latlong but projected data instead.....
    library(rgdal)
    handmade2           <- readOGR(file.path(general$main.path.param.gis, "MANAGEMENT", name_gis_file_for_total_abundance_per_polygon) ) #  Projection info in a .prj associated with the shp should be imported automagically.
   # How can I get the proj4 string from a shapefile .prj file? http://gis.stackexchange.com/questions/55196/how-can-i-get-the-proj4-string-or-epsg-code-from-a-shapefile-prj-file
    library(sp)
    library(rgdal)
    handmade_WGS84 <- spTransform(handmade2, CRS("+proj=longlat +datum=WGS84"))    # convert to longlat
    }

    names(handmade_WGS84)  # "ID"         name_gis_layer_field



    # test coord for polygon inclusion
    coord <-  detectingCoordInPolygonsFromSH (handmade_WGS84, coord, name_column="poly_id")
    points(coord[,1], coord[,2], col=as.numeric(coord[,"poly_id"])+1)  # check


    handmade_WGS84_df <- as.data.frame(handmade_WGS84)

    # debug...
    handmade_WGS84_df$ID <- as.character(handmade_WGS84_df$ID)
    handmade_WGS84_df$ID <- 1:length(handmade_WGS84_df$ID)

    # then merge to coord  (caution: 'poly' give the polygon in the harbour range; 'poly_id' give the polygon id from the handmade_WGS84 shape file)
    coord<- merge(coord, handmade_WGS84_df, by.x="poly_id", by.y="ID")

    if(nrow(coord)!=0){

    # dispatch
    coord$landings <- total_catches_this_year_in_kg/nrow(coord) # catches are evenly dispatched among the relevant nodes

     # save .dat files
    coord$pt_graph <-  as.numeric(as.character(coord$pt_graph)) - 1 ##!!! OFFSET FOR C++ !!!##
    coord<- orderBy(~pt_graph, data=coord)
    } else{
    coord <- matrix(c(0,0),ncol=2,nrow=2, dimnames=list(c(0,1),c('pt_graph', 'landings')))
    }


    # save and export to multimap c++ for this pop
   for (semester in c("1", "2")){

              # save for a multimap in c++  pt_graph / landings kg per month (to be used every months)
            # to fill in the pop attribute
        write.table(round(coord[, c('pt_graph', 'landings')]),
               file= file.path(general$main.path.param, "popsspe",
                  paste((popid)-1, 'spe_stecf_oth_land_per_month_per_node_semester', semester, ".dat", sep='')),
                 row.names=FALSE, col.names=TRUE, quote=FALSE)



      }


    } # end popid
