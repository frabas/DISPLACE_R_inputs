   
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
 
  
   path      <- file.path(general$main.path.param.gis, "POPULATIONS", "pops_config_files")
   namefiles <- list.files(file.path( path))

 
  for (a_file in namefiles){
 
   dat  <- readLines(file.path(path, a_file))
   
   my_split <- function(x) unlist(strsplit(x, " "))
   my_split2 <- function(x) unlist(strsplit(x, "_"))
   
   general <- list()
   general$main.path.param.gis   <- as.character(dat[5])
   general$main.path.param       <- as.character(dat[7])
   general$main.path.ibm         <- as.character(dat[9])                                      
   general$application           <- as.character(dat[11])  
   general$igraph                <- as.numeric(dat[13])  
  
   dir.create(file.path(general$main.path.ibm, paste("popsspe_", general$application, sep='')))
   dir.create(file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=''), "static_avai"))

 
                                  
   do_append                     <- as.logical(dat[15])
   name_gis_file_for_total_abundance_per_polygon <- my_split(dat[17])
   name_gis_layer_field                        <- dat[19]
   is_gis_layer_field_relative_numbers         <- dat[21]
   xfold_gis_layer_field                        <- as.numeric(my_split(dat[23]))  
   popids                     <- as.character(my_split(dat[25]))
   if(length(my_split2(dat[27]))>1){
    szgroups                   <- sapply(my_split2(dat[27]), my_split) # return a list()
   }else{
    szgroups                   <- list(as.character(my_split(dat[27])))  # return a list()
    }
   selected_szgroups          <-  as.character(my_split(dat[29]))

   # quick check
   if(length(name_gis_file_for_total_abundance_per_polygon) != length(szgroups)) stop("Need for same number of GIS layers and sets of size groups....")




#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-----utils---------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

 # loop over the SpatialPoly
 detectingCoordInPolygonsFromSH <- function (sh, coord, name_column="poly"){

     coord <- eval(parse(text=paste("cbind(coord, ",name_column,"= 0)", sep='')))
     dd                   <- sapply(slot(sh, "polygons"), function(x) lapply(slot(x, "Polygons"), function(x) x@coords), simplify=FALSE) # tricky there...
     if(length(dd)==1) dd <- dd[[1]]   # debug if only one level of polygons
 
     ids <- sapply(slot(sh, "polygons"),  function (x) x@ID)
     
     library(sp)
     for(iLand in 1:length(dd)){
      if(length(dd)>1){
       for(i in 1:length(dd[[iLand]])){
          print(iLand)
          # Points on land are 1 or 2 and not is 0
          er <- try({res   <- point.in.polygon(coord[,1],coord[,2],dd[[iLand]][[i]][,1],dd[[iLand]][[i]][,2])}, silent=TRUE)
          if(class(er)=="try-error") res   <- point.in.polygon(coord[,1],coord[,2],dd[[iLand]][,1],dd[[iLand]][,2])
          if(length(ids)==1) coord[which(res!=0), name_column] <- 1 else  coord[which(res!=0), name_column] <- ids[iLand]
       }
      } else{
          er <- try({res   <- point.in.polygon(coord[,1],coord[,2],dd[[1]][,1],dd[[1]][,2])}, silent=TRUE)
          if(class(er)=="try-error") res   <- point.in.polygon(coord[,1],coord[,2],dd[[1]][,1],dd[[1]][,2])
          if(length(ids)==1) coord[which(res!=0), name_column] <- 1 else  coord[which(res!=0), name_column] <- ids[iLand]
  
      }

     }
 return(coord)
 }

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
 avai_allszgroups <- NULL
 for (ly in 1: length(name_gis_file_for_total_abundance_per_polygon)){



 # load the graph
  #load(file.path(general$main.path.igraph, paste(general$igraph, "_graphibm.RData",sep=''))) # built from the R code
  coord <- read.table(file=file.path(general$main.path.param.gis, "GRAPH",
             paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
  coord <- as.matrix(as.vector(coord))
  coord <- matrix(coord, ncol=3)
  coord <- cbind(coord, 1:nrow(coord))
  colnames(coord) <- c('x', 'y', 'harb', 'pt_graph')
  plot(coord[,1], coord[,2])

  graph <- read.table(file=file.path(general$main.path.param.gis , "GRAPH",
           paste("graph", general$igraph, ".dat", sep=""))) # build from the c++ gui
  graph <- as.matrix(as.vector(graph))
  graph <- matrix(graph, ncol=3)
  segments(coord[graph[,1]+1,1], coord[graph[,1]+1,2], coord[graph[,2]+1,1], coord[graph[,2]+1,2], col=4) # CAUTION: +1, because c++ to R




#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

 # in ArcGIS 10.1:
 # Create a blank shapefile in ArcCatalog by right click the folder and select New > shapefile (Feature Type: Polygon)
 # Go to ArcMap and add the shape file with File>Add Data...choose a file name that will be put in the config file as well (see name_gis_file_for_total_effort_per_polygon)
 # (optional) open a coastline shape file e.g. in DISPLACE_input_raw\shp (if cannot see anything appearing then click right in the tree and Zoom to layer)
 # (optional) open an informative XY layer e.g. grounds for cod from VMS analysis e.g. in DISPLACE_input_raw
 # Open the Editor toolbar from Custumize > toolbars> Editor, then start editing
 # Create (non-intersecting) polygons by selecting the polygon layr in Create Features dialog and choosing the Polygon Construction tools, click once to start the polygon, etc. and right click and Finish sketch...save and stop editing
 # Define the projection in ArcToolbox>Data Management Tools>Define Projection
 # Add a Field in menu TOC Open Attribute Table and click on Options button in the Table Frame and choose Add field... e.g. "abundance"
 # ...then go to the Editor toolbar >start editing and double click on a polygon to edit the 'abundance' value...save and stop editing
 # and voilà!

    
    library(maptools)
    handmade_WGS84            <- readShapePoly(file.path(general$main.path.param.gis, "POPULATIONS", name_gis_file_for_total_abundance_per_polygon[ly] ) , proj4string=CRS("+proj=longlat +datum=WGS84"))  # build in ArcGIS 10.1
    
    if(FALSE){ # in case not latlong but projected data instead.....
    library(rgdal)
    handmade2           <- readOGR(file.path(general$main.path.param.gis, "POPULATIONS", "SpatialLayers"), gsub("SpatialLayers/","", name_gis_file_for_total_abundance_per_polygon[ly]) ) #  Projection info in a .prj associated with the shp should be imported automagically.
   # How can I get the proj4 string from a shapefile .prj file? http://gis.stackexchange.com/questions/55196/how-can-i-get-the-proj4-string-or-epsg-code-from-a-shapefile-prj-file
   if(is.na( projection(handmade2))) projection(handmade2) <- CRS("+proj=longlat +datum=WGS84")   # a guess!
   library(sp)
    handmade_WGS84 <- spTransform(handmade2, CRS("+proj=longlat +datum=WGS84"))    # convert to longlat
    }
    
    names(handmade_WGS84)  # "ID"         name_gis_layer_field  
 
    plot(handmade_WGS84,  add=TRUE, border=as.data.frame(handmade_WGS84)[,name_gis_layer_field])



    # test coord for polygon inclusion
     spo                          <- SpatialPoints(coordinates(data.frame(CELL_LONG=coord[,1],
                                             CELL_LATI=coord[,2])))
     projection(spo)              <-  CRS("+proj=longlat +datum=WGS84")
     idx                          <- over(spo, handmade_WGS84, returnList=FALSE)  # idx in handmade_WGS84_reduced
     coord                        <- cbind(coord, idx[,name_gis_layer_field])
     colnames(coord)[ncol(coord)] <- name_gis_layer_field

     coord               <- cbind.data.frame(coord, xfold=factor(coord[,name_gis_layer_field])) # init
     levels(coord$xfold) <- xfold_gis_layer_field
  
    # check
    plot(handmade_WGS84,  add=FALSE, border=as.data.frame(handmade_WGS84)[,name_gis_layer_field])
    points(as.numeric(as.character(coord[, "x"])), as.numeric(as.character(coord[, "y"])), col=  1, pch=16)
    points(as.numeric(as.character(coord[, "x"])), as.numeric(as.character(coord[, "y"])), col=  as.numeric(coord[,name_gis_layer_field])+2, pch=16)
 

 # create the fgrounds files for DISPLACE
 # TWO WORKFLOWS ON A POPULATION EDITOR:
 # 1 - create from a XY survey based data 
 # 2 - create from a GIS shape layer with attributes e.g. abundance

 
 
 # WORKFLOW 1 -

 #....go to step 11 "Obtain the spatial distribution of the fish/shellfish population e.g. from surveys"



# WORKFLOW 2 - SEMESTER-BASED----------- 
# however, note that the seasonnality of the spatial and total effort application 
# is not parameterize but is instead an emerging feature from the model.
 library(doBy)
 avai <- NULL
 an <- function(x) as.numeric(as.character(x))
for (a.semester in c("S1", "S2")){

    # dispatch the abundance among nodes by dividing 'abundance' per the number of included graph nodes
    abundance_this_semester                   <- coord[!is.na(coord [,name_gis_layer_field]) & coord [,name_gis_layer_field]!= 0,]
    abundance_this_semester                   <- cbind.data.frame(abundance_this_semester, semester=a.semester, 
                                                  abundance= factor( an(abundance_this_semester [,name_gis_layer_field]) * an(abundance_this_semester [,"xfold"])    )) # init
    abundance_this_semester$abundance         <- factor(abundance_this_semester$abundance)
    levels(abundance_this_semester$abundance) <- an(levels(abundance_this_semester$abundance))  / table(abundance_this_semester$abundance)
    abundance_this_semester$abundance         <- an(abundance_this_semester$abundance) /sum(an(abundance_this_semester$abundance))
     #=> scale to 1 to obtain a avai per node
    
 
    avai <- rbind.data.frame(avai, abundance_this_semester)
 }
 
 # duplicate per size group (i.e. assuming the same parameterisation for all the pops)
 for(sid in szgroups[[ly]]){
  avai_allszgroups <- rbind.data.frame(avai_allszgroups, cbind(avai, szgroups=sid))
 }


 


 } # end loop over sets of size group


 
 # caution: fill in the gap
  all_combi                   <- expand.grid(pt_graph=unique(avai_allszgroups$pt_graph), szgroups=0:13, semester=c("S1", "S2"))
  avai_allszgroups            <- merge(avai_allszgroups,  all_combi, all=TRUE)
  #avai_allszgroups$abundance <- replace(avai_allszgroups$abundance, is.na (avai_allszgroups$abundance), 0.0000000000001)                                                                       
  avai_allszgroups$abundance  <- replace(avai_allszgroups$abundance, is.na (avai_allszgroups$abundance), 0.0)                                                                       




  # duplicate per pop id  (i.e. assuming the same parameterisation for all the pops)
 avai_allszgroups_allpops <- NULL
 for(pid in popids){
  avai_allszgroups_allpops <- rbind.data.frame(avai_allszgroups_allpops, cbind(avai_allszgroups, pids=pid))
 }



 # create the c++ input files  (CAUTION if also active Workflow 1 then need to append to the existing files...)
 # i.e.
 # 0spe_avai_szgroup_nodes_semester1
 # 0spe_full_avai_szgroup_nodes_semester1
 #...
 
 
  ####-------
 an <- function(x) as.numeric(as.character(x))
 options(scipen=999)
 for (a.semester in c("1", "2")){

    #-----------
    x        <- avai_allszgroups_allpops[avai_allszgroups_allpops$semester==paste("S",a.semester, sep=''),]
    x$popids   <- factor( x$pids )
    
    
    x$abundance <- round(x$abundance, 8)
    
    # a check
    tapply(an(x$abundance), list(x$pids, x$szgroups), sum, na.rm=TRUE  ) # should be full of 1
  
    # save .dat files
    x$pt_graph <-  as.numeric(as.character(x$pt_graph)) - 1 ##!!! OFFSET FOR C++ !!!##
    x<- orderBy(~pt_graph, data=x)
   
       popsspe_avai_semester          <- x[,c('pids','pt_graph', 'abundance')]
       popsspe_avai_semester_no_sz    <- popsspe_avai_semester[!duplicated(data.frame(popsspe_avai_semester$pids, popsspe_avai_semester$pt_graph)),]
       
       for(pid in unique(popsspe_avai_semester[,c('pids')])){
         popsspe_avai_semester_this_pop <- x[x$pids==pid, c('pids','pt_graph', 'abundance')]
         write.table(popsspe_avai_semester_this_pop[,c('pt_graph', 'abundance')],  # the szgroup dim is implicit....
            file=file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=''), "static_avai", 
              paste(pid, "spe_full_avai_szgroup_nodes_semester",gsub("Q","",a.semester),".dat",sep='')),
                  col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE, append=FALSE)
    
      }
       for(pid in unique(popsspe_avai_semester[,c('pids')])){
         popsspe_avai_semester_this_pop_these_sz <- x[x$pids==pid & x$szgroups %in% selected_szgroups, c('pids','pt_graph', 'abundance')]
         write.table(popsspe_avai_semester_this_pop_these_sz[,c('pt_graph', 'abundance')],     # the szgroup dim is implicit....
            file=file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=''), "static_avai",
              paste(pid, "spe_avai_szgroup_nodes_semester",gsub("Q","",a.semester),".dat",sep='')),
                  col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE, append=FALSE)
    
      }
      
      
      # stock presence/absence distribution
      distrib <- popsspe_avai_semester_no_sz[,c('pids', 'pt_graph')]
      distrib <- orderBy(~pids, data=distrib)
      write.table(distrib,     # the szgroup dimension is removed....
            file=file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=''), "static_avai",
              paste("lst_idx_nodes_per_pop_semester",gsub("Q","",a.semester),".dat",sep='')),
                  col.names=ifelse(do_append, FALSE, TRUE),  row.names=FALSE, sep= ' ', quote=FALSE, append=do_append)

   }   
   #-----------


 
 
} 
 
  ####-------
  