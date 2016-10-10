 # GENERAL SETTINGS

   args <- commandArgs(trailingOnly = TRUE)

   general <- list()

   if (length(args) < 2) {
     if(.Platform$OS.type == "windows") {
       general$application           <- "testexample" # ...or myfish
       general$main_path_gis         <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis", general$application)
       general$main.path.ibm         <- file.path("C:","Users","fbas","Documents","GitHub", paste("DISPLACE_input_", general$application, sep=''))
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
  
  
   cat(paste("START \n"))
 
   # (caution: give the order for naming stocks in integer from 0 to n-1)
   spp_table <-  read.table(file=file.path(general$main_path_gis, "POPULATIONS", 
                           paste("pop_names_", general$application,".txt",sep='')), header=TRUE)
   spp                        <- as.character(spp_table$spp)
   cat(paste("Reading the stock names in", paste(general$main_path_gis, "POPULATIONS", 
                           paste("pop_names_", general$application,".txt",sep='')),"....done \n"))
 
   
   path      <- file.path(general$main_path_gis, "POPULATIONS", "pops_config_files")
   namefiles <- list.files(file.path( path))

  
   cat(paste("Entering /POPULATIONS/pops_config_files folder....done \n"))

 
  for (a_file in namefiles){
   cat(paste("Process ", a_file, "\n"))
 
   dat  <- readLines(file.path(path, a_file))
   
   my_split <- function(x) unlist(strsplit(x, " "))
   my_split2 <- function(x) unlist(strsplit(x, "_"))
   
   #general <- list()
   #general$main.path.param.gis   <- as.character(dat[5])
   #general$main.path.param       <- as.character(dat[7])
   #general$main.path.ibm         <- as.character(dat[9])                                      
   #general$application           <- as.character(dat[11])  
   #general$igraph                <- as.numeric(dat[13])  
   dir.create(file.path(general$main.path.ibm, paste("popsspe_", general$application, sep='')))
   dir.create(file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=''),  "static_avai"))

 
                                  
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

  if(as.numeric(popids) %in%  as.numeric(spp_table$idx)){
      cat(paste("this stock", popids, "is in the list...\n"))

 
  avai_allszgroups <- NULL
  for (ly in 1: length(name_gis_file_for_total_abundance_per_polygon)){



 # load the graph
  #load(file.path(general$main.path.igraph, paste(general$igraph, "_graphibm.RData",sep=''))) # built from the R code
  coord <- read.table(file=file.path(general$main_path_gis, "GRAPH",
             paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
  coord <- as.matrix(as.vector(coord))
  coord <- matrix(coord, ncol=3)
  coord <- cbind(coord, 1:nrow(coord))
  colnames(coord) <- c('x', 'y', 'harb', 'pt_graph')
  if(do_plot) plot(coord[,1], coord[,2])

  graph <- read.table(file=file.path(general$main_path_gis , "GRAPH",
           paste("graph", general$igraph, ".dat", sep=""))) # build from the c++ gui
  graph <- as.matrix(as.vector(graph))
  graph <- matrix(graph, ncol=3)
  if(do_plot) segments(coord[graph[,1]+1,1], coord[graph[,1]+1,2], coord[graph[,2]+1,1], coord[graph[,2]+1,2], col=4) # CAUTION: +1, because c++ to R

  cat(paste("Read the graph....done\n"))
 


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
    handmade_WGS84            <- readShapePoly(file.path(general$main_path_gis, "POPULATIONS", name_gis_file_for_total_abundance_per_polygon[ly] ) , proj4string=CRS("+proj=longlat +datum=WGS84"))  # build in ArcGIS 10.1
    
    if(FALSE){ # in case not latlong but projected data instead.....
    library(rgdal)
    handmade2           <- readOGR(file.path(general$main_path_gis, "POPULATIONS", "SpatialLayers"), gsub("SpatialLayers/","", name_gis_file_for_total_abundance_per_polygon[ly]) ) #  Projection info in a .prj associated with the shp should be imported automagically.
   # How can I get the proj4 string from a shapefile .prj file? http://gis.stackexchange.com/questions/55196/how-can-i-get-the-proj4-string-or-epsg-code-from-a-shapefile-prj-file
   if(is.na( projection(handmade2))) projection(handmade2) <- CRS("+proj=longlat +datum=WGS84")   # a guess!
   library(sp)
    library(rgdal)
    handmade_WGS84 <- spTransform(handmade2, CRS("+proj=longlat +datum=WGS84"))    # convert to longlat
    }
    
    names(handmade_WGS84)  # "ID"         name_gis_layer_field  
 
    if(do_plot) plot(handmade_WGS84,  add=TRUE, border=as.data.frame(handmade_WGS84)[,name_gis_layer_field])



    # test coord for polygon inclusion
     spo                          <- SpatialPoints(coordinates(data.frame(CELL_LONG=coord[,1],
                                             CELL_LATI=coord[,2])), proj4string=CRS("+proj=longlat +datum=WGS84"))
     idx                          <- over(spo, handmade_WGS84, returnList=FALSE)  # idx in handmade_WGS84_reduced
     coord                        <- cbind(coord, idx[,name_gis_layer_field])
     colnames(coord)[ncol(coord)] <- name_gis_layer_field

     coord               <- cbind.data.frame(coord, xfold=factor(coord[,name_gis_layer_field])) # init
     levels(coord$xfold) <- xfold_gis_layer_field
  
    # check
    if(do_plot){
       plot(handmade_WGS84,  add=FALSE, border=as.data.frame(handmade_WGS84)[,name_gis_layer_field])
       points(as.numeric(as.character(coord[, "x"])), as.numeric(as.character(coord[, "y"])), col=  1, pch=16)
       points(as.numeric(as.character(coord[, "x"])), as.numeric(as.character(coord[, "y"])), col=  as.numeric(coord[,name_gis_layer_field])+2, pch=16)
    }

 # create the fgrounds files for DISPLACE
 # TWO WORKFLOWS ON A POPULATION EDITOR:
 # 1 - create from a XY survey based data 
 # 2 - create from a GIS shape layer with attributes e.g. abundance

 

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
    
         cat(paste("Write", pid, "spe_full_avai_szgroup_nodes_semester",gsub("Q","",a.semester),".dat....done \n"))
  
      }
       for(pid in unique(popsspe_avai_semester[,c('pids')])){
         popsspe_avai_semester_this_pop_these_sz <- x[x$pids==pid & x$szgroups %in% selected_szgroups, c('pids','pt_graph', 'abundance')]
         write.table(popsspe_avai_semester_this_pop_these_sz[,c('pt_graph', 'abundance')],     # the szgroup dim is implicit....
            file=file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=''), "static_avai",
              paste(pid, "spe_avai_szgroup_nodes_semester",gsub("Q","",a.semester),".dat",sep='')),
                  col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE, append=FALSE)
   
         cat(paste("Write", pid, "spe_avai_szgroup_nodes_semester",gsub("Q","",a.semester),".dat....done \n"))
    
      }
      
      
      # stock presence/absence distribution
      distrib <- popsspe_avai_semester_no_sz[,c('pids', 'pt_graph')]
      distrib <- orderBy(~pids, data=distrib)
      write.table(distrib,     # the szgroup dimension is removed....
            file=file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=''), "static_avai",
              paste("lst_idx_nodes_per_pop_semester",gsub("Q","",a.semester),".dat",sep='')),
                  col.names=ifelse(do_append, FALSE, TRUE),  row.names=FALSE, sep= ' ', quote=FALSE, append=do_append)

      cat(paste("Write lst_idx_nodes_per_pop_semester",gsub("Q","",a.semester),".dat....done \n"))

   }   
   #-----------


                          
                         


  
    for (a.semester in 1:2){
   #-----------
   #-----------
    ## POP SPE----------
      for(rg in c('0', '2', '3', '5', '7')){
        # export betas specific to the avai szgroup given this pop (caution: remenber the scaling i.e *1000)
        # mean estimates
         popsspe_delta_semester <- cbind.data.frame(0:(length(spp)-1), rep(0, length(0:(length(spp)-1))) )
         colnames(popsspe_delta_semester) <- c('pop', 'delta.nb_indiv')

        # save .dat files
        write.table(popsspe_delta_semester,
           file=file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=''),
             paste("avai", rg, "_betas_semester", a.semester,".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, quote=FALSE, append=FALSE, sep = " ")
     
           cat(paste("Write avai", rg, "_betas_semester", a.semester,".dat....done \n"))
 
     
     
          }



   } # end a.semester


    # the selected groups in the catch rate equation
    write(c('nm2',  'selected_szgroups'),
                   file=file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=''),
                       paste("the_selected_szgroups.dat",sep=' ')), append=FALSE,  ncol=2,
                          sep=" ")
                         
    for(sp in (0:(length(spp)-1))){
       write.table(cbind(nm2=sp, selected_szgroups=c(0, 2, 3, 5, 7)),
                   file=file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=''),
                       paste("the_selected_szgroups.dat",sep=' ')), append=TRUE,
                         quote = FALSE, sep=" ", col.names=FALSE, row.names=FALSE)
 
    }
 cat(paste("Write the_selected_szgroups.dat....done \n"))
 
   
 
 
 



   
     # quick check
   if(length(name_gis_file_for_total_abundance_per_polygon) != length(szgroups)) stop("Need for same number of GIS layers and sets of size groups....")


 cat(paste("Process ", a_file, "....done \n"))
  
 } else{
    cat(paste("....a config file is found but the stock is not in the list....config ignored\n"))
 
 }
} 
 
cat(paste("....done \n"))
  