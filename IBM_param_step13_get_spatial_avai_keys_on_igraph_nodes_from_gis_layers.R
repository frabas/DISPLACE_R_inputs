 # some args for the bunch of vessels to be created....
 create_example <- FALSE
 if(create_example){
 
   # GENERAL SETTINGS
   general <- list()
   if(.Platform$OS.type == "windows") {
     general$main.path             <- file.path("C:","DISPLACE_outputs")
     general$application           <- "adriatic" # ...or myfish
     general$main.path.param       <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_raw")
     general$main.path.param.gis   <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis", general$application)
     general$main.path.ibm         <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input")
   }

   
   if(general$application=="myfish"){
   general$namefolderinput    <- "myfish"
   general$igraph             <- 56  # caution: should be consistent with existing pops already built upon a given graph
   do_append                  <- FALSE
   name_gis_file_for_total_abundance_per_polygon <- "totabundance_on_fgrounds_handmade_polygons"
   popids                     <- paste( 0:3, sep="")  # e.g. 3 stocks
   szgroups                   <-  0:13              # 14 size groups
   selected_szgroups          <-  c(2,5,7,9)
   name_gis_layer_field       <- "abundance"    # e.g. giving absolute abundance in polygon
   is_gis_layer_field_relative_numbers          <- FALSE   # if relative categories (e.g. high to low) then xfold_gis_layer_field will be used to convert in absolute
   xfold_gis_layer_field      <- c(1, 1, 1, 1, 1)  # [not used if is_gis_layer_field_relative_numbers is FALSE] 
   }
   
   if(general$application=="adriatic"){
   general$namefolderinput    <- "adriatic"
   general$igraph             <- 1  # caution: should be consistent with existing pops already built upon a given graph
   do_append                  <- FALSE
   name_gis_file_for_total_abundance_per_polygon <- "adriatic_totabundance_on_fgrounds_handmade_polygons"
   popids                     <- paste(0:13, sep="")  # e.g. 13 stocks
   szgroups                   <-  0:13          # 14 size groups
   selected_szgroups          <-  c(2,5,7,9)
   name_gis_layer_field       <- "abundance"    # e.g. giving absolute abundance in polygon
   is_gis_layer_field_relative_numbers          <- FALSE   # if relative categories (e.g. high to low) then xfold_gis_layer_field will be used to convert in absolute
   xfold_gis_layer_field      <- c(1, 1, 1, 1, 1)  # [not used if is_gis_layer_field_relative_numbers is FALSE] 
   }
   
   
    # create a config file
   namefile <- file.path(general$main.path.param.gis, paste("pops_creator_args_",general$namefolderinput, ".dat", sep=''))
  
   write("# config file for the vessel editor: adding some vessel(s)", file=file.path(general$main.path.param.gis, "pops_creator_args.dat"))
   write("# (the shortestPaths library will have to be re-created for the graph)", file=file.path(general$main.path.param.gis, "pops_creator_args.dat"), ncolumns=1, append=TRUE)
   write("# --------------", file=file.path(general$main.path.param.gis, "pops_creator_args.dat"), ncolumns=1, append=TRUE)
  
   write("# input folder for config file", file=file.path(general$main.path.param.gis, "pops_creator_args.dat"), ncolumns=1, append=TRUE)
   write(general$main.path.param.gis, file=file.path(general$main.path.param.gis, "pops_creator_args.dat"), ncolumns=1, append=TRUE)
  
   write("# output folder for parameterisation file", file=file.path(general$main.path.param.gis, "pops_creator_args.dat"), ncolumns=1, append=TRUE)
   write(general$main.path.param, file=file.path(general$main.path.param.gis, "pops_creator_args.dat"), ncolumns=1, append=TRUE)
  
   write("# input folder for DISPLACE", file=file.path(general$main.path.param.gis, "pops_creator_args.dat"), ncolumns=1, append=TRUE)
   write(general$main.path.ibm, file=file.path(general$main.path.param.gis, "pops_creator_args.dat"), ncolumns=1, append=TRUE)  
  
   write("# name of the application", file=file.path(general$main.path.param.gis, "pops_creator_args.dat"), ncolumns=1, append=TRUE)
   write(general$namefolderinput, file=file.path(general$main.path.param.gis, "pops_creator_args.dat"), ncolumns=1, append=TRUE)  
  
   write("# name of the graph for this application", file=file.path(general$main.path.param.gis, "pops_creator_args.dat"), ncolumns=1, append=TRUE)
   write(general$igraph, file=file.path(general$main.path.param.gis, "pops_creator_args.dat"), ncolumns=1, append=TRUE)  
   
   write("# append to existing pop files", file=file.path(general$main.path.param.gis, "pops_creator_args.dat"), ncolumns=1, append=TRUE)
   write(do_append, file=file.path(general$main.path.param.gis, "pops_creator_args.dat"), ncolumns=1, append=TRUE)
  
   write("# name gis file for total abundance per polygon", file=file.path(general$main.path.param.gis, "pops_creator_args.dat"), ncolumns=1, append=TRUE)
   write(name_gis_file_for_total_abundance_per_polygon, file=file.path(general$main.path.param.gis, "pops_creator_args.dat"), ncolumns=1, append=TRUE)
 
   write("# name_gis_layer_field",file=file.path(general$main.path.param.gis, "pops_creator_args.dat"), ncolumns=1, append=TRUE)
   write(name_gis_layer_field, file=file.path(general$main.path.param.gis, "pops_creator_args.dat"), ncolumns=1, append=TRUE)
 
   write("# is_gis_layer_field_relative_numbers",file=file.path(general$main.path.param.gis, "pops_creator_args.dat"), ncolumns=1, append=TRUE)
   write(is_gis_layer_field_relative_numbers, file=file.path(general$main.path.param.gis, "pops_creator_args.dat"), ncolumns=1, append=TRUE)
 
   write("# xfold_gis_layer_field",file=file.path(general$main.path.param.gis, "pops_creator_args.dat"), ncolumns=1, append=TRUE)
   write(xfold_gis_layer_field, file=file.path(general$main.path.param.gis, "pops_creator_args.dat"), ncolumns=length(xfold_gis_layer_field), append=TRUE)
  
   write("# popids", file=file.path(general$main.path.param.gis, "pops_creator_args.dat"), ncolumns=1, append=TRUE)
   write(popids, file=file.path(general$main.path.param.gis, "pops_creator_args.dat"), ncolumns=length(popids), append=TRUE)
   
   write("# all size groups", file=file.path(general$main.path.param.gis, "pops_creator_args.dat"), ncolumns=1, append=TRUE)
   write(szgroups, file=file.path(general$main.path.param.gis, "pops_creator_args.dat"), ncolumns=length(szgroups), append=TRUE)
   
   write("# selected size groups", file=file.path(general$main.path.param.gis, "pops_creator_args.dat"), ncolumns=1, append=TRUE)
   write(selected_szgroups, file=file.path(general$main.path.param.gis, "pops_creator_args.dat"), ncolumns=length(selected_szgroups), append=TRUE)
   
   }

      

 #------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-----read input config file----------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

   
   application           <- "adriatic" # ...or myfish etc.
   path <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis") # where is the config file?
   dat  <- readLines(file.path(path, application, "pops_creator_args.dat"))
   
   my_split <- function(x) unlist(strsplit(x, " "))
   
   general <- list()
   general$main.path.param.gis   <- as.character(dat[5])
   general$main.path.param       <- as.character(dat[7])
   general$main.path.ibm         <- as.character(dat[9])                                      
   general$namefolderinput       <- as.character(dat[11])  
   general$igraph                <- as.numeric(dat[13])  
                                  
   do_append                     <- as.logical(dat[15])
   name_gis_file_for_total_abundance_per_polygon <- dat[17]
   name_gis_layer_field                        <- dat[19]
   is_gis_layer_field_relative_numbers         <- dat[21]
   xfold_gis_layer_field                        <- as.numeric(my_split(dat[23]))  
   popids                     <- as.character(my_split(dat[25]))
   szgroups                   <- as.character(my_split(dat[27]))
   selected_szgroups          <-  as.character(my_split(dat[29]))

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-----utils---------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
 # loop over the SpatialPoly
 detectingCoordInPolygonsFromSH <- function (sh, coord, name_column="poly"){

     coord <- eval(parse(text=paste("cbind(coord, ",name_column,"= 0)", sep='')))
     dd                   <- sapply(slot(sh, "polygons"), function(x) lapply(slot(x, "Polygons"), function(x) x@coords)) # tricky there...
     library(sp)
     for(iLand in 1:length(dd)){
      if(length(dd)>1){
       for(i in 1:length(dd[[iLand]])){
          print(iLand)
          # Points on land are 1 or 2 and not is 0
          er <- try({res   <- point.in.polygon(coord[,1],coord[,2],dd[[iLand]][[i]][,1],dd[[iLand]][[i]][,2])}, silent=TRUE)
          if(class(er)=="try-error") res   <- point.in.polygon(coord[,1],coord[,2],dd[[iLand]][,1],dd[[iLand]][,2])
          coord[which(res!=0), name_column] <- iLand
       }
      } else{
          er <- try({res   <- point.in.polygon(coord[,1],coord[,2],dd[[1]][,1],dd[[1]][,2])}, silent=TRUE)
          if(class(er)=="try-error") res   <- point.in.polygon(coord[,1],coord[,2],dd[[1]][,1],dd[[1]][,2])
          coord[which(res!=0), name_column] <- iLand

      }

     }
 return(coord)
 }

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
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
 handmade            <- readShapePoly(file.path(general$main.path.param.gis, name_gis_file_for_total_abundance_per_polygon))  # build in ArcGIS 10.1
 library(rgdal)
 handmade2           <- readOGR(file.path(general$main.path.param.gis), name_gis_file_for_total_abundance_per_polygon) #  Projection info in a .prj associated with the shp should be imported automagically.

 # How can I get the proj4 string from a shapefile .prj file? http://gis.stackexchange.com/questions/55196/how-can-i-get-the-proj4-string-or-epsg-code-from-a-shapefile-prj-file

 library(sp)
 library(rgdal)
 handmade_WGS84 <- spTransform(handmade2, CRS("+proj=longlat +datum=WGS84"))    # convert to longlat

 names(handmade_WGS84)  # "ID"         name_gis_layer_field  
 
 plot(handmade_WGS84,  add=TRUE, border=as.data.frame(handmade_WGS84)[,name_gis_layer_field])


 # test coord for polygon inclusion
  coord <-  detectingCoordInPolygonsFromSH (handmade_WGS84, coord, name_column="poly_id")
  points(coord[,1], coord[,2], col=as.numeric(coord[,"poly_id"])+1)  # check


   handmade_WGS84_df <- as.data.frame(handmade_WGS84)

 # caution:
 handmade_WGS84_df$xfold         <- factor(handmade_WGS84_df[,name_gis_layer_field]) # init
 levels(handmade_WGS84_df$xfold) <- xfold_gis_layer_field
 handmade_WGS84_df$xfold         <-   as.character(handmade_WGS84_df$xfold)
 handmade_WGS84_df               <- rbind.data.frame( c(ID=0, 0, min(xfold_gis_layer_field)/2), handmade_WGS84_df)   # add an ID 0 for not included coord points AND ASSUME SOME ACTIVITY IN IT
 handmade_WGS84_df$xfold         <-   as.factor(handmade_WGS84_df$xfold)
 
 # then merge to coord  (caution: 'poly' give the polygon in the harbour range; 'poly_id' give the polygon id from the handmade_WGS84 shape file) 
 coord<- merge(coord, handmade_WGS84_df, by.x="poly_id", by.y="ID")

 
 # check
 plot(handmade_WGS84,  add=TRUE, border=as.data.frame(handmade_WGS84)[,name_gis_layer_field])
 coord$color <-  factor(coord[,name_gis_layer_field]) #init   
 levels(coord$color) <- 1:length(levels(coord$color))
 points(coord[, "x"], coord[, "y"], col=  coord[,"color"])



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
    abundance_this_semester                   <- coord[coord [,name_gis_layer_field]!= 0,]
    abundance_this_semester                   <- cbind.data.frame(abundance_this_semester, semester=a.semester, 
                                                  abundance= factor( an(abundance_this_semester [,name_gis_layer_field]) * an(abundance_this_semester [,"xfold"])    )) # init
    abundance_this_semester$abundance         <- factor(abundance_this_semester$abundance)
    levels(abundance_this_semester$abundance) <- an(levels(abundance_this_semester$abundance))  / table(abundance_this_semester$abundance)
    abundance_this_semester$abundance         <- an(abundance_this_semester$abundance) /sum(an(abundance_this_semester$abundance))
     #=> scale to 1 to obtain a avai per node
    
 
    avai <- rbind.data.frame(avai, abundance_this_semester)
 }
 
 # duplicate per size group (i.e. assuming the same parameterisation for all the pops)
 avai_allszgroups <- NULL
 for(sid in szgroups){
  avai_allszgroups <- rbind.data.frame(avai_allszgroups, cbind(avai, szgroups=sid))
 }

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
 for (a.semester in c("1", "2")){

    #-----------
    x        <- avai_allszgroups_allpops[avai_allszgroups_allpops$semester==paste("S",a.semester, sep=''),]
    x$popids   <- factor( x$pids )
    
    x<- orderBy(~pt_graph, data=x)
    
    x$abundance <- round(x$abundance, 8)
    
    # a check
    tapply(an(x$abundance), list(x$pids, x$szgroups), sum, na.rm=TRUE  ) # should be full of 1
  
    # save .dat files
    x$pt_graph <-  x$pt_graph - 1 ##!!! OFFSET FOR C++ !!!##
       popsspe_avai_semester          <- x[,c('pids','pt_graph', 'abundance')]
       popsspe_avai_semester_this_pop <- x[x$szgroups %in% selected_szgroups, c('pids','pt_graph', 'abundance')]
       
       for(pid in unique(popsspe_avai_semester[,c('pids')])){
         write.table(popsspe_avai_semester_this_pop[,c('pt_graph', 'abundance')],  # the szgroup dim is implicit....
            file=file.path(general$main.path.param, "popsspe", "static_avai", 
              paste(pid, "spe_full_avai_szgroup_nodes_semester",gsub("Q","",a.semester),".dat",sep='')),
                  col.names=ifelse(do_append, FALSE, TRUE),  row.names=FALSE, sep= ' ', quote=FALSE, append=do_append)
    
      }
       for(pid in unique(popsspe_avai_semester[,c('pids')])){
         write.table(popsspe_avai_semester_this_pop[,c('pt_graph', 'abundance')],     # the szgroup dim is implicit....
            file=file.path(general$main.path.param, "popsspe", "static_avai",
              paste(pid, "spe_avai_szgroup_nodes_semester",gsub("Q","",a.semester),".dat",sep='')),
                  col.names=ifelse(do_append, FALSE, TRUE),  row.names=FALSE, sep= ' ', quote=FALSE, append=do_append)
    
      }
   #-----------


} 
 