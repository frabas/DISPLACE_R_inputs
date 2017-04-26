# some args for the bunch of vessels to be created....
# Usage:
# RunVesselsConfigFiles.R application gis_path input_application_path igraph

args <- commandArgs(trailingOnly = TRUE)

general <- list()


if (length(args) < 2) {
  if(.Platform$OS.type == "windows") {
    general$application           <- "testexample" # ...or myfish
    general$main.path.param.gis   <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis", general$application)
    general$main.path.ibm         <- file.path("C:","Users","fbas","Documents","GitHub",paste("DISPLACE_input_", general$application, sep=''))
    general$igraph                <- 56
    do_plot <- TRUE
    }
} else {            
  general$application           <- args[1]
  general$main.path.param.gis   <- args[2]
  general$main.path.ibm         <- args[3]
  general$igraph                <- args[4]
  do_plot <- FALSE
 }

dir.create(file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep='')))
dir.create(file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep='')))


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-----read input config file----------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


   # create from different set of vessels, for example:
   
   path        <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis", general$application, "FISHERIES", "vessels_config_files")
   namefiles   <- list.files(file.path( path))

  

 count <- 0
 metierspe_betas_all <- NULL
 for (namefile in namefiles){ # LOOP OVER CONFIG FILES
   cat(paste("Treatment for", namefile, "\n"))
   count <- count+1

    dat              <- readLines(file.path(path, namefile))

   my_split  <- function(x) unlist(strsplit(x, " "))
   my_split2 <- function(x) unlist(strsplit(x, "_"))
 
   do_append                     <- as.logical(dat[which(dat=="# [append_to_existing_vessel_files]")+1])
   if (count==1) do_append <- FALSE # NOT ADDED TO PREVIOUS FILE......


   name_gis_file_for_fishing_effort_per_polygon <- dat[which(dat=="# [name_gis_file_for_total_effort_per_polygon]")+1]
   name_gis_layer_field                         <- dat[which(dat=="# [name_gis_layer_field]")+1]
   is_gis_layer_field_relative_numbers          <- dat[which(dat=="# [is_gis_layer_field_relative_numbers]")+1]
   xfold_gis_layer_field                        <- as.numeric( dat[which(dat=="# [xfold_gis_layer_field]")+1] )
   vesselids                                    <- as.character(my_split( dat[which(dat=="# [vesselids]")+1] ))
   vessel_range_km                              <- as.numeric( dat[which(dat=="# [vessel_range_km]")+1] )
   metierids                                    <- as.numeric(my_split( dat[which(dat=="# [metierids]")+1] )) 
   metierids_frequencies                        <- as.numeric(my_split( dat[which(dat=="# [metierids_frequencies]")+1] ))
   if(length(metierids)!=length(metierids_frequencies)) stop("Check config file for vessel creator - length(metierids)")
   visited_ports                                <- as.character(my_split( dat[which(dat=="# [visited_ports_but_look_at_names_in_harbours.dat_in_harboursspe_folder]")+1] ))
   visited_ports_frequencies                    <- as.numeric(my_split( dat[which(dat=="# [visited_ports_frequencies]")+1] ))
   if(length(visited_ports)!=length(visited_ports_frequencies)) stop("Check config file for vessel creator - length(visited_ports)")
   name_file_ports                              <- dat[which(dat=="# [name_file_ports]")+1]
   nb_stocks                                    <- as.numeric(my_split( dat[which(dat=="# [nb_fish_or_shellfish_stocks_which_should_be_consistent_with_popsspe_folder]")+1] ))
   fixed_cpue_per_stock                         <- as.numeric(my_split( dat[which(dat=="# [fixed_cpue_per_stock_on_fgrounds_for_planB]")+1] ))
   if(length(fixed_cpue_per_stock)!=nb_stocks) stop("Check config file for vessel creator - length(fixed_cpue_per_stock)")
   gshape_cpue_per_stock                        <- as.numeric(my_split( dat[which(dat=="# [Gamma_shape_parameter_for_cpue_per_stock_on_fgrounds_for_planA_but_for_implicit_stocks_or_out_of_range_nodes]")+1]))
   if(length(gshape_cpue_per_stock)!=nb_stocks) stop("Check config file for vessel creator - length(gshape_cpue_per_stock)")
   gscale_cpue_per_stock                        <- as.numeric(my_split( dat[which(dat=="# [Gamma_scale_parameter_for_cpue_per_stock_on_fgrounds_for_planA_but_for_implicit_stocks_or_out_of_range_nodes]")+1]))
   if(length(gscale_cpue_per_stock)!=nb_stocks) stop("Check config file for vessel creator - length(gscale_cpue_per_stock)")
   vessel_features                              <- as.numeric(my_split( dat[which(dat=="# [vessel_features_speed_fuelconsrate_length_kW_carryingcapacity_tankcapacity_nbpingspertrip_shapeinbtw_scaleinbtw_avtripduration]")+2] ))
   step_in_share                                <- as.numeric(my_split( dat[which(dat=="# [percent_step_in_share_for_TAC_per_stock_for_these_incoming_vessels_but_only_used_if_existing_vessels_already]")+1] ))
   if(length(step_in_share)!=nb_stocks) stop("Check config file for vessel creator - length(step_in_share)")
                                                  
   # from this vessel, add catch equation vessel effect
   vesselsspe_betas                             <- as.numeric(my_split(dat[which(dat=="# [vessel_effect_per_stock_in_the_catch_rate_equation]")+1] ))
   if(length(vesselsspe_betas)!=nb_stocks) stop("Check config file for vessel creator - length(vesselsspe_betas)")

   # from this vessel, add catch equation metier effect informed for the vessel´s metiers (caution, we expect the same whatever the vessel if we talk about the same metiers)
   metierspe_betas                              <-  as.numeric(my_split(dat[which(dat=="# [metier_effect_per_stock_in_the_catch_rate_equation]")+1] ))
   if(length(nb_stocks)>1) for(st in 2: length(nb_stocks)) metierspe_betas <- c(metierspe_betas,  as.numeric(my_split(which(dat=="# [metier_effect_per_stock_in_the_catch_rate_equation]")+1+(st-1))))
   metierspe_betas                              <- matrix(metierspe_betas, ncol=length(metierids), nrow=(nb_stocks), byrow=TRUE)
   colnames(metierspe_betas)                    <- metierids
   metierspe_betas                              <- cbind( as.numeric(rep(colnames(metierspe_betas), each=nrow(metierspe_betas))) , c( metierspe_betas) ) 
                                                  # =>final format: met idx / met effect along st
   if(!is.null(metierspe_betas_all))
      {
       metierspe_betas_all            <- rbind(metierspe_betas_all, metierspe_betas[!(metierspe_betas[,1] %in% metierspe_betas_all[,1]), ]) # add only if not already present
       } else{
       metierspe_betas_all            <- rbind(metierspe_betas_all, metierspe_betas) # add 
       }
                                    
   # from this vessel, add catch equation sizegroup effect (caution, we expect the same whatever the vessel)
   avaispe_betas                                <-  as.numeric(my_split(dat[which(dat=="# [avai_effect_per_size_group_per_stock_in_the_catch_rate_equation]")+1] ))
   if(length(nb_stocks)>1) for(st in 2: length(nb_stocks)) avaispe_betas <- c(avaispe_betas,  as.numeric(my_split( dat[which(dat=="# [avai_effect_per_size_group_per_stock_in_the_catch_rate_equation]")+1+(st-1)] )) )
   avaispe_betas                             <- matrix(avaispe_betas, ncol=14, nrow=(nb_stocks), byrow=TRUE)
   
   create_file_for_fuel_price_per_vessel_size   <-  as.logical(dat[which(dat=="# [create_the_file_for_fuel_price_per_vessel_size]")+1])
   some_fuel_price_per_vessel_size              <-  as.numeric(my_split(dat[which(dat=="# [some_fuel_prices_per_vessel_size_euro_per_litre]")+1]))
   step_in_share_credits                        <-  as.numeric(dat[which(dat=="# [percent_fishing_credits_taken_by_incomers_for_RTI_management]")+1])


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-----utils---------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

getPolyAroundACoord <- function(dat, a_dist_m){
  lst <- list()
    for(i in 1: nrow(dat)){
      x <- dat[i,'coords.x1'] + a_dist_m * cos(seq(1,360, by=5)*pi/180)
      y <- dat[i,'coords.x2'] + a_dist_m * sin(seq(1,360, by=5)*pi/180)
      assign(paste("poly", i, sep=""),
        Polygon(cbind(  c(x,x[1]),
                   c(y, y[1])
                    )))
      assign  (paste("Poly", i, sep=""),  Polygons(list(get(paste("poly", i, sep=""))), ID=paste(dat[i, "ID"])) )
      lst[[i]] <- get(paste("Poly", i, sep=""))
     }
  return(lst)
  }



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
 # load the graph
  coord <- read.table(file=file.path(general$main.path.param.gis, "GRAPH",
             paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
  coord <- as.matrix(as.vector(coord))
  coord <- matrix(coord, ncol=3)
  coord <- cbind(coord, 1:nrow(coord))
  colnames(coord) <- c('x', 'y', 'harb', 'pt_graph')
  if(do_plot) plot(coord[,1], coord[,2])

  saved_coord <- coord

  graph <- read.table(file=file.path(general$main.path.param.gis, "GRAPH",
           paste("graph", general$igraph, ".dat", sep=""))) # build from the c++ gui
  graph <- as.matrix(as.vector(graph))
  graph <- matrix(graph, ncol=3)
  if(do_plot) segments(coord[graph[,1]+1,1], coord[graph[,1]+1,2], coord[graph[,2]+1,1], coord[graph[,2]+1,2], col=4) # CAUTION: +1, because c++ to R


  #-------------------------------------------------------------------------------
  #-------------------------------------------------------------------------------
  # GET ALL THE NODES IN THE RANGE OF THE VESSEL SPECIFIC HARBOURS

  harbours <- read.table(file.path(general$main.path.param.gis, "GRAPH", name_file_ports), sep=";", row.names=NULL, header=TRUE)
  harbours <- harbours[harbours[,1] %in% visited_ports,]
  harbours <- cbind.data.frame(harbours, ID=1:nrow(harbours))
  # convert to UTM
  library(sp)
  library(rgdal)
  SP       <- SpatialPoints(cbind(as.numeric(as.character(harbours[,'lon'])), as.numeric(as.character(harbours[,'lat']))),
                       proj4string=CRS("+proj=longlat +ellps=WGS84"))
  UTMzone  <- 32
  harbours <- cbind.data.frame(harbours,
                 spTransform(SP, CRS(paste("+proj=utm +zone=",UTMzone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep=''))))    # convert to UTM


  lst             <- getPolyAroundACoord(harbours, a_dist_m= vessel_range_km * 1000)
  sp              <- SpatialPolygons(lst, 1:nrow(harbours))
  if(do_plot) plot(sp)


  library(rgdal)
  library(maptools)
  library(raster)

  projection(sp)  <- CRS(paste("+proj=utm +zone=",UTMzone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep='') )
   # transform back to decimal longlat
  sp            <- spTransform(sp, CRS("+proj=longlat +ellps=WGS84"))

  spo             <- SpatialPoints(coordinates(data.frame(CELL_LONG=coord[,1],
                                             CELL_LATI=coord[,2])))
  projection(spo) <-  CRS("+proj=longlat +ellps=WGS84")
  idx                           <- over(spo,sp)
  coord       <- cbind.data.frame(coord, poly=names(sp)[idx])

  #...then only keep the nodes inside the range of these particular vessels
  coord <- coord[!is.na(coord$poly),]

  # check
  if(do_plot) plot(sp)
  if(do_plot) points(coord[, c(1,2)], col=3)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

 # IF (CREATING A SHAPE FILE FROM SCRATCH)
 # {
 # in ArcGIS 10:
 # create a blank shapefile in ArcCatalog by right click the folder and select New > shapefile (Feature Type: Polygon)
 # go to ArcMap and add the shape file with File>Add Data...choose a file name that will be put in the config file as well (see name_gis_file_for_fishing_effort_per_polygon)
 # (optional) open a coastline shape file e.g. in DISPLACE_input_raw\shp
 # (optional) open an informative XY layer e.g. grounds for cod from VMS analysis e.g. in DISPLACE_input_raw
 # Open the Editor toolbar from Custumize > toolbars> Editor
 # create (non-intersecting) polygons by selecting the polygon layr in Create Features dialog and choosing the Polygon Construction tools, click once to start the polygon, etc. and right click and Finish sketch...save and stop editing
 # define the projection
 # add a Field in menu TOC Open Attribute Table and click on Options button in the Table Frame and choose Add field... e.g. "feffort_h"
 # ...then go to the Editor toolbar >start editing and double click on a polygon to edit the feffort_h value...save and stop editing
 # }
 # {
 # ELSE USE INFO FROM PARTNERS
 # }


 cat(paste("Read the GIS layer for", namefile, "\n"))
 library(maptools)
 #handmade            <- readShapePoly(file.path(general$main.path.param.gis, "FISHERIES", name_gis_file_for_fishing_effort_per_polygon))  # build in ArcGIS 10.1
 library(rgdal)
 name_gis_file_for_fishing_effort_per_polygon   <- unlist(strsplit(name_gis_file_for_fishing_effort_per_polygon, split=" "))
 name_gis_layer_field                           <- unlist(strsplit(name_gis_layer_field, split=" "))   [1] # caution
 for (a_layer in 1: length(name_gis_file_for_fishing_effort_per_polygon)) { # if more than 1 layer....
     handmade  <- readOGR(file.path(general$main.path.param.gis,  "FISHERIES", "SpatialLayers"), name_gis_file_for_fishing_effort_per_polygon[a_layer] ) 
                                                   #  Projection info in a .prj associated with the shp should be imported automagically.
     if(is.na( projection(handmade ))) projection(handmade ) <- CRS("+proj=longlat +datum=WGS84")   # a guess!
     if(a_layer>1){
        row.names(handmade) <- as.character(as.numeric(row.names(handmade))+length(row.names(handmade2)))  
        handmade2 <- spRbind(handmade2, handmade) # caution: different from gUnion
       } else{
       handmade2 <- handmade
       }
     }
     
 
 
 # How can I get the proj4 string from a shapefile .prj file? http://gis.stackexchange.com/questions/55196/how-can-i-get-the-proj4-string-or-epsg-code-from-a-shapefile-prj-file
 row.names(handmade2)


 library(sp)
 library(rgdal)
 handmade_WGS84 <- spTransform(handmade2, CRS("+proj=longlat +datum=WGS84"))    # convert to longlat

 names(handmade_WGS84)  # "Id"         name_gis_layer_field

 if(do_plot) plot(handmade_WGS84,  add=TRUE, border=as.data.frame(handmade_WGS84)[,name_gis_layer_field])

 
 # test coord for polygon inclusion
 library(rgeos)
 sp            <- spTransform(sp, CRS("+proj=longlat +datum=WGS84"))

 
  sauv <- coord
 
  cat(paste("Intersect vessel home range with the GIS effort layer and retrieve the corresponding graph nodes for", namefile, "\n"))
 # heuristic to reduce the search to within the circles around harbours i.e. sp
  handmade_WGS84_reduced <-  gIntersection(handmade_WGS84, sp, byid = TRUE) 
  if(is.null(handmade_WGS84_reduced)) { # for the few "edge" cases where nothing is found within the harbour circles (!) then redo with a larger radius.
                                      lst                    <- getPolyAroundACoord(harbours, a_dist_m= vessel_range_km*10 * 1000)
                                      sp                     <- SpatialPolygons(lst, 1:nrow(harbours))
                                      projection(sp)         <- CRS(paste("+proj=utm +zone=",UTMzone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep='') )
                                      sp                     <- spTransform(sp, CRS("+proj=longlat +datum=WGS84"))
                                      handmade_WGS84_reduced <- gIntersection(handmade_WGS84, sp, byid = TRUE) 
                                      coord                  <- saved_coord
                                      }
                                      
  spo                    <- SpatialPoints(coordinates(data.frame(CELL_LONG=as.numeric(as.character(coord[,1])),
                                             CELL_LATI=as.numeric(as.character(coord[,2])))))
  projection(spo)        <-  CRS("+proj=longlat +datum=WGS84")
  idx                    <- over(spo, handmade_WGS84_reduced)  # idx in handmade_WGS84_reduced
  
  # if it is wished to retrieve the full info by returning indices from overlapping polygons then we might use instead:
  #idx2                    <- over(spo, handmade_WGS84_reduced, returnList=TRUE)
  # but can be overly complicated for the rest of the processing....so better to keep only one idx from one polygon and then avoid duplicates and need for effort aggregation

  
  # caution: need to recall the initial ids 
  initial_ids            <- do.call(rbind,strsplit(row.names(handmade_WGS84_reduced)," "))[,1] # ids in handmade_WGS84
  coord                  <- cbind.data.frame(coord, poly_id=  initial_ids[idx]) # map ids from initial handmade_WGS84
 
  # tricky. we need to recreate a SpatialPolygonsDataFrame for handmade_WGS84_reduced and the ids should be from the original untouched shp i.e handmade_WGS84....
  new.attribs                       <- data.frame(do.call(rbind,strsplit(row.names(handmade_WGS84_reduced)," ")),stringsAsFactors = FALSE)    # get the attributes back!
  new.attrib.data                   <- handmade_WGS84[new.attribs$X1,]@data
  row.names(handmade_WGS84_reduced) <- row.names(new.attrib.data)
  handmade_WGS84_reduced            <- SpatialPolygonsDataFrame(handmade_WGS84_reduced, new.attrib.data)
  handmade_WGS84_reduced_df         <- as.data.frame(handmade_WGS84_reduced) # get attributes(handmade_WGS84)$data
  handmade_WGS84_reduced_df$SP_ID   <- as.numeric(row.names(handmade_WGS84_reduced_df))   # that one is really tricky
 
 # caution:
 if(all(xfold_gis_layer_field==1)){
  # do nothing
   handmade_WGS84_reduced_df$xfold <- 1
 } else{
 handmade_WGS84_reduced_df$xfold         <- factor(handmade_WGS84_reduced_df[,name_gis_layer_field]) # init
 levels(handmade_WGS84_reduced_df$xfold) <- xfold_gis_layer_field
 handmade_WGS84_reduced_df$xfold         <- as.character(handmade_WGS84_reduced_df$xfold)
 handmade_WGS84_reduced_df               <- rbind.data.frame( c(ID=0, 0, min(xfold_gis_layer_field)/2), handmade_WGS84_reduced_df)   # add an ID 0 for not included coord points AND ASSUME SOME ACTIVITY IN IT
 handmade_WGS84_reduced_df$xfold         <- as.factor(handmade_WGS84_reduced_df$xfold)
 }

 # then merge to coord  (c 'poly_id' give the polygon id from the handmade_WGS84 shape file)
 if(all(is.na(coord$poly_id))) coord$poly_id <- coord$poly # debug if not a single fishing ground found in the circle! then assume all pts in circle are fishing ground!
 coord <- coord[!is.na(coord$poly_id),]
 if(nrow(merge(coord, handmade_WGS84_reduced_df, by.x="poly_id", by.y="SP_ID"))==0) {coord$poly_id <- handmade_WGS84_reduced_df$SP_ID[1]} # debug - force a merge 
 coord <- merge(coord, handmade_WGS84_reduced_df, by.x="poly_id", by.y="SP_ID")

 
 #-------------------------------------------------------------------------------
 #-------------------------------------------------------------------------------
 # reduce the dimensionality by random selection
 prop_to_keep <- 1.0
 coord <- coord[sample(x=1:nrow(coord), size=ceiling(nrow(coord)*prop_to_keep), replace=FALSE ),]
 # => e.g. keep 100 % of the potential grounds by default...but you might reduce this prop if untractable simus
 
 # caution if  prop_to_keep<1 then you´ll potentially have to redo the shortPaths library each time this present routine is re-run....
 #-------------------------------------------------------------------------------
 #-------------------------------------------------------------------------------


 
 # check
 do_plot2 <- FALSE
 if(do_plot2){
  plot(sp)
  plot(handmade_WGS84_reduced,  add=TRUE, border=as.data.frame(handmade_WGS84_reduced)[,name_gis_layer_field])
  coord$color <-  factor(coord[,name_gis_layer_field]) #init
  levels(coord$color) <- 1:length(levels(coord$color))
  points(coord[, "CELL_LONG"], coord[, "CELL_LATI"], col=  coord[,"color"], pch=16)
  } else{
  if(do_plot) points(coord[, "CELL_LONG"], coord[, "CELL_LATI"], col=  2, pch=16)
  }
  
 # or if on contineous scale:
 #par(mfrow=c(1,2))
 #plot(coord[, "x"], coord[, "y"], col= as.numeric( cut(coord[,name_gis_layer_field], breaks=c(-1,100,200, 500, 1000, 2000)) ), pch=16)
 #if("XMIN" %in% colnames(handmade_WGS84_df)) plot(handmade_WGS84_df[, "XMIN"], handmade_WGS84_df[, "YMIN"], col= as.numeric( cut(handmade_WGS84_df[,name_gis_layer_field], breaks=c(-1,100,200, 500, 1000, 2000)) ), pch=16)


 cat(paste("Produce individual vessel DISPLACE input files from", namefile, "\n"))

# WORKFLOW 2 - QUARTER-BASED-----------
# however, note that the seasonnality of the spatial and total effort application
# is not parameterized but is instead an emerging feature from the model.
 fgrounds <- NULL
 an <- function(x) as.numeric(as.character(x))
for (a.quarter in c("Q1","Q2","Q3","Q4")){

    # dispatch the feffort among nodes by dividing proba in area per the number of included graph nodes
    # first vanish the metier dimension
    fgrounds_this_quarter                                <- aggregate(coord[,name_gis_layer_field], list(coord$poly_id, coord$pt_graph, coord$ctry, coord$CELL_LONG, coord$CELL_LATI, coord$xfold), sum, na.rm=TRUE) # if several GIS layers when more than 1 metier
    colnames(fgrounds_this_quarter)                      <- c("poly_id", "pt_graph", "ctry", "CELL_LONG", "CELL_LATI", "xfold", "feffort")
    fgrounds_this_quarter                                <- cbind.data.frame(fgrounds_this_quarter, quarter=a.quarter) # init
    fgrounds_this_quarter$nb_nodes_in_this_cat           <- factor(fgrounds_this_quarter[,'poly_id']) # init
    levels(fgrounds_this_quarter$nb_nodes_in_this_cat )  <- table(as.character(fgrounds_this_quarter[,'poly_id']))
    fgrounds_this_quarter$effort_on_node                 <- an(fgrounds_this_quarter[,name_gis_layer_field]) * an(fgrounds_this_quarter[,'xfold'])  /   an(fgrounds_this_quarter$nb_nodes_in_this_cat)
    fgrounds_this_quarter$freq_feffort                   <- an(fgrounds_this_quarter$effort_on_node) /sum(an(fgrounds_this_quarter$effort_on_node))
     #=> scale to 1 to obtain a proba of visit per node
    fgrounds_this_quarter$freq_feffort  <- replace(fgrounds_this_quarter$freq_feffort, is.na(fgrounds_this_quarter$freq_feffort), 1) # debug

    # check
    if(nrow(coord[duplicated(fgrounds_this_quarter$pt_graph),])>0) browser()


    fgrounds <- rbind.data.frame(fgrounds, fgrounds_this_quarter)
 }
  cat(paste("fgrounds_this_quarter....OK", "\n"))
  

 # duplicate per vessel id  (i.e. assuming the same parameterisation for all the vesselids)
 fgrounds  <- fgrounds[fgrounds$freq_feffort!=0,] # remove if 0
 fgrounds_allvessels <- NULL
 for(vid in vesselids){
  cat(paste(vid, "\n"))
  fgrounds_allvessels <- rbind.data.frame(fgrounds_allvessels, cbind(fgrounds, vids=vid))
 }

 # duplicate per metier id (i.e. assuming the same relative effort distribution per polygon for all the metierids)
 fgrounds_allvessels_allmet <- NULL
 for(met in metierids){
  cat(paste(met, "\n"))
  fgrounds_allvessels_allmet <- rbind.data.frame(fgrounds_allvessels_allmet, cbind(fgrounds_allvessels, met))
 }


 # create the c++ input files  (CAUTION if also active Workflow 1 then need to append to the existing files...)
 # i.e.
 # vesselsspe_fgrounds_quarter
 # vesselsspe_freq_fgrounds_quarter
 # vesselsspe_harbours_quarter
 # vesselsspe_freq_harbours_quarter

  ####-------
 an <- function(x) as.numeric(as.character(x))
 for (a.quarter in c("Q1","Q2","Q3","Q4")){

    x        <- fgrounds_allvessels[ fgrounds_allvessels$quarter==a.quarter,]
    x$freq   <- round(an(x$freq_feffort) ,6)

    # save .dat files
    x$pt_graph <-  as.numeric(as.character(x$pt_graph)) - 1 ##!!! OFFSET FOR C++ !!!##
        vesselsspe_fgrounds_quarter <- x[,c('vids','pt_graph')]
        write.table(vesselsspe_fgrounds_quarter,
            file=file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep=''),
              paste("vesselsspe_fgrounds_quarter",gsub("Q","",a.quarter),".dat",sep='')),
                  col.names=ifelse(do_append, FALSE, TRUE),  row.names=FALSE, sep= ' ', quote=FALSE, append=do_append)
        vesselsspe_freq_fgrounds_quarter <- x[,c('vids','freq')]
        vesselsspe_freq_fgrounds_quarter$freq <- format(vesselsspe_freq_fgrounds_quarter$freq, scientific = FALSE)
        write.table(vesselsspe_freq_fgrounds_quarter,
          file=file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep=''),
            paste("vesselsspe_freq_fgrounds_quarter",gsub("Q","",a.quarter),".dat",sep='')),
               col.names=ifelse(do_append, FALSE, TRUE),  row.names=FALSE, sep= ' ', quote=FALSE, append=do_append)
    #-----------

   cat(paste("vesselsspe_fgrounds_quarter",gsub("Q","",a.quarter),".dat....OK", "\n"))
   cat(paste("vesselsspe_freq_fgrounds_quarter",gsub("Q","",a.quarter),".dat....OK", "\n"))




    #-----------
    # vesselsspe_harbours_quarter[xx].dat
    # vesselsspe_freq_harbours_quarter[xx].dat
     ## get back the port name
    port_names <- read.table(file.path(general$main.path.param.gis, "GRAPH", name_file_ports), sep=";", row.names=NULL, header=TRUE)
    if(!"idx_port"  %in% colnames(port_names)) stop("a field named 'idx_port' is required in the harbour.dat file!!")   
    port_names$pt_graph   <- saved_coord[match(port_names$idx_port, saved_coord[,"harb"]), "pt_graph"]
    rownames(port_names)  <- port_names[,1]
    
    visited               <- expand.grid( port_names[visited_ports, "pt_graph"], vesselids) # retrieve the corresponding pt_graph
    visited               <- visited[,2:1]
    colnames(visited)     <- c('vids','pt_graph')
    visited_freq          <- visited # init
    visited_freq[,2]      <- rep (visited_ports_frequencies)
    colnames(visited_freq)     <- c('vids','freq')

    # save .dat files
        visited$pt_graph <-  as.numeric(as.character(visited$pt_graph)) - 1 ##!!! FOR C++ !!!##
        write.table(visited,
            file=file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep=''),
              paste("vesselsspe_harbours_quarter",gsub("Q","",a.quarter),".dat",sep='')),
               col.names=ifelse(do_append, FALSE, TRUE),  row.names=FALSE, sep= ' ', quote=FALSE, append=do_append)
        visited_freq$freq <- format(visited_freq$freq, scientific = FALSE)
        write.table(visited_freq,
            file=file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep=''),
              paste("vesselsspe_freq_harbours_quarter",gsub("Q","",a.quarter),".dat",sep='')),
                col.names=ifelse(do_append, FALSE, TRUE),  row.names=FALSE, sep= ' ', quote=FALSE, append=do_append)

        # check for NAs
        dd<- visited [is.na(visited[,c('pt_graph')]) , ]
        if(nrow(dd)!=0) {print ("NAs in visited"); browser()}
        dd<- visited_freq [is.na(visited_freq[,c('freq')]) , ]
        if(nrow(dd)!=0) {print ("NAs in visited_freq"); browser()}
   #----------

    cat(paste("vesselsspe_harbours_quarter",gsub("Q","",a.quarter),".dat....OK", "\n"))
    cat(paste("vesselsspe_freq_harbours_quarter",gsub("Q","",a.quarter),".dat....OK", "\n"))


   #-----------
   # DNK00[xx]_possible_metiers_quarter2.dat
   # DNK00[xx]_freq_possible_metiers_quarter[xx].dat
    x        <-  fgrounds_allvessels_allmet[ fgrounds_allvessels_allmet$quarter==a.quarter,]
    x$vids   <- factor(x$vids)
    for(vid in unique(x$vids)){
       x.vid                <- x[  x$vids %in% vid,]
       x.vid$VE_REF         <- factor(x.vid$vids)
       x.vid$pt_graph       <- factor(x.vid$pt_graph)
       x.vid$met            <- factor(x.vid$met)

       # save .dat files (vessel-spe .dat files)
       x.vid$pt_graph            <- as.numeric(as.character(x.vid$pt_graph)) - 1 ##!!! FOR C++ !!!##
       vesselsspe_possible_metiers_quarter <- x.vid[,c('pt_graph', 'met')]
       vesselsspe_possible_metiers_quarter <- vesselsspe_possible_metiers_quarter [order(vesselsspe_possible_metiers_quarter$pt_graph),]
       write.table(vesselsspe_possible_metiers_quarter,
           file=file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep=''),
             paste(vid,"_possible_metiers_quarter",gsub("Q","",a.quarter),".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE, append=FALSE)

       vesselsspe_freq_possible_metiers_quarter <- x.vid[,c('pt_graph', 'met')]
       levels(vesselsspe_freq_possible_metiers_quarter[,'met']) <-   metierids_frequencies
       vesselsspe_freq_possible_metiers_quarter[,2]  <- as.numeric(as.character(vesselsspe_freq_possible_metiers_quarter[,2] ))
       colnames(vesselsspe_freq_possible_metiers_quarter) <- c('pt_graph', 'freq')
       vesselsspe_freq_possible_metiers_quarter <- vesselsspe_freq_possible_metiers_quarter [order(vesselsspe_freq_possible_metiers_quarter$pt_graph),]

        vesselsspe_freq_possible_metiers_quarter$freq <- format(vesselsspe_freq_possible_metiers_quarter$freq, scientific = FALSE)
       write.table(vesselsspe_freq_possible_metiers_quarter,
           file=file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep=''),
             paste(vid,"_freq_possible_metiers_quarter",gsub("Q","",a.quarter),".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE, append=FALSE)

      # check for NAs
      dd<- vesselsspe_possible_metiers_quarter [is.na(vesselsspe_possible_metiers_quarter[,c('met')]) , ]
        if(nrow(dd)!=0) {print ("NAs in vesselsspe_possible_metiers_quarter"); browser()}
      dd <- vesselsspe_freq_possible_metiers_quarter [is.na(vesselsspe_freq_possible_metiers_quarter[,c('freq')]) , ]
        if(nrow(dd)!=0){print ("NAs in vesselsspe_freq_possible_metiers_quarter"); browser()}

       cat(paste(vid,"_possible_metiers_quarter",gsub("Q","",a.quarter),".dat....OK", "\n"))
       cat(paste(vid,"_freq_possible_metiers_quarter",gsub("Q","",a.quarter),".dat....OK", "\n"))
     } # end vid
     #----------


   #-----------
   #-----------
   # DNK000XXX_gshape_cpue_per_stk_on_nodes_quarter  # plan A
   # DNK000XXX_gscale_cpue_per_stk_on_nodes_quarter  # plan A
    x        <- fgrounds_allvessels[fgrounds_allvessels$quarter==a.quarter,]
    x$vids   <- factor( x$vids )

    for(vid in unique(x$vids)){
       x.vid                <- x[  x$vids %in% vid,]
       x.vid$VE_REF         <- factor(x.vid$vids)
       x.vid$pt_graph       <- factor(x.vid$pt_graph)
       x.vid$pt_graph       <- as.numeric(as.character(x.vid$pt_graph)) - 1 ##!!! FOR C++ !!!##

       vesselsspe_gshape_cpue_per_stock_fgrounds_quarter <-   cbind.data.frame(rep(x.vid$pt_graph, each=nb_stocks), gshape_cpue_per_stock)
       colnames(vesselsspe_gshape_cpue_per_stock_fgrounds_quarter) <-  c('pt_graph', 'shape')

       # save .dat files
       write.table(vesselsspe_gshape_cpue_per_stock_fgrounds_quarter,
           file=file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep=''),
             paste(vid,"_gshape_cpue_per_stk_on_nodes_quarter",gsub("Q","",a.quarter),".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE, append=FALSE)
    
       cat(paste(vid,"_gshape_cpue_per_stk_on_nodes_quarter",gsub("Q","",a.quarter),".dat....OK", "\n"))
 
    }
   # DNK000XXX_gscale_cpue_per_stk_on_nodes_quarter  # plan A
    #-----------
    x        <- fgrounds_allvessels[fgrounds_allvessels$quarter==a.quarter,]
    x$vids   <- factor( x$vids )

    for(vid in unique(x$vids)){
       x.vid                <- x[  x$vids %in% vid,]
       x.vid$VE_REF         <- factor(x.vid$vids)
       x.vid$pt_graph       <- factor(x.vid$pt_graph)
       x.vid$pt_graph       <- as.numeric(as.character(x.vid$pt_graph)) - 1 ##!!! FOR C++ !!!##

       vesselsspe_gscale_cpue_per_stock_fgrounds_quarter <-   cbind.data.frame(rep(x.vid$pt_graph, each=nb_stocks), gscale_cpue_per_stock)
       colnames(vesselsspe_gscale_cpue_per_stock_fgrounds_quarter) <-  c('pt_graph', 'scale')

       # save .dat files
       write.table(vesselsspe_gscale_cpue_per_stock_fgrounds_quarter,
           file=file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep=''),
             paste(vid,"_gscale_cpue_per_stk_on_nodes_quarter",gsub("Q","",a.quarter),".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE, append=FALSE)
 
       cat(paste(vid,"_gscale_cpue_per_stk_on_nodes_quarter",gsub("Q","",a.quarter),".dat....OK", "\n"))
    }
    #-----------



   #-----------
   #-----------
   # DNK000XXX__cpue_per_stk_on_nodes_quarter        # plan B
    x        <- fgrounds_allvessels[fgrounds_allvessels$quarter==a.quarter,]
    x$vids   <- factor( x$vids )

    for(vid in unique(x$vids)){
       x.vid                <- x[  x$vids %in% vid,]
       x.vid$VE_REF         <- factor(x.vid$vids)
       x.vid$pt_graph       <- factor(x.vid$pt_graph)
       x.vid$pt_graph       <- as.numeric(as.character(x.vid$pt_graph)) - 1 ##!!! FOR C++ !!!##

       vesselsspe_fixed_cpues_fgrounds_quarter <-   cbind.data.frame(rep(x.vid$pt_graph, each=nb_stocks), fixed_cpue_per_stock)
       colnames(vesselsspe_fixed_cpues_fgrounds_quarter) <-  c('pt_graph', 'cpue_kghour')

       # save .dat files
       write.table(vesselsspe_fixed_cpues_fgrounds_quarter,
           file=file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep=''),
             paste(vid,"_cpue_per_stk_on_nodes_quarter",gsub("Q","",a.quarter),".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE, append=FALSE)
    
       cat(paste(vid,"_cpue_per_stk_on_nodes_quarter",gsub("Q","",a.quarter),".dat....OK", "\n"))
    
    }
   #-----------


   #-----------
   #-----------
   #vesselsspe_features_quarter1

   vesselsspe_features_quarter <- cbind.data.frame (vesselids,  matrix(rep(vessel_features, length(vesselids)),ncol= length(vessel_features), byrow=TRUE) )
     # save .dat files
       write.table(vesselsspe_features_quarter,
           file=file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep=''),
             paste("vesselsspe_features_quarter",gsub("Q","",a.quarter),".dat",sep='')),
               col.names=FALSE,  row.names=FALSE, quote=FALSE, append=do_append, sep = "|")
  
       cat(paste("vesselsspe_features_quarter",gsub("Q","",a.quarter),".dat....OK", "\n"))
   #-----------


 } # end a.quarter






# WORKFLOW 2 - SEMESTER-BASED-----------
 for (a.semester in 1:2){


   #-----------
   #-----------
   # vesselsspe_percent_tacs_per_pop_semester
   vesselsspe_percent_tacs_per_pop <- cbind.data.frame(rep(vesselids, each=nb_stocks), rep(100/ length(vesselids), length(vesselids)) )
   #colnames(vesselsspe_percent_tacs_per_pop) <- c('vid', 'percent')
   colnames(vesselsspe_percent_tacs_per_pop) <- c('V1', 'V2')


   if(do_append && file.exists(file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep=''),
             paste("vesselsspe_percent_tacs_per_pop_semester",a.semester,".dat",sep='')))){ # need to re-compute the percent for ALL vessels i.e. existing and incomers depending on step_in_share
      existing <- read.table(file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep=''),
             paste("vesselsspe_percent_tacs_per_pop_semester",a.semester,".dat",sep='')), header=TRUE)
      existing[,2] <-  an(existing[,2])* (100-rep(step_in_share, length(unique(existing[,1])) ))/100

      if(any(vesselsspe_percent_tacs_per_pop[,1] %in% existing[,1])) stop("Check incomers vesselids...already there!")

      vesselsspe_percent_tacs_per_pop[,2] <- vesselsspe_percent_tacs_per_pop[,2]  * (rep(step_in_share, length(vesselids) ))/100

      vesselsspe_percent_tacs_per_pop <- rbind.data.frame(existing, vesselsspe_percent_tacs_per_pop)
   }

     # if(floor(sum(vesselsspe_percent_tacs_per_pop[,2])) != (100*nb_stocks)) stop("The TAC share over vessels per stock should sum to 100%...check the dimension")

     # save .dat files
       write.table(vesselsspe_percent_tacs_per_pop,
           file=file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep=''),
             paste("vesselsspe_percent_tacs_per_pop_semester",a.semester,".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, quote=FALSE, append=FALSE, sep = " ")
   
         cat(paste("vesselsspe_percent_tacs_per_pop_semester",a.semester,".dat....OK", "\n"))
 
   #-----------



   #-----------
   #-----------
   ## VESSEL SPE----------
   # vesselsspe_betas_semester
   vesselsspe_betas_semester <- cbind.data.frame(rep(vesselids, each=nb_stocks), rep(vesselsspe_betas, length(vesselids)) )
   colnames(vesselsspe_betas_semester) <- c('VE_REF', 'beta.VE_REF')

     # save .dat files
       write.table(vesselsspe_betas_semester,
           file=file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep=''),
             paste("vesselsspe_betas_semester", a.semester,".dat",sep='')),
               col.names=ifelse(do_append, FALSE, TRUE),  row.names=FALSE, quote=FALSE, append=do_append, sep = " ")
  
         cat(paste("vesselsspe_betas_semester",a.semester,".dat....OK", "\n"))
 

 
 }

 # WORKFLOW 2 - ADDITIONAL FILE(S)-----------
 if(create_file_for_fuel_price_per_vessel_size){
  fuel_price_per_vessel_size <- data.frame(
                                        vsize=c(0:4),
                                        fuelprice_euro= some_fuel_price_per_vessel_size
                                        )
    # save .dat files
       write.table(fuel_price_per_vessel_size,
           file=file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep=''),
             paste("fuel_price_per_vessel_size.dat",sep='')),
               col.names=TRUE,  row.names=FALSE, quote=FALSE, append=FALSE, sep = " ")

       cat(paste("fuel_price_per_vessel_size.dat....OK", "\n"))
 }


# fishing credits allocation taken from the pool
  share_of_fishing_credits <- cbind.data.frame(vesselids,  100/length(vesselids))

  if(do_append && file.exists(file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep=''), paste("initial_fishing_credits_per_vid.dat",sep='')))
             ){ # need to re-compute the percent for ALL vessels i.e. existing and incomers depending on step_in_share
      existing <- read.table(file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep=''),
             paste("initial_fishing_credits_per_vid.dat",sep='')), header=TRUE)
      existing[,2] <-  an(existing[,2])* (100-rep(step_in_share_credits, length(unique(existing[,1])) ))/100

      if(any(share_of_fishing_credits[,1] %in% existing[,1])) stop("Check incomers vesselids...already there!")

      share_of_fishing_credits[,2] <- share_of_fishing_credits[,2]  * (rep(step_in_share_credits, length(vesselids) ))/100

      colnames(share_of_fishing_credits) <- colnames(existing)
      fishing_credits <- rbind.data.frame(existing, share_of_fishing_credits)

   } else{
     fishing_credits            <- cbind.data.frame(vesselids, an(share_of_fishing_credits[,2])*10000 )
     colnames(fishing_credits)  <- c("VE_REF", "annual_fishing_credits_per_vid")
     fishing_credits$annual_fishing_credits_per_vid <- format(fishing_credits$annual_fishing_credits_per_vid, scientific = FALSE)
   }


# save .dat files
       write.table(fishing_credits,
           file=file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep=''),
             paste("initial_fishing_credits_per_vid.dat",sep='')),
               col.names=TRUE,  row.names=FALSE, quote=FALSE, append=FALSE, sep = " ")

       cat(paste("initial_fishing_credits_per_vid.dat....OK", "\n"))

 cat(paste("............done for", namefile, "\n"))

} # END LOOP OVER CONFIG FILES



  cat(paste("lastly, collect fgrounds and frequency on fgrounds in one single file for all vessels", "\n"))
 ## LASTLY,
   # fix the files to avoid vessel fishing in harbour (!)
   # but do not remove a vessel by inadvertance....
   
 
 for (a.quarter in c("Q1","Q2","Q3","Q4")){

    port_names             <- read.table(file.path(general$main.path.param.gis, "GRAPH", name_file_ports), sep=";", row.names=NULL, header=TRUE)
    rownames(port_names)   <- port_names[,1]   
    port_names$pt_graph    <- saved_coord[match(port_names$idx_port, saved_coord[,"harb"]), "pt_graph"]
    port_names$pt_graph    <- as.numeric(as.character(port_names$pt_graph)) - 1 ##!!! FOR C++ !!!##

    # 1
    vesselsspe_fgrounds_quarter <- read.table(file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep=''),
              paste("vesselsspe_fgrounds_quarter",gsub("Q","",a.quarter),".dat",sep='')), header=TRUE)
    idx  <- vesselsspe_fgrounds_quarter$pt_graph %in%  port_names$pt_graph
    vids_before <- vesselsspe_fgrounds_quarter[,1]
    vesselsspe_fgrounds_quarter <- vesselsspe_fgrounds_quarter[!idx,]  # correct
    vids_after <- vesselsspe_fgrounds_quarter[,1]
    #print(vids_before[!vids_before %in% vids_after])  #check if all vid kept on board...if not you are in trouble....

    colnames(vesselsspe_fgrounds_quarter) <- c("vids", "pt_graph")


    write.table(vesselsspe_fgrounds_quarter,
          file=file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep=''),
            paste("vesselsspe_fgrounds_quarter",gsub("Q","",a.quarter),".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE, append=FALSE)
    # 2
    vesselsspe_freq_fgrounds_quarter <- read.table(file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep=''),
              paste("vesselsspe_freq_fgrounds_quarter",gsub("Q","",a.quarter),".dat",sep='')), header=TRUE)
    vesselsspe_freq_fgrounds_quarter <- vesselsspe_freq_fgrounds_quarter[!idx,]  # correct
    colnames(vesselsspe_freq_fgrounds_quarter) <- c("vids", "freq")


    vesselsspe_freq_fgrounds_quarter$freq <- format(vesselsspe_freq_fgrounds_quarter$freq, scientific = FALSE)
    write.table(vesselsspe_freq_fgrounds_quarter,
          file=file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep=''),
            paste("vesselsspe_freq_fgrounds_quarter",gsub("Q","",a.quarter),".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE, append=FALSE)
    }
    #-----------





    cat(paste("lastly, produce additional files e.g. related to the catch equation", "\n"))
    cat(paste("metier effect in the catch equation", "\n"))
   ## ADDITIONAL FILES FOR THE CATCH EQUATION - METIER EFFECT
   spp_table    <- read.table(file=file.path(general$main.path.param.gis, "POPULATIONS", paste("pop_names_",general$application ,".txt",sep='')), header=TRUE)
   spp          <- as.character(spp_table$spp)
   metier_names <- read.table( file=file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep=''), "metier_names.dat"), header=TRUE)
                                              
    for (a.semester in 1:2){
   #-----------
   #-----------
   ## METIER SPE----------
      # export betas specific to the metier given this pop
      # mean estimates
      nb_met         <- (nrow(metier_names))
      nb_stk         <- length(spp)
      # Note that we assume exp(vesseleffect+metiereffect+stockdensityeffect*sel) in the catch equation 
      # so you will have to put -20 if this actually metier not catching this stock at all....for now we put 0
      
      metiersspe_gamma_semester <- as.data.frame(metierspe_betas_all)
      colnames(metiersspe_gamma_semester) <- c('LE_MET_level6', 'gamma.LE_MET_level6')
      if(length(unique(metiersspe_gamma_semester[,'LE_MET_level6'])) != nb_met)   stop("missing metier(s) for info on metier effects")

      # reorder:
      library(doBy)
      metiersspe_gamma_semester <- orderBy(~LE_MET_level6, data=metiersspe_gamma_semester)
      
     
      # save .dat files
       write.table(metiersspe_gamma_semester,
           file=file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep=''),
             paste("metierspe_betas_semester", a.semester,".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, quote=FALSE, append=FALSE, sep = " ")
  

 

 } # end a.semester




   cat(paste("metier effect in the catch equation", "\n"))
   ## ADDITIONAL FILES FOR THE CATCH EQUATION - AVAI EFFECT
   selected_szgroups        <- c('0', '2', '3', '5', '7')  # DISPLACE hardcoding
   colnames (avaispe_betas) <- 0:(ncol(avaispe_betas)-1)
    for (a.semester in 1:2){
   #-----------
   #-----------
    ## POP SPE----------
      for(rg in selected_szgroups){
        # export betas specific to the avai szgroup given this pop (caution: remenber the scaling i.e *1000)
        # mean estimates
         #popsspe_delta_semester <- cbind.data.frame(0:(length(spp)-1), rep(0, length(0:(length(spp)-1))) )
         popsspe_delta_semester <- cbind.data.frame(0:(length(spp)-1), avaispe_betas[,rg])
         colnames(popsspe_delta_semester) <- c('pop', 'delta.nb_indiv')

        # save .dat files
        write.table(popsspe_delta_semester,
           file=file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=''),
             paste("avai", rg, "_betas_semester", a.semester,".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, quote=FALSE, append=FALSE, sep = " ")
     
           cat(paste("Write avai", rg, "_betas_semester", a.semester,".dat....done \n"))      
        } # end rg
   } # end a.semester


    # the selected groups in the catch rate equation
    write(c('nm2',  'selected_szgroups'),
                   file=file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=''),
                       paste("the_selected_szgroups.dat",sep=' ')), append=FALSE,  ncol=2,
                          sep=" ")
                         
    for(sp in (0:(length(spp)-1))){
       write.table(cbind(nm2=sp, selected_szgroups=selected_szgroups),
                   file=file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=''),
                       paste("the_selected_szgroups.dat",sep=' ')), append=TRUE,
                         quote = FALSE, sep=" ", col.names=FALSE, row.names=FALSE)
 
    }  # end sp
 cat(paste("Write the_selected_szgroups.dat....done \n"))
 
 


cat("Remenber that because some new fgrounds are created \n you will have (in DISPLACE GUI) to derive a new graph with a new shortestPath library (say 57 by loading the graph 56 and then create the shortestPaths) \n and also create a new baseline.dat /simusspe with the right missing port idx (say 9979) (absence of the right index for the port makes the ui crash)\n")






