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
  
  
  #if(general$application=="adriatic")   name_GIS_file <- "Benthos_GSA17"
  #if(general$application=="testexample")  name_GIS_file <- "landscapes.tif"
  #if(general$application=="testexample")  name_GIS_file <- "biomc_rfmodel_Gogina_et_al_2016_Figure7_wgs84" 
  if(general$application=="testexample")  name_GIS_file <- "habitat_landscapes" # this name used by the Objects Editor ui
  
   cat(paste("START \n"))

## INFORM THE BENTHOS SPATIALLY ON NODES 
# should be useful to model the dynamic of the benthic communities
# under different spatial fishing pressures

# obtain XXloss_after_one_passage_per_landscape_per_func_group.dat per metier

# per node
# 1. attach a marine landscape
# 2. attach benthos abundances
# 3. attach a mortality function
# 4. attach a recovery function
     
  
  dir.create(file.path(general$main.path.ibm, paste("benthosspe_", general$application, sep='')))
  dir.create(file.path(general$main.path.ibm, "graphsspe"))
                                                    
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

 
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##
  ##----------------------- OBTAIN A MARINE LANDSCAPE -------------------------##
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##

 # the code is only provided as info here - better to use the DISPLACE GUI with Graph>Assign Landscape code
 if(FALSE){
 
  ## FROM A SHAPE FILE 
  #------------------
  # SHAPE-----------
  #------------------
   library(maptools)
   anf                  <- function(x) as.numeric(as.character(x))

   ## load shape.......
   landscapes       <- readShapePoly(file.path(general$main_path_gis, "HABITATS", name_GIS_file),
                                       proj4string=CRS("+proj=longlat +datum=WGS84")    )    # probably need an update of rgdal here....
   cat(paste("Read GIS shape file (this one must include a CATEGORY field)...done\n"))

   coord            <- cbind(x=anf(coord[,'x']), y=anf(coord[,'y']))
  
   # convert to UTM
   library(sp)
   library(rgdal)
   spo <- SpatialPoints(cbind(as.numeric(as.character(coord[,'x'])), as.numeric(as.character(coord[,'y']))),
                       proj4string=CRS("+proj=longlat +datum=WGS84"))


  # use the magic 'over' function to see in which polygon it is located
  idx                           <- over(spo, landscapes); #print(idx)
  coord                         <- cbind.data.frame(as.data.frame(coord), landscapes_code=NA)
  coord$landscapes_code         <- as.character(idx$CATEGORY)
  cat(paste("overlay...done\n"))


  
   # assign 0 to the NA code
   coord[is.na(coord$landscapes_code), 'landscapes_code'] <- 0
  
   # a visual check
   if(do_plot) plot(coord[,1], coord[,2], col=coord[,3])

 
 
  # EXPORT FOR C++------------
  write(coord[, 'landscapes_code'], file=file.path(general$main.path,"graphsspe", paste("coord", general$igraph,"_with_landscape.dat", sep='')), ncol=1)
  nrow(coord)
  ncol(coord)
  cat(paste("Write coordXX_with_landscape.dat in /graphsspe...done\n"))
  #----------------------------

}


  if(FALSE){ # RASTER layer currently not handled by the DISPLACE Objects Editor
  #------------------
  # RASTER-----------
  #------------------
   library(maptools)
   library(raster)
   anf                  <- function(x) as.numeric(as.character(x))
  
   ## use point-raster overlay.......
   library(raster)
   landscapes       <- raster(file.path(general$main_path_gis, "HABITATS", name_GIS_file))    # probably need an update of rgdal here....
 
  
   coord            <- cbind(x=anf(coord[,'x']), y=anf(coord[,'y']))
  
   # convert to UTM
   library(sp)
   library(rgdal)
   SP <- SpatialPoints(cbind(as.numeric(as.character(coord[,'x'])), as.numeric(as.character(coord[,'y']))),
                       proj4string=CRS("+proj=longlat +datum=WGS84"))
   a_utm_zone <- "34"
   coord      <- cbind.data.frame(coord,
                                  spTransform(SP, CRS(paste("+proj=utm +zone=",a_utm_zone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep=''))))    # convert to UTM


   dd <- extract (landscapes, coord[,3:4]) # get the landscape on the coord points!

   coord <- cbind(coord,  landscapes_code=dd)  # look at “Towards a representative MPA network in the Baltic Sea”.


   # translate the coding, just for info...
   bottom   <- substr(coord$landscapes_code, 1,1)
   photic   <- substr(coord$landscapes_code, 2,2)
   salinity <- substr(coord$landscapes_code, 3,3)
  
  
   bottom_levels <- factor(bottom)
   levels(bottom_levels) <- c('Bedrock', 'Hard Bottom', 'Sand', 'Hard Clay', 'Mud')
 
   photic_levels <- factor(photic)
   levels(photic_levels) <- c('Photic', 'Aphotic')
  
   salinity_levels <- factor(salinity)
   levels(salinity_levels) <- c('0-5psu', '5-7.5psu', '7.5-11psu', '11-18psu', '18-30psu', '>30psu')
 
   coord <- cbind.data.frame (coord, landscape=paste(bottom_levels, photic_levels, salinity_levels, sep="_"))
 
   # just for info...
   percent_habitats <- round(table(coord$landscape)/sum(table(coord$landscape))*100, 2) # get an idea of the importance of a given landscape in terms of number of nodes over the total number of nodes
   sum(percent_habitats) # should be 100
 
   percent_habitats[order(percent_habitats, decreasing=TRUE)]
 
 
  
   # assign 0 to the NA code
   coord[is.na(coord$landscapes_code), 'landscapes_code'] <- 0
  
  
 
 
  # EXPORT FOR C++------------
  write(coord[, 'landscapes_code'], file=file.path(general$main.path.ibm, "graphsspe", paste("coord", general$igraph,"_with_landscape.dat", sep='')), ncol=1)
  nrow(coord)
  ncol(coord)
  #----------------------------
  } # end if raster
                                                
 
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##
  ##----------------------- OBTAIN A BIOMASS ON NODE -------------------------##
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##

   # use the DISPLACE GUI with Graph>Assign Benthos Biomass 
  
   ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##
  ##----------------------- OBTAIN A Number ON NODE -------------------------##
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##

   # use the DISPLACE GUI with Graph>Assign Benthos Number 
  
   
 ##----------------------------------------------------------------------------##
 ##----------------------------------------------------------------------------##
 ##----------------------------------------------------------------------------##
 ##----------------------------------------------------------------------------##
 ##----------------------------------------------------------------------------##
 ##----------------------------------------------------------------------------##

 ## FROM THE GRAPH file map
 landscape_per_node <- read.table(file=file.path(general$main.path.ibm, "graphsspe", 
           paste("coord", general$igraph,"_with_landscape.dat", sep='')))

   # translate the coding, CAUTION just a fake example here...
   #bottom   <- substr(as.numeric(as.character(landscape_per_node[,1])), 1,1)
   #photic   <- substr(as.numeric(as.character(landscape_per_node[,1])), 2,2)
   #salinity <- substr(as.numeric(as.character(landscape_per_node[,1])), 3,3)


   #bottom_levels <- factor(bottom)
   #levels(bottom_levels) <- c('Deep', 'Not_deep')

   #photic_levels <- factor(photic)
   #levels(photic_levels) <- c('NA','Photic', 'Aphotic')

   #salinity_levels <- factor(salinity)
   #levels(salinity_levels) <- c('NA', '<30psu', '>30psu')

   #landscape_per_node <- cbind.data.frame (landscape_per_node, landscape=paste(bottom_levels, photic_levels, salinity_levels, sep="_"))


   codes <- unique(landscape_per_node[,1])
   codes <- codes[order(codes)]

 
  
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##
  ##-------------- OBTAIN A RECOVERY FUNCTION PER FUNCTIONAL GROUP-------------##
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##
  
  
  # logistic recovery rate (r, month-1) for habitat and functional group
  
   # for the time being,
   # creating files with fake copy/paste info
   # and assuming at least two functional groups (--> will be a multimap in c++)


 if(!file.exists(file.path(general$main_path_gis, "HABITATS", "logistic_recovery_rates_per_month_per_funcgr.csv"))){
     logistic_recovery_rates_per_month_per_funcgr <- cbind.data.frame(
                                                 landscape=rep(codes, each=2), # assuming 2 func grps
                                                 logistic_recovery_rate_per_month=0.4  
                                                 ) 
     write.table(logistic_recovery_rates_per_month_per_funcgr, file=file.path(general$main.path.ibm, paste("benthosspe_", general$application, sep=''),
                        paste("logistic_recovery_rates_per_month_per_funcgr.dat", sep='')), col.names=TRUE, row.names=FALSE, quote=FALSE)
 } else{
   recovery <- read.table(file.path(general$main_path_gis, "HABITATS", "logistic_recovery_rates_per_month_per_funcgr.csv"), header=TRUE, sep=";")
        logistic_recovery_rates_per_month_per_funcgr <- recovery[,c('landscape', 'logistic_recovery_rate_per_month')]
        write.table(logistic_recovery_rates_per_month_per_funcgr, file=file.path(general$main.path.ibm, paste("benthosspe_", general$application, sep=''),
                        paste("logistic_recovery_rates_per_month_per_funcgr.dat", sep='')), col.names=TRUE, row.names=FALSE, quote=FALSE)
   
 }                           

   
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##
  ##-------------- INFORM A BENTHOS CARRYING CAPACITY K -----------------------##
  ##-------------- PER LANDSCAPE PER FUNCTIONAL GROUP -------------------------##
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##
  
  
   # for the time being,
   # creating files with fake copy/paste info
   # and assuming at least two functional groups (--> will be a multimap in c++)

 # biomass
 if(!file.exists(file.path(general$main_path_gis, "HABITATS", "benthos_biomass_carrying_capacity_K_per_landscape_per_funcgr.csv"))){
     benthos_biomass_carrying_capacity_K_per_landscape_per_funcgr <- cbind.data.frame(
                                                 landscape=rep(codes, each=2), # assuming 2 func grps
                                                 benthos_biomass_carrying_capacity_K=500 
                                                 ) 
     write.table(benthos_biomass_carrying_capacity_K_per_landscape_per_funcgr, file=file.path(general$main.path.ibm, paste("benthosspe_", general$application, sep=''),
                        paste("benthos_biomass_carrying_capacity_K_per_landscape_per_funcgr.dat", sep='')), col.names=TRUE, row.names=FALSE, quote=FALSE)
 } else{
   K <- read.table(file.path(general$main_path_gis, "HABITATS", "benthos_biomass_carrying_capacity_K_per_landscape_per_funcgr.csv"), header=TRUE, sep=";")
        benthos_biomass_carrying_capacity_K_per_landscape_per_funcgr <- K[,c('landscape', 'benthos_biomass_carrying_capacity_K')]
        write.table(benthos_biomass_carrying_capacity_K_per_landscape_per_funcgr, file=file.path(general$main.path.ibm, paste("benthosspe_", general$application, sep=''),
                        paste("benthos_biomass_carrying_capacity_K_per_landscape_per_funcgr.dat", sep='')), col.names=TRUE, row.names=FALSE, quote=FALSE)
   
 }                           

 # number
 if(!file.exists(file.path(general$main_path_gis, "HABITATS", "benthos_number_carrying_capacity_K_per_landscape_per_funcgr.csv"))){
     benthos_number_carrying_capacity_K_per_landscape_per_funcgr <- cbind.data.frame(
                                                 landscape=rep(codes, each=2), # assuming 2 func grps
                                                 benthos_number_carrying_capacity_K=500 
                                                 ) 
     write.table(benthos_number_carrying_capacity_K_per_landscape_per_funcgr, file=file.path(general$main.path.ibm, paste("benthosspe_", general$application, sep=''),
                        paste("benthos_number_carrying_capacity_K_per_landscape_per_funcgr.dat", sep='')), col.names=TRUE, row.names=FALSE, quote=FALSE)
 } else{
   K <- read.table(file.path(general$main_path_gis, "HABITATS", "benthos_number_carrying_capacity_K_per_landscape_per_funcgr.csv"), header=TRUE, sep=";")
        benthos_number_carrying_capacity_K_per_landscape_per_funcgr <- K[,c('landscape', 'benthos_number_carrying_capacity_K')]
        write.table(benthos_number_carrying_capacity_K_per_landscape_per_funcgr, file=file.path(general$main.path.ibm, paste("benthosspe_", general$application, sep=''),
                        paste("benthos_number_carrying_capacity_K_per_landscape_per_funcgr.dat", sep='')), col.names=TRUE, row.names=FALSE, quote=FALSE)
   
 }                           

  
  ##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
##------------------CREATE ONE FILE .dat PER METIER FOR-----------------------##
##--------------------LOSS AFTER ONE PASSAGE----------------------------------##
##----------------------PER MARINE LANDSCAPE----------------------------------##
##----------------------------------------------------------------------------##


 # for the time being,
 # creating files with fake copy/paste info
 # and assuming at least two functional groups (--> will be a multimap in c++)
 
 
 # reuse the exported metier names in GenerateVesselConfigFiles.R
 metier_names <-  read.table(
       file=file.path(general$main.path.ibm,  paste("metiersspe_", general$application, sep=''), "metier_names.dat"),
          header=TRUE)
  
 max_met <-  max(as.numeric(metier_names[,1]))

 if(!file.exists(file.path(general$main_path_gis, "HABITATS", "prop_loss_on_habitat_after_one_passage_per_metier_per_sz.csv"))){
   for(met in 0: max_met){
     loss_after_one_passage_this_metier <- cbind.data.frame(
                                                 landscape=rep(codes, each=2),
                                                 loss_after_one_passage=0.20  # 20% (fake)
                                                 ) 
     write.table(loss_after_one_passage_this_metier, file=file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep=''),
                        paste(met,"loss_after_one_passage_per_landscape_per_func_group.dat", sep='')), col.names=TRUE, row.names=FALSE, quote=FALSE)
     }
 } else{
   loss <- read.table(file.path(general$main_path_gis, "HABITATS", "prop_loss_on_habitat_after_one_passage_per_metier_per_sz.csv"), header=TRUE, sep=";")
   for(met in 0: max_met){
        loss_after_one_passage_this_metier <- loss[loss$metier==met, c('landscape', 'loss_after_one_passage')]
        write.table(loss_after_one_passage_this_metier, file=file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep=''),
                        paste(met,"loss_after_one_passage_per_landscape_per_func_group.dat", sep='')), col.names=TRUE, row.names=FALSE, quote=FALSE)
   
   }
 }                           


##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
##------------------CREATE ONE FILE .dat FOR ESTIMATES OF---------------------##
##--------------------OF PROP BIOMASS OR NUMBER PER CELL PER FUNCTIONAL GROUP-##
##----------------------PER MARINE LANDSCAPE----------------------------------##
##----------------------------------------------------------------------------##



   if(!file.exists(file.path(general$main_path_gis, "HABITATS", "prop_funcgr_biomass_per_node_per_landscape.csv"))){
       prop_funcgr_biomass_per_node_per_landscape <- cbind.data.frame(landscape=rep(codes, each=2), prop_funcgr_per_node=0.5)  # sum to 1 per node per landscape 
       write.table(prop_funcgr_biomass_per_node_per_landscape, file=file.path(general$main.path.ibm, paste("benthosspe_", general$application, sep=''),
                        paste("prop_funcgr_biomass_per_node_per_landscape.dat", sep='')), col.names=TRUE, row.names=FALSE, quote=FALSE)
  } else{
     prop_funcgr_biomass_per_node_per_landscape <- read.table(file.path(general$main_path_gis, "HABITATS", "prop_funcgr_biomass_per_node_per_landscape.csv"), header=TRUE, sep=";")
       write.table(prop_funcgr_biomass_per_node_per_landscape, file=file.path(general$main.path.ibm, paste("benthosspe_", general$application, sep=''),
                        paste("prop_funcgr_biomass_per_node_per_landscape.dat", sep='')), col.names=TRUE, row.names=FALSE, quote=FALSE)
  
  }


 
   if(!file.exists(file.path(general$main_path_gis, "HABITATS", "prop_funcgr_number_per_node_per_landscape.csv"))){
       prop_funcgr_number_per_node_per_landscape <- cbind.data.frame(landscape=rep(codes, each=2), prop_funcgr_per_node=0.5)  # sum to 1 per node per landscape 
       write.table(prop_funcgr_number_per_node_per_landscape, file=file.path(general$main.path.ibm, paste("benthosspe_", general$application, sep=''),
                        paste("prop_funcgr_number_per_node_per_landscape.dat", sep='')), col.names=TRUE, row.names=FALSE, quote=FALSE)
  } else{
     prop_funcgr_number_per_node_per_landscape <- read.table(file.path(general$main_path_gis, "HABITATS", "prop_funcgr_number_per_node_per_landscape.csv"), header=TRUE, sep=";")
       write.table(prop_funcgr_number_per_node_per_landscape, file=file.path(general$main.path.ibm, paste("benthosspe_", general$application, sep=''),
                        paste("prop_funcgr_number_per_node_per_landscape.dat", sep='')), col.names=TRUE, row.names=FALSE, quote=FALSE)
  
  }


##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
##------------------CREATE ONE FILE .dat FOR ESTIMATES OF---------------------##
##--------------------MEAN WEIGHT PER FUNCTIONAL GROUP-##
##----------------------PER MARINE LANDSCAPE----------------------------------##
##----------------------------------------------------------------------------##



   if(!file.exists(file.path(general$main_path_gis, "HABITATS", "meanw_funcgr_per_landscape.csv"))){
       meanw_funcgr_per_landscape <- cbind.data.frame(landscape=rep(codes, each=2), meanweight_funcgr_per_node=0.5)  # sum to 1 per node per landscape 
       write.table(meanw_funcgr_per_landscape, file=file.path(general$main.path.ibm, paste("benthosspe_", general$application, sep=''),
                        paste("meanw_funcgr_per_landscape.dat", sep='')), col.names=TRUE, row.names=FALSE, quote=FALSE)
  } else{
     meanw_funcgr_per_landscape <- read.table(file.path(general$main_path_gis, "HABITATS", "meanw_funcgr_per_landscape.csv"), header=TRUE, sep=";")
       write.table(meanw_funcgr_per_landscape, file=file.path(general$main.path.ibm, paste("benthosspe_", general$application, sep=''),
                        paste("meanw_funcgr_per_landscape.dat", sep='')), col.names=TRUE, row.names=FALSE, quote=FALSE)
  
  }


cat(paste(".....stored in", general$main.path.ibm, "/benthossspe \n"))

cat(paste(".....done\n"))

