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
 
  ## FROM A SHAPE FILE (OR A RASTER FILE)
  
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
  ##----------------------- OBTAIN A ABUNDANCE PER FUNCTIONAL GROUP------------##
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##

   # TO DO e.g. from Gogina et al 2016 ICESJMS for balticRTI
   
   
   
  
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##
  ##-------------- OBTAIN A MORTALITY FUNCTION PER FUNCTIONAL GROUP------------##
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##

   # TO DO
 
   # a fake example:
   maxpassage                              <- 10
   Fk_this_metier                          <- 1: maxpassage # frequency of passage on the landscape-node
   loss_after_one_passage_this_metier_this_landscape_j <- 0.20 # 20%
   a_trawl_swept_area_per_tstep_km2        <- (0.300 * 4*1.853) # swept 300m fishing at 4knots 
   area_around_node_km2                    <- 2.5*2.5 # graph node spaced by 2.5 km
   M_on_landscape_j                        <- 1-((1-(loss_after_one_passage_this_metier_this_landscape_j*(a_trawl_swept_area_per_tstep_km2/area_around_node_km2)))^Fk_this_metier)
 
   # convert in an instantaneous depletion rate
   M_per_tstep <- rep(0, 9)
   for(passage in 1: 9)
      M_per_tstep[passage] <- - ( (1-M_on_landscape_j[passage+1]) -  (1-M_on_landscape_j[passage]) )/     (1-M_on_landscape_j[passage])

   # then check:
   N <- 1000 
   for(passage in 1: 9){
     N<- N* (1-M_per_tstep[passage])
    print(N)
     }
   
   # which should be equivalent to: 
   N <- 1000
   N <- N*(1-M_on_landscape_j[9])
   print(N)  
     
   # so we can summarize by applying the following at each time step,
   M_on_landscape_j                        <- 1-((1-(loss_after_one_passage_this_metier_this_landscape_j*(a_trawl_swept_area_per_tstep_km2/area_around_node_km2))))
  
   
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
   bottom   <- substr(as.numeric(as.character(landscape_per_node[,1])), 1,1)
   photic   <- substr(as.numeric(as.character(landscape_per_node[,1])), 2,2)
   salinity <- substr(as.numeric(as.character(landscape_per_node[,1])), 3,3)


   bottom_levels <- factor(bottom)
   levels(bottom_levels) <- c('Deep', 'Not_deep')

   photic_levels <- factor(photic)
   levels(photic_levels) <- c('NA','Photic', 'Aphotic')

   salinity_levels <- factor(salinity)
   levels(salinity_levels) <- c('NA', '<30psu', '>30psu')

   landscape_per_node <- cbind.data.frame (landscape_per_node, landscape=paste(bottom_levels, photic_levels, salinity_levels, sep="_"))


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
                                                 logistic_recovery_rate_per_month=0.4  # 20% (fake)
                                                 ) 
     write.table(logistic_recovery_rates_per_month_per_funcgr, file=file.path(general$main.path.ibm, paste("benthosspe_", general$application, sep=''),
                        paste("logistic_recovery_rates_per_month_per_funcgr.dat", sep='')), col.names=TRUE, row.names=FALSE, quote=FALSE)
 } else{
   recovery <- read.table(file.path(general$main_path_gis, "HABITATS", "logistic_recovery_rates_per_month_per_funcgr.csv"), header=TRUE, sep=";")
        logistic_recovery_rates_per_month_per_funcgr <- recovery[,c('landscape', 'logistic_recovery_rate_per_month')]
        write.table(logistic_recovery_rates_per_month_per_funcgr, file=file.path(general$main.path.ibm, paste("benthosspe_", general$application, sep=''),
                        paste("logistic_recovery_rates_per_month_per_funcgr.dat", sep='')), col.names=TRUE, row.names=FALSE, quote=FALSE)
   
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
##--------------------OF BIOMASS PER CELL PER FUNCTIONAL GROUP----------------##
##----------------------PER MARINE LANDSCAPE----------------------------------##
##----------------------------------------------------------------------------##



   if(!file.exists(file.path(general$main_path_gis, "HABITATS", "prop_loss_on_habitat_after_one_passage_per_metier_per_sz.csv"))){
       estimates_biomass_per_cell_per_funcgr_per_landscape <- cbind.data.frame(landscape=rep(codes, each=2), biomass_per_cell=10000)  # ??
       write.table(estimates_biomass_per_cell_per_funcgr_per_landscape, file=file.path(general$main.path.ibm, paste("benthosspe_", general$application, sep=''),
                        paste("estimates_biomass_per_cell_per_funcgr_per_landscape.dat", sep='')), col.names=TRUE, row.names=FALSE, quote=FALSE)
  } else{
     estimates_biomass_per_cell_per_funcgr_per_landscape <- read.table(file.path(general$main_path_gis, "HABITATS", "tot_benthos_biomass_on_habitat_per_node_per_sz.csv"), header=TRUE, sep=";")
       write.table(estimates_biomass_per_cell_per_funcgr_per_landscape, file=file.path(general$main.path.ibm, paste("benthosspe_", general$application, sep=''),
                        paste("estimates_biomass_per_cell_per_funcgr_per_landscape.dat", sep='')), col.names=TRUE, row.names=FALSE, quote=FALSE)
  
  }




cat(paste(".....stored in", general$main.path.ibm, "/benthossspe \n"))

cat(paste(".....done\n"))

