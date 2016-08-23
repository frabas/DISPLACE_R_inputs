
## INFORM THE BENTHOS SPATIALLY ON NODES 
# should be useful to model the dynamic of the benthic communities
# under different spatial fishing pressures

# obtain XXloss_after_one_passage_per_landscape_per_func_group.dat per metier

# per node
# 1. attach a marine landscape
# 2. attach benthos abundances
# 3. attach a mortality function
# 4. attach a recovery function

 a_case <- "balticRTI"
 
  # GENERAL SETTINGS
  if(a_case=="balticRTI"){
     general <- list()
     general$application           <- "balticRTI" # ...or myfish
     general$igraph                <- 56
     general$main.path.param       <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_raw")
     general$main.path.param.gis   <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis", general$application)
     general$main.path.ibm         <- file.path("C:","Users","fbas","Documents","GitHub", paste("DISPLACE_input_" , general$application, sep=""))
      
     general$case_study_countries  <- c("DEN", "SWE", "DEU")   
     general$a.year                <- "2015"
     }
     
  dir.create(file.path(general$main.path.ibm, paste("benthosspe_", general$application, sep='')))
  dir.create(file.path(general$main.path.ibm, "graphsspe"))
                                                    
  #load
  coord <- read.table(file=file.path(general$main.path.param.gis, "GRAPH", paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
  coord <- as.matrix(as.vector(coord))
  coord <- matrix(coord, ncol=3)
  colnames(coord) <- c('x', 'y', 'harb')
  plot(coord[,1], coord[,2])


 
  graph <- read.table(file=file.path(general$main.path.param.gis, "GRAPH", paste("graph", general$igraph, ".dat", sep=""))) # build from the c++ gui
  graph <- as.matrix(as.vector(graph))
  graph <- matrix(graph, ncol=3)
  segments(coord[graph[,1]+1,1], coord[graph[,1]+1,2], coord[graph[,2]+1,1], coord[graph[,2]+1,2], col=4) # CAUTION: +1, because c++ to R
 
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##
  ##----------------------- OBTAIN A MARINE LANDSCAPE -------------------------##
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##
 


  #------------------
  # RASTER-----------
  #------------------
   library(maptools)
   library(raster)
   polPath              <- "C:/BENTHIS/BalanceMaps"
   anf                  <- function(x) as.numeric(as.character(x))
   sh_coastlines        <- readShapePoly(file.path(polPath,"francois_EU"))

   ## use point-raster overlay.......
   library(raster)
   landscapes       <- raster(file.path(polPath, "landscapes.tif"))    # probably need an update of rgdal here....
 
  
   coord           <- cbind(x=anf(coord[,'x']), y=anf(coord[,'y']))
  
   # convert to UTM
   library(sp)
   library(rgdal)
   SP <- SpatialPoints(cbind(as.numeric(as.character(coord[,'x'])), as.numeric(as.character(coord[,'y']))),
                       proj4string=CRS("+proj=longlat +datum=WGS84"))
   coord <- cbind.data.frame(coord,
                 spTransform(SP, CRS(paste("+proj=utm +zone=34 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep=''))))    # convert to UTM


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

 ## FROM THE BALANCE map
   landscape_per_node <- read.table(file=file.path(general$main.path.ibm, "graphsspe", 
           paste("coord", general$igraph,"_with_landscape.dat", sep='')))

   # translate the coding, just for info...
   bottom   <- substr(as.numeric(as.character(landscape_per_node[,1])), 1,1)
   photic   <- substr(as.numeric(as.character(landscape_per_node[,1])), 2,2)
   salinity <- substr(as.numeric(as.character(landscape_per_node[,1])), 3,3)


   bottom_levels <- factor(bottom)
   levels(bottom_levels) <- c('NA', 'Bedrock', 'Hard Bottom', 'Sand', 'Hard Clay', 'Mud')

   photic_levels <- factor(photic)
   levels(photic_levels) <- c('NA','Photic', 'Aphotic')

   salinity_levels <- factor(salinity)
   levels(salinity_levels) <- c('NA', '0-5psu', '5-7.5psu', '7.5-11psu', '11-18psu', '18-30psu', '>30psu')

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
  
  
  # TO DO

  
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
  
 max_met <-  max(as.numeric(metier_names[,2]))

 for(met in 0: max_met){
   loss_after_one_passage_this_metier <- cbind.data.frame(
                                                 landscape=rep(codes, each=2),
                                                 loss_after_one_passage=0.20  # 20% (fake)
                                                 ) 
   write.table(loss_after_one_passage_this_metier, file=file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep=''),
                        paste(met,"loss_after_one_passage_per_landscape_per_func_group.dat", sep='')), col.names=TRUE, row.names=FALSE, quote=FALSE)
 }



##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
##------------------CREATE ONE FILE .dat FOR ESTIMATES OF---------------------##
##--------------------OF BIOMASS PER CELL PER FUNCTIONAL GROUP----------------##
##----------------------PER MARINE LANDSCAPE----------------------------------##
##----------------------------------------------------------------------------##



   estimates_biomass_per_cell_per_funcgr_per_landscape <- cbind.data.frame(landscape=rep(codes, each=2), biomass_per_cell=10000)  # ??
   write.table(estimates_biomass_per_cell_per_funcgr_per_landscape, file=file.path(general$main.path.ibm, paste("benthosspe_", general$application, sep=''),
                        paste("estimates_biomass_per_cell_per_funcgr_per_landscape.dat", sep='')), col.names=TRUE, row.names=FALSE, quote=FALSE)


