
## INFORM THE BENTHOS SPATIALLY ON NODES 
# should be useful to model the dynamic of the benthic communities
# under different spatial fishing pressures

# per node
# 1. attach a marine landscape
# 2. attach benthos abundances
# 3. attach a mortality function
# 4. attach a recovery function

 a_case <- "balticonly"

  # GENERAL SETTINGS
  if(a_case=="balticonly"){
     general <- list()
     general$main.path      <- file.path("C:","displace-project.org","repository", "ibm_vessels_param")
     general$main.path.code <- file.path("C:","displace-project.org","repository", "ibm_vessels_param_R")
   
     general$igraph                <- 11
     general$case_study            <- "baltic_only"
     general$case_study_countries  <- c("DEN", "SWE", "DEU")    # for the Baltic only
     general$a.year                <- "2012"
     }
    
 
  load(file.path(general$main.path, "igraph", paste(general$igraph, "_graphibm.RData",sep=''))) # built from the R code

 
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
  write(coord[, 'landscapes_code'], file=file.path(general$main.path,"igraph", paste("coord", general$igraph,"_with_landscape.dat", sep='')), ncol=1)
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

   # TO DO
   
   
   
  
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
  
   
   
  
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##
  ##-------------- OBTAIN A RECOVERY FUNCTION PER FUNCTIONAL GROUP-------------##
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##
  
  
  # TO DO

  
