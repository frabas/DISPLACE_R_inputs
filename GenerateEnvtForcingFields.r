

 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## GENERAL SETTINGS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 
   args <- commandArgs(trailingOnly = TRUE)

   general <- list()

   if (length(args) < 2) {
     if(.Platform$OS.type == "windows") {
       general$application           <- "minitest" # ...or myfish
       general$main_path_gis         <- file.path("C:","Users","fbas","Documents","GitHub",paste("DISPLACE_input_gis_", general$application, sep=""))
       general$main.path.ibm         <- file.path("C:","Users","fbas","Documents","GitHub", paste("DISPLACE_input_", general$application, sep=''))
       general$igraph                <- 0  # caution: should be consistent with existing objects already built upon a given graph
       do_plot                       <- TRUE
     }
  } else {
       general$application           <- args[1]
       general$main_path_gis         <- args[2]
       general$main.path.ibm         <- args[3]
       general$igraph                <- args[4]  # caution: should be consistent with existing objects already built upon a given graph
       do_plot                       <- FALSE
  }
  
   if(general$application=="testexample")  name_GIS_file <- "habitat_landscapes" # this name used by the Objects Editor ui
   if(general$application=="myfish")  name_GIS_file <- "EUNIS_codes_Combined_ICES_FAO9_clipped" # this name used by the Objects Editor ui
   if(general$application=="BalticSea")  name_GIS_file <- "EUNIS_codes_Combined_ICES_FAO9_clipped" # this name used by the Objects Editor ui
   if(general$application=="minitest")  name_GIS_file <- "EUNIS_codes_Combined_ICES_FAO9_clipped" # this name used by the Objects Editor ui
 
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## THe ROUTINE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

   cat(paste("START \n"))
 
  

     # load the DISPLACE graph
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
     if(do_plot) segments(coord[graph[,1]+1,1], coord[graph[,1]+1,2], coord[graph[,2]+1,1], coord[graph[,2]+1,2], col=4) 
     # CAUTION: +1, because c++ to R

     cat(paste("Read the graph....done\n"))
 


    # adding initial values and vector fields per environmental forcing variable
    # code_area / landscape / sst / salinity / wind / Nitrogen / Phosphorus / Oxygen / DissolvedCarbon
    
     ## CODE AREA (-10 if outside)
     code_area              <- 0 # TODO: use input data to overlay a spatialPoints on a polygon shape
     coord <- cbind(coord,  code_area=code_area)
     
    
    
    if(FALSE){
     #------------------
     # RASTER-----------
     #------------------
     library(maptools)
     library(raster)
     polPath              <- file.path(general$main_path_gis, "HABITATS")
     anf                  <- function(x) as.numeric(as.character(x))
     #sh_coastlines        <- readShapePoly(file.path(polPath,"francois_EU"))
     ## use point-raster overlay.......
     library(raster)
     landscapes       <- raster(file.path(polPath, "habitat_landscapes.tif"))    # probably need an update of rgdal here....
     coord           <- cbind(x=anf(coord[,'x']), y=anf(coord[,'y']))
     # convert to UTM
     library(sp)
     library(rgdal)
     SP <- SpatialPoints(cbind(as.numeric(as.character(coord[,'x'])), as.numeric(as.character(coord[,'y']))),
                       proj4string=CRS("+proj=longlat +datum=WGS84"))
     coord <- cbind.data.frame(coord,
                 spTransform(SP, 
                  CRS(paste("+proj=utm +zone=34 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep='')))) 
                     # convert to UTM
     dd <- extract (landscapes, coord[,3:4]) # get the landscape on the coord points!
     coord <- cbind(coord,  landscapes_code=dd)  # look at �Towards a representative MPA network in the Baltic Sea�.
    } # end FALSE
     
    if(TRUE){
     #------------------
     # RASTER-----------
     #------------------
     extractNodesOnRaster <- function (coord=coord,
                                       general=general,
                                       filename=file.path("Input_of_nutrients_Total_nitrogen", "Total nitrogen1.tif"),
                                       namefield="nitrogen"){
        library(maptools)
        library(raster)
        anf                  <- function(x) as.numeric(as.character(x))
        #sh_coastlines        <- readShapePoly(file.path(polPath,"francois_EU"))
        ## use point-raster overlay.......
        library(raster)
        raster_var       <- raster(file.path(filename))    # probably need an update of rgdal here....
        coord           <- cbind(x=anf(coord[,'x']), y=anf(coord[,'y']))
        # convert to UTM
        library(sp)
        library(rgdal)
        SP <- SpatialPoints(cbind(as.numeric(as.character(coord[,'x'])), as.numeric(as.character(coord[,'y']))),
                       proj4string=CRS("+proj=longlat +datum=WGS84"))
        coord <- cbind.data.frame(coord,
                 spTransform(SP, 
                  CRS(paste(attributes(raster_var)$crs)))) 
        dd <- extract (raster_var, coord[,3:4]) # get the landscape on the coord points!
        coord <- cbind(coord,  raster_var=dd)  # look at �Towards a representative MPA network in the Baltic Sea�.
        colnames(coord)[ncol(coord)] <- namefield
    
    return(coord)
    }
    
    # calls
    polPath              <- file.path(general$main_path_gis, "HABITATS")
    filename <- file.path(polPath, "Input_of_nutrients_Total_nitrogen", "Total nitrogen1.tif")    # HELCOM HOLAS II Total nitrogen as �mol / l
    dd <- extractNodesOnRaster (coord=coord, general=general, filename=filename, namefield="nitrogen")
    nitrogen <- dd [,"nitrogen"]
    nitrogen <- replace(nitrogen, is.na(nitrogen), 0)
    
    polPath              <- file.path(general$main_path_gis, "HABITATS")
    filename <- file.path(polPath, "Input_of_nutrients_Total_phosphorus", "Total phosphorus1.tif")  # HELCOM HOLAS II Total phosphorus as �mol / l
    dd <- extractNodesOnRaster (coord=coord, general=general, filename=filename, namefield="phosphorus")
    phosphorus  <- dd [,"phosphorus"]
    phosphorus <- replace(phosphorus, is.na(phosphorus), 0)
    
    polPath              <- file.path("D:","FBA")
    filename <- file.path(polPath, "Bathy_GEBCO_1_min", "GRIDONE_1D.nc")  # GRIDONE_1D
    dd <- extractNodesOnRaster (coord=coord, general=general, filename=filename, namefield="bathymetry")
    bathymetry  <- dd [,"bathymetry"]
    bathymetry <- replace(bathymetry, is.na(bathymetry), 0)

    # check
    plot(coord[,c(1,2)], col=cut(bathymetry, breaks=c(-10000,-1000, -200, -100, -50, -20, 10000)))
    
    polPath              <- file.path(general$main_path_gis, "SHIPPING")
    filename <- file.path(polPath, "CargoPlusTank.tif")  # HELCOM 2016
    dd <- extractNodesOnRaster (coord=coord, general=general, filename=filename, namefield="shippingdensity")
    shippingdensity  <- dd [,"shippingdensity"]
    shippingdensity <- replace(shippingdensity, is.na(shippingdensity), 0)

     # check
    plot(coord[,c(1,2)], col=cut(shippingdensity, breaks=c(-1,1000, 2000, 10000)))
   

    polPath              <- file.path(general$main_path_gis, "HABITATS")
    filename <- file.path(polPath, "oxygen", "hypoxia_only_on_Figure8.tif")  # from Gogina et al., Mapping hypoxia area only
    dd <- extractNodesOnRaster (coord=coord, general=general, filename=filename, namefield="oxygen")
    oxygen  <- dd [,"oxygen"]
    oxygen <- replace(oxygen, is.na(oxygen), 0)
   
   
    } # end FALSE
     
     
      
                    
  if(TRUE){
  ## FROM A SHAPE FILE 
  #------------------
  # SHAPE-----------
  #------------------
   library(maptools)
   anf                  <- function(x) as.numeric(as.character(x))

   ## load shape.......                                 
   landscapes       <- readShapePoly(file.path(general$main_path_gis, "HABITATS", name_GIS_file),
                                       proj4string=CRS("+proj=longlat +datum=WGS84")    )    # probably need an update of rgdal here....
   cat(paste("Read GIS shape file (this one must include a hab_code field)...done\n"))

   coords            <- cbind(x=anf(coord[,'x']), y=anf(coord[,'y']))
  
   # convert to UTM
   library(sp)
   library(rgdal)
   spo <- SpatialPoints(cbind(as.numeric(as.character(coords[,'x'])), as.numeric(as.character(coords[,'y']))),
                       proj4string=CRS("+proj=longlat +datum=WGS84"))


  # use the magic 'over' function to see in which polygon it is located
  idx                           <- over(spo, landscapes); #print(idx)
  coord                         <- cbind.data.frame(as.data.frame(coord), landscapes_code=NA)
  coord$landscapes_code         <- as.character(idx$hab_code)
  cat(paste("overlay...done\n"))


  
   # assign 0 to the NA code
   coord[is.na(coord$landscapes_code), 'landscapes_code'] <- 0
  
   # a visual check
   if(do_plot) plot(coord[,1], coord[,2], col=coord[,'landscapes_code'])
   } # end TRUE
               
               
               
               
               
     
     ## LANSCAPE (BENTHOS)
     #landscape_code         <- 0 # TODO: use input data to overlay a spatialPoints on a polygon shape
     landscape_norm         <- rlnorm(nrow(coord), meanlog=log(0.05),sdlog=log(1.5)) 
     landscape_alpha        <- rnorm(nrow(coord),90,sd=40) # TODO: use input data to overlay a spatialPoints on a polygon shape
     #coord <- cbind(coord,  landscapes_code=landscape_code)
     coord <- cbind(coord,  landscape_norm=landscape_norm)
     coord <- cbind(coord,  landscape_alpha=landscape_alpha)
     
     ## wind speed
     wind              <- 12 # TODO: use input data to overlay a spatialPoints on a polygon shape
     wind_norm         <- rlnorm(nrow(coord), meanlog=log(0.05),sdlog=log(1.5)) 
     wind_alpha        <- rnorm(nrow(coord),90,sd=40) # TODO: use input data to overlay a spatialPoints on a polygon shape
     coord <- cbind(coord,  wind=wind)
     coord <- cbind(coord,  wind_norm=wind_norm)
     coord <- cbind(coord,  wind_alpha=wind_alpha)
   
     ## SST
     sst              <- 12 # TODO: use input data to overlay a spatialPoints on a polygon shape
     sst_norm         <- rlnorm(nrow(coord), meanlog=log(0.05),sdlog=log(1.5)) 
     sst_alpha        <- rnorm(nrow(coord),90,sd=40) # TODO: use input data to overlay a spatialPoints on a polygon shape
     coord <- cbind(coord,  sst=sst)
     coord <- cbind(coord,  sst_norm=sst_norm)
     coord <- cbind(coord,  sst_alpha=sst_alpha)

     ## salinity
     salinity              <- 20 # TODO: use input data to overlay a spatialPoints on a polygon shape
     salinity_norm         <- rlnorm(nrow(coord), meanlog=log(0.05),sdlog=log(1.5))  
     salinity_alpha        <- rnorm(nrow(coord),90,sd=40) # TODO: use input data to overlay a spatialPoints on a polygon shape
     coord <- cbind(coord,  salinity=salinity)
     coord <- cbind(coord,  salinity_norm=salinity_norm)
     coord <- cbind(coord,  salinity_alpha=salinity_alpha)

     ## Nitrogen
     #nitrogen              <- 1 # TODO: use input data to overlay a spatialPoints on a polygon shape
     nitrogen_norm         <- rlnorm(nrow(coord), meanlog=log(0.05),sdlog=log(1.5))  
     nitrogen_alpha        <- rnorm(nrow(coord),90,sd=40) # TODO: use input data to overlay a spatialPoints on a polygon shape
     coord <- cbind(coord,  nitrogen=nitrogen)
     coord <- cbind(coord,  nitrogen_norm=nitrogen_norm)
     coord <- cbind(coord,  nitrogen_alpha=nitrogen_alpha)

     ## Phosphorus
     #phosphorus              <- 1 # TODO: use input data to overlay a spatialPoints on a polygon shape
     phosphorus_norm         <- rlnorm(nrow(coord), meanlog=log(0.05),sdlog=log(1.5))
     phosphorus_alpha        <- rnorm(nrow(coord),90,sd=40) # TODO: use input data to overlay a spatialPoints on a polygon shape
     coord <- cbind(coord,  phosphorus=phosphorus)
     coord <- cbind(coord,  phosphorus_norm=phosphorus_norm)
     coord <- cbind(coord,  phosphorus_alpha=phosphorus_alpha)

     ## Oxygen
     #oxygen              <- 1 # Hypoxia area added from a raster file.....
     oxygen_norm         <- rlnorm(nrow(coord), meanlog=log(0.05),sdlog=log(1.5))  
     oxygen_alpha        <- rnorm(nrow(coord),90,sd=40) # TODO: use input data to overlay a spatialPoints on a polygon shape
     coord <- cbind(coord,  oxygen=oxygen)
     coord <- cbind(coord,  oxygen_norm=oxygen_norm)
     coord <- cbind(coord,  oxygen_alpha=oxygen_alpha)

     ## DissolvedCarbon
     dissolvedcarbon              <- 1 # TODO: use input data to overlay a spatialPoints on a polygon shape
     dissolvedcarbon_norm         <- rlnorm(nrow(coord), meanlog=log(0.05),sdlog=log(1.5))  
     dissolvedcarbon_alpha        <- rnorm(nrow(coord),90,sd=40) # TODO: use input data to overlay a spatialPoints on a polygon shape
     coord <- cbind(coord,  dissolvedcarbon=dissolvedcarbon)
     coord <- cbind(coord,  dissolvedcarbon_norm=dissolvedcarbon_norm)
     coord <- cbind(coord,  dissolvedcarbon_alpha=dissolvedcarbon_alpha)

     ## bathymetry
     bathymetry              <- bathymetry #  using input data to overlay a spatialPoints on a raster
     coord <- cbind(coord,  bathymetry=bathymetry)

     ## Shippingdensity
     shippingdensity              <- shippingdensity #  using input data to overlay a spatialPoints on a raster
     coord <- cbind(coord,  shippingdensity=shippingdensity)

     if(ncol(coord) !=31) stop("check to get 31 fields!")
     # colnames(coord)
     #[1] "x"                     "y"                     "harb"                  "pt_graph"              "code_area"             "landscapes_code"       "landscape_norm"       
     #[8] "landscape_alpha"       "wind"                  "wind_norm"             "wind_alpha"            "sst"                   "sst_norm"              "sst_alpha"            
     #[15] "salinity"              "salinity_norm"         "salinity_alpha"        "nitrogen"              "nitrogen_norm"         "nitrogen_alpha"        "phosphorus"           
     #[22] "phosphorus_norm"       "phosphorus_alpha"      "oxygen"                "oxygen_norm"           "oxygen_alpha"          "dissolvedcarbon"       "dissolvedcarbon_norm" 
     #[29] "dissolvedcarbon_alpha" "bathymetry", "shippingdensity"           

     
      # export
     write.table(coord, 
       file=file.path(general$main.path.ibm, "graphsspe", paste0('environment_on_coord',general$igraph,'.dat')),
                 row.names=FALSE, col.names=TRUE, sep= ",", quote=FALSE)



