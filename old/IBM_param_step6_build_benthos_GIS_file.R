

  # GENERAL SETTINGS
  
  FRANCOIS <- TRUE
  if(FRANCOIS){
     general <- list()
     general$main.path      <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input")
     general$main.path.code <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_R_inputs")
     }
     

  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##
  ##----------------------- OBTAIN A MARINE LANDSCAPE -------------------------##
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##
  ##---------------------------------------------------------------------------##
 
# e.g. here from the existing:
  
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
 
  
  
