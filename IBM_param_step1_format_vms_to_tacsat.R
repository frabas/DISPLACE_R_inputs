
FRANCOIS <- TRUE
XXX      <- FALSE

# set your own path here:
if(FRANCOIS){
   main_path_data         <- file.path("C:","merging", "EflaloAndTacsat")
   main_path_ibm_param    <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_raw")  
   main_path_ibm_param_R  <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_R_inputs") 

   }
if(XXX){
   main_path_data          <-  file.path("C:","merging", "EflaloAndTacsat","GermanEflaloTacsat")
   main_path_ibm_param     <- file.path("C:","displace-project.org","repository", "ibm_vessels_param")   
   main_path_ibm_param_R   <- file.path("C:","displace-project.org","repository", "ibm_vessels_param_R")   
  }
  
 
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  ### LOOK AT EXAMPLE DATA           ###
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  
                                           
library(vmstools)  
library(help=vmstools)  # getting help...

data(tacsat)
head(tacsat)

  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  ### PROCESS YOUR OWN TO OBTAIN TACSAT#
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###

the user should do it from its raw data

  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  ### save                           ###
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###

y <- "2015"
save(tacsat, file=file.path(main_path_data, paste("tacsat",y,".RData",sep="")))


                    