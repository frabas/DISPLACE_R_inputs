# some args for the bunch of vessels to be created....
# Usage:
# RunVesselsConfigFiles.R application gis_path input_application_path igraph

args <- commandArgs(trailingOnly = TRUE)

general <- list()


if (length(args) < 2) {
  if(.Platform$OS.type == "windows") {
    general$application           <- "testexample" # ...or myfish
    general$main_path_gis         <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis", general$application)
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

  filename <- file.path(general$main_path_gis, "FISHERIES", "fisheries_economics_variables.csv")
   cnts     <- count.fields(filename, sep = ";") 
   vessel_specifications <- read.table(file=filename, sep=";", header=TRUE )
   vessel_specifications <- cbind.data.frame(vessel_specifications, id=1:nrow(vessel_specifications))
   #=> CAUTION: do not leave white rows at the end of this file! otherwise will create some extra wrong config_files
   cat(paste("Read vessels_specifications_per_harbour_metiers.csv \n"))
 
    


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-----export file as a DISPLACE input file--------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


     # save .dat files
       write.table(vessel_specifications,
           file=file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep=''),
             paste("vesselsspe_economic_features.dat",sep='')),
               col.names=FALSE,  row.names=FALSE, quote=FALSE, append=FALSE, sep = "|")
  
       cat(paste("vesselsspe_economic_features.dat....OK", "\n"))





