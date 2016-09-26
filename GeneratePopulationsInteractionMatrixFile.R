# GENERAL SETTINGS

   args <- commandArgs(trailingOnly = TRUE)

   general <- list()

   if (length(args) < 2) {
     if(.Platform$OS.type == "windows") {
       general$application           <- "balticRTI" # ...or myfish
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
  
  
   cat(paste("START \n"))


   # (caution: give the order for naming stocks in integer from 0 to n-1)
   spp_table <-  read.table(file=file.path(general$main_path_gis, "POPULATIONS", 
                           paste("pop_names_", general$application,".txt",sep='')), header=TRUE)
   spp                        <- as.character(spp_table$spp)
   cat(paste("Reading the stock names in", paste(general$main_path_gis, "POPULATIONS", 
                           paste("pop_names_", general$application,".txt",sep='')),"....done \n"))
 



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#species_interactions_mortality_proportion_matrix_biolsce1
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


mortality_interactions <- matrix(0, nrow= length(spp), ncol=length(spp))
diag (mortality_interactions) <- 1.0  # i.e. assuming no interaction by default


write.table(mortality_interactions,     # the szgroup dimension is removed....
            file=file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=''), 
              paste("species_interactions_mortality_proportion_matrix_biolsce1.dat",sep='')),
                  col.names=FALSE,  row.names=FALSE, sep= ' ', quote=FALSE, append=FALSE)

cat(paste("Write species_interactions_mortality_proportion_matrix_biolsce1.dat....done \n"))

   
cat(paste("....done \n"))  