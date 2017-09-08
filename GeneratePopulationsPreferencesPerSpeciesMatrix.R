
# GENERAL SETTINGS


   args <- commandArgs(trailingOnly = TRUE)

   general <- list()

   if (length(args) < 2) {
     if(.Platform$OS.type == "windows") {
       general$application           <- "DanishFleet" # ...
       general$main_path_gis         <- file.path("C:","Users","fbas","Documents","GitHub", paste("DISPLACE_input_gis_", general$application, sep=""))
       general$main.path.ibm         <- file.path("C:","Users","fbas","Documents","GitHub",
                                                     paste("DISPLACE_input_", general$application, sep=''))
       general$igraph                <- 40  # caution: should be consistent with existing objects already built upon a given graph

     }
  } else {
       general$application           <- args[1]
       general$main_path_gis         <- args[2]
       general$main.path.ibm         <- args[3]
       general$igraph                <- args[4]  # caution: should be consistent with existing vessels already built upon a given graph
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
# preferences_per_species_matrix_biolsce1
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


nbszgroups <- 14 # caution: global constant in DISPLACE

obj <- matrix(1, ncol=nbszgroups, nrow=nbpops)

for (spp in 0:(length(spp)-1)){
for (sce in 1:17){

filename <- paste(spp, "preferences_per_species_matrix_semester1_biolsce", sce,".dat", sep="")

write.table(obj, file= file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_myfish","popsspe_myfish", filename), col.names=FALSE, row.names=FALSE, sep=" ")

}}

