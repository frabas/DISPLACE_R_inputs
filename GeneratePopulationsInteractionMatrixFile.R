
 general <- list()

 general$application           <- "balticRTI"  

 general$main.path.ibm         <- file.path("C:","Users","fbas","Documents","GitHub", paste("DISPLACE_input_" , general$application, sep=""))

 # (caution: give the order for naming stocks in integer from 0 to n-1)
 spp_table <-  read.table(file=file.path(general$main.path.ibm , paste("popsspe_" , general$application, sep=""), paste("pop_names_",general$application ,".txt",sep='')),
              header=TRUE)
 spp                        <- as.character(spp_table$spp)



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#species_interactions_mortality_proportion_matrix_biolsce1
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


mortality_interactions <- matrix(0, nrow= length(spp), ncol=length(spp))
diag (mortality_interactions) <- 1.0


write.table(mortality_interactions,     # the szgroup dimension is removed....
            file=file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=''), 
              paste("species_interactions_mortality_proportion_matrix_biolsce1.dat",sep='')),
                  col.names=FALSE,  row.names=FALSE, sep= ' ', quote=FALSE, append=FALSE)

   
  