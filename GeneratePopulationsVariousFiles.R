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
# XXctrysspe_relative_stability_semesterXX.dat
# percent_landings_from_simulated_vessels.dat
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
 
  for (pid in 0: (length(spp)-1)){
 
    for (a.semester in c(1,2)){
 
      if(general$application=="balticRTI"){
        relative_stability_this_sp <- cbind(ctry=c('DEN', 'DEU', 'SWE'), percent=c(33, 33, 33)) # not used if no individual quotas... 
        } else{
        stop('Relative stability key needs to be defined for your own app')
        }
        
      write.table(relative_stability_this_sp,     # the szgroup dimension is removed....
            file=file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=''), 
              paste(pid, "ctrysspe_relative_stability_semester", a.semester,".dat",sep='')),
                  col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE, append=FALSE)

      cat(paste("Write", pid, "ctrysspe_relative_stability_semester", a.semester,".dat....done \n"))

      }
   
   }   
 
 
 
  #------------------------------------
      percent_landings_from_simulated_vessels <- cbind(stock= 0: (length(spp)-1), percent=100) # not used if no individual quotas... 
      
      write.table(percent_landings_from_simulated_vessels,     # the szgroup dimension is removed....
            file=file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=''), 
              paste("percent_landings_from_simulated_vessels.dat",sep='')),
                  col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE, append=FALSE)

      cat(paste("Write percent_landings_from_simulated_vessels.dat....done \n"))

   
  