
 general <- list()

 general$application           <- "balticRTI"  

 general$main.path.ibm         <- file.path("C:","Users","fbas","Documents","GitHub", paste("DISPLACE_input_" , general$application, sep=""))

 # (caution: give the order for naming stocks in integer from 0 to n-1)
 spp_table <-  read.table(file=file.path(general$main.path.ibm , paste("popsspe_" , general$application, sep=""), paste("pop_names_",general$application ,".txt",sep='')),
              header=TRUE)
 spp                        <- as.character(spp_table$spp)

   
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# XXctrysspe_relative_stability_semesterXX.dat
# percent_landings_from_simulated_vessels.dat
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
 
  for (pid in 0: (length(spp)-1)){
 
    for (a.semester in c(1,2)){
 
      relative_stability_this_sp <- cbind(ctry=c('DEN', 'DEU', 'SWE'), percent=c(33, 33, 33)) # not used if no individual quotas... 
      
      write.table(relative_stability_this_sp,     # the szgroup dimension is removed....
            file=file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=''), 
              paste(pid, "ctrysspe_relative_stability_semester", a.semester,".dat",sep='')),
                  col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE, append=FALSE)

   
      }
   
   }   
 
 
 
  #------------------------------------
      percent_landings_from_simulated_vessels <- cbind(stock= 0: (length(spp)-1), percent=100) # not used if no individual quotas... 
      
      write.table(percent_landings_from_simulated_vessels,     # the szgroup dimension is removed....
            file=file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=''), 
              paste("percent_landings_from_simulated_vessels.dat",sep='')),
                  col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE, append=FALSE)

   
   
  