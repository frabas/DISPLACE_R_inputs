

# replicate files from a vessel
# JUST GIVEN AS A FAKE EXAMPLE, USE WITH CARE


namefiles <-          c( "_gshape_cpue_per_stk_on_nodes_quarter", "_freq_possible_metiers_quarter", "_gscale_cpue_per_stk_on_nodes_quarter",
                             "_possible_metiers_quarter", "_cpue_per_stk_on_nodes_quarter")

appname     <-  "testexample"
foldername  <-  paste("DISPLACE_input_", appname, sep="")

vesselid_to_replicate <- "DNK00001"
vesselids             <- 2:100

for (namefile in namefiles) {

  for (Q in 1:4){
    er <- try(
     dd <- read.table(paste("C:\\Users\\fbas\\Documents\\GitHub\\",foldername,"\\vesselsspe_",appname,"\\",vesselid_to_replicate,namefile, Q, ".dat", sep=""), header=TRUE)
    ) 
    if(class(er)=="try-error") stop("not such a vessel id in the original data")
    
     for (vesselid in vesselids) {
      if (vesselid < 10) prefix <- "DNK00" else prefix <- "DNK0"
      write.table(dd, file=paste("C:\\Users\\fbas\\Documents\\GitHub\\",foldername,"\\vesselsspe_",appname,"\\",prefix,vesselid,namefile,Q,".dat", sep=""),col.names=TRUE, row.names=FALSE, sep=" ", quote=FALSE)
      }
      
     }
   }




   for (namefile in c("vesselsspe_betas_semester", "vesselsspe_percent_tacs_per_pop_semester")){
  for (S in 1:2){
   dd <- read.table(paste("C:\\Users\\fbas\\Documents\\GitHub\\",foldername,"\\vesselsspe_",appname,"\\",namefile,S,".dat", sep=""), header=TRUE)
    for (vesselid in vesselids){
      obj <- dd[dd[,1]==vesselid_to_replicate, ]
      if (vesselid < 10) prefix <- "DNK00" else prefix <- "DNK0" 
      obj[,1] <- paste(prefix,vesselid, sep='') 
      dd <- rbind.data.frame (dd, obj)
      }
    write.table(dd, file=paste("C:\\Users\\fbas\\Documents\\GitHub\\",foldername,"\\vesselsspe_",appname,"\\",namefile, S,".dat", sep=""),col.names=TRUE, row.names=FALSE, sep=" ", quote=FALSE)
   }
 }
 
   
   
   for (namefile in c("vesselsspe_fgrounds_quarter", "vesselsspe_freq_fgrounds_quarter", "vesselsspe_harbours_quarter", "vesselsspe_freq_harbours_quarter")){
     for (Q in 1:4){
      dd <- read.table(paste("C:\\Users\\fbas\\Documents\\GitHub\\",foldername,"\\vesselsspe_",appname,"\\",namefile,Q,".dat", sep=""), header=TRUE)
     
      for (vesselid in vesselids){
         obj <- dd[dd[,1]==vesselid_to_replicate, ]
         if (vesselid < 10) prefix <- "DNK00" else prefix <- "DNK0" 
         obj[,1] <- paste(prefix,vesselid, sep='') 
         dd <- rbind.data.frame (dd, obj)
         }
      write.table(dd, file=paste("C:\\Users\\fbas\\Documents\\GitHub\\",foldername,"\\vesselsspe_",appname,"\\",namefile,Q,".dat", sep=""),col.names=TRUE, row.names=FALSE, sep=" ", quote=FALSE)
  
     }
   } 
   
  
     for (Q in 1:4){
      dd <- read.table(paste("C:\\Users\\fbas\\Documents\\GitHub\\",foldername,"\\vesselsspe_",appname,"\\","vesselsspe_features_quarter",Q,".dat", sep=""),  sep="|", header=FALSE)
      for (vesselid in vesselids){
         obj <- dd[dd[,1]==vesselid_to_replicate, ]
         if (vesselid < 10) prefix <- "DNK00" else prefix <- "DNK0" 
         obj[,1] <- paste(prefix,vesselid, sep='') 
         dd <- rbind.data.frame (dd, obj)
         }     
      write.table(dd, file=paste("C:\\Users\\fbas\\Documents\\GitHub\\",foldername,"\\vesselsspe_",appname,"\\","vesselsspe_features_quarter",Q,".dat", sep=""), sep="|", col.names=FALSE, row.names=FALSE,  quote=FALSE)
     } 
     
   
   
   
   
   