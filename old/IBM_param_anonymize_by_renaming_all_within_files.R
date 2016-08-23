

# first run the rename_all.bat batch file in the right folder eg vesselspe_*
# (this batch file is renaming the files in itself)
# then use the following R routine to continue the anonymization but within the relevant files...

a_path <- file.path("C:","Users", "fbas", "Documents", "GitHub", "DISPLACE_input", "vesselsspe_balticonly")

vids     <- read.table(file=file.path(a_path, "names.txt")) # obtained from the bat file
vids[,2] <- unlist(lapply(strsplit(as.character(vids[,2]), split="_"), function(x) ifelse(x[[1]]=="SWE", paste(x[[1]],x[[2]], sep="_"), x[[1]])  ))
colnames(vids) <- c("idx", "vid")
#=> SWE is giving trouble because of '_'...

 for (a_quarter in 1:4){
  file1 <- read.table(file=file.path(a_path, paste("vesselsspe_features_quarter", a_quarter, ".dat", sep='')), sep="|", header=FALSE)
  file1[,1] <- vids[ match(file1[,1], vids[,2]), 1]
  write.table(file1, file=file.path(a_path, paste("vesselsspe_features_quarter", a_quarter, ".dat", sep='')), sep="|", row.names=FALSE, col.names=FALSE, quote=FALSE)
  
  file2 <- read.table(file=file.path(a_path, paste("vesselsspe_harbours_quarter", a_quarter, ".dat",sep='')), header=TRUE)
  file2[,1] <- vids[ match(file2[,1], vids[,2]), 1] 
  write.table(file2, file=file.path(a_path, paste("vesselsspe_harbours_quarter", a_quarter, ".dat", sep='')), sep=" ", row.names=FALSE, col.names=TRUE, quote=FALSE)
  
  file3 <- read.table(file=file.path(a_path, paste("vesselsspe_freq_harbours_quarter", a_quarter, ".dat",sep='')), header=TRUE)
  file3[,1] <- vids[ match(file3[,1], vids[,2]), 1]   
  write.table(file3, file=file.path(a_path, paste("vesselsspe_freq_harbours_quarter", a_quarter, ".dat", sep='')), sep=" ", row.names=FALSE, col.names=TRUE, quote=FALSE)
  
  file4 <- read.table(file=file.path(a_path, paste("vesselsspe_fgrounds_quarter", a_quarter,".dat", sep='')), header=TRUE)
  file4[,1] <- vids[ match(file4[,1], vids[,2]), 1] 
  write.table(file4, file=file.path(a_path, paste("vesselsspe_fgrounds_quarter", a_quarter, ".dat", sep='')), sep=" ", row.names=FALSE, col.names=TRUE, quote=FALSE)
  
  file5 <- read.table(file=file.path(a_path, paste("vesselsspe_freq_fgrounds_quarter", a_quarter, ".dat",sep='')), header=TRUE)
  file5[,1] <- vids[ match(file5[,1], vids[,2]), 1] 
  write.table(file5, file=file.path(a_path, paste("vesselsspe_freq_fgrounds_quarter", a_quarter, ".dat", sep='')), sep=" ", row.names=FALSE, col.names=TRUE, quote=FALSE)
 
 
 }  
  
 
 for (a_semester in 1:2){
  file6 <- read.table(file=file.path(a_path, paste("vesselsspe_percent_tacs_per_pop_semester", a_semester, ".dat",sep='')), header=TRUE)
  file6[,1] <- vids[ match(file6[,1], vids[,2]), 1] 
  write.table(file6, file=file.path(a_path, paste("vesselsspe_percent_tacs_per_pop_semester", a_semester, ".dat", sep='')), sep=" ", row.names=FALSE, col.names=TRUE, quote=FALSE)
 
  file7 <- read.table(file=file.path(a_path, paste("vesselsspe_betas_semester", a_semester, ".dat",sep='')), header=TRUE)
  file7[,1] <- vids[ match(file7[,1], vids[,2]), 1] 
  write.table(file7, file=file.path(a_path, paste("vesselsspe_betas_semester", a_semester, ".dat", sep='')), sep=" ", row.names=FALSE, col.names=TRUE, quote=FALSE)
 
  file8 <- read.table(file=file.path(a_path, paste("vesselsspe_betas_se_semester", a_semester, ".dat",sep='')), header=TRUE)
  file8[,1] <- vids[ match(file8[,1], vids[,2]), 1] 
  write.table(file8, file=file.path(a_path, paste("vesselsspe_betas_se_semester", a_semester, ".dat", sep='')), sep=" ", row.names=FALSE, col.names=TRUE, quote=FALSE)
 
 
 }
    

 
 
  