


  a_case <- "balticonly"

  # GENERAL SETTINGS
  if(a_case=="balticonly"){
     general <- list()
     general$main.path      <- file.path("C:","displace-project.org","repository", "ibm_vessels_param")
     general$main.path.code <- file.path("C:","displace-project.org","repository", "ibm_vessels_param_R")

     general$igraph                <- 11
     general$case_study            <- "baltic_only"
     general$case_study_countries  <- c("DEN", "SWE", "DEU")    # for the Baltic only
     general$a.year                <- "2012"
     }


##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##

 ## FROM THE BALANCE map
 landscape_per_node <- read.table(file=file.path(general$main.path, "igraph",
           paste("coord", general$igraph,"_with_landscape.dat", sep='')))

   # translate the coding, just for info...
   bottom   <- substr(as.numeric(as.character(landscape_per_node[,1])), 1,1)
   photic   <- substr(as.numeric(as.character(landscape_per_node[,1])), 2,2)
   salinity <- substr(as.numeric(as.character(landscape_per_node[,1])), 3,3)


   bottom_levels <- factor(bottom)
   levels(bottom_levels) <- c('NA', 'Bedrock', 'Hard Bottom', 'Sand', 'Hard Clay', 'Mud')

   photic_levels <- factor(photic)
   levels(photic_levels) <- c('NA','Photic', 'Aphotic')

   salinity_levels <- factor(salinity)
   levels(salinity_levels) <- c('NA', '0-5psu', '5-7.5psu', '7.5-11psu', '11-18psu', '18-30psu', '>30psu')

   landscape_per_node <- cbind.data.frame (landscape_per_node, landscape=paste(bottom_levels, photic_levels, salinity_levels, sep="_"))


   codes <- unique(landscape_per_node[,1])
   codes <- codes[order(codes)]


##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
##------------------CREATE ONE FILE .dat PER METIER FOR-----------------------##
##--------------------LOSS AFTER ONE PASSAGE----------------------------------##
##----------------------PER MARINE LANDSCAPE----------------------------------##
##----------------------------------------------------------------------------##


 # for the time being,
 # creating files with fake copy/paste info
 # and assuming at least two functional groups (--> will be a multimap in c++)
 met_names <- read.table(file=file.path("C:","Users", "fbas", "Documents", "GitHub", "DISPLACE_input", paste("metiersspe_",a_case,sep=''), "combined_met_names.txt"))

 max_met <-  max(met_names[,2])

 for(met in 0: max_met){
   loss_after_one_passage_this_metier <- cbind.data.frame(landscape=rep(codes, each=2), loss_after_one_passage=0.20)  # 20%
   write.table(loss_after_one_passage_this_metier, file=file.path("C:","Users", "fbas", "Documents", "GitHub", "DISPLACE_input", paste("metiersspe_",a_case,sep=''),
                        paste(met,"loss_after_one_passage_per_landscape_per_func_group.dat", sep='')), col.names=TRUE, row.names=FALSE, quote=FALSE)
 }






