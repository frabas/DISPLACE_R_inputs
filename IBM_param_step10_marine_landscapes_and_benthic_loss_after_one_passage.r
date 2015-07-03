 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!estimates_biomass_per_cell_per_funcgr_per_landscape!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 
 
 
 ## BENTHOS SPE
 ## IBM parametrisation
 ## Francois Bastardie (DTU-Aqua)
 ## outputs: mainly .dat files to be used for the IBM simulations

# GENERAL SETTINGS
  general <- list()
  general$main.path             <- file.path("C:", "Users", "fbas", "Documents", "GitHub", "DISPLACE_input_raw")
  general$main.path.code        <- file.path("C:", "Users", "fbas", "Documents", "GitHub", "DISPLACE_R_inputs")
  general$main_path_input       <- file.path("C:", "Users", "fbas", "Documents", "GitHub", "DISPLACE_input")

  general$igraph                <- 11
  general$case_study            <- "baltic_only"
  general$case_study_countries  <- c("DEN", "SWE", "DEU")    # for the Baltic only
  general$a.year                <- "2012"
  general$a.country             <- "DEN"
  #general$a.country             <- "DEU"
  #general$a.country             <- "SWE"


  general$igraph                <- 56
  general$case_study            <- "myfish"
  general$case_study_countries  <- c("DEN")    # for the Baltic only
  general$a.year                <- "2012"
  general$a.country             <- "DEN"



##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##

 ## FROM THE BALANCE map
   landscape_per_node <- read.table(file=file.path(general$main_path_input, "graphsspe",
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
 #metier_names <- read.table(file=file.path("C:","Users", "fbas", "Documents", "GitHub", "DISPLACE_input", paste("metiersspe_",a_case,sep=''), "combined_met_names.txt"))
 load(file.path(general$main.path,"merged_tables", general$case_study, paste("metier_names.", general$a.country,".", general$a.year,".igraph",general$igraph,".RData",sep='')))
 metier_names
   
 max_met <-  max(as.numeric(metier_names[,2]))

 for(met in 0: max_met){
   loss_after_one_passage_this_metier <- cbind.data.frame(
                                                 landscape=rep(codes, each=2),
                                                 loss_after_one_passage=0.20  # 20% (fake)
                                                 ) 
   write.table(loss_after_one_passage_this_metier, file=file.path(general$main.path, paste("metiersspe", sep=''),
                        paste(met,"loss_after_one_passage_per_landscape_per_func_group.dat", sep='')), col.names=TRUE, row.names=FALSE, quote=FALSE)
 }



##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
##------------------CREATE ONE FILE .dat FOR ESTIMATES OF---------------------##
##--------------------OF BIOMASS PER CELL PER FUNCTIONAL GROUP----------------##
##----------------------PER MARINE LANDSCAPE----------------------------------##
##----------------------------------------------------------------------------##



   estimates_biomass_per_cell_per_funcgr_per_landscape <- cbind.data.frame(landscape=rep(codes, each=2), biomass_per_cell=10000)  # ??
   write.table(estimates_biomass_per_cell_per_funcgr_per_landscape, file=file.path(general$main.path, paste("benthosspe", sep=''),
                        paste("estimates_biomass_per_cell_per_funcgr_per_landscape.dat", sep='')), col.names=TRUE, row.names=FALSE, quote=FALSE)


