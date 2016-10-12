 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!VARIOUS METIERS FILES!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!met_target_names.dat!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!metier_fspeed.dat!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!metierspe_mls_cat_semesterXX.dat!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!percent_revenue_completenesses.dat!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!metier_gear_widths_param_a.dat!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!metier_gear_widths_param_b.dat!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!metier_gear_widths_model_type.dat!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!20loss_after_one_passage_per_landscape_per_func_group.dat!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
     
      
  # some args for the bunch of vessels to be created....
 # Usage:
 # GenerateMetiersVariousFiles.R application gis_path input_application_path 

 
   # GENERAL SETTINGS

    # GENERAL SETTINGS

   args <- commandArgs(trailingOnly = TRUE)

   general <- list()

   if (length(args) < 2) {
     if(.Platform$OS.type == "windows") {
       general$application           <- "testexample" # ...or myfish
       general$main_path_gis         <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis", general$application)
       general$main.path.ibm         <- file.path("C:","Users","fbas","Documents","GitHub",paste("DISPLACE_input_", general$application, sep=''))
       general$igraph                <- 56  # caution: should be consistent with existing vessels already built upon a given graph
       do_plot                       <- TRUE
     }
  } else {
       general$application           <- args[1]
       general$main_path_gis         <- args[2]
       general$main.path.ibm         <- args[3]
       general$igraph                <- args[4]  # caution: should be consistent with existing vessels already built upon a given graph
       do_plot                       <- FALSE
  }
   cat(paste("START \n"))



   dir.create(file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep='')))
  
  
   a_size_group_bin_in_cm <- 5 # caution: hardcoding....
    mid                    <- a_size_group_bin_in_cm/2
   cat(paste("caution: harcoded bin size \n"))
 
   spp_table <-  read.table(file=file.path(general$main_path_gis, "POPULATIONS",
                                    paste("pop_names_",general$application,".txt",sep='')), header=TRUE)
   spp                        <- as.character(spp_table$spp)

   options(scipen=999)


  # reuse the exported metier names in GenerateVesselConfigFiles.R
    metier_names  <- read.table( file=file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep=''), "metier_names.dat"), header=TRUE)


   #########################################
   ## metier_target_name ###################
   #########################################
    write(c("LE_MET_level6", "mapped_stk_code"),
          file=file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep=''),
           "met_target_names.dat"), ncolumns=2,
            sep= ' ', append=FALSE)
   cat(paste("Write met_target_names.dat \n"))
 
  
  # by default, create a fake selectivity ogive i.e. all at 1 (and not species-specific...)
  for (met in unique(metier_names$idx) ) {

  met_target_names <- NULL
    the_met <- as.character(metier_names[metier_names[, 'idx']==met, "name"])

    demersal_gear <- FALSE  ; pelagic_gear <- FALSE
  
    if (length (grep("DEF", the_met))!=0 || length (grep("MCD", the_met))!=0 || length (grep("CRU", the_met))!=0)  {demersal_gear <- TRUE; pelagic_gear <- FALSE}
    if (length (grep("SPF", the_met))!=0 )  {demersal_gear <- FALSE; pelagic_gear <- TRUE}
 
    all_species <- sapply(spp, function (spp) substr(spp,1,3))   
    count <- -1
    for (sp in all_species ) {
      count <- count+1 # idx sp
      demersal_sp <- FALSE  ; pelagic_sp <- FALSE
      if(sp %in% c('COD', 'FLE', 'PLE', 'SOL', 'WHG', 'DAB', 'TUR') )  {pelagic_sp<- FALSE; demersal_sp <- TRUE}
      if(sp %in% c('HER', 'SPR')) {pelagic_sp<- TRUE; demersal_sp <- FALSE}
    
      if(
         demersal_gear && demersal_sp ||
         pelagic_gear && pelagic_sp 
         ){
          write(c(met, count),
          file=file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep=''),
           "met_target_names.dat"), ncolumns=2,
             sep= ' ', append=TRUE)
         }
    }
    
  }
   cat(paste("Write met_target_names.dat....done \n"))
  
  
  
   #########################################
   ## metier_fspeed ########################
   #########################################
       write(c("LE_MET_level6", "fspeed"),
          file=file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep=''),
           "metier_fspeed.dat"), ncolumns=2,
            sep= ' ', append=FALSE)

 
     # by default, create a fake selectivity ogive i.e. all at 1 (and not species-specific...)
     for (met in unique(metier_names$idx) ) {

         fspeed <- 4  # knots
         
         write(c(met, fspeed),
          file=file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep=''),
           "metier_fspeed.dat"), ncolumns=2,
             sep= ' ', append=TRUE)
             
           }
             
   cat(paste("Write metier_fspeed.dat....done \n"))
  
      
   #########################################
   ## metierspe_mls_cat_semesterXX #########
   #########################################
    # pop parameters
    pa <- read.csv(file=file.path(general$main_path_gis, "POPULATIONS",
                  paste("Stock_biological_traits.csv", sep='')), 
                    sep=';', header=TRUE)
    rownames(pa) <- pa$stock

    
    
     write(c("LE_MET_level6", "mls"),
          file=file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep=''),
           "metierspe_mls_cat_semester1.dat"), ncolumns=2,
            sep= ' ', append=FALSE)
     write(c("LE_MET_level6", "mls"),
          file=file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep=''),
           "metierspe_mls_cat_semester2.dat"), ncolumns=2,
            sep= ' ', append=FALSE)

  
     for (met in unique(metier_names$idx) ) {
             
        for (sp in spp) {

         sz <- pa[sp, "mls_cat"]
     
         write(c(met, sz),
          file=file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep=''),
           "metierspe_mls_cat_semester1.dat"), ncolumns=2,
             sep= ' ', append=TRUE)
         write(c(met, sz),
          file=file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep=''),
           "metierspe_mls_cat_semester2.dat"), ncolumns=2,
             sep= ' ', append=TRUE)
      } 
     }
     
     
   cat(paste("Write metierspe_mls_cat_semesterXX.dat....done \n"))
     
     
     
     
       
   #########################################
   ## percent_revenue_completenesses########
   #########################################
     
     write(c("LE_MET_level6", "completeness"),
          file=file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep=''),
           "percent_revenue_completenesses.dat"), ncolumns=2,
            sep= ' ', append=FALSE)
  
     
     
     
       for (met in unique(metier_names$idx) ) {
    
            percent_revenue_completenesses <- 100 # % revenue of this metier from described spp
            
            write(c(met, percent_revenue_completenesses),
          file=file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep=''),
           "percent_revenue_completenesses.dat"), ncolumns=2,
             sep= ' ', append=TRUE)
  
      
      }
      
   cat(paste("Write percent_revenue_completenesses.dat....done \n"))
      
   #########################################
   ## metier_gear_widths_model_type ########
   ## metier_gear_widths_param_a   #########
   ## metier_gear_widths_param_b  ##########
   #########################################
   write(c("LE_MET_level6", "model_type"),
          file=file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep=''),
           "metier_gear_widths_model_type.dat"), ncolumns=2,
            sep= ' ', append=FALSE)
   write(c("LE_MET_level6", "param_a"),
          file=file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep=''),
           "metier_gear_widths_param_a.dat"), ncolumns=2,
            sep= ' ', append=FALSE)
   write(c("LE_MET_level6", "param_b"),
          file=file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep=''),
           "metier_gear_widths_param_b.dat"), ncolumns=2,
            sep= ' ', append=FALSE)

  
  for (met in unique(metier_names$idx) ) {

  met_target_names <- NULL
    the_met <- as.character(metier_names[metier_names[, 'idx']==met, "name"])

    trawl_gear <- TRUE  ; net_gear <- FALSE ; seine_gear <- TRUE  # default
  
    if (length (grep("OTB", the_met))!=0 || length (grep("OTT", the_met))!=0 || length (grep("PTM", the_met))!=0)  {trawl_gear <- TRUE; net_gear <- FALSE; seine_gear <- FALSE}
    if (length (grep("GNS", the_met))!=0)  {trawl_gear <- FALSE; net_gear <- TRUE; seine_gear <- FALSE}
    if (length (grep("SDN", the_met))!=0)  {trawl_gear <- FALSE; net_gear <- FALSE; seine_gear <- TRUE}
  
      
    model_type <- "DoS=a*(kW^b)" ; param_a <- 9.6053549509854; param_b <- 0.433672763959314 # default
    if(trawl_gear ){   model_type <- "a*(kW^b)" ; param_a <- 9.6053549509854; param_b <- 0.433672763959314}
    if(net_gear ){       model_type <- "if_LOA<20_a_else_b" ; param_a <- 0.001; param_b <- 0.5}
    if(seine_gear ){   model_type <- "a*(LOA^b)" ; param_a <- 4461.27004311913; param_b <- 0.117589220782479}
         
         
      write(c(met, model_type),
          file=file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep=''),
           "metier_gear_widths_model_type.dat"), ncolumns=2,
             sep= ' ', append=TRUE)
      write(c(met, param_a),
          file=file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep=''),
           "metier_gear_widths_param_a.dat"), ncolumns=2,
             sep= ' ', append=TRUE)
     write(c(met, param_b),
          file=file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep=''),
           "metier_gear_widths_param_b.dat"), ncolumns=2,
             sep= ' ', append=TRUE)
        
    
    
  }
  
   cat(paste("Write metier_gear_widths_model_type.dat....done \n"))
   cat(paste("Write metier_gear_widths_param_a.dat....done \n"))
   cat(paste("Write metier_gear_widths_param_b.dat....done \n"))
   
   #########################################
   ## combined_met_types.dat ###############
   #########################################
    write(c("LE_MET_level6", "model_type"),
          file=file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep=''),
           "combined_met_types.dat"), ncolumns=2,
            sep= ' ', append=FALSE)

  
  for (met in unique(metier_names$idx) ) {

  met_target_names <- NULL
    the_met <- as.character(metier_names[metier_names[, 'idx']==met, "name"])

    trawl_gear <- TRUE  ; net_gear <- FALSE ; seine_gear <- TRUE  # default
  
    if (length (grep("OTB", the_met))!=0 || length (grep("OTT", the_met))!=0 || length (grep("PTM", the_met))!=0)  {trawl_gear <- TRUE; net_gear <- FALSE; seine_gear <- FALSE}
    if (length (grep("GNS", the_met))!=0)  {trawl_gear <- FALSE; net_gear <- TRUE; seine_gear <- FALSE}
    if (length (grep("SDN", the_met))!=0)  {trawl_gear <- FALSE; net_gear <- FALSE; seine_gear <- TRUE}
  
      
    combined_met_types <- 1 # default
    if(trawl_gear ){   combined_met_types <- 1}
    if(net_gear ){      combined_met_types <- 0}
    if(seine_gear ){    combined_met_types <- 0}
         
         
      write(c(met, combined_met_types),
          file=file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep=''),
           "combined_met_types.dat"), ncolumns=2,
             sep= ' ', append=TRUE)
         
   cat(paste("Write combined_met_types.dat....done \n"))
    
    
  }
  

 
            
 cat(paste(".......done \n"))
  



  