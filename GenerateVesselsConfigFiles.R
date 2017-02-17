 # some args for the bunch of vessels to be created....
 # Usage:
 # GenerateVesselsConfigFiles.R application gis_path input_application_path igraph

 
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
 
  
   dir.create(path=file.path(general$main_path_gis, "FISHERIES", "vessels_config_files"))
   dir.create(path=file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep='')))
   dir.create(path=file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep='')))
   
   
   cat(paste("Config files will be stored in /FISHERIES/vessels_config_files folder\n"))

  
  
   filename <- file.path(general$main_path_gis, "FISHERIES", "vessels_specifications_per_harbour_metiers.csv")
   cnts     <- count.fields(filename, sep = ";") 
   vessel_specifications <- read.table(file=filename, sep=";", header=TRUE )
   vessel_specifications <- cbind.data.frame(vessel_specifications, id=1:nrow(vessel_specifications))
   #=> CAUTION: do not leave white rows at the end of this file! otherwise will create some extra wrong config_files
   cat(paste("Read vessels_specifications_per_harbour_metiers.csv \n"))
 
 
   # a quick important check on the metier names (should be consistent with the ones available in spatialLayers)
   for (met in 1:length(vessel_specifications$LE_MET)) {
   if(length(
        grep(as.character(vessel_specifications$name_gis_file_for_fishing_effort_per_polygon[met]), list.files(file.path(general$main_path_gis, "FISHERIES", "spatialLayers")))
            )==0){
           stop(paste("a spatial layer for ", vessel_specifications$name_gis_file_for_fishing_effort_per_polygon[met], "is missing!! rename the metier in specs or better to get this layer"))
           } 
   }
   
 
   nb_agent_per_vessels <- 1  # caution: super-individuals to reduce the total nb of vessels to be simulated
   vessel_specifications[, "N..of.vessels"] <- ceiling(vessel_specifications[, "N..of.vessels"])/nb_agent_per_vessels
   
   port_names <- read.table(file=file.path(general$main_path_gis,
                                 "GRAPH",
                                  paste("harbours.dat", sep='')), sep=";")
   cat(paste("Read harbours.dat \n"))
 
  # quick check
  # if(!"Dredge" %in% unique(vessel_specifications[, "metier"]) 
  #     && !"Passive" %in% unique(vessel_specifications[, "metier"]) 
  #     && !"Trawl" %in% unique(vessel_specifications[, "metier"]))  stop("Not defined for these metiers...check metier names")
   
  
  # order along vid
   library(doBy)
   vessel_specifications <- orderBy(~vid, data=vessel_specifications)
   
   
  # export metier names
   metier_names <-  cbind.data.frame(idx=0: (length(levels(vessel_specifications$LE_MET))-1),  name=levels(vessel_specifications$LE_MET))
   write.table(
     metier_names,
       file=file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep=''), "metier_names.dat"),
       quote=FALSE, row.names=FALSE, col.names=TRUE)
   cat(paste("Read metier names from the Vessels specs and write metier names in metiersspe_ folder \n"))
   

   
   cat(paste("Now, read the specs table one by one \n"))
   for (i in 1 : nrow(vessel_specifications)){

    cat(paste("line",i," \n"))
      
    if(!any("vid" %in% colnames(vessel_specifications)) || 
       any("vid" %in% colnames(vessel_specifications)) && i>1 && vessel_specifications[i, "vid"]!=vessel_specifications[i-1, "vid"] ||
        any("vid" %in% colnames(vessel_specifications)) && i==1 ){
       # test for truly individual vessel data i.e. one vessel => one config file
    
    
      spp_table <-  read.table(file=file.path(general$main_path_gis, "POPULATIONS", paste("pop_names_",general$application,".txt",sep='')),
              header=TRUE)
      spp                        <- as.character(spp_table$spp)

      
      do_append                  <- TRUE
      nbvids                     <- vessel_specifications[i, "N..of.vessels"]
      
      
      vesselids                                    <- as.character(vessel_specifications[i, "vid"]) # caution: three first letters (nationality) should be consistent with  popsspe/XXctrysspe_relative_stability_semesterXX 
      vessel_range_km                              <- vessel_specifications[i, "vessel_range_km"] 
      if(any("vid" %in% colnames(vessel_specifications))){
        all_records_this_vid                         <- vessel_specifications [vessel_specifications$vid ==vessel_specifications[i, "vid"],]
        sum_per_met_this_vid                         <- tapply( all_records_this_vid$LE_EFF_VMS,  all_records_this_vid$metier,   sum, na.rm=TRUE)
        prop_per_met_this_vid                        <- sum_per_met_this_vid/ sum(sum_per_met_this_vid, na.rm=TRUE)
        metierids                                    <- which(prop_per_met_this_vid!=0) -1 # OFF -1 IN c++
            # e.g. c(0,1) # look at /metiersspe.... 0:trawler; 1 gillnetter
        metierids_frequencies                        <- prop_per_met_this_vid[!is.na(prop_per_met_this_vid)] #  c(1)  # pure fishery        # or e.g. c(0.2,0.8)
        imax                                         <- which.max(all_records_this_vid$LE_EFF_VMS) # assume the spatial distribution from the metier with the highest record in effort
        #name_gis_file_for_fishing_effort_per_polygon <- as.character(all_records_this_vid[imax, "name_gis_file_for_fishing_effort_per_polygon"])
        #name_gis_layer_field                         <- as.character(all_records_this_vid[imax, "name_gis_layer_field"])       # giving releative effort ditribtion e.g. in 5 categories: 1 to 5 with 1 high occurence
        names_gis_layers                             <- all_records_this_vid[!duplicated(data.frame(all_records_this_vid$name_gis_file_for_fishing_effort_per_polygon, all_records_this_vid$name_gis_layer_field )),]
        name_gis_file_for_fishing_effort_per_polygon <- as.character(names_gis_layers[, "name_gis_file_for_fishing_effort_per_polygon"])
        name_gis_layer_field                         <- as.character(names_gis_layers[, "name_gis_layer_field"])       # giving releative effort ditribtion e.g. in 5 categories: 1 to 5 with 1 high occurence
        is_gis_layer_field_relative_numbers          <- all_records_this_vid[imax, "is_gis_layer_field_relative_numbers"]  # if relative effort categories (e.g. high to low) then xfold_gis_layer_field will be used to convert in absolute
        xfold_gis_layer_field                        <- all_records_this_vid[imax, "xfold_gis_layer_field"] # giving relative importance of the 5 categories e.g. visting an area of cat 1 is 10000 times more probable than for cat 5
      } else{
        metierids                                    <- 0 # adapt
        metierids_frequencies                        <- c(1)  # pure fishery        # or e.g. c(0.2,0.8)
        name_gis_file_for_fishing_effort_per_polygon <- as.character(vessel_specifications[i, "name_gis_file_for_fishing_effort_per_polygon"])
        name_gis_layer_field                         <- as.character(vessel_specifications[i, "name_gis_layer_field"])       # giving releative effort ditribtion e.g. in 5 categories: 1 to 5 with 1 high occurence
        is_gis_layer_field_relative_numbers          <- vessel_specifications[i, "is_gis_layer_field_relative_numbers"]  # if relative effort categories (e.g. high to low) then xfold_gis_layer_field will be used to convert in absolute
        xfold_gis_layer_field                        <- vessel_specifications[i, "xfold_gis_layer_field"] # giving relative importance of the 5 categories e.g. visting an area of cat 1 is 10000 times more probable than for cat 5
      }
 
    
 
      cruisespeed                                  <- vessel_specifications[i, "cruise.speed.knots"] # knots
   
     if(any("vid" %in% colnames(vessel_specifications))){
      all_records_this_vid                         <- vessel_specifications [vessel_specifications$vid ==vessel_specifications[i, "vid"],]
      sum_per_harbour_this_vid                     <- tapply( all_records_this_vid$LE_EFF_VMS,  all_records_this_vid$Harbor,   sum, na.rm=TRUE)
      prop_per_harbour_this_vid                    <- sum_per_harbour_this_vid/ sum(sum_per_harbour_this_vid, na.rm=TRUE)
      visited_ports                                <- names(prop_per_harbour_this_vid) [which(prop_per_harbour_this_vid!=0)]
      visited_ports_frequencies                    <- prop_per_harbour_this_vid[!is.na(prop_per_harbour_this_vid)]
     harbcode                   <- paste(substr (visited_ports[which.max(visited_ports_frequencies)], 1, 4), vessel_specifications[i, "id"], sep="")
    } else{ 
     visited_ports              <- vessel_specifications[i, "Harbor"]   # e.g. c("ANCONA", "RIMINI") # should exist in harbour.dat!
     name_file_ports            <- "harbours.dat" 
     visited_ports_frequencies  <- c(1)          # e.g. c(0.8, 0.2)
     harbcode                   <- paste(substr (vessel_specifications[i, "Harbor"], 1, 4), vessel_specifications[i, "id"], sep="")
    }
    
    
  
   if(!all(visited_ports %in% port_names[,1])){  
      stop("Inconsistencies in port names!")
     }
    name_file_ports            <- "harbours.dat" 

     

   nb_stocks                  <- length(spp)  # from 0 in c++
   if(any("vid" %in% colnames(vessel_specifications))){
      all_records_this_vid      <- vessel_specifications [vessel_specifications$vid ==vessel_specifications[i, "vid"],]
      fixed_cpue_per_stock      <- apply(all_records_this_vid[, paste(substr(spp, 1,3), "_kg_h", sep='') ], 2, mean) *nb_agent_per_vessels# kg per hour 
    } else{
     fixed_cpue_per_stock       <- vessel_specifications[i, paste(substr(spp, 1,3), "_kg_h", sep='') ] *nb_agent_per_vessels# kg per hour 
    } 
   gshape_cpue_per_stock      <- rep(1, nb_stocks)     # for Gamma on each node
   gscale_cpue_per_stock      <-apply(all_records_this_vid[, paste(substr(spp, 1,3), "_kg_h", sep='') ], 2, mean)  *nb_agent_per_vessels# for Gamma on each node e.g. hist(rgamma(1000,shape=0.74,scale=1))
   vessel_features            <- c(cruisespeed, 
                                   vessel_specifications[i, "fuel.cons.h"] *nb_agent_per_vessels,
                                   vessel_specifications[i, "mean_LOA_m"] *nb_agent_per_vessels, 
                                   vessel_specifications[i, "mean_kW"] *nb_agent_per_vessels,
                                   vessel_specifications[i, "ave.storage.fish.kg"] *nb_agent_per_vessels,
                                   vessel_specifications[i, "fuel.tank.liter"] *nb_agent_per_vessels,
                                   vessel_specifications[i, "nb_pings_per_trip"], # nb_pings_per_trip
                                   vessel_specifications[i, "Gamma_shape"], # Gamma shape
                                   vessel_specifications[i, "Gamma_scale"], # Gamma scale
                                   vessel_specifications[i, "trip.duration.h"],
                                   vessel_specifications[i, "multip.fuel.steaming"],
                                   vessel_specifications[i, "multip.fuel.fishing"],
                                   vessel_specifications[i, "multip.fuel.ret.port.fish"],
                                   vessel_specifications[i, "multip.fuel.inactive"],
                                   vessel_specifications[i, "weekEndStartDay"],
                                   vessel_specifications[i, "WeekEndEndDay"],
                                   vessel_specifications[i, "WorkHoursStart"],
                                   vessel_specifications[i, "WorkHoursEnd"],
                                   vessel_specifications[i, "firm_id"]
                                   ) 
   step_in_share              <- rep(vessel_specifications[i, "N..of.vessels"]/ sum(vessel_specifications[, "N..of.vessels"], na.rm=TRUE), nb_stocks) 
                                       # i.e. 100 % of each TAC per stock will be booked for these new vessels
   # catch equation parameters: totcatch_inkgperhour_thisspecies = exp( vesselspe_sp + metierspe_sp + (avaispe_sp_szgr * avai_sp_szgr *1000 * gearsel_szgr ) )
   # by default, only the vessel effect is !=0....
   vesselsspe_betas           <- log(fixed_cpue_per_stock*nb_agent_per_vessels +0.01 )  # catch rate log(kg per hour)  CAUTION: log(0)=-Inf !
   # ....but we could imagine informing metier effect here:
   metiersspe_betas           <- matrix(rep(0, length(metierids)), nrow=length(spp), ncol=length(metierids))    # 0 means not adding to the effect 
                                                                                        # (but metiers are having catch if >=0, so consider putting -20 if the metier is not catching the species)
   # ....and we could also imagine informing sizegroup avai effect here:
   avaisspe_betas             <- matrix(rep(0, 14), nrow=length(spp), ncol=14)          # 0 means not adding to the effect 
   create_file_for_fuel_price_per_vessel_size <- TRUE
   some_fuel_price_per_vessel_size            <- c(0.4,0.4,0.4,0.4,0.4)  # euro per litre
   step_in_share_credits                      <- vessel_specifications[i, "N..of.vessels"]/ sum(vessel_specifications[, "N..of.vessels"]) # i.e. % of the credits will be booked for these new vessels

   # create a (intermediate) config file
   if(any("vid" %in% colnames(vessel_specifications))){
      namefile <- file.path(general$main_path_gis, "FISHERIES", "vessels_config_files",
               paste("vessels_creator_args_",general$application,"_", as.character(vessel_specifications[i, "vid"]), "_", harbcode, "_", all_records_this_vid[imax, "name_gis_file_for_fishing_effort_per_polygon"], ".dat", sep=''))
   } else{
      namefile <- file.path(general$main_path_gis, "FISHERIES", "vessels_config_files",
               paste("vessels_creator_args_",general$application, "_", harbcode, "_", vessel_specifications[i, "name_gis_file_for_fishing_effort_per_polygon"], ".dat", sep=''))
   }
 
   cat(paste("write the config file.. \n"))
  
   write("# config file for the vessel editor: adding some vessel(s)", file=namefile)
   write("# (the shortestPaths library will have to be re-created for the graph)", file=namefile, ncolumns=1, append=TRUE)
   write("# --------------", file=namefile, ncolumns=1, append=TRUE)
  
   write("# [input_folder_for_config_file]", file=namefile, ncolumns=1, append=TRUE)
   write(general$main_path_gis, file=namefile, ncolumns=1, append=TRUE)
  
   write("# [input_folder_for_DISPLACE]", file=namefile, ncolumns=1, append=TRUE)
   write(general$main.path.ibm, file=namefile, ncolumns=1, append=TRUE)
  
   write("# [name_of_the_application]",file=namefile, ncolumns=1, append=TRUE)
   write(general$application, file=namefile, ncolumns=1, append=TRUE)
  
   write("# [name_of_the_graph_for_this_application]", file=namefile, ncolumns=1, append=TRUE)
   write(general$igraph,file=namefile, ncolumns=1, append=TRUE)
   
   write("# [append_to_existing_vessel_files]", file=namefile, ncolumns=1, append=TRUE)
   write(do_append,file=namefile, ncolumns=1, append=TRUE)
  
   write("# [name_gis_file_for_total_effort_per_polygon]",file=namefile, ncolumns=1, append=TRUE)
   write(name_gis_file_for_fishing_effort_per_polygon, file=namefile,  ncolumns=length(name_gis_file_for_fishing_effort_per_polygon), append=TRUE)
 
   write("# [name_gis_layer_field]", file=namefile, ncolumns=1, append=TRUE)
   write(name_gis_layer_field, file=namefile, ncolumns=length(name_gis_layer_field), append=TRUE)
 
   write("# [is_gis_layer_field_relative_numbers]", file=namefile, ncolumns=1, append=TRUE)
   write(is_gis_layer_field_relative_numbers, file=namefile, ncolumns=1, append=TRUE)
 
   write("# [xfold_gis_layer_field]", file=namefile, ncolumns=1, append=TRUE)
   write(xfold_gis_layer_field, file=namefile, ncolumns=length(xfold_gis_layer_field), append=TRUE)
 
   write("# [vesselids]", file=namefile, ncolumns=1, append=TRUE)
   write(vesselids, file=namefile, ncolumns=length(vesselids), append=TRUE)
 
   write("# [vessel_range_km]", file=namefile, ncolumns=1, append=TRUE)
   write(vessel_range_km, file=namefile, ncolumns=1, append=TRUE)
     
   write("# [metierids]", file=namefile, ncolumns=1, append=TRUE)
   write(metierids, file=namefile, ncolumns=length(metierids), append=TRUE)
   
   write("# [metierids_frequencies]", file=namefile, ncolumns=1, append=TRUE)
   write(metierids_frequencies, file=namefile, ncolumns=length(metierids_frequencies), append=TRUE)
   
   write("# [visited_ports_but_look_at_names_in_harbours.dat_in_harboursspe_folder]", file=namefile, ncolumns=1, append=TRUE)
   write(as.character(visited_ports), file=namefile, ncolumns=length(as.character(visited_ports)), append=TRUE)
   
   write("# [visited_ports_frequencies]", file=namefile, ncolumns=1, append=TRUE)
   write(visited_ports_frequencies, file=namefile, ncolumns=length(visited_ports_frequencies), append=TRUE)
   
   write("# [name_file_ports]", file=namefile, ncolumns=1, append=TRUE)
   write(name_file_ports, file=namefile, ncolumns=1, append=TRUE)
      
   write("# [nb_fish_or_shellfish_stocks_which_should_be_consistent_with_popsspe_folder]",file=namefile, ncolumns=1, append=TRUE)
   write(nb_stocks, file=namefile, ncolumns=length(nb_stocks), append=TRUE)
  
   write("# [fixed_cpue_per_stock_on_fgrounds_for_planB]", file=namefile, ncolumns=1, append=TRUE)
   write(as.character(as.numeric(fixed_cpue_per_stock)), file=namefile, ncolumns=length(as.character(as.numeric(fixed_cpue_per_stock))), append=TRUE)
  
   write("# [Gamma_shape_parameter_for_cpue_per_stock_on_fgrounds_for_planA_but_for_implicit_stocks_or_out_of_range_nodes]", file=namefile, ncolumns=1, append=TRUE)
   write(as.character(as.numeric(gshape_cpue_per_stock)), file=namefile, ncolumns=length(as.character(as.numeric(gshape_cpue_per_stock))), append=TRUE)
  
   write("# [Gamma_scale_parameter_for_cpue_per_stock_on_fgrounds_for_planA_but_for_implicit_stocks_or_out_of_range_nodes]", file=namefile, ncolumns=1, append=TRUE)
   write(as.character(as.numeric(gscale_cpue_per_stock)), file=namefile, ncolumns=length(as.character(as.numeric(gscale_cpue_per_stock))), append=TRUE)
  
   write("# [vessel_features_speed_fuelconsrate_length_kW_carryingcapacity_tankcapacity_nbpingspertrip_shapeinbtw_scaleinbtw_avtripduration]",
   file=namefile, ncolumns=1, append=TRUE)
  
   write("# [multfuelconswhensteaming_multfuelconswhenfishing_multfuelconswhenreturning_multfuelconswheninactive_firmid]",
     file=namefile, ncolumns=1, append=TRUE)         
   write(vessel_features, file=namefile, ncolumns=length(vessel_features), append=TRUE)
  
   write("# [percent_step_in_share_for_TAC_per_stock_for_these_incoming_vessels_but_only_used_if_existing_vessels_already]",file=namefile, ncolumns=1, append=TRUE)
   write(step_in_share, file=namefile, ncolumns=length(step_in_share), append=TRUE)
   
   write("# [vessel_effect_per_stock_in_the_catch_rate_equation]", file=namefile, ncolumns=1, append=TRUE)
   write(as.character(as.numeric(vesselsspe_betas)), file=namefile, ncolumns=length(as.character(as.numeric(vesselsspe_betas))), append=TRUE)
  
   write("# [metier_effect_per_stock_in_the_catch_rate_equation]", file=namefile, ncolumns=1, append=TRUE)
   write.table(metiersspe_betas, file=namefile, row.names = FALSE, col.names = FALSE, quote=FALSE, append=TRUE)
 
   write("# [avai_effect_per_size_group_per_stock_in_the_catch_rate_equation]", file=namefile, ncolumns=1, append=TRUE)
   write.table(avaisspe_betas, file=namefile, row.names = FALSE, col.names = FALSE, quote=FALSE, append=TRUE)
 
   write("# [create_the_file_for_fuel_price_per_vessel_size]", file=namefile, ncolumns=1, append=TRUE)
   write(create_file_for_fuel_price_per_vessel_size, file=namefile, ncolumns=length(create_file_for_fuel_price_per_vessel_size), append=TRUE)
  
   write("# [some_fuel_prices_per_vessel_size_euro_per_litre]",file=namefile, ncolumns=1, append=TRUE)
   write(some_fuel_price_per_vessel_size, file=namefile, ncolumns=length(some_fuel_price_per_vessel_size), append=TRUE)
  
   write("# [percent_fishing_credits_taken_by_incomers_for_RTI_management]", file=namefile, ncolumns=1, append=TRUE)
   write(step_in_share_credits, file=namefile, ncolumns=length(step_in_share_credits), append=TRUE)

   }   # end test
    
    cat(paste("write the config file...done \n"))

  }   # end for loop over record
    
   
 cat(paste("......done.  stored in /FISHERIES/vessels_config_files folder"))  
   
      
 
