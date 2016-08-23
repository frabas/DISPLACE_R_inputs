 # some args for the bunch of vessels to be created....
 
   # GENERAL SETTINGS
   general <- list()
   if(.Platform$OS.type == "windows") {
     general$main.path             <- file.path("C:","DISPLACE_outputs")
     general$application           <- "balticRTI" # ...or myfish
     general$main.path.param       <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_raw")
     general$main.path.param.gis   <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis", general$application)
     general$main.path.ibm         <- file.path("C:","Users","fbas","Documents","GitHub",paste("DISPLACE_input_", general$application, sep=''))
  
 
   }
   
  
  
   dir.create(path=file.path(general$main.path.param.gis, "FISHERIES", "vessels_config_files"))
   dir.create(path=file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep='')))
  
  
  
   filename <- file.path(general$main.path.param.gis, "FISHERIES", "vessels_specifications_per_harbour_metiers.csv")
   cnts     <- count.fields(filename, sep = ";") 
   vessel_specifications <- read.table(file=filename, sep=";", header=TRUE )
   vessel_specifications <- cbind.data.frame(vessel_specifications, id=1:nrow(vessel_specifications))
   #=> CAUTION: do not leave white rows at the end of this file! otherwise will create some extra wrong config_files
   
   nb_agent_per_vessels <- 1  # caution: super-individuals to reduce the total nb of vessels to be simulated
   vessel_specifications[, "N..of.vessels"] <- ceiling(vessel_specifications[, "N..of.vessels"])/nb_agent_per_vessels
   
   port_names <- read.table(file=file.path(general$main.path.param.gis,
                                 "GRAPH",
                                  paste("harbours.dat", sep='')), sep=";")
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
     file=file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep=''), "metier_names.dat"),
     quote=FALSE, row.names=FALSE, col.names=TRUE)
     

   
   for (i in 1 : nrow(vessel_specifications)){

      
    if(!any("vid" %in% colnames(vessel_specifications)) || 
       any("vid" %in% colnames(vessel_specifications)) && i>1 && vessel_specifications[i, "vid"]!=vessel_specifications[i-1, "vid"] ||
        any("vid" %in% colnames(vessel_specifications)) && i==1 ){
       # test for truly individual vessel data i.e. one vessel => one config file
    
    
      spp_table <-  read.table(file=file.path(general$inPathPop, paste("pop_names_",general$application ,".txt",sep='')),
              header=TRUE)
      spp                        <- as.character(spp_table$spp)

      
      general$igraph             <- 56  # caution: should be consistent with existing vessels already built upon a given graph
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
    
    
  
     #  dd <- read.table ("C:\\Users\\fbas\\Documents\\GitHub\\DISPLACE_input_adriatic\\harboursspe_adriatic\\names_harbours.dat", header=T)
    # unique(as.character(vessel_specifications$Harbor))[! unique(as.character(vessel_specifications$Harbor)) %in% dd$name]
   if(!all(visited_ports %in% rownames(port_names))){  
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
                                   vessel_specifications[i, "WorkHoursEnd"]
                                   ) 
   step_in_share              <- rep(vessel_specifications[i, "N..of.vessels"]/ sum(vessel_specifications[, "N..of.vessels"], na.rm=TRUE), nb_stocks) # i.e. 100 % of each TAC per stock will be booked for these new vessels
   vesselsspe_betas           <- log(fixed_cpue_per_stock*nb_agent_per_vessels +0.01 )  # catch rate log(kg per hour) from the Marche region # stock name 0:Hake, 1:Sole, 2: Mullet, 3: Mantis  CAUTION: log(0)=-Inf !
   create_file_for_fuel_price_per_vessel_size <- TRUE
   some_fuel_price_per_vessel_size            <- c(0.4,0.4,0.4,0.4,0.4)  # euro per litre
   step_in_share_credits                      <- vessel_specifications[i, "N..of.vessels"]/ sum(vessel_specifications[, "N..of.vessels"]) # i.e. % of the credits will be booked for these new vessels

   # create a (intermediate) config file
   if(any("vid" %in% colnames(vessel_specifications))){
      namefile <- file.path(general$main.path.param.gis, "FISHERIES", "vessels_config_files",
               paste("vessels_creator_args_",general$application,"_", as.character(vessel_specifications[i, "vid"]), "_", harbcode, "_", all_records_this_vid[imax, "metier"], ".dat", sep=''))
   } else{
      namefile <- file.path(general$main.path.param.gis, "FISHERIES", "vessels_config_files",
               paste("vessels_creator_args_",general$application, "_", harbcode, "_", vessel_specifications[i, "metier"], ".dat", sep=''))
   }
 
   
   write("# config file for the vessel editor: adding some vessel(s)", file=namefile)
   write("# (the shortestPaths library will have to be re-created for the graph)", file=namefile, ncolumns=1, append=TRUE)
   write("# --------------", file=namefile, ncolumns=1, append=TRUE)
  
   write("# input folder for config file", file=namefile, ncolumns=1, append=TRUE)
   write(general$main.path.param.gis, file=namefile, ncolumns=1, append=TRUE)
  
   write("# output folder for parameterisation file", file=namefile, ncolumns=1, append=TRUE)
   write(general$main.path.param, file=namefile, ncolumns=1, append=TRUE)
  
   write("# input folder for DISPLACE", file=namefile, ncolumns=1, append=TRUE)
   write(general$main.path.ibm, file=namefile, ncolumns=1, append=TRUE)
  
   write("# name of the application",file=namefile, ncolumns=1, append=TRUE)
   write(general$application, file=namefile, ncolumns=1, append=TRUE)
  
   write("# name of the graph for this application", file=namefile, ncolumns=1, append=TRUE)
   write(general$igraph,file=namefile, ncolumns=1, append=TRUE)
   
   write("# append to existing vessel files", file=namefile, ncolumns=1, append=TRUE)
   write(do_append,file=namefile, ncolumns=1, append=TRUE)
  
   write("# name gis file for total effort per polygon",file=namefile, ncolumns=1, append=TRUE)
   write(name_gis_file_for_fishing_effort_per_polygon, file=namefile,  ncolumns=length(name_gis_file_for_fishing_effort_per_polygon), append=TRUE)
 
   write("# name_gis_layer_field",file=namefile, ncolumns=1, append=TRUE)
   write(name_gis_layer_field, file=namefile, ncolumns=length(name_gis_layer_field), append=TRUE)
 
   write("# is_gis_layer_field_relative_numbers",file=namefile, ncolumns=1, append=TRUE)
   write(is_gis_layer_field_relative_numbers, file=namefile, ncolumns=1, append=TRUE)
 
   write("# xfold_gis_layer_field",file=namefile, ncolumns=1, append=TRUE)
   write(xfold_gis_layer_field, file=namefile, ncolumns=length(xfold_gis_layer_field), append=TRUE)
 
   write("# vesselids", file=namefile, ncolumns=1, append=TRUE)
   write(vesselids, file=namefile, ncolumns=length(vesselids), append=TRUE)
 
   write("# vessel_range_km", file=namefile, ncolumns=1, append=TRUE)
   write(vessel_range_km, file=namefile, ncolumns=1, append=TRUE)
     
   write("# metierids", file=namefile, ncolumns=1, append=TRUE)
   write(metierids, file=namefile, ncolumns=length(metierids), append=TRUE)
   
   write("# metierids_frequencies", file=namefile, ncolumns=1, append=TRUE)
   write(metierids_frequencies, file=namefile, ncolumns=length(metierids_frequencies), append=TRUE)
   
   write("# visited_ports (look at the names in harbours.dat in /harboursspe)", file=namefile, ncolumns=1, append=TRUE)
   write(as.character(visited_ports), file=namefile, ncolumns=length(as.character(visited_ports)), append=TRUE)
   
   write("# visited_ports_frequencies", file=namefile, ncolumns=1, append=TRUE)
   write(visited_ports_frequencies, file=namefile, ncolumns=length(visited_ports_frequencies), append=TRUE)
   
   write("# name_file_ports", file=namefile, ncolumns=1, append=TRUE)
   write(name_file_ports, file=namefile, ncolumns=1, append=TRUE)
      
   write("# nb fish or shellfish stocks (should be consistent with /popsspe)",file=namefile, ncolumns=1, append=TRUE)
   write(nb_stocks, file=namefile, ncolumns=length(nb_stocks), append=TRUE)
  
   write("# fixed cpue per stock on fgrounds(plan B)", file=namefile, ncolumns=1, append=TRUE)
   write(as.character(as.numeric(fixed_cpue_per_stock)), file=namefile, ncolumns=length(as.character(as.numeric(fixed_cpue_per_stock))), append=TRUE)
  
   write("# Gamma (shape parameter) cpue per stock on fgrounds (plan A but for implicit stocks or out of range nodes)", file=namefile, ncolumns=1, append=TRUE)
   write(as.character(as.numeric(gshape_cpue_per_stock)), file=namefile, ncolumns=length(as.character(as.numeric(gshape_cpue_per_stock))), append=TRUE)
  
   write("# Gamma (scale parameter) cpue per stock on fgrounds(plan A but for implicit stocks or out of range nodes)", file=namefile, ncolumns=1, append=TRUE)
   write(as.character(as.numeric(gscale_cpue_per_stock)), file=namefile, ncolumns=length(as.character(as.numeric(gscale_cpue_per_stock))), append=TRUE)
  
   write("# vessel features (speed, fuelconsrate, length, kW, carrying_capacity, tank_capacity, nb_pings_per_trip, shape_in_btw, scale_in_btw, av.trip.duration)",
   file=namefile, ncolumns=1, append=TRUE)
  
   write("#  mult_fuelcons_when_steaming, mult_fuelcons_when_fishing, mult_fuelcons_when_returning, mult_fuelcons_when_inactive) ",
     file=namefile, ncolumns=1, append=TRUE)         
   write(vessel_features, file=namefile, ncolumns=length(vessel_features), append=TRUE)
  
   write("# percent step in share for TAC (per stock) for these incoming vessels (only used if existing vessels)",file=namefile, ncolumns=1, append=TRUE)
   write(step_in_share, file=namefile, ncolumns=length(step_in_share), append=TRUE)
   
   write("# vessel effect (per stock) in the catch rate equation ", file=namefile, ncolumns=1, append=TRUE)
   write(as.character(as.numeric(vesselsspe_betas)), file=namefile, ncolumns=length(as.character(as.numeric(vesselsspe_betas))), append=TRUE)
  
   write("# create the file for fuel price per vessel size  ",file=namefile, ncolumns=1, append=TRUE)
   write(create_file_for_fuel_price_per_vessel_size, file=namefile, ncolumns=length(create_file_for_fuel_price_per_vessel_size), append=TRUE)
  
   write("# some fuel price per vessel size (Euro per litre) ",file=namefile, ncolumns=1, append=TRUE)
   write(some_fuel_price_per_vessel_size, file=namefile, ncolumns=length(some_fuel_price_per_vessel_size), append=TRUE)
  
   write("# i% fishing credits taken from incomers (for RTI management) ", file=namefile, ncolumns=1, append=TRUE)
   write(step_in_share_credits, file=namefile, ncolumns=length(step_in_share_credits), append=TRUE)

   }   # end test
    
  }   # end for loop over record
    
   
   
   
      
 