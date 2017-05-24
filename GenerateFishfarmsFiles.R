  # GENERAL SETTINGS

   args <- commandArgs(trailingOnly = TRUE)

   general <- list()

   if (length(args) < 2) {
     if(.Platform$OS.type == "windows") {
       general$application           <- "testexample" # ...or myfish
       general$main_path_gis         <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis", general$application)
       general$main.path.ibm         <- file.path("C:","Users","fbas","Documents","GitHub",paste("DISPLACE_input_", general$application, sep=''))
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
  cat(paste("START\n"))
  

  dir.create(file.path(general$main.path.ibm, paste("fishfarmsspe_", general$application, sep='')))

  # load
  coord <- read.table(file=file.path(general$main_path_gis, "GRAPH", paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
  dd    <- coord
  coord <- as.matrix(as.vector(coord))
  coord <- matrix(coord, ncol=3)
  colnames(coord) <- c('x', 'y', 'harb')
  if(do_plot) plot(coord[,1], coord[,2])

  graph <- read.table(file=file.path(general$main_path_gis, "GRAPH", paste("graph", general$igraph, ".dat", sep=""))) # build from the c++ gui
  graph <- as.matrix(as.vector(graph))
  graph <- matrix(graph, ncol=3)
  if(do_plot) segments(coord[graph[,1]+1,1], coord[graph[,1]+1,2], coord[graph[,2]+1,1], coord[graph[,2]+1,2], col=4) # CAUTION: +1, because c++ to R
  
  cat(paste("Read graph...done\n"))



  # read
  fishfarms_features   <-  read.table(file.path(general$main_path_gis,"FISHFARMS", "fishfarms_features.csv"), sep=";", header=TRUE)
  cat(paste("Read fishfarms specs...done\n"))

  # find the nearest graph node
  cat(paste("Finding neighbours....\n"))
  library(spatstat)
  an <- function(x)  as.numeric(as.character(x))
  coord_f            <- coord[coord[,'harb']==0,]  
   X                 <- ppp(x=an(fishfarms_features$long), y=an(fishfarms_features$lat),
                        xrange=range(an(fishfarms_features$long)), yrange=range(an(fishfarms_features$lat)))
   Y                 <- ppp(x=coord_f[,"x"], y=coord_f[,"y"],
                               xrange=range(coord_f[,"x"]), yrange=range(coord_f[,"y"]))
   N                 <- nncross (X=X, Y=Y)$which # caution: just euclidean distance on coord
   fishfarms_features <- cbind(fishfarms_features, idx_node= N)
    
  #some_fishfarms_features           <- fishfarms_features[,c('pt_graph', 'size_km2')] 
  #colnames(some_fishfarms_features) <- c('idx_node', 'size_km2')
  
  
  
  
  #check growth from parameters e.g. for Raimbow Trout (assuming fish grow in cage at sea from age 12 months)
  vbg <- function (Linf, K, t0, timestep){
    return( Linf*(1-exp(-K*(timestep-t0)))^3 ) 
    }
  meanlength_mm_per_month <- vbg(536.7, 1.481, -0.274, seq(1, 5, by=0.083333)) # from end year 1 to end year 5 by month 
 
  weight_based_vbg <- function (Linf, K, t0, fulton_condition_factor, timestep){
    return( ((fulton_condition_factor/100000*Linf^3)*(1-exp(-K*(timestep-t0)))^3) )
    }
  meanw_grams_per_month                <- weight_based_vbg(536.7, 1.481, -0.274, 2.10, seq(1, 5, by=0.083333)) # from end year 1 to end year 5 by month 
  meanw_kg_per_month_from_end_1st_y    <- meanw_grams_per_month/1000
  
  
  
  
  ## caution: aggregate per graph node given c++ object are created on nodes only - so likely some farms will be grouped
  variables_to_average    <- c("size_km2", "mean_SST", "mean_salinity", "mean_windspeed", "mean_currentspeed", "max_depth", "diss_O2_mg_per_l", 
                               "Linf_mm",   "K_y",   "t0_y", "fulton_condition_factor", "start_day_growing", "end_day_harvest", "nb_days_fallowing_period",
                               "meanw_at_start", "price_per_kg_at_start", "target_meanw_at_harvest", "meanw_at_harvest", "prop_harvest_kg_sold", "kg_eggs_per_kg",  "feed_price_per_kg", "price_eggs_per_kg",
                               "prop_N_in_feed", "prop_P_in_feed", "feed_vet_price_per_kg", "prop_N_in_feed_vet", "prop_P_in_feed_vet",
                               "net_harvest_kg_per_sqkm_y", "market_price_sold_fish", "operating_cost_per_day")
  ids                     <- paste( "list(", paste(paste("fishfarms_features[['", variables_to_average,"']]", sep=""), collapse=","), ")")
  fishfarms_features_mean <- aggregate( eval(parse(text=ids)) , list(fishfarms_features$idx_node), mean)
  colnames(fishfarms_features_mean) <- c("idx_node", variables_to_average) 
 
  variables_to_sum        <- c("long", "lat", "nb_fish_at_start", "nb_fish_at_harvest", 
                                "N_in_fish_kg_3per", "P_in_fish_kg_0.5per", "total_feed_kg", "total_feed_N_kg", "total_feed_P_kg", "total_feed_vet_kg", "total_feed_vet_N_kg", "total_feed_vet_P_kg",
                                "annual_discharge_N_kg", "annual_discharge_P_kg", "annual_discharge_C_kg", "annual_discharge_heavymetals_kg", "annual_discharge_medecine_kg",
                                "annual_profit")
  ids                     <- paste( "list(", paste(paste("fishfarms_features[['", variables_to_sum,"']]", sep=""), collapse=","), ")")
  fishfarms_features_sum  <- aggregate( eval(parse(text=ids)) , list(fishfarms_features$idx_node), sum)
  colnames(fishfarms_features_sum) <- c("idx_node", variables_to_sum) 
 
  if(nrow(fishfarms_features) !=   nrow(fishfarms_features_sum)){
     print("Caution: some farms have to be merged! assumption on string var such as meanw_growth_model_type need to be made...")
    some_fishfarms_features  <- cbind.data.frame(data.frame(meanw_growth_model_type= "((fulton_condition_factor/100000*Linf_mm^3)*(1-exp(-K_y*(tstep-t0_y)))^3)",
                                                 feed_type="BioMar_Ecolife_23",
                                                 feed_type_vet="AquaVet-OA"), 
                                                 fishfarms_features_mean,
                                                 fishfarms_features_sum)  [,colnames(fishfarms_features[,-c(1,2)])]
    } else{
     some_fishfarms_features <- fishfarms_features
    }
  

  ## lastly, set up the activity
  some_fishfarms_features <- cbind.data.frame(some_fishfarms_features, is_active=1)

  
  # write
  write.table(some_fishfarms_features,   
            file=file.path(general$main.path.ibm, paste("fishfarmsspe_", general$application, sep=''), 
              paste("fishfarmsspe_features.dat",sep='')),
                  col.names=FALSE,  row.names=FALSE, sep= '|', quote=FALSE, append=FALSE)
  cat(paste("Write fishfarms-related files...done\n"))



  cat(paste("..........done\n"))
