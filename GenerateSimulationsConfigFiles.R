   # GENERAL SETTINGS

   args <- commandArgs(trailingOnly = TRUE)

   general <- list()

   if (length(args) < 2) {
     if(.Platform$OS.type == "windows") {
       general$application           <- "balticRTI" # ...or myfish
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
   cat(paste("START \n"))


 
 
   if( general$application == "balticRTI") {
     #general$implicit_stocks       <- c(0, 2, 3, 7, 8, 9, 10, 11, 12) # implicit level1 = we don´t know the absolute abundance 
     general$implicit_stocks       <- c("") # implicit level1 = we don´t know the absolute abundance 
     general$implicit_stocks_level2<- c("") # implicit level2 = we don´t know the absolute abundance but we use the relative abudance to draw some catch rates 
   } else{
      stop("adapt the script to this app")
   }
   

  dir.create(file.path(general$main.path.ibm, paste("simusspe_", general$application, sep='')))


  #load
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




  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ## a scenario file e.g. baseline.dat ##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  namefile  <- file.path(general$main.path.ibm, paste("simusspe_", general$application, sep=''), paste("baseline.dat", sep=''))

   dyn_alloc_sce <- c('baseline focus_on_high_profit_grounds')
   dyn_pop_sce   <- c('baseline')
   biolsce       <- 1
   Frequency     <- 3
   Frequency2    <- 3
   a_graph       <- general$igraph
   nrow_coord    <- nrow(coord)
   nrow_graph    <- nrow(graph)
   a_port        <- which(coord[,'harb']!=0)[1]
   grid_res_km   <- 3
   is_individual_vessel_quotas <- 0
   check_all_stocks <- 0
   Go_Fishing_DTree <- "GoFishing_Example.dt.csv"
   Choose_Ground_DTree <- ""
   Start_Fishing_DTree <- ""
   Change_Ground_DTree <- ""
   Stop_Fishing_DTree  <- ""
   Change_Port_DTree   <- ""
   Use_Dtrees          <- 1
   tariff_pop          <- ""
   freq_update_tariff_code <- ""
   arbitrary_breaks_for_tariff <- ""
   total_amount_credited <- ""
   tariff_annual_       <- ""
   banned_metiers       <- ""
   
   write("# dyn_alloc_sce", file=namefile)
   write(dyn_alloc_sce, file=namefile, ncolumns=1, append=TRUE)

   write("# dyn_pop_sce", file=namefile, ncolumns=1, append=TRUE)
   write(dyn_pop_sce, file=namefile, ncolumns=1, append=TRUE)

   write("# biolsce", file=namefile, ncolumns=1, append=TRUE)
   write(biolsce, file=namefile, ncolumns=1, append=TRUE)

   write("# Frequency to apply growth (0:daily; 1:weekly; 2:monthly; 3:quarterly; 4:semester)", file=namefile, ncolumns=1, append=TRUE)
   write(Frequency, file=namefile, ncolumns=1, append=TRUE)

   write("# Frequency to redispatch the pop (0:daily; 1:weekly; 2:monthly; 3:quarterly; 4:semester)", file=namefile, ncolumns=1, append=TRUE)
   write(Frequency2, file=namefile, ncolumns=1, append=TRUE)

   write("# a_graph", file=namefile, ncolumns=1, append=TRUE)
   write(a_graph, file=namefile, ncolumns=1, append=TRUE)

   write("# nrow_coord", file=namefile, ncolumns=1, append=TRUE)
   write(nrow_coord, file=namefile, ncolumns=1, append=TRUE)

   write("# nrow_graph",file=namefile, ncolumns=1, append=TRUE)
   write(nrow_graph, file=namefile, ncolumns=1, append=TRUE)

   write("# a_port",file=namefile, ncolumns=1, append=TRUE)
   write(a_port, file=namefile, ncolumns=1, append=TRUE)

   write("# grid res km",file=namefile, ncolumns=1, append=TRUE)
   write(grid_res_km, file=namefile, ncolumns=1, append=TRUE)

   write("# is_individual_vessel_quotas", file=namefile, ncolumns=1, append=TRUE)
   write(is_individual_vessel_quotas, file=namefile, ncolumns=1, append=TRUE)

   write("#  check all stocks before going fishing (otherwise, explicit pops only)", file=namefile, ncolumns=1, append=TRUE)
   write(check_all_stocks, file=namefile, ncolumns=1, append=TRUE)

   write("# Go Fishing DTree", file=namefile, ncolumns=1, append=TRUE)
   write(Go_Fishing_DTree, file=namefile, ncolumns=1, append=TRUE)

   write("# Choose Ground DTree", file=namefile, ncolumns=1, append=TRUE)
   write(Choose_Ground_DTree, file=namefile, ncolumns=1, append=TRUE)

   write("# Start Fishing DTree", file=namefile, ncolumns=1, append=TRUE)
   write(Start_Fishing_DTree, file=namefile, ncolumns=1, append=TRUE)

   write("# Change Ground DTree", file=namefile, ncolumns=1, append=TRUE)
   write(Change_Ground_DTree, file=namefile, ncolumns=1, append=TRUE)

   write("# Stop Fishing DTree", file=namefile, ncolumns=1, append=TRUE)
   write(Stop_Fishing_DTree, file=namefile, ncolumns=1, append=TRUE)

   write("# Change Port DTree", file=namefile, ncolumns=1, append=TRUE)
   write(Change_Port_DTree, file=namefile, ncolumns=1, append=TRUE)
  
   write("# Use Dtrees", file=namefile, ncolumns=1, append=TRUE)
   write(Use_Dtrees, file=namefile, ncolumns=1, append=TRUE)
 
   write("#tariff_pop", file=namefile, ncolumns=1, append=TRUE)
   write(tariff_pop, file=namefile, ncolumns=1, append=TRUE)
   
   write("#req_update_tariff_code", file=namefile, ncolumns=1, append=TRUE)
   write(freq_update_tariff_code, file=namefile, ncolumns=1, append=TRUE)
   
   write("#arbitrary_breaks_for_tariff", file=namefile, ncolumns=1, append=TRUE)
   write(arbitrary_breaks_for_tariff, file=namefile, ncolumns=1, append=TRUE)
   
   write("#total_amount_credited", file=namefile, ncolumns=1, append=TRUE)
   write(total_amount_credited, file=namefile, ncolumns=1, append=TRUE)
   
   write("#tariff_annual_", file=namefile, ncolumns=1, append=TRUE)
   write(tariff_annual_, file=namefile, ncolumns=1, append=TRUE)
  
   write("# banned metiers", file=namefile, ncolumns=1, append=TRUE)
   write(banned_metiers, file=namefile, ncolumns=1, append=TRUE)
  
 
    cat(paste("Write baseline.dat ...done \n"))



  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ## a config.dat                      ##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  namefile  <- file.path(general$main.path.ibm, paste("simusspe_", general$application, sep=''), paste("config.dat", sep=''))

  # caution: give the order for naming stocks in integer from 0 to n-1
  spp_table <-  read.table(file=file.path(general$main_path_gis, "POPULATIONS", paste("pop_names_", general$application ,".txt",sep='')),
              header=TRUE)
  spp                        <- as.character(spp_table$spp)

  nbpops           <- length(spp)
  nbbenthospops    <- 2
  implicit_stocks  <- general$implicit_stocks
  implicit_stocks_level2  <- general$implicit_stocks_level2
  calib_other_landings        <- rep(1, nbpops)
  calib_weight_at_size_group  <- rep(1, nbpops)
  calib_cpue_multipliers      <- rep(1, nbpops)
  interesting_harbours <- ""

 
  write("# nbpops", file=namefile)
  write(nbpops, file=namefile, ncolumns=1, append=TRUE)

  write("# nbbenthospops", file=namefile, ncolumns=1, append=TRUE)
  write(nbbenthospops, file=namefile, ncolumns=1, append=TRUE)

  write("# implicit stocks", file=namefile, ncolumns=1, append=TRUE)
  write(implicit_stocks, file=namefile, ncolumns=length(implicit_stocks), append=TRUE)
 
  write("# calib the other landings per stock", file=namefile, ncolumns=1, append=TRUE)
  write(calib_other_landings, file=namefile, ncolumns=length(calib_other_landings), append=TRUE)
 
  write("# calib weight-at-szgroup per stock", file=namefile, ncolumns=1, append=TRUE)
  write(calib_weight_at_size_group, file=namefile, ncolumns=length(calib_weight_at_size_group), append=TRUE)
 
  write("# calib the cpue multiplier per stock", file=namefile, ncolumns=1, append=TRUE)
  write(calib_cpue_multipliers, file=namefile, ncolumns=length(calib_weight_at_size_group), append=TRUE)
 
   write("# interesting harbours", file=namefile, ncolumns=1, append=TRUE)
  write(interesting_harbours, file=namefile, ncolumns=1, append=TRUE)

   write("# implicit stocks level 2", file=namefile, ncolumns=1, append=TRUE)
   write(implicit_stocks_level2, file=namefile, ncolumns=length(implicit_stocks_level2), append=TRUE)

 
    cat(paste("Write config.dat ...done \n"))


 
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ## a tstep_months_.dat               ##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

start_y <- 2009
end_y   <- 2015
#start_y <- 2012
#end_y   <- 2017
t.seq <- seq(as.POSIXct(paste(start_y,"-01-01 00:00:00",sep='')),
        as.POSIXct(paste(end_y,"-12-31 00:00:00",sep='')), by="hours")


#year-days
idx <- NULL
idx <- which( t.seq %in% as.POSIXct(paste(rep(start_y:end_y, each=365),"-",
                                 c(rep('01',each=31), rep('02',each=28), rep('03',each=31), rep('04',each=30), 
                                  rep('05',each=31), rep('06',each=30), rep('07',each=31), rep('08',each=31), rep('09',each=30), rep('10',each=31), rep('11',each=30), rep('12',each=31)),
                                 "-", sprintf("%02d", c(1:31, 1:28,1:31, 1:30, 1:31, 1:30, 1:31, 1:31, 1:30, 1:31, 1:30, 1:31)),
                                 " 00:00:00",sep='')))
idx <- c(idx,   -1) # sentinel

write.table(idx[-1], file= file.path(general$main.path.ibm, paste("simusspe_", general$application, sep=''), paste("tstep_days_",start_y,"_",end_y,".dat", sep="")),
                      col.names=FALSE, row.names=FALSE)


#year-months
idx <- NULL
idx <- which( t.seq %in% as.POSIXct(paste(rep(start_y:end_y, each=12),"-",
                                c('01','02','03','04','05','06','07','08','09','10','11','12'),
                                 "-01 00:00:00",sep='')))
                                        
idx <- c(idx,   -1) # sentinel

write.table(idx[-1], file= file.path(general$main.path.ibm, paste("simusspe_", general$application, sep=''), paste("tstep_months_",start_y,"_",end_y,".dat", sep="")),
                      col.names=FALSE, row.names=FALSE)


#year-quarters
idx <- NULL
for(y in c(start_y:end_y)){
   idx <- c(idx, which( t.seq==as.POSIXct(paste(y,"-01-01 00:00:00",sep=''))),
                  which( t.seq==as.POSIXct(paste(y,"-04-01 00:00:00",sep=''))),
                   which( t.seq==as.POSIXct(paste(y,"-07-01 00:00:00",sep=''))),
                    which( t.seq==as.POSIXct(paste(y,"-10-01 00:00:00",sep='')))
                    
                    )
}
idx <- c(idx,   -1) # sentinel

write.table(idx[-1], file= file.path(general$main.path.ibm, paste("simusspe_", general$application, sep=''), paste("tstep_quarters_",start_y,"_",end_y,".dat", sep="")),
                      col.names=FALSE, row.names=FALSE)

#semesters
idx <- NULL
for(y in c(start_y:end_y)){
   idx <- c(idx, which( t.seq==as.POSIXct(paste(y,"-01-01 00:00:00",sep=''))),
                   which( t.seq==as.POSIXct(paste(y,"-07-01 00:00:00",sep='')))
                    )
}
idx <- c(idx,   -1) # sentinel


write.table(idx[-1], file=file.path(general$main.path.ibm, paste("simusspe_", general$application, sep=''), paste("tstep_semesters_",start_y,"_",end_y,".dat", sep="")),
                      col.names=FALSE, row.names=FALSE)

#years
idx <- NULL
for(y in c(start_y:end_y)){
   idx <- c(idx, which( t.seq==as.POSIXct(paste(y,"-01-01 00:00:00",sep='')))
                    )
}
idx <- c(idx,   -1) # sentinel

write.table(idx[-1], file=file.path(general$main.path.ibm, paste("simusspe_", general$application, sep=''), paste("tstep_years_",start_y,"_",end_y,".dat", sep="")),
                      col.names=FALSE, row.names=FALSE)



cat(paste("Write time steps related files ...done \n"))
   

# the graph-related files are needed....
file.copy(
          from=file.path(general$main_path_gis, "GRAPH", paste("coord",general$igraph,".dat", sep='')),
          to=file.path(general$main.path.ibm, paste("graphsspe", sep=''), paste("coord",general$igraph,".dat", sep=''))
          )
file.copy(
          from=file.path(general$main_path_gis, "GRAPH", paste("graph",general$igraph,".dat", sep='')),
          to=file.path(general$main.path.ibm, paste("graphsspe", sep=''), paste("graph",general$igraph,".dat", sep=''))
          )
file.copy(
          from=file.path(general$main_path_gis, "GRAPH", paste("metier_closure_a_graph",general$igraph,"_quarter1.dat", sep='')),
          to=file.path(general$main.path.ibm, paste("graphsspe", sep=''), paste("metier_closure_a_graph",general$igraph,"_quarter1.dat", sep=''))
          )
file.copy(
          from=file.path(general$main_path_gis, "GRAPH", paste("metier_closure_a_graph",general$igraph,"_quarter2.dat", sep='')),
          to=file.path(general$main.path.ibm, paste("graphsspe", sep=''), paste("metier_closure_a_graph",general$igraph,"_quarter2.dat", sep=''))
          )
file.copy(
          from=file.path(general$main_path_gis, "GRAPH", paste("metier_closure_a_graph",general$igraph,"_quarter3.dat", sep='')),
          to=file.path(general$main.path.ibm, paste("graphsspe", sep=''), paste("metier_closure_a_graph",general$igraph,"_quarter3.dat", sep=''))
          )
file.copy(
          from=file.path(general$main_path_gis, "GRAPH", paste("metier_closure_a_graph",general$igraph,"_quarter4.dat", sep='')),
          to=file.path(general$main.path.ibm, paste("graphsspe", sep=''), paste("metier_closure_a_graph",general$igraph,"_quarter4.dat", sep=''))
          )
file.copy(
          from=file.path(general$main_path_gis, "GRAPH", paste("code_area_for_graph",general$igraph,"_points.dat", sep='') ),
          to=file.path(general$main.path.ibm, paste("graphsspe", sep=''),  paste("code_area_for_graph",general$igraph,"_points.dat", sep='') )
          )
file.copy(
          from=file.path(general$main_path_gis, "GRAPH", paste("nodes_in_polygons_a_graph",general$igraph,"_quarter1.dat", sep='') ),
          to=file.path(general$main.path.ibm, paste("graphsspe", sep=''),  paste("nodes_in_polygons_a_graph",general$igraph,"_quarter1.dat", sep='') )
          )
file.copy(
          from=file.path(general$main_path_gis, "GRAPH", paste("nodes_in_polygons_a_graph",general$igraph,"_quarter2.dat", sep='') ),
          to=file.path(general$main.path.ibm, paste("graphsspe", sep=''),  paste("nodes_in_polygons_a_graph",general$igraph,"_quarter2.dat", sep='') )
          )
file.copy(
          from=file.path(general$main_path_gis, "GRAPH", paste("nodes_in_polygons_a_graph",general$igraph,"_quarter3.dat", sep='') ),
          to=file.path(general$main.path.ibm, paste("graphsspe", sep=''),  paste("nodes_in_polygons_a_graph",general$igraph,"_quarter3.dat", sep='') )
          )
file.copy(
          from=file.path(general$main_path_gis, "GRAPH", paste("nodes_in_polygons_a_graph",general$igraph,"_quarter4.dat", sep='') ),
          to=file.path(general$main.path.ibm, paste("graphsspe", sep=''),  paste("nodes_in_polygons_a_graph",general$igraph,"_quarter4.dat", sep='') )
          )
          

cat(paste("Transfer graph related files ...done \n"))



dir.create(file.path(general$main.path.ibm, paste("dtrees")))

write("#TreeVersion: 6", file.path(general$main.path.ibm, paste("dtrees"),"GoFishing_Example.dt.csv"))
write("#TreeType: GoFishing", file.path(general$main.path.ibm, paste("dtrees"),"GoFishing_Example.dt.csv"), append=TRUE)
write("# id,variable,posx,posy,nchld,children...,value", file.path(general$main.path.ibm, paste("dtrees"),"GoFishing_Example.dt.csv"), append=TRUE)
write("0,todayIs,2564,2378,2,1,0,2,1,0", file.path(general$main.path.ibm, paste("dtrees"),"GoFishing_Example.dt.csv"), append=TRUE)
write("1,probability,2465,2543,0,0.8", file.path(general$main.path.ibm, paste("dtrees"),"GoFishing_Example.dt.csv"), append=TRUE)
write("2,probability,2633,2544,0,0", file.path(general$main.path.ibm, paste("dtrees"),"GoFishing_Example.dt.csv"), append=TRUE)
 
cat(paste("Create an example GoFishing dtree ...done \n"))


dir.create(file.path(general$main.path.ibm, paste("timeseries")))
dir.create(file.path(general$main.path.ibm, paste("externalforcing_", general$application, sep='')))



cat(paste("Create missing folders...done \n"))


cat(paste(".......done \n"))
   
