

 # GENERAL SETTINGS

   args <- commandArgs(trailingOnly = TRUE)

   general <- list()

   if (length(args) < 2) {
     if(.Platform$OS.type == "windows") {
       general$application           <- "testexample" # ...or myfish
       general$main_path_gis         <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis", general$application)
       general$main.path.ibm         <- file.path("C:","Users","fbas","Documents","GitHub",paste("DISPLACE_input_", general$application, sep=''))
       general$igraph                <- 56  # caution: should be consistent with existing objects already built upon a given graph
       general$main_path_R_inputs    <- file.path("C:", "Users", "fbas", "Documents", "GitHub", "DISPLACE_R_inputs")
       do_plot                       <- TRUE
     }
  } else {
       general$application           <- args[1]
       general$main_path_gis         <- args[2]
       general$main.path.ibm         <- args[3]
       general$igraph                <- args[4]  # caution: should be consistent with existing vessels already built upon a given graph
       general$main_path_R_inputs    <- args[5]
       general$main_path_R_inputs     <- file.path("C:", "Users", "fbas", "Documents", "GitHub", "DISPLACE_R_inputs")
       do_plot                       <- FALSE
 }
 cat(paste("START \n"))


  if(general$application=="testexample"){
  
      general$casestudy           <- "testexample"             
      species.to.keep             <- c("COD", "PLE", "WHG", "TUR", "FLE", "DAB", "SOL", "SPR", "HER")
      general$method              <- "maximum" # for the canadian paper
      general$threshold           <- 50
      general$method              <- "inverse" # for the baltic only CS. because the grid mesh size if much finer
      general$threshold           <- 25
      general$p                   <- 0.1
      years <- c(2013:2015)
 
  
  } else{
      stop("to be defined for this app - please look at the R script and adapt for this app - anyway this is an optional step - see the doc for an explanation")
  }


#------------------------  
#------------------------  
#------------------------  

 c.listquote <- function( ... ) {

   args <- as.list( match.call()[ -1 ] )
   lstquote <- list( as.symbol( "list" ) );
   for ( i in args ) {
      # Evaluate expression in parent eviron to see what it refers to
      if ( class( i ) == "name" || ( class( i ) == "call" && i[[1]] != "list" ) ) {
         i <- eval( substitute( i ), sys.frame( sys.parent() ) )
      }
      if ( class( i ) == "call" && i[[1]] == "list" ) {
         lstquote <- c( lstquote, as.list( i )[ -1 ] )                                                
      }
      else if ( class( i ) == "character" )
      {
         for ( chr in i ) {
            lstquote <- c( lstquote, list( parse( text=chr )[[1]] ) )
         }
      }
      else
         stop( paste( "[", deparse( substitute( i ) ), "] Unknown class [", class( i ), "] or is not a list()", sep="" ) )
   }
   return( as.call( lstquote ) )
}

#------------------------  
#------------------------  
#------------------------  
 
   # euclidean distance  (from crossdist in spatstat)
   if(FALSE){
   x1 <- coord[,1] # long graph
   y1 <- coord[,2] # lat graph
   x2 <- an(bits.cpue$ShootLon) # long bits
   y2 <- an(bits.cpue$ShootLat) # lat bits
   n1 <- length(x1)
   n2 <- length(x2)
   X1 <- matrix(rep(x1, n2), ncol = n2)
   Y1 <- matrix(rep(y1, n2), ncol = n2)
   X2 <- matrix(rep(x2, n1), ncol = n1)
   Y2 <- matrix(rep(y2, n1), ncol = n1)
   mat <- sqrt((X1 - t(X2))^2 + (Y1 - t(Y2))^2) 
   mat[1,] <- sqrt((X1[1,]-t(X2)[1,])^2 + (Y1[1,]-t(Y2)[1,])^2) 
   lst.graph.pts.with.idx.in.bits <-
       lapply(1:nrow(mat), function(i,mat) {which(mat[i,]<0.3)}, mat) # return the index in X less  than < 0.3 for each Y
   }
  
  
#------------------------  
#------------------------  
#------------------------  
   # great circle distance
  `distance` <-
function(lon,lat,lonRef,latRef){
                    x1 <- lon
                    y1 <- lat
                    x2 <- lonRef
                    y2 <- latRef

                    pd <- pi/180

                    a1<- sin(((y2-y1)*pd)/2)
                    a2<- cos(y1*pd)
                    a3<- cos(y2*pd)
                    a4<- sin(((x2-x1)*pd)/2)
                    a <- a1*a1+a2*a3*a4*a4

                                      c <- 2*atan2(sqrt(a),sqrt(1-a));
                                      R <- 6371;
                                      dx1 <- R*c
                    return(dx1)}
  
  


#------------------------  
#------------------------  
#------------------------  

  bits.cpue <- read.table(file.path(general$main_path_gis, "POPULATIONS", paste("Stock_spatial_research_survey_vessel_data.csv", sep='')), header=TRUE, sep=";")
  cat(paste("Loading Stock_spatial_research_survey_vessel_data.csv....done \n"))
  
  # subset for the chosen period
  bits.cpue <- bits.cpue[bits.cpue$Year %in% years,]
  bits.cpue$Year <- factor(bits.cpue$Year)

  # get species fao code + add missing or mispelling species e.g. PRA and SAN
  load(file.path(general$main_path_gis, "POPULATIONS", "Stock_latin_names.RData"))
  speciesLatinNames <- rbind.data.frame(speciesLatinNames, 
                              data.frame(species_eng="Pandalus", ff_species_latin="Pandalus", fao_code="PRA"))
  speciesLatinNames <- rbind.data.frame(speciesLatinNames, 
                              data.frame(species_eng="Ammodytes marinus", ff_species_latin="Ammodytes marinus", fao_code="SAN"))
  speciesLatinNames <- rbind.data.frame(speciesLatinNames, 
                              data.frame(species_eng="TUR", ff_species_latin="Scophthalmus maximus", fao_code="TUR"))

  bits.cpue$Species_latin <- bits.cpue$Species
  bits.cpue$Species       <- speciesLatinNames$fao_code[match(bits.cpue$Species, speciesLatinNames$ff_species_latin)]
  bits.cpue               <- bits.cpue[!is.na(bits.cpue$Species),] 

  ## FILES FOR BUILDING A IGRAPH
  coord <- read.table(file=file.path(general$main_path_gis,  "GRAPH", paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
  cat(paste("Loading the graph....done \n"))
  coord <- as.matrix(as.vector(coord))
  coord <- matrix(coord, ncol=3)
  colnames(coord) <- c('x', 'y', 'dist')
  #plot(coord[,1], coord[,2])

  coord <- cbind(coord, 1:nrow(coord)) # keep track of the idx_node

  # add a semester code
  bits.cpue$Semester <- factor(bits.cpue$Quarter) # init
  levels(bits.cpue$Semester) <- c(1,2) # BTS in Q1 and Q4


 #------------------------  
 #------------------------  
 #------------------------  
 # keep only relevant species
 # BITS
 bits.cpue           <- bits.cpue [bits.cpue$Species %in% species.to.keep,] 
 bits.cpue$Species   <- factor(bits.cpue$Species)


 #------------------------  
 #------------------------  
 #------------------------  
 # design size group (every 5 cm)
 bits.cpue$size_group <- floor(as.numeric(as.character(bits.cpue$LngtClas)) / 50 )

 bits.cpue$size_group[bits.cpue$size_group>13] <-13 # a plusgroup when > 70 cm  if every 5cm
 cat(paste("14 size groups, 5 cm bins....done \n"))
  
 #------------------------  
 #------------------------  
 #------------------------  
 # fast aggregation using data.table
 # to sum the nb of individuals over the new defined size_group
 library(data.table)

 # BITS
 bits.cpue$ShootLat        <- factor(bits.cpue$ShootLat)
 bits.cpue$ShootLon        <- factor(bits.cpue$ShootLon)
 bits.cpue$size_group      <- factor(bits.cpue$size_group)
 bits.cpue$id              <- factor(paste(bits.cpue$Year,".",bits.cpue$Quarter,".",
                                          bits.cpue$HaulNo,".",bits.cpue$Species,".",bits.cpue$ShootLon,".",bits.cpue$ShootLat, sep=''))
 DT                        <- data.table(bits.cpue) # library data.table for fast grouping replacing aggregate()
 eq1                       <- c.listquote(paste("sum(","CPUE_number_per_hour",",na.rm=TRUE)",sep=""))
 agg.bits.cpue             <- DT[,eval(eq1),by=list(Survey,Year,Semester,Quarter,id, ShootLon,ShootLat, Species,size_group)]
 agg.bits.cpue             <- data.frame(agg.bits.cpue)
 colnames(agg.bits.cpue)   <- c("Survey", "Year", "Semester", "Quarter", "id", "ShootLon", "ShootLat", "Species", "size_group", "nb_indiv")

 #------------------------  
 #------------------------  
 #------------------------  
 #reshape to the wide format
 # tricky because reshape() is painful for large dataset
 # so do it chunk by chunk according to species.
 library(doBy)


 # BITS
 agg.bits.cpue <- orderBy(~size_group, data=agg.bits.cpue)
 bits.cpue.lst <- NULL # chunk by chunk
 levels(agg.bits.cpue$id) <- 1:length(levels(agg.bits.cpue$id)) # make it easier...
 for(sp in unique(bits.cpue$Species)){
 cat(paste(sp, "\n"))
 bits.cpue.lst[[sp]] <- reshape(agg.bits.cpue[agg.bits.cpue$Species==sp,], timevar="size_group",
         idvar="id", direction="wide", v.names="nb_indiv")
  
  # make dim compatible for do.call("rbind")
  nbcol.to.add <- 14 - length(grep("nb_", colnames(bits.cpue.lst[[sp]])))   # knowing that we need 14 szgroup bins
  if(nbcol.to.add>0){
      tmp <- matrix(0,ncol=nbcol.to.add, nrow=nrow(bits.cpue.lst[[sp]]))
      bits.cpue.lst[[sp]] <- cbind(bits.cpue.lst[[sp]], tmp)  
      colnames(bits.cpue.lst[[sp]])  <- 
       c('Survey', 'Year', 'Semester', 'Quarter', 'id', 'ShootLon', 'ShootLat', 'Species',
           'nb_indiv.0', 'nb_indiv.1', 'nb_indiv.2', 'nb_indiv.3',
             'nb_indiv.4', 'nb_indiv.5', 'nb_indiv.6', 'nb_indiv.7',
              'nb_indiv.8', 'nb_indiv.9', 'nb_indiv.10', 'nb_indiv.11','nb_indiv.12', 'nb_indiv.13' )
   }

 }                  
 bits.cpue <- do.call("rbind", bits.cpue.lst)

 bits.cpue <- replace(bits.cpue, is.na(bits.cpue) | bits.cpue=="Inf", 0)

 # check
 head(bits.cpue[bits.cpue$Species==sp,])

 # save   
 dir.create(path=file.path(general$main_path_gis, "POPULATIONS", "avai"))                   
 save(bits.cpue, 
   file=file.path(general$main_path_gis,  "POPULATIONS", "avai", paste("cpue_graph", general$igraph,".RData",sep='')))
  cat(paste("Save survey files in /POPULATIONS/avai folder....done \n"))
 


  
#------------------------  
#------------------------       
#------------------------  
get_cpue_on_graph_nodes <-  function (obj=bits.cpue, coord=coord, sp=sp, S=S, survey="bits", general=general){
  
  nb_size_group <- 13  # caution: magic number

 # subset species and semester
 obj <- obj[obj$Species==sp,]
 obj <- obj[obj$Semester==S,]
 cat(paste("sp ", sp, "\n"))
 cat(paste("S ", S, "\n"))
 
 if(nrow(obj)!=0){
 
 # remove NA
 obj <- replace(obj, is.na(obj), 0)
 
 # 
 an <- function(x) as.numeric(as.character(x))
 obj$ShootLon <- an(obj$ShootLon)
 obj$ShootLat <- an(obj$ShootLat)
 
 # a comment
 a.comment <- paste("graph", general$igraph, "_", sp,"_","S",S,"_",general$method,"_", survey, sep='')
 

  # compute the distance from each node of the graph to the nearest survey points 
  lst.graph.pts.with.idx.in.obj <- vector("list", length=nrow(coord))
  lst.graph.pts.with.idx.in.obj <- 
      lapply(1:length(lst.graph.pts.with.idx.in.obj), function(i, obj, coord){
        dd <- distance(an(obj$ShootLon), an(obj$ShootLat), coord[i,1],coord[i,2])
        names(dd) <- 1:length(obj$ShootLon)
        dd[dd < general$threshold]  ##!!!! THRESHOLD IN km  !!!!##
     }, obj, coord)  
  
 
 # check the links between one randomly chosen node and its neighbours by plotting
 if(FALSE){
   an <- function(x) as.numeric(as.character(x))
   plot(x=an(obj$ShootLon), y=an(obj$ShootLat),  col=grey(0.7), pch=16)
   segments(an(coord[,1])[760], an(coord[,2])[760],
            an(obj$ShootLon)[ an(names(lst.graph.pts.with.idx.in.obj[[760]])) ], 
               an(obj$ShootLat)[ an(names(lst.graph.pts.with.idx.in.obj[[760]])) ], col=3)
   points(x=coord[,"x"], y=coord[,"y"], col="black") # graph g
   points(x=coord[760,"x"], y=coord[760,"y"], col="red") # a pt of the graph g

  segments(an(coord[,1])[746], an(coord[,2])[746],
            an(obj$ShootLon)[ an(names(lst.graph.pts.with.idx.in.obj[[746]])) ], 
               an(obj$ShootLat)[ an(names(lst.graph.pts.with.idx.in.obj[[746]])) ], col=3)
   points(x=coord[746,"x"], y=coord[746,"y"], col="red") # a pt of the graph g

   }

 # inverse distance weighted average
  nm <- paste(rep(sp, each=nb_size_group),".nb_indiv.", 0:nb_size_group,sep='')
   tmp <- matrix(0, ncol=length(nm), nrow=nrow(coord))
   colnames(tmp) <- nm
   coord <- cbind(coord, tmp)
   for(szgroup in paste("nb_indiv.",0:nb_size_group,sep='')){
    if(do_plot) plot(x=an(obj$ShootLon), y=an(obj$ShootLat),  col=grey(0.7), pch=16)
    cat(paste("szgroup", szgroup, "\n"))
     for(node in 1:nrow(coord)){
      #cat(paste("node", node, "\n"))
         idx.pts.obj  <- an(names(lst.graph.pts.with.idx.in.obj[[node]]))
         idx.pts.obj  <- idx.pts.obj  [  !is.na( an(obj[obj$Species==sp, szgroup][ idx.pts.obj ]) ) ] # remove distance with NA for catch (check why NA here)
         dist.pts.obj  <- lst.graph.pts.with.idx.in.obj[[node]] [as.character(idx.pts.obj)]
         if(length(dist.pts.obj)>0){
         coord[node, paste(sp,".",szgroup,sep='')] <-  
                   switch(general$method,
                      inverse= sum(an(obj[obj$Species==sp, szgroup][ idx.pts.obj ]) * ( (1/(dist.pts.obj^general$p)) / sum(1/(dist.pts.obj^general$p)) )), #weighted mean
                      maximum= max(an(obj[obj$Species==sp, szgroup][ idx.pts.obj ])) # keep only the neighbour with max
                      )
         } else{coord[node, paste(sp,".",szgroup,sep='')] <-0}             
         # check by plotting
         if(FALSE && szgroup=="nb_indiv.0")
         if(length(lst.graph.pts.with.idx.in.obj[[node]])!=0 && do_plot){
              segments(an(coord[,1])[node], an(coord[,2])[node],
                an(obj$ShootLon)[ an(names(lst.graph.pts.with.idx.in.obj[[node]])) ], 
                 an(obj$ShootLat)[ an(names(lst.graph.pts.with.idx.in.obj[[node]])) ], col=3)
              points(x=coord[node,"x"], y=coord[node,"y"], col="red") # a pt of the graph g
         savePlot(filename = file.path(general$main_path_gis, "POPULATIONS", "avai", 
                            paste(a.comment,"link_nodes_with_surveys_",general$threshold,".jpeg",sep="")),type ="jpeg")

         }
      } # end node  


  # check 
   if(do_plot){
      plot(x= obj[obj$Species==sp, "ShootLon"], y=obj[obj$Species==sp, "ShootLat"],  col=2, pch=1, 
                cex=obj[obj$Species==sp,szgroup] /max(coord[,paste(sp,".",szgroup,sep='')], obj[obj$Species==sp,szgroup])*4)
      #points(x= coord[,1], y=coord[,2],  col=1, pch=16, 
      #             cex=coord[,paste(sp,".",szgroup,sep='')] /max(coord[,paste(sp,".",szgroup,sep='')], obj[obj$Species==sp,szgroup])*4)
      points(x= coord[,1], y=coord[,2],  col=1, pch=16, 
                 cex=coord[,paste(sp,".",szgroup,sep='')]/ max(coord[,paste(sp,".",szgroup,sep='')])*4)
      points(x= obj[obj$Species==sp, "ShootLon"], y=obj[obj$Species==sp, "ShootLat"],  col=2, pch=1,  # overlay...
                cex=obj[obj$Species==sp,szgroup] /max(coord[,paste(sp,".",szgroup,sep='')], obj[obj$Species==sp,szgroup])*4)
      map(add=TRUE, xlim=c(-10,25),ylim=c(50,65))
      title(szgroup)
      savePlot(filename = file.path(general$main_path_gis,  "POPULATIONS", "avai",
                            paste(a.comment,"_",szgroup,".jpeg",sep="")),type ="jpeg")
      }
   } # end szgroup

coord <-  replace(coord, is.infinite(coord), 0)          

} else{
print("no observation there....fill in with 0s")
   nm <- paste(rep(sp, each=nb_size_group),".nb_indiv.", 0:nb_size_group,sep='')
   tmp <- matrix(0, ncol=length(nm), nrow=nrow(coord))
   colnames(tmp) <- nm
   coord <- cbind(coord, tmp)
    for(szgroup in paste("nb_indiv.",0:nb_size_group,sep='')){
     for(node in 1:nrow(coord)){
         coord[node, paste(sp,".",szgroup,sep='')] <- 0
         } 
     }    
}

return(coord)
}    

#--------------------
#--------------------
#--------------------
#--------------------
set.avai <- function(lst.avai, sp, S, areas){
     obj <- get(paste("coord.",sp,".",S, sep=''), env=.GlobalEnv) # get in .GlobalEnv
     source(file=file.path(general$main_path_R_inputs, "old", "vmstools_longlat_to_ICESareas.r"))
     ICESareas       <- longlat_to_ICESareas(obj)
     idx.areas          <- which(ICESareas %in% areas)
     obj.in.areas <- obj[idx.areas,]
     obj.in.areas <- replace(obj.in.areas, is.infinite(obj.in.areas), NA)
     obj.in.areas[,c(5:18)] <-
                sweep(obj.in.areas[,c(5:18)], 2,
                   apply(obj.in.areas[,c(5:18)], 2, sum, na.rm=TRUE), FUN="/")
     name.areas <- 'undefined'
     if(sp %in% c("WHG", "SPR", "TUR", "DAB")){  # special case: west and east baltic merged
        if(all(c("IVa", "IVb", "IVc") %in% areas)) name.areas <- 'nsea'
        if(all(c("IIIan","IIIas") %in% areas)) name.areas <- 'kask'
        if(all(c("22", "23", "24") %in% areas)) name.areas <- '2232'
        if(all(c("25", "26", "27","28-1","28-2","29","30","31","32") %in% areas)) name.areas <- '2232'
     }else{
     if(sp %in% c("FLE")){  
        if(all(c("IVa", "IVb", "IVc") %in% areas)) name.areas <- 'nsea'
        if(all(c("IIIan","IIIas") %in% areas)) name.areas <- 'kask'
        if(all(c("22", "23") %in% areas)) name.areas <- '2223'
        if(all(c("24","25") %in% areas)) name.areas <- '2425'
        if(all(c("26", "28-1", "28-2") %in% areas)) name.areas <- '2628'
        if(all(c("27", "29","30","31","32") %in% areas)) name.areas <- '2732'    
     } else{
     if(sp %in% c("PLE")){  
        if(all(c("IVa", "IVb", "IVc", "IIIas") %in% areas)) name.areas <- 'nsea'
        if(all(c("IIIas", "22", "23") %in% areas)) name.areas <- '2123'
        if(all(c("24","25","26", "28-1", "28-2", "29","30","31","32") %in% areas)) name.areas <- '2432'
     } else{
       if(sp %in% c("SOL")){  
        if(all(c("IVa", "IVb", "IVc") %in% areas)) name.areas <- 'nsea'
        if(all(c("IIIas", "22", "23") %in% areas)) name.areas <- 'IIIa2223'
     } else{
        if(sp %in% c("COD", "HAD")){ # special case: skagerat with nsea, kat alone
        if(all(c("IVa", "IVb", "IVc","IIIan") %in% areas)) name.areas <- 'nsea'
        if(all(c("IIIas") %in% areas)) name.areas <- 'kat'
        if(all(c("22", "23", "24") %in% areas)) name.areas <- '2224'
        if(all(c("25", "26", "27","28-1","28-2","29","30","31","32") %in% areas)) name.areas <- '2532'
       }else{
      if(sp %in% c("HER")){  # special case: 3a with western baltic
        if(all(c("IVa", "IVb", "IVc") %in% areas)) name.areas <- 'nsea'
        if(all(c("IIIan") %in% areas)) name.areas <- 'IIIa22'
        if(all(c("22", "23", "24") %in% areas)) name.areas <- 'IIIa22'
        if(all(c("25", "26", "27","28-1","28-2","29","30","31","32") %in% areas)) name.areas <- '2532'
       } else{ # default
        if(all(c("IVa", "IVb", "IVc") %in% areas)) name.areas <- 'nsea'
        if(all(c("IIIan","IIIas") %in% areas)) name.areas <- 'kask'
        if(all(c("22", "23", "24") %in% areas)) name.areas <- '2224'
        if(all(c("25", "26", "27","28-1","28-2","29","30","31","32") %in% areas)) name.areas <- '2532'
      }
      }
      }
      }
      }
      }
     
     
     
     nm <- paste(sp,paste(name.areas ,collapse='.'),sep='.')
     lst.avai[[ nm ]][[S]] <- obj.in.areas
     # availability key: each col should sum to 1 i.e. for each age
return(lst.avai)
}

##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!MAIN!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

if(do_plot){
   library(maps)
   library(maptools)
   library(mapdata)
   map("worldHires", add=FALSE, col="green", fill=TRUE, bg="white",  xlim=c(-10,20) , ylim=c(49,63) , 
      regions=c('uk','ireland','france','germany','netherlands', 'norway','belgium',
      'spain','luxembourg','denmark', 'sweden','iceland', 'portugal','italy','sicily','ussr','sardinia','albania',
      'monaco','turkey','austria',
      'switzerland','czechoslovakia','finland','libya', 'hungary','yugoslavia',
      'poland','greece','romania','bulgaria', 'slovakia','morocco',
      'tunisia','algeria','egypt' ))
   library(maptools)
   ices_areas <- readShapeSpatial(file.path(general$inPathManagement, "ices_areas", "ices_areas"))
   plot(ices_areas, col="grey",  add=TRUE)
}


 ##-----------------##
 load(file.path(general$main_path_gis,  "POPULATIONS", "avai", paste("cpue_graph", general$igraph, ".RData",sep='')))
  cat(paste("Load survey files in /POPULATIONS/avai folder....done \n"))
   
 # load a graph
 coord <- read.table(file=file.path(general$main_path_gis, "GRAPH", paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
 coord <- as.matrix(as.vector(coord))
 coord <- matrix(coord, ncol=3)
 colnames(coord) <- c('x', 'y', 'dist')
 #plot(coord[,1], coord[,2])
 coord <- cbind(coord, 1:nrow(coord)) # keep track of the idx_node


 an <<- function(x) as.numeric(as.character(x))
 # calls
 # => assign the returned objects in .GlobalEnv
 # testexample
 spp                        <- unique(bits.cpue$Species)
 for (sp in as.character(spp)){ # per species
   for (S in 1:2){              # per semester
      if(sp %in% unique(bits.cpue$Species)){
       cat(paste("This will take some time to look at sp ", sp, "...\n"))
       assign(paste("bits.coord.",sp,".",S,sep=''),
            get_cpue_on_graph_nodes (obj=bits.cpue, coord=coord, sp=sp, S=S, survey="bits", general=general),  envir =.GlobalEnv )}                 
       assign(paste("coord.",sp,".",S,sep=''), get(paste("bits.coord.",sp,".",S,sep='')),  envir =.GlobalEnv  )          
       
   } # end S
 } # end sp 



# check with plot
if(do_plot){
   obj <- coord.SOL.1
   szgroup <-"nb_indiv.2"
   plot(x= obj[,1], y=obj[,2],  col=1, pch=16, 
                 cex=obj[,paste("SOL",".",szgroup,sep='')] /max(obj[,paste("SOL",".",szgroup,sep='')],na.rm=TRUE)*4)
   library(maps)
   map(add=TRUE)
   }
# => then, a spatial allocation key (i.e. the availability coeff) can be easily obtained from there... 
# CAUTION: per stock i.e. per region i.e. nscod, wcod, ecod...


## potential pitfall: the spatial coverage of the surveys is not perfect (even if 5 years is taken)
## and then potential nodes with 0 avai in some place (blank areas) while not relevant... 

# calls
lst.avai <- list() # init
for (sp in as.character(spp)){ # per species
  if(sp %in% c("WHG", "SPR", "TUR", "DAB")){
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas= c("IVa", "IVb", "IVc") ) # NS
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas= c("IVa", "IVb", "IVc") ) # NS
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas= c("IIIan","IIIas") ) # IIIa
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas= c("IIIan","IIIas") ) # IIIa)
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas= c("22", "23", "24","25", "26", "27","28-1","28-2","29","30","31","32") ) # 2232
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas= c("22", "23", "24","25", "26", "27","28-1","28-2","29","30","31","32") ) # 2232 
      }else{
       if(sp %in% c("PLE")){       
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas= c("IVa", "IVb", "IVc", "IIIan") ) # NS
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas= c("IVa", "IVb", "IVc", "IIIan") ) # NS
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas= c("IIIas", "22", "23")) # 2123
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas= c("IIIas", "22", "23")) # 2123
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas= c("24","25","26", "28-1", "28-2", "29","30","31","32") ) # 2432
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas= c("24","25","26", "28-1", "28-2", "29","30","31","32") ) # 2432       
       } else{
       if(sp %in% c("FLE")){
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas= c("IVa", "IVb", "IVc") ) # NS
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas= c("IVa", "IVb", "IVc") ) # NS
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas= c("IIIan","IIIas")) # kask
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas= c("IIIan","IIIas")) # kask
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas= c("22", "23") ) # 2223
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas= c("22", "23") ) # 2223       
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas= c("24","25") ) # 2425
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas= c("24","25") ) # 2425       
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas=  c("26", "28-1", "28-2") ) # 2628
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas=  c("26", "28-1", "28-2") ) # 2628       
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas=  c("27", "29","30","31","32") ) # 2732
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas= c("27", "29","30","31","32") ) # 2732       
      } else{
       if(sp %in% c("SOL")){
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas= c("IVa", "IVb", "IVc") ) # NS
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas= c("IVa", "IVb", "IVc") ) # NS
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas= c("IIIan","IIIas", "22", "23")) # IIIa2223
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas= c("IIIan","IIIas", "22", "23")) # IIIa2223      
       } else{
      if(sp %in% c("COD", "HAD")){
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas= c("IVa", "IVb", "IVc", "IIIan") ) # NS
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas= c("IVa", "IVb", "IVc", "IIIan") ) # NS
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas= c("IIIas") ) # NS
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas= c("IIIas") ) # NS
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas=  c("22", "23", "24") ) # BW
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas=  c("22", "23", "24") ) # BW
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas= c("25", "26", "27","28-1","28-2","29","30","31","32") ) # BE
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas= c("25", "26", "27","28-1","28-2","29","30","31","32") ) # BE
      } else{
       if(sp %in% c("HER")){
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas= c("IVa", "IVb", "IVc") ) # NS
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas= c("IVa", "IVb", "IVc") ) # NS
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas=  c("IIIan","IIIas","22", "23", "24") ) # BW
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas=  c("IIIan","IIIas","22", "23", "24") ) # BW
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas= c("25", "26", "27","28-1","28-2","29","30","31","32") ) # BE
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas= c("25", "26", "27","28-1","28-2","29","30","31","32") ) # BE
      }  else{
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas= c("IVa", "IVb", "IVc") ) # NS
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas= c("IVa", "IVb", "IVc") ) # NS
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas= c("IIIan","IIIas") ) # IIIa
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas= c("IIIan","IIIas") ) # IIIa)
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas=  c("22", "23", "24") ) # BW
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas=  c("22", "23", "24") ) # BW
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas= c("25", "26", "27","28-1","28-2","29","30","31","32") ) # BE
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas= c("25", "26", "27","28-1","28-2","29","30","31","32") ) # BE
      }
      }
      }
      }
      }
      }
} #=> NOTE THAT AVAI KEY IS RESTRICTED TO THE PROVIDED AREAS GIVEN THE SURVEY COVERAGE
  # EVEN IF VESSELS USE TO GO FARTHER THAN THAT...
  # SO VESSELS GOING FAR WILL LIKELY FISH ON AREAS WITHOUT ANY FISH BECAUSE AVAI WILL BE AT 0...
  # THIS IS WHY EXPLICIT POP BECOME IMPLICIT POP IF OUT OF RANGE (SEE THE C++ IBM CODE)
  
  #=> NOTE THAT POSSIBLE BIAS FROM VARIOUS NODE GRID RESOLUTION 
  # WHILE SOME STOCKS OVERALPPING SEVERAL REGIONS e.g. COD.NSEA including skagerrat 
  # SO THE RULE OF THUMB IS TO DEFINE THE SAME GRID RESOLUTION FOR THE AREA OF A GIVEN STUDIED STOCK....
  

 #=>  check: lapply(lst.avai,function(x) lapply(x, head))  and apply(lst.avai[[1]][[1]],2,sum,na.rm=TRUE) =>1,1,1,etc.

# check with plot
if(do_plot){
   obj <- lst.avai$COD.2224$"1"
   szgroup <-"nb_indiv.4"
   plot(x= obj[,1], y=obj[,2],  col=1, pch=16, 
                 cex=obj[,paste("COD",".",szgroup,sep='')] /max(obj[,paste("COD",".",szgroup,sep='')],na.rm=TRUE)*4)
}

# check with plot
if(do_plot){
   obj <- lst.avai$SOL.IIIa2223$"1"
   szgroup <-"nb_indiv.5"
   plot(x= obj[,1], y=obj[,2],  col=1, pch=16, 
                 cex=obj[,paste("SOL",".",szgroup,sep='')] /max(obj[,paste("SOL",".",szgroup,sep='')],na.rm=TRUE)*4)
}


# save to R
save("lst.avai", file = file.path(general$main_path_gis,  "POPULATIONS", "avai",
            paste("lst_avai_igraph", general$igraph,"_", general$method, "_", general$threshold, ".RData",sep="")) )
  cat(paste("Save lst.avai file in /POPULATIONS/avai folder....done \n"))

## CAUTION the ibts and bits surveys are combinations of hauls from different scientifc vessels
## with potentially gear trawl with different selectivity ogives....
### TO DO: NEED PELAGIC SURVEYS (ACOUSTIC?) TO COVER THE SPRAT AND HERRING
### THEN JUST INFORM THIS PIECE OF CODE WITH DATA HAVING THE SAME FORMAT LIKE THE IBTS ONE...
 
  cat(paste("....done \n"))



    