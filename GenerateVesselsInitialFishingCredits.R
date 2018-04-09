
# GENERAL SETTINGS

   args <- commandArgs(trailingOnly = TRUE)

   general <- list()

   if (length(args) < 2) {
     if(.Platform$OS.type == "windows") {
       general$application           <- "testexample" # ...or myfish
       general$main_path_gis         <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis", general$application)
       general$main.path.ibm         <- file.path("C:","Users","fbas","Documents","GitHub", paste("DISPLACE_input_", general$application, sep=''))
       general$main_path_R_inputs    <- file.path("C:","Users","fbas","Documents","GitHub", "DISPLACE_R_inputs")
       general$igraph                <- 56  # caution: should be consistent with existing objects already built upon a given graph
       do_plot                       <- TRUE
     }
  } else {
       general$application           <- args[1]
       general$main_path_gis         <- args[2]
       general$main.path.ibm         <- args[3]
       general$main_path_R_inputs    <- file.path("C:","Users","fbas","Documents","GitHub", "DISPLACE_R_inputs")
       general$igraph                <- args[4]  # caution: should be consistent with existing objects already built upon a given graph
       do_plot                       <- FALSE
  }
  
  
   cat(paste("START \n"))


  # mkdir
  dir.create(path=file.path(general$main.path, "merged_tables", general$case_study),
                      showWarnings = TRUE, recursive = TRUE, mode = "0777")


   
  #!#!#!#!#!#
  #!#!#!#!#!#
  # choose your country (and adapt below accordingly)
  ctry <- "DNK"
  year <- "2015"
  #!#!#!#!#!#
  #!#!#!#!#!#
cat(paste("Country chosen is",ctry,"; otherwise adapt the script. \n"))


 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##



getTacsatp<- function(what="weight", year="2015", ctry="DNK") {
 
  # a generic case
  if(what=="weight")  load(file=file.path(general$main_path_gis, "FISHERIES", paste("coupled_VMS_logbooks_",ctry,"_",year,".RData", sep=""))) # get the coupled_VMS_logbooks object
  if(what=="value")   load(file=file.path(general$main_path_gis, "FISHERIES", paste("coupled_VMS_logbooks_",ctry,"_",year,"_value.RData", sep="")))
  tacsatp            <- coupled_VMS_logbooks
  tacsatp            <- tacsatp[!is.na(as.numeric(as.character(tacsatp$SI_LONG))) &  !is.na(as.numeric(as.character(tacsatp$SI_LATI))), ]
  tacsatp$SI_LONG    <- as.numeric(as.character(tacsatp$SI_LONG))
  tacsatp$SI_LATI    <- as.numeric(as.character(tacsatp$SI_LATI))
  tacsatp$SI_STATE   <- as.numeric(as.character(tacsatp$SI_STATE))
  tacsatp$LE_EFF_VMS <- as.numeric(as.character(tacsatp$LE_EFF_VMS)) /60
  tacsatp            <- tacsatp[tacsatp$SI_STATE==1, ]  # keep fishing positions only
  tacsatp$ctry       <- ctry
 
 
  # e.g. a special case
  if(ctry=="SWE"){
   if(what=="weight") load(file=file.path(general$main_path_gis, paste("coupled_VMS_logbooks_",ctry,"_",year,".RData", sep="")))
   if(what=="value")  load(file=file.path(general$main_path_gis, paste("coupled_VMS_logbooks_",ctry,"_",year,"_value.RData", sep="")))
    tacsatp_swe            <- tacsat.swe
    tacsatp_swe$SI_LONG    <- as.numeric(as.character(tacsatp_swe$SI_LONG))
    tacsatp_swe$SI_LATI    <- as.numeric(as.character(tacsatp_swe$SI_LATI))
    tacsatp_swe$SI_STATE   <- as.numeric(as.character(tacsatp_swe$SI_STATE))
    tacsatp_swe$LE_MET_level6 <- tacsatp_swe$VE_MET  
    format_date            <- "%Y-%m-%d %H:%M:%S" 
    tacsatp_swe$SI_DATIM   <- as.POSIXct( tacsatp_swe$SI_DATIM, tz='GMT',   format_date)
    tacsatp_swe$LE_EFF_VMS <- abs(c(0, as.numeric( tacsatp_swe[-nrow( tacsatp_swe),"SI_DATIM"] - tacsatp_swe[-1,"SI_DATIM"], units="hours")))
    start.trip <- c(1,diff( tacsatp_swe[,"FT_REF"]))
    tacsatp_swe$all_effort <- tacsatp_swe$LE_EFF_VMS  # save...
    tacsatp_swe[start.trip!=0, "LE_EFF_VMS"] <- 0  # just correct for the trip change points
    tacsatp_swe$LE_EFF_VMS <- as.numeric(as.character(tacsatp_swe$LE_EFF_VMS))
    tacsatp_swe            <- tacsatp_swe[!is.na(as.numeric(as.character(tacsatp_swe$SI_LONG))) &  !is.na(as.numeric(as.character(tacsatp_swe$SI_LATI))), ]
    tacsatp_swe            <- tacsatp_swe[tacsatp_swe$SI_STATE==1, ]  # keep fishing positions only
    tacsatp_swe$ctry       <- "SWE"
    tacsatp                <- tacsatp_swe
   }
 
 
 return(tacsatp)
}

  # calls
  tacsatp       <- getTacsatp(what="weight", year=year, ctry=ctry)
  tacsatp_value <- getTacsatp(what="value", year=year, ctry=ctry)


 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!AGGREGATE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  c.listquote <- function (...)
   {
    args <- as.list(match.call()[-1])
    lstquote <- list(as.symbol("list"))
    for (i in args) {
        if (class(i) == "name" || (class(i) == "call" && i[[1]] !=
            "list")) {
            i <- eval(substitute(i), sys.frame(sys.parent()))
        }
        if (class(i) == "call" && i[[1]] == "list") {
            lstquote <- c(lstquote, as.list(i)[-1])
        }
        else if (class(i) == "character") {
            for (chr in i) {
                lstquote <- c(lstquote, list(parse(text = chr)[[1]]))
            }
        }
        else stop(paste("[", deparse(substitute(i)), "] Unknown class [",
            class(i), "] or is not a list()", sep = ""))
    }
    return(as.call(lstquote))
   }


 x <- tacsatp[tacsatp$SI_STATE==1,]  # ONLY USE THE FISHING GROUNDS
 

 # add quarters, and then semesters
 x$quarter <- quarters(as.POSIXct(x$SI_DATE, tz="GMT"))
 x$semester <- factor(x$quarter)
 levels(x$semester) <- c(1,1,2,2)

 # debug metier names
 levels(x$LE_MET_level6) <- gsub(">=", "o", levels(x$LE_MET_level6))  
 levels(x$LE_MET_level6) <- gsub("<",  "u", levels(x$LE_MET_level6))  
 levels(x$LE_MET_level6) <- gsub(">",  "o", levels(x$LE_MET_level6))  
 levels(x$LE_MET_level6) <- gsub("-",  "_", levels(x$LE_MET_level6))  
 
 # reuse the exported metier names in GenerateVesselConfigFiles.R
 metier_names  <- read.table( file=file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep=''), "metier_names.dat"), header=TRUE)


  
 # NEED JUST INTEGERS! for c++, 
 dd <- metier_names [match(levels(x$LE_MET_level6), as.character(metier_names[,2])), 1]
 dd <- replace(dd, is.na(dd),     metier_names[ metier_names[,2]=="other", 1])
 levels(x$LE_MET_level6) <- dd


 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## PLAN FOR GETTING CPUEs 
 

  ## FILES FOR BUILDING A IGRAPH
  coord <- read.table(file=file.path(general$main_path_gis,  "GRAPH", paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
  cat(paste("Loading the graph....done \n"))
  coord <- as.matrix(as.vector(coord))
  coord <- matrix(coord, ncol=3)
  colnames(coord) <- c('x', 'y', 'dist')
  #plot(coord[,1], coord[,2])

  coord <- cbind(coord, idx=1:nrow(coord)) # keep track of the idx_node

  # find the closest graph node
  library(spatstat)
  an <- function(x)  as.numeric(as.character(x))
  # for fishing pings
  x.fishing.ppp    <- ppp(x=an(x$SI_LONG), y=an(x$SI_LATI),
                         xrange=range(an(x$SI_LONG)), yrange=range(an(x$SI_LATI)))
  graph.ppp        <- ppp(x=coord[,"x"], y=coord[,"y"],
                            xrange=range(coord[,"x"]), yrange=range(coord[,"y"]))
  X                <- x.fishing.ppp #[sample(1:x.ppp$n)[1:10] ]
  Y                <- graph.ppp #[sample(1:graph.ppp$n)[1:10] ]
  N                <- nncross (X=X, Y=Y)$which # caution: just euclidean distance on coord
  # visual check
  if(FALSE){
      plot(superimpose(X=X, Y=Y), main="nncross", cols=c("red","blue"))
      arrows(X$x, X$y, Y[N]$x, Y[N]$y, length=0.15)
  }
  # add
  x <- cbind(x, pt_graph= coord[N, 'idx'])




  coord_pt_graph <- coord[x$pt_graph,] # replace coord of vms point by the coord of the graph node before finding out the stock area

 #x$SI_LONG <- as.numeric(as.character(coord_pt_graph[,'x']))
 #x$SI_LATI <- as.numeric(as.character(coord_pt_graph[,'y']))
 #x$area   <- ICESarea2(x, string=TRUE) # utils

 
 # find out areas
 source(file=file.path(general$main_path_R_inputs, "old", "vmstools_longlat_to_ICESareas.r"))
 x$x      <- as.numeric(as.character(x[,'SI_LONG']))
 x$y      <- as.numeric(as.character(x[,'SI_LATI']))
 x$area   <- longlat_to_ICESareas(x)
  
  
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!# 
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!# 
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!# 
 ##!!!!!!!!!!!!!!!!!!!!!!!AGGREGATE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!# 
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!# 
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!# 
  
  

 # aggregate weight and effort before computing cpue
 library(data.table)
 nm           <- names(x)
 idx.col.w    <- grep('KG', nm) # index columns with species weight
 idx.col.e    <- grep('EFF', nm) # index columns with species weight
 idx.col      <- c(idx.col.w,idx.col.e ) # index columns with species weight
 DT           <- data.table(x) # library data.table for fast grouping replacing aggregate()
 # AGGREGATE WEIGHT AND EFFORT PER SPECIES
 eq1              <- c.listquote( paste ("sum(",nm[idx.col],",na.rm=TRUE)",sep="") )
 x.agg2           <- DT[,eval(eq1),by=list(VE_REF, pt_graph, area)]
 x.agg2           <- data.frame( x.agg2)
 colnames(x.agg2) <- c("VE_REF",  "pt_graph", "area",nm[idx.col])

 
 ## CPUE COMPUTATION + RESHAPING (WIDE TO LONG FORMAT)
 nm                      <- names(x.agg2)
 idx.col.w               <- grep('KG', nm) # index columns with species weight
 # compute cpue kg per hour
 x.agg2[,idx.col.w]      <- x.agg2[,idx.col.w] / (x.agg2$LE_EFF_VMS/60)
 # remove no longer used col
 x.agg2                  <- x.agg2[, !colnames(x.agg2) %in% "LE_EFF_VMS"]
 # reshape
 x.agg2$id               <- paste(x.agg2$VE_REF,'.', x.agg2$pt_graph, '.', x.agg2$area, sep='')
 x.agg2.long             <- reshape(x.agg2, direction="long", ids="id",
                              times=nm[idx.col.w], timevar="species",
                               v.names="LE_KG_", varying=4:(ncol(x.agg2)-1))  # be patient....
 x.agg2.long             <- x.agg2.long[,c("VE_REF", "pt_graph", "area", "species", "LE_KG_")]
 rownames(x.agg2.long)   <- NULL
 colnames(x.agg2.long)   <- c("VE_REF",   "pt_graph", "area",  "species", "cpue_kghour")


 ####------
  # keep only the relevant stocks
  x.agg2.long$Species  <- gsub("LE_KG_", "",x.agg2.long$species)
 
 assignAStock <- function(x, area="area") {
   # convert Species in Stock name
  idx                <- x$Species %in% c("SPR", "PLE", "FLE", "TUR", "DAB") & x[,area] %in% c("IVa", "IVb", "IVc") 
  x[idx, "Stock" ] <- paste(x[idx,"Species"], 'nsea', sep=".")
  idx                <- x$Species %in% c("SPR", "PLE", "FLE", "TUR", "DAB") & x[,area] %in% c("IIIan","IIIas")
  x[idx, "Stock" ] <- paste(x[idx,"Species"], 'kask', sep=".")
  idx                <- x$Species %in% c("SPR", "PLE", "FLE", "TUR", "DAB") & x[,area] %in% c("IIIan","IIIas")
  x[idx, "Stock" ] <- paste(x[idx,"Species"], 'kask', sep=".")
  idx                <- x$Species %in% c("SPR", "PLE", "FLE", "TUR", "DAB") & x[,area] %in% c("22", "23", "24", "25", "26", "27","28-1","28-2","29","30","31","32")
  x[idx, "Stock" ] <- paste(x[idx,"Species"], '2232', sep=".")

  idx                <- x$Species %in% c("COD", "HAD") & x[,area] %in% c("IVa", "IVb", "IVc","IIIan")
  x[idx, "Stock" ] <- paste(x[idx,"Species"], 'nsea', sep=".")
  idx                <- x$Species %in% c("COD", "HAD") & x[,area] %in% c("IIIas")
  x[idx, "Stock" ] <- paste(x[idx,"Species"], 'kat', sep=".")
  idx                <- x$Species %in% c("COD", "HAD") & x[,area] %in% c("22", "23", "24")
  x[idx, "Stock" ] <- paste(x[idx,"Species"], '2224', sep=".")
  idx                <- x$Species %in% c("COD", "HAD") & x[,area] %in% c("25", "26", "27","28-1","28-2","29","30","31","32")
  x[idx, "Stock" ] <- paste(x[idx,"Species"], '2532', sep=".")

  idx                <- x$Species %in% c("HER") & x[,area] %in% c("IVa", "IVb", "IVc")
  x[idx, "Stock" ] <- paste(x[idx,"Species"], 'nsea', sep=".")
  idx                <- x$Species %in% c("HER") & x[,area] %in% c("IIIan")
  x[idx, "Stock" ] <- paste(x[idx,"Species"], '3a22', sep=".")
  idx                <- x$Species %in% c("HER") & x[,area] %in% c("22", "23", "24")
  x[idx, "Stock" ] <- paste(x[idx,"Species"], '3a22', sep=".")
  idx                <- x$Species %in% c("HER") & x[,area] %in% c("25", "26", "27","28-1","28-2","29","30","31","32")
  x[idx, "Stock" ] <- paste(x[idx,"Species"], '2532', sep=".")

  all_other_species  <- !x$Species %in% c("SPR", "PLE", "FLE", "TUR", "DAB", "COD", "HAD", "HER")
  idx                <- all_other_species & x[,area] %in% c("IVa", "IVb", "IVc")
  x[idx, "Stock" ] <- paste(x[idx,"Species"], 'nsea', sep=".")
  idx                <- all_other_species & x[,area] %in% c("IIIan","IIIas")
  x[idx, "Stock" ] <- paste(x[idx,"Species"], 'kask', sep=".")
  idx                <- all_other_species & x[,area] %in% c("22", "23", "24")
  x[idx, "Stock" ] <- paste(x[idx,"Species"], '2224', sep=".")
  idx                <- all_other_species & x[,area] %in% c("25", "26", "27","28-1","28-2","29","30","31","32")
  x[idx, "Stock" ] <- paste(x[idx,"Species"], '2532', sep=".")

  return(x)
  }
  # call
  x.agg2.long <- assignAStock(x=x.agg2.long)
 
 
  
  # subset for relevant populations
  spp_table <-  read.table(file=file.path(general$main_path_gis, "POPULATIONS",
                           paste("pop_names_", general$application,".txt",sep='')), header=TRUE)
  spp                        <- as.character(spp_table$spp)
  x.agg2.long$StockId <- factor(x.agg2.long$Stock) # init
  levels(x.agg2.long$StockId)  <- spp_table[match(levels(x.agg2.long$StockId), as.character(spp_table[,2])), 1]

  x.agg2.long <- x.agg2.long[!is.na(x.agg2.long$StockId),] # get rid of stocks not in this DISPLACE app
  x.agg2.long$StockId <- factor(x.agg2.long$StockId)
  



  x.agg2.long                 <- x.agg2.long[x.agg2.long$Stock %in% spp_table[,2],] # keep simulated stocks only
  x.agg2.long$Stock           <- factor(x.agg2.long$Stock)
  levels(x.agg2.long$Stock )  <- spp_table[,1][ match(levels(x.agg2.long$Stock), as.character(spp_table[,2]))] # map the name to integer
  x.agg2.long$mapped_stk_code <- as.numeric(as.character(x.agg2.long$Stock))
  library(doBy)
  x.agg2.long                 <- orderBy(~VE_REF, data=x.agg2.long) # library(doBy) # order from 0 to nbstock

  x.agg2.long                 <- x.agg2.long[,c("VE_REF",  "pt_graph", "mapped_stk_code", "cpue_kghour")]
  x.agg2.long                 <- x.agg2.long[!is.na(x.agg2.long$mapped_stk_code),] # remove NA stocks

 

  ## clean up  (e.g. inf and NaN comes from division by 0 when LE_EFF_VMS at 0 for some few cases....)
  x.agg2.long[is.na(x.agg2.long$cpue_kghour), "cpue_kghour"]       <- 0
  x.agg2.long[is.infinite(x.agg2.long$cpue_kghour), "cpue_kghour"] <- 0


  ## merge with all combi to get all pop informed even if cpue at 0.(required fro Cpp multimap)
  ## (tricky to get all combi because need to exclude pt_graph because no need to complete for all combi of nodes!)
  all.combi             <- x.agg2.long[!duplicated( x.agg2.long [,c('VE_REF','pt_graph')]), c('VE_REF','pt_graph')]
  all.combi             <- merge(all.combi,   spp_table[,1] , all=TRUE)
  colnames(all.combi)   <- c('VE_REF','pt_graph', 'mapped_stk_code')
  x.agg2.long           <- merge(all.combi, x.agg2.long , all=TRUE)
  x.agg2.long[is.na( x.agg2.long$cpue_kghour ), 'cpue_kghour' ]  <- 0   # replace NA cpue by 0

  ## order
  library(doBy)
  x.agg2.long <- orderBy(~VE_REF+pt_graph+mapped_stk_code , data=x.agg2.long)# order from 0 to nb of pops for the cpp multimap to be in the right order...
  
  
  
   ####-------
   ####-------
   ####-------
   ####-------
   # to obtain initial_fishing_credits_per_vid.dat based on cpues, 
   # subset for the explicit pop, aggregate and compute a share per vid
   #explicit_pops <- c(10,11)
   explicit_pops <- c(0,1)
   arbitrary_categories  <- c(-1, 0.1,0.5,1,2,5, 100000)  # i.e. nb of time the LPUE of reference
   corresponding_tariffs <- c(0.1, 0.5,1,2,5, 10)  # i.e. nb of time the LPUE of reference
   total_amount_credited <- 100000

   initial_fishing_credits_per_vid                         <- x.agg2.long [x.agg2.long$mapped_stk_code %in% explicit_pops, ]
   initial_fishing_credits_per_vid                         <- aggregate(initial_fishing_credits_per_vid$cpue_kghour, list(initial_fishing_credits_per_vid$VE_REF), mean, na.rm=TRUE)
   colnames(initial_fishing_credits_per_vid)               <- c("VE_REF", "cpue")
   quant                                                   <- quantile(initial_fishing_credits_per_vid$cpue[initial_fishing_credits_per_vid$cpue!=0])
   # need to bound in an interval to avoid outlier effect on cpue
   initial_fishing_credits_per_vid[initial_fishing_credits_per_vid$cpue<quant["25%"] & initial_fishing_credits_per_vid$cpue!=0, "cpue"] <- quant["25%"]
   initial_fishing_credits_per_vid[initial_fishing_credits_per_vid$cpue>quant["75%"] & initial_fishing_credits_per_vid$cpue!=0, "cpue"] <- quant["75%"]
   cpue_reference                                          <- mean(initial_fishing_credits_per_vid$cpue)
   initial_fishing_credits_per_vid$needs_for_credits       <- 1/    (initial_fishing_credits_per_vid$cpue /cpue_reference) # assume that you need less credit if your cpue is xx times higher than the average cpue
   initial_fishing_credits_per_vid[is.infinite(initial_fishing_credits_per_vid$needs_for_credits), "needs_for_credits"]       <- 0 # no need if the vessel is not targetting the explicit pops....
   initial_fishing_credits_per_vid$needs_for_credits_scaled_to_1   <-  initial_fishing_credits_per_vid$needs_for_credits/ sum( initial_fishing_credits_per_vid$needs_for_credits, na.rm=TRUE)
   initial_fishing_credits_per_vid$share_annual_fishing_credits_per_vid <-  initial_fishing_credits_per_vid$needs_for_credits_scaled_to_1 
 
   initial_fishing_credits_per_vid                         <- initial_fishing_credits_per_vid[, c("VE_REF", "share_annual_fishing_credits_per_vid")]
  
# save .dat files
       write.table(initial_fishing_credits_per_vid,
           file=file.path(general$main.path, paste("vesselsspe_",general$application, sep=""),
             paste("initial_share_fishing_credits_per_vid.dat",sep='')),
               col.names=TRUE,  row.names=FALSE, quote=FALSE, append=FALSE, sep = " ")  
  
  

   ####-------
   ####-------
   ####-------
   ####-------
   # to obtain initial_tariffs_on_nodes.dat based on cpues, 
   # subset for the explicit pop, remove the vid and the stock dimension,  and compute a standardized tariff to the mean LPUE per pt_graph
    
   initial_tariffs_on_nodes                         <- x.agg2.long [x.agg2.long$mapped_stk_code %in% explicit_pops, ]
   initial_tariffs_on_nodes                         <- aggregate(initial_tariffs_on_nodes$cpue_kghour, list(initial_tariffs_on_nodes$pt_graph), sum, na.rm=TRUE)
   colnames(initial_tariffs_on_nodes)               <- c("pt_graph", "cpue")
 
   # smooth
   quant                                                   <- quantile(initial_tariffs_on_nodes$cpue[initial_tariffs_on_nodes$cpue!=0])
   # need to bound in an interval to avoid outlier effect on cpue
   initial_tariffs_on_nodes[initial_tariffs_on_nodes$cpue<quant["25%"] & initial_tariffs_on_nodes$cpue!=0, "cpue"] <- quant["25%"]
   initial_tariffs_on_nodes[initial_tariffs_on_nodes$cpue>quant["75%"] & initial_tariffs_on_nodes$cpue!=0, "cpue"] <- quant["75%"]
 
   cpue_reference2                                          <- mean(initial_tariffs_on_nodes$cpue)
  
   initial_tariffs_on_nodes$tariff_per_day          <- initial_tariffs_on_nodes$cpue / cpue_reference2
   initial_tariffs_on_nodes$tariff_per_day          <- cut (initial_tariffs_on_nodes$tariff_per_day , breaks= arbitrary_categories) 
   any(is.na(  initial_tariffs_on_nodes$tariff_per_day  )) # check
   levels(initial_tariffs_on_nodes$tariff_per_day)  <- corresponding_tariffs
   initial_tariffs_on_nodes                         <- initial_tariffs_on_nodes[, c("pt_graph", "tariff_per_day")]
  
   initial_tariffs_on_nodes$pt_graph  <-  initial_tariffs_on_nodes$pt_graph - 1 ##!!! OFFSET FOR C++ !!!##
 
# save .dat files
       write.table(initial_tariffs_on_nodes,
           file=file.path(general$main.path, "graphsspe",
             paste("initial_tariffs_on_nodes_a_graph",general$igraph,".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, quote=FALSE, append=FALSE, sep = " ")  
 
  
    




