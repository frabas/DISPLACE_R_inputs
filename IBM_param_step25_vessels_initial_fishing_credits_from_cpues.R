
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
  # mkdir
  dir.create(path=file.path(general$main.path, "merged_tables", general$case_study),
                      showWarnings = TRUE, recursive = TRUE, mode = "0777")




 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
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


 # load the combined graph with the "merged" table
 load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.fgrounds.",general$a.country,".",general$a.year,".igraph",
                general$igraph,".RData",sep='')))
 

 x <- ping.fgrounds  # ONLY USE THE FGROUNDS

 # add quarters, and then semesters
 x$quarter <- quarters(as.POSIXct(x$SI_DATE, tz="GMT"))
 x$semester <- factor(x$quarter)
 levels(x$semester) <- c(1,1,2,2)

 #...and slightly modify the name of metiers
 x$LE_MET_level6 <- factor(x$LE_MET_level6)
 x$LE_MET_level6 <- as.factor(unlist(lapply(strsplit( as.character(x$LE_MET_level6), split="_"),
                                  function(x) paste(x[1],'_',x[2],sep=''))))   # remove mesh size and useless selective grid info

 
 # get metier_names
 load(file.path(general$main.path,"merged_tables", general$case_study, paste("metier_names.", general$a.country,".", general$a.year,".igraph",general$igraph,".RData",sep='')))
  
 # NEED JUST INTEGERS! for c++, 
 levels(x$LE_MET_level6) <- metier_names [match(levels(x$LE_MET_level6), metier_names[,1]), 2]



 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## PLAN FOR GETTING CPUEs 
 
 #load(file.path(general$main.path, "igraph", paste(general$igraph, "_graphibm.RData",sep=''))) # built from the R code
 coord <- read.table(file=file.path(general$main_path_input, "graphsspe", paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
 coord <- as.matrix(as.vector(coord))
 coord <- matrix(coord, ncol=3)
 colnames(coord) <- c('x', 'y', 'idx.port')
 #plot(coord[,1], coord[,2])

  coord_pt_graph <- coord[x$pt_graph,] # replace coord of vms point by the coord of the graph node before finding out the stock area

 #x$SI_LONG <- as.numeric(as.character(coord_pt_graph[,'x']))
 #x$SI_LATI <- as.numeric(as.character(coord_pt_graph[,'y']))
 #x$area   <- ICESarea2(x, string=TRUE) # utils

 # find out areas
 source(file=file.path(general$main.path.code,"IBM_param_utils_longlat_to_ICESareas.r"))
 x$x      <- as.numeric(as.character(coord_pt_graph[,'x']))
 x$y      <- as.numeric(as.character(coord_pt_graph[,'y']))
 x$area   <- longlat_to_ICESareas(x)
 x[is.na(x$area) | !(x$area %in% c('22', '23', '24', '25', '26', '27', '28-1', '28-2', '29', '30', '31', '32',
                         'IIIan', 'IIIas') ), 'area'] <- 'nsea' # if out of range, assign all these nodes to North Sea stocks...
 x[(x$area %in% c('22', '23', '24')), 'area'] <- '2224'
 x[(x$area %in% c('25', '26', '27', '28-1', '28-2', '29','30', '31', '32')), 'area'] <- '2532'

  ## check
  x[x$VE_REF=="DNK000005491" & x$LE_KG_SPR!=0 & !is.na(x$LE_KG_SPR), ]
  x[x$VE_REF=="DNK000006040" & x$LE_KG_COD!=0 & !is.na(x$LE_KG_COD), ]
  
  
  

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

 # check
 x.agg2[x.agg2$VE_REF=="DNK000041932" ,]

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
  x.agg2.long$stock  <- paste( gsub("LE_KG_", "",x.agg2.long$species), ".", x.agg2.long$area, sep='')
  # correct names for special cases (those across management areas)
  x.agg2.long[x.agg2.long$stock %in% c("COD.IIIan"), "stock"] <- 'COD.nsea'
  x.agg2.long[x.agg2.long$stock %in% c("COD.IIIas"), "stock"] <- 'COD.kat'
  x.agg2.long[x.agg2.long$stock %in% c("HAD.IIIan"), "stock"] <- 'HAD.nsea'
  x.agg2.long[x.agg2.long$stock %in% c("HER.IIIan", "HER.IIIas", "HER.2224"), "stock"] <- 'HER.3a22'
  x.agg2.long[x.agg2.long$stock %in% c("SPR.2224", "SPR.2532"), "stock"] <- 'SPR.2232'
  x.agg2.long[x.agg2.long$stock %in% c("PLE.2224", "PLE.2532"), "stock"] <- 'PLE.2232'
  x.agg2.long[x.agg2.long$stock %in% c("FLE.2224", "FLE.2532"), "stock"] <- 'FLE.2232'
  x.agg2.long[x.agg2.long$stock %in% c("TUR.2224", "TUR.2532"), "stock"] <- 'TUR.2232'
  x.agg2.long[x.agg2.long$stock %in% c("DAB.2224", "DAB.2532"), "stock"] <- 'DAB.2232'
  x.agg2.long[grep('IIIa', x.agg2.long$stock), "stock"] <- # for all other species, correct kask
     paste( gsub("LE_KG_", "",x.agg2.long[grep('IIIa', x.agg2.long$stock),'species']), ".kask", sep='')

  # subset for relevant populations
  pop_names                   <- read.table(file.path(general$main.path, "popsspe",paste("pop_names_",general$case_study,".txt", sep='')))
  x.agg2.long                 <- x.agg2.long[x.agg2.long$stock %in% pop_names[,1],] # keep simulated stocks only
  x.agg2.long$stock           <- factor(x.agg2.long$stock)
  levels(x.agg2.long$stock )  <- pop_names[,2][ match(levels(x.agg2.long$stock), as.character(pop_names[,1]))] # map the name to integer
  x.agg2.long$mapped_stk_code <- as.numeric(as.character(x.agg2.long$stock))
  x.agg2.long                 <- orderBy(~VE_REF, data=x.agg2.long) # library(doBy) # order from 0 to nbstock

  x.agg2.long                 <- x.agg2.long[,c("VE_REF",  "pt_graph", "mapped_stk_code", "cpue_kghour")]
  x.agg2.long                 <- x.agg2.long[!is.na(x.agg2.long$mapped_stk_code),] # remove NA stocks

  ## check
  dd<-x.agg2.long[x.agg2.long$VE_REF=="DNK000039090" & x.agg2.long$mapped_stk_code=="1","cpue_kghour"]
  dd<-x.agg2.long[x.agg2.long$VE_REF=="DNK000007161" ,"cpue_kghour"]


  ## clean up  (e.g. inf and NaN comes from division by 0 when LE_EFF_VMS at 0 for some few cases....)
  x.agg2.long[is.na(x.agg2.long$cpue_kghour), "cpue_kghour"]       <- 0
  x.agg2.long[is.infinite(x.agg2.long$cpue_kghour), "cpue_kghour"] <- 0


  ## merge with all combi to get all pop informed even if cpue at 0.(required fro Cpp multimap)
  ## (tricky to get all combi because need to exclude pt_graph because no need to complete for all combi of nodes!)
  all.combi             <- x.agg2.long[!duplicated( x.agg2.long [,c('VE_REF','pt_graph')]), c('VE_REF','pt_graph')]
  all.combi             <- merge(all.combi,   pop_names[,2] , all=TRUE)
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
   explicit_pops <- c(10,11)
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
           file=file.path(general$main.path, "vesselsspe",
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
 
  
    




