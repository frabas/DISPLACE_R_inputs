
## IBM parametrisation
## Francois Bastardie (DTU-Aqua)
## outputs: mainly .dat files to be used for the IBM simulations

 # GENERAL SETTINGS
  general <- list()
  general$main.path      <- file.path("C:", "Users", "fbas", "Documents", "GitHub", "DISPLACE_input_raw")
  general$main.path.code <- file.path("C:", "Users", "fbas", "Documents", "GitHub", "DISPLACE_R_inputs")

  general$igraph                <- 11
  general$case_study            <- "baltic_only"
  general$case_study_countries  <- c("DEN", "SWE", "DEU")    # for the Baltic only
  general$a.year                <- "2012"
  general$a.country             <- "DEN"
  #general$a.country             <- "DEU"
  #general$a.country             <- "SWE"

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
 load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.harbours.",general$a.country,".",general$a.year,".igraph",
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

 metier_names <- cbind(levels(x$LE_MET_level6), 0:(length(levels(x$LE_MET_level6))-1))
 save(metier_names,  file=file.path(general$main.path, "merged_tables", general$case_study, ## SAVE
                     paste("metier_names.",general$a.country,".",general$a.year,".igraph",
                      general$igraph,".RData", sep='')))
 write.table(metier_names, file.path(general$main.path, "metiersspe",paste("metier_names_",general$a.country,"_",general$a.year,".txt",sep='')),
               quote=FALSE, col.names=FALSE, row.names=FALSE)

 #levels(x$LE_MET_level6) <-
 #   paste("met", 0:(length(levels(x$LE_MET_level6))-1), "_", levels(x$LE_MET_level6), "_",general$a.country, sep='')
 # NEED JUST INTEGERS! for c++, replaced by:
 levels(x$LE_MET_level6) <- 0:(length(levels(x$LE_MET_level6))-1)

 # then, aggregate...
 nm           <- names(x)
 idx.col.w    <- grep('KG', nm) # index columns with species weight
 idx.col.e    <- grep('LE_EFF_VMS', nm)
 x$LE_EFF_VMS <- as.numeric(as.character(x$LE_EFF_VMS))
 idx.col      <- c(idx.col.w, idx.col.e)

 library(data.table)
 DT  <- data.table(x) # library data.table for fast grouping replacing aggregate()
 # AGGREGATE WEIGHT AND EFFORT PER SPECIES
 eq1             <- c.listquote( paste ("sum(",nm[idx.col],",na.rm=TRUE)",sep="") )
 DT$pt_graph     <- as.integer(DT$pt_graph)
 DT$VE_REF       <- as.factor(DT$VE_REF)
 x.agg           <- DT[,eval(eq1),by=list(VE_REF, LE_MET_level6, semester, pt_graph)]
 x.agg           <- data.frame( x.agg)
 colnames(x.agg) <- c("VE_REF", "LE_MET_level6", "semester", "pt_graph", nm[idx.col])


  # save
  save(x.agg,  file=file.path(general$main.path, "merged_tables", general$case_study,
             paste("x.agg.",general$a.country,".",general$a.year,".igraph",
              general$igraph,".RData", sep='')))


 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## PLAN B FOR GETTING CPUEs (i.e. not from parameterizedv relationship but from average instead):
 ## second way: get the vessel-specific cpue per node per species
 ## (to be used as in Bastardie et al 2010)
 ## will be used for stocks for which we do not have N_pops because not assessed by ICES
 ## => implicit populations vs. explicit ones
 load(file.path(general$main.path, "igraph", paste(general$igraph, "_graphibm.RData",sep=''))) # built from the R code
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
 if(FRANCOIS){ x[x$VE_REF=="DNK000005491" & x$LE_KG_SPR!=0 & !is.na(x$LE_KG_SPR), ]
  x[x$VE_REF=="DNK000006040" & x$LE_KG_COD!=0 & !is.na(x$LE_KG_COD), ]
  }
  
  
  
  
  ####### PLAN A ##########
  ####### PLAN A ##########
  ####### PLAN A ##########
  ### THE CPUE ARE LIKELY TO BE UNDERESTIMATED BECAUSE AN AVERAGE ON NODES...
  ## (in addition the cpues here are not accounting for possible underlying metiers.....)
  ## PRELIMINARY RESULTS SHOWED THAT INDEED THIS DOES NOT SEEM ENOUGH TO
  ## DESCRIBE THE SHAPE OF THE DISTRIBUTION OF CPUEs (i.e. skewed with long tail)...
  ## SO IT COULD BE BETTER TO FIT A GAMMA DISTRIBUTION INSTEAD (ON EACH NODE)
  nm            <- names(x)
  idx.col.w     <- grep('KG', nm) # index columns with species weight
  tmp           <- x[,idx.col.w]
  colnames(tmp) <- gsub('KG', 'cpue', colnames(tmp))
  x             <- cbind (x, tmp / (x$LE_EFF_VMS/60)) # kghour
  gamma.nll     <- function(par,data) -sum(dgamma(data,shape=par[1],scale=par[2],log=T))
  cpues         <- list(NULL)
  filen         <- file.path(general$main.path, "merged_tables",  general$case_study,
                              paste("cpues_gamma.", general$a.country, ".", general$a.year, ".igraph",
                               general$igraph,".txt", sep=''))
  write.table("", append=FALSE, file=filen, row.names=FALSE, col.names=FALSE, quote=FALSE)
  for (sp in colnames(tmp)){
     d <- x[, c('VE_REF','quarter','pt_graph','area',sp)]
     cat(paste("\n"))
     cat(paste(sp,"\n"))
     for(vid in unique(d$VE_REF)){
        cat(paste("."))
        for (a.quarter in c("Q1","Q2","Q3","Q4")){
           dd <- d[d$VE_REF==vid & d$quarter== a.quarter, ]
           for (a.pt_graph in unique(dd$pt_graph)){
            ddd   <- dd[dd$pt_graph== a.pt_graph,]
            dddd  <- ddd[, sp]
            area  <- ddd[1,"area"]
            dddd  <- dddd[dddd>1]
            if(length(dddd)!=0){
              er <-  try(opt <- optim(c(1,1),gamma.nll,data=dddd,method='BFGS') , silent=TRUE)
             if(class(er)=="try-error"){
                opt <- list(par=c(1,mean(dddd)))  # best guess
             }
               # check
         #      par(mfrow=c(1,2))
         #      hist(dddd)
         #      hist(rgamma(100, shape=opt$par[1], scale = opt$par[2]) )
         # browser()
            }   else{opt <- list(par=c(0,0))}
           # save (line by line to avoid out of memory)
           write.table(data.frame(VE_REF=vid, quarter=a.quarter, pt_graph= a.pt_graph, area= area, stock= sp,
                                   shape=round(opt$par[1],4), scale = round(opt$par[2],4)),
                        file = filen, append = TRUE, quote = FALSE, sep = " ",
                         row.names=FALSE, col.names=FALSE)
   }}} } # be (really) patient.....

 ## Then,
 ## R:   produce two .dat files, one for each gamma parameter
 ## C++: draw cpue on nodes from gamma distribution
  cpues_gamma           <- read.table(filen, sep=" ")
  colnames(cpues_gamma) <- c('VE_REF','quarter','pt_graph','area','species','shape','scale')
   ####------
  # keep only the relevant stocks
  # (not creating any duplicates here because pt_graph is unique....)
  cpues_gamma$stock <- paste( gsub("LE_cpue_", "", cpues_gamma$species), ".", cpues_gamma$area, sep='')
  # correct names for special cases (those across management areas)
  cpues_gamma[cpues_gamma$stock %in% c("COD.IIIan"), "stock"] <- 'COD.nsea'
  cpues_gamma[cpues_gamma$stock %in% c("COD.IIIas"), "stock"] <- 'COD.kat'
  cpues_gamma[cpues_gamma$stock %in% c("HAD.IIIan"), "stock"] <- 'HAD.nsea'
  cpues_gamma[cpues_gamma$stock %in% c("HER.IIIan", "HER.IIIas", "HER.2224"), "stock"] <- 'HER.3a22'
  cpues_gamma[cpues_gamma$stock %in% c("SPR.2224", "SPR.2532"), "stock"] <- 'SPR.2232'
  cpues_gamma[cpues_gamma$stock %in% c("PLE.2224", "PLE.2532"), "stock"] <- 'PLE.2232'
  cpues_gamma[cpues_gamma$stock %in% c("FLE.2224", "FLE.2532"), "stock"] <- 'FLE.2232'
  cpues_gamma[cpues_gamma$stock %in% c("TUR.2224", "TUR.2532"), "stock"] <- 'TUR.2232'
  cpues_gamma[cpues_gamma$stock %in% c("DAB.2224", "DAB.2532"), "stock"] <- 'DAB.2232'
  cpues_gamma[grep('IIIa', cpues_gamma$stock), "stock"] <- # for all other species, correct kask
     paste( gsub("LE_cpue_", "",cpues_gamma[grep('IIIa', cpues_gamma$stock),'species']), ".kask", sep='')

  # subset for relevant populations
  pop_names                   <- read.table(file.path(general$main.path, "popsspe",paste("pop_names_",general$case_study,".txt", sep='')))  ## CAUTION: circularity for pop_names
  cpues_gamma                 <- cpues_gamma[cpues_gamma$stock %in% pop_names[,1],] # keep simulated stocks only
  cpues_gamma$stock           <- factor(cpues_gamma$stock)
  levels(cpues_gamma$stock )  <- pop_names[,2][ match(levels(cpues_gamma$stock), as.character(pop_names[,1]))] # map the name to integer
  cpues_gamma$mapped_stk_code <- as.numeric(as.character(cpues_gamma$stock))
  library(doBy)
  cpues_gamma                 <- orderBy(~VE_REF, data=cpues_gamma) # library(doBy) # order from 0 to nbstock
  cpues_gamma                 <- cpues_gamma[,c("VE_REF",  "quarter", "pt_graph", "mapped_stk_code", "shape", "scale")]

  ## merge with all combi to get all pop informed even if cpue at 0.(required fro Cpp multimap)
  ## (tricky to get all combi because need to exclude pt_graph because no need to complete for all combi of nodes!)
  all.combi           <- cpues_gamma [!duplicated(cpues_gamma  [,c('VE_REF','quarter','pt_graph')]), c('VE_REF','quarter','pt_graph')]
  all.combi           <- merge(all.combi,  pop_names[,2],  all=TRUE)
  colnames(all.combi) <- c('VE_REF','quarter','pt_graph', 'mapped_stk_code')
  cpues_gamma         <- merge(all.combi, cpues_gamma , all=TRUE)
  cpues_gamma[is.na( cpues_gamma$shape ), 'shape' ]  <- 0   # replace NA  by 0
  cpues_gamma[is.na( cpues_gamma$scale ), 'scale' ]  <- 0   # replace NA  by 0


  ## order
  library(doBy)
  cpues_gamma <- orderBy(~VE_REF+quarter+pt_graph+mapped_stk_code , data=cpues_gamma)# order from 0 to nb of pops for the cpp multimap to be in the right order...

   ####-------
  # save .dat files
   for (a.quarter in c("Q1","Q2","Q3","Q4")){
      cpues_gamma.Q          <- cpues_gamma[cpues_gamma$quarter==a.quarter,]
      cpues_gamma.Q$VE_REF   <- factor( cpues_gamma.Q$VE_REF )
      cpues_gamma.Q$pt_graph <- cpues_gamma.Q$pt_graph - 1 ##!!! OFFSET FOR C++ !!!##

      for(vid in unique(cpues_gamma$VE_REF)){
        cpues_gamma.Q.vid                              <- cpues_gamma.Q[cpues_gamma.Q$VE_REF==vid,]
        vesselspe_gshape_cpue_per_stk_on_nodes_quarter <- cpues_gamma.Q.vid[,c('pt_graph', 'shape')]
        vesselspe_gscale_cpue_per_stk_on_nodes_quarter <- cpues_gamma.Q.vid[,c('pt_graph', 'scale')]
        # vessel spe .dat file
        write.table(round(vesselspe_gshape_cpue_per_stk_on_nodes_quarter, 2),
          file=file.path(general$main.path, "vesselsspe",
           paste(vid, "_gshape_cpue_per_stk_on_nodes_quarter",gsub("Q","",a.quarter),".dat",sep='')),
            col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)
        # vessel spe .dat file
        write.table(round(vesselspe_gscale_cpue_per_stk_on_nodes_quarter),
          file=file.path(general$main.path, "vesselsspe",
           paste(vid, "_gscale_cpue_per_stk_on_nodes_quarter",gsub("Q","",a.quarter),".dat",sep='')),
            col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)
      }
   }

   ## check
   cpues_gamma[cpues_gamma$VE_REF=="DNK000006040" & cpues_gamma$mapped_stk_code=="11", ]
   cpues_gamma[cpues_gamma$VE_REF=="DNK000007161", ]




  ####### PLAN B ##########
  ####### PLAN B ##########
  ####### PLAN B ##########
 # the 'average' way: aggregate weight and effort before computing cpue
 library(data.table)
 nm           <- names(x)
 idx.col.w    <- grep('KG', nm) # index columns with species weight
 idx.col.e    <- grep('EFF', nm) # index columns with species weight
 idx.col      <- c(idx.col.w,idx.col.e ) # index columns with species weight
 DT           <- data.table(x) # library data.table for fast grouping replacing aggregate()
 # AGGREGATE WEIGHT AND EFFORT PER SPECIES
 eq1              <- c.listquote( paste ("sum(",nm[idx.col],",na.rm=TRUE)",sep="") )
 x.agg2           <- DT[,eval(eq1),by=list(VE_REF, quarter, pt_graph, area)]
 x.agg2           <- data.frame( x.agg2)
 colnames(x.agg2) <- c("VE_REF",  "quarter", "pt_graph","area",nm[idx.col])

 # check
 x.agg2[x.agg2$VE_REF=="DNK000007161" ,]

 ## CPUE COMPUTATION + RESHAPING (WIDE TO LONG FORMAT)
 nm                      <- names(x.agg2)
 idx.col.w               <- grep('KG', nm) # index columns with species weight
 # compute cpue kg per hour
 x.agg2[,idx.col.w]      <- x.agg2[,idx.col.w] / (x.agg2$LE_EFF_VMS/60)
 # remove no longer used col
 x.agg2                  <- x.agg2[, !colnames(x.agg2) %in% "LE_EFF_VMS"]
 # reshape
 x.agg2$id               <- paste(x.agg2$VE_REF, '.', x.agg2$quarter, '.', x.agg2$pt_graph, '.', x.agg2$area, sep='')
 x.agg2.long             <- reshape(x.agg2, direction="long", ids="id",
                              times=nm[idx.col.w], timevar="species",
                               v.names="LE_KG_", varying=5:(ncol(x.agg2)-1))  # be patient....
 x.agg2.long             <- x.agg2.long[,c("VE_REF",  "quarter", "pt_graph", "area", "species", "LE_KG_")]
 rownames(x.agg2.long)   <- NULL
 colnames(x.agg2.long)   <- c("VE_REF",  "quarter", "pt_graph", "area", "species", "cpue_kghour")


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

  x.agg2.long                 <- x.agg2.long[,c("VE_REF",  "quarter", "pt_graph", "mapped_stk_code", "cpue_kghour")]
  x.agg2.long                 <- x.agg2.long[!is.na(x.agg2.long$mapped_stk_code),] # remove NA stocks

  ## check
  dd<-x.agg2.long[x.agg2.long$VE_REF=="DNK000039090" & x.agg2.long$mapped_stk_code=="1","cpue_kghour"]
  dd<-x.agg2.long[x.agg2.long$VE_REF=="DNK000007161" ,"cpue_kghour"]


  ## clean up  (e.g. inf and NaN comes from division by 0 when LE_EFF_VMS at 0 for some few cases....)
  x.agg2.long[is.na(x.agg2.long$cpue_kghour), "cpue_kghour"]       <- 0
  x.agg2.long[is.infinite(x.agg2.long$cpue_kghour), "cpue_kghour"] <- 0


  ## merge with all combi to get all pop informed even if cpue at 0.(required fro Cpp multimap)
  ## (tricky to get all combi because need to exclude pt_graph because no need to complete for all combi of nodes!)
  all.combi             <- x.agg2.long[!duplicated( x.agg2.long [,c('VE_REF','quarter','pt_graph')]), c('VE_REF','quarter','pt_graph')]
  all.combi             <- merge(all.combi,   pop_names[,2] , all=TRUE)
  colnames(all.combi)   <- c('VE_REF','quarter','pt_graph', 'mapped_stk_code')
  x.agg2.long           <- merge(all.combi, x.agg2.long , all=TRUE)
  x.agg2.long[is.na( x.agg2.long$cpue_kghour ), 'cpue_kghour' ]  <- 0   # replace NA cpue by 0

  ## order
  library(doBy)
  x.agg2.long <- orderBy(~VE_REF+quarter+pt_graph+mapped_stk_code , data=x.agg2.long)# order from 0 to nb of pops for the cpp multimap to be in the right order...

   ####-------
   ####-------
   ####-------
   ####-------
  # save .dat files
   for (a.quarter in c("Q1","Q2","Q3","Q4")){
      x.agg.Q           <- x.agg2.long[x.agg2.long$quarter==a.quarter,]
      x.agg.Q$VE_REF    <- factor( x.agg.Q$VE_REF )
      x.agg.Q$pt_graph  <-  x.agg.Q$pt_graph - 1 ##!!! OFFSET FOR C++ !!!##

      for(vid in unique(x.agg2.long$VE_REF)){
        x.agg.Q.vid <- x.agg.Q[x.agg.Q$VE_REF==vid,]
        vesselspe_cpue_per_stk_on_nodes_quarter <- x.agg.Q.vid[,c('pt_graph', 'cpue_kghour')]
        # vessel spe .dat file
        write.table(round(vesselspe_cpue_per_stk_on_nodes_quarter),
          file=file.path(general$main.path, "vesselsspe",
           paste(vid, "_cpue_per_stk_on_nodes_quarter",gsub("Q","",a.quarter),".dat",sep='')),
            col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)
      }
   }




