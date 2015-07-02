 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!LOAD & COMPUTE FOR OBTAINING!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## vesselsspe_percent_tacs_quarter[xx].dat      !!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

 ## POPSPE
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
 general$case_study_countries  <- c("DEN")    
 general$a.year                <- "2012"
 general$a.country             <- "DEN"

 
 
 
 load(file.path(general$main.path,"merged_tables", general$case_study,
           paste("all_merged_weight_",general$a.country,"_",general$a.year,".RData",sep='')))
  x <- all.merged ; rm(all.merged); gc(reset=TRUE)

  # debug to get consistency between c++ list of vesselids and c++ [DNK000]_possible_metiers files
  # because possible vessels have been removed earlier e.g. those targetting OYE or MUS exclusively...
  load(file.path (general$main.path,"merged_tables", general$case_study,
                   paste("ping.fgrounds.",general$a.country ,".",general$a.year,".igraph",general$igraph,".RData", sep="")))
  
 
  load(file.path(general$main.path, "popsspe", paste("betas",general$case_study,"_INTEGER_",general$case_study,".RData", sep='')) )
  nrow(x)
  ping.fgrounds                <- ping.fgrounds[ping.fgrounds$VE_REF %in% unique(all.betas.vid$VE_REF),]
  ping.fgrounds$VE_REF         <- factor(ping.fgrounds$VE_REF)
  vessels.still.there          <- unique(as.character(ping.fgrounds$VE_REF))
  x                            <- x[x$VE_REF %in% vessels.still.there,]
  nrow(x)



  # find out areas
  source(file=file.path(general$main.path.code,"IBM_param_utils_longlat_to_ICESareas.r"))
  x$x      <- as.numeric(as.character(x$SI_LONG))
  x$y      <- as.numeric(as.character(x$SI_LATI))
  x$area   <- longlat_to_ICESareas(x)
  x[is.na(x$area) | !(x$area %in% c('22', '23', '24', '25', '26', '27', '28-1', '28-2', '29', '30', '31', '32',
                         'IIIan', 'IIIas') ), 'area'] <- 'nsea' # if out of range, assign all these nodes to North Sea stocks...
  x[(x$area %in% c('22', '23', '24')), 'area'] <- '2224'
  x[(x$area %in% c('25', '26', '27', '28-1', '28-2', '29','30', '31', '32')), 'area'] <- '2532'


  # find total land per species & reshape
  totland            <- aggregate(x[,grep("LE_KG", colnames(x))], list(x$area), sum, na.rm=TRUE)
  totland_long       <- reshape(totland, direction="long",
                              times=colnames(totland)[grep("LE_KG", colnames(totland))], timevar="species",
                               v.names="LE_KG_tot", varying=2:(ncol(totland)))  # be patient....
  totland_long$stock <- paste( gsub("LE_KG_", "", totland_long$species), ".", totland_long$Group.1, sep='')

  # correct names for special cases (those across management areas)
  totland_long[totland_long$stock %in% c("COD.IIIan"), "stock"] <- 'COD.nsea'
  totland_long[totland_long$stock %in% c("COD.IIIas"), "stock"] <- 'COD.kat'
  totland_long[totland_long$stock %in% c("HAD.IIIan"), "stock"] <- 'HAD.nsea'
  totland_long[totland_long$stock %in% c("HER.IIIan", "HER.IIIas", "HER.2224"), "stock"] <- 'HER.3a22'
  totland_long[totland_long$stock %in% c("SPR.2224", "SPR.2532"), "stock"] <- 'SPR.2232'
  totland_long[totland_long$stock %in% c("PLE.2224", "PLE.2532"), "stock"] <- 'PLE.2232'
  totland_long[totland_long$stock %in% c("FLE.2224", "FLE.2532"), "stock"] <- 'FLE.2232'
  totland_long[totland_long$stock %in% c("TUR.2224", "TUR.2532"), "stock"] <- 'TUR.2232'
  totland_long[totland_long$stock %in% c("DAB.2224", "DAB.2532"), "stock"] <- 'DAB.2232'
  totland_long[grep('IIIa', totland_long$stock), "stock"] <- # for all other species, correct kask
     paste( gsub("LE_KG_", "",totland_long[grep('IIIa', totland_long$stock),'species']), ".kask", sep='')

  # then aggregate again for correctness
  totland_agg           <- aggregate(totland_long$LE_KG_tot, list(totland_long$stock), sum, na.rm=TRUE)
  colnames(totland_agg) <- c("stock", "totland")

  if(general$a.country=="DEN")  totland_agg_den <- cbind.data.frame(totland_agg, ctry="DNK")
  if(general$a.country=="DEU")  totland_agg_deu <- cbind.data.frame(totland_agg, ctry="DEU")
  if(general$a.country=="SWE")  totland_agg_swe <- cbind.data.frame(totland_agg, ctry="SWE")


  # find total land per vessel & reshape
  vessland                   <- aggregate(x[,grep("LE_KG", colnames(x))], list(x$area, x$VE_REF), sum, na.rm=TRUE)
  vessland$id                <- paste(vessland$Group.1, '.', vessland$Group.2, sep='')
  vessland_long              <- reshape(vessland, direction="long", ids="id",
                                                      times=colnames(vessland)[grep("LE_KG", colnames(vessland))], timevar="species",
                                                      v.names="LE_KG_", varying=3:(ncol(vessland)-1))  # be patient....
  vessland_long$stock        <- paste( gsub("LE_KG_", "", vessland_long$species), ".", vessland_long$Group.1, sep='')

  # correct names for special cases (those across management areas)
  vessland_long[vessland_long$stock %in% c("COD.IIIan"), "stock"] <- 'COD.nsea'
  vessland_long[vessland_long$stock %in% c("COD.IIIas"), "stock"] <- 'COD.kat'
  vessland_long[vessland_long$stock %in% c("HAD.IIIan"), "stock"] <- 'HAD.nsea'
  vessland_long[vessland_long$stock %in% c("HER.IIIan", "HER.IIIas", "HER.2224"), "stock"] <- 'HER.3a22'
  vessland_long[vessland_long$stock %in% c("SPR.2224", "SPR.2532"), "stock"] <- 'SPR.2232'
  vessland_long[vessland_long$stock %in% c("PLE.2224", "PLE.2532"), "stock"] <- 'PLE.2232'
  vessland_long[vessland_long$stock %in% c("FLE.2224", "FLE.2532"), "stock"] <- 'FLE.2232'
  vessland_long[vessland_long$stock %in% c("TUR.2224", "TUR.2532"), "stock"] <- 'TUR.2232'
  vessland_long[vessland_long$stock %in% c("DAB.2224", "DAB.2532"), "stock"] <- 'DAB.2232'
  vessland_long[grep('IIIa', vessland_long$stock), "stock"] <- # for all other species, correct kask
     paste( gsub("LE_KG_", "",vessland_long[grep('IIIa', vessland_long$stock),'species']), ".kask", sep='')

  # then aggregate again for correctness
  vessland_agg <- aggregate(vessland_long$LE_KG_, list(vessland_long$Group.2, vessland_long$stock), sum, na.rm=TRUE)
  colnames(vessland_agg) <- c("vid", "stock", "vessland")


  # merge & divide to get a percentage...
  vessland_totland         <-  merge(vessland_agg, totland_agg, all=TRUE)
  vessland_totland$percent <-  vessland_totland$vessland / vessland_totland$totland *100


  # keep relevant species and order according to pop_name
  pop_names                             <- read.table(file.path(general$main.path, "popsspe", paste("pop_names_", general$case_study,".txt", sep='')))
  vessland_totland$stock                <- factor(vessland_totland$stock)
  levels(vessland_totland$stock )       <- pop_names[,2][ match(levels(vessland_totland$stock), as.character(pop_names[,1]))] # map the name to integer
  vessland_totland$mapped_stk_code      <- as.numeric(as.character(vessland_totland$stock))
  vessland_totland                      <- vessland_totland[!is.na(vessland_totland$mapped_stk_code),] # remove NA stocks

  library(doBy)
  vessland_totland                 <- orderBy(~vid+mapped_stk_code, data=vessland_totland) # library(doBy) # order from 0 to nbstock


  ## merge with all combi to get all pop informed even if percent at 0.(required fro Cpp multimap)
  all.combi                  <- expand.grid(vid=unique(vessland_totland[, c('vid')]),
                                       mapped_stk_code=pop_names[,2] )
  vessland_totland           <- merge(all.combi, vessland_totland , all=TRUE)
  vessland_totland[is.na( vessland_totland$percent ), 'percent' ]  <- 0   # replace NA  by 0


  vessland_totland[, 'percent' ]  <- round(vessland_totland[, 'percent' ], 4)


  FRANCOIS <- TRUE
  if(FRANCOIS) { do.append <- FALSE}
  if(!FRANCOIS)    { do.append <- TRUE}

  # copy/paste the same for semester because we don´t care right now
  semester <- "semester1"
  write.table(vessland_totland[, c('vid', 'percent')], # for c++ multimap<vid,percent> with stock implicit
           file=file.path(general$main.path, "vesselsspe",
             paste("vesselsspe_percent_tacs_per_pop_",semester,".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE, append=do.append)

  semester <- "semester2"
  write.table(vessland_totland[, c('vid', 'percent')], # for c++ multimap<vid,percent> with stock implicit
           file=file.path(general$main.path, "vesselsspe",
             paste("vesselsspe_percent_tacs_per_pop_", semester,".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE, append=do.append)




  ## -------------------------------------------------------
  ## get aslo a relative key for the share between countries
  if(general$case_study!="myfish") relative_stability <- rbind.data.frame(totland_agg_den,
                                         totland_agg_deu,
                                         totland_agg_swe)
  if(general$case_study=="myfish") relative_stability <- rbind.data.frame(totland_agg_den)


  # correct names for special cases (those across management areas)
  relative_stability[relative_stability$stock %in% c("COD.IIIan"), "stock"] <- 'COD.nsea'
  relative_stability[relative_stability$stock %in% c("COD.IIIas"), "stock"] <- 'COD.kat'
  relative_stability[relative_stability$stock %in% c("HAD.IIIan"), "stock"] <- 'HAD.nsea'
  relative_stability[relative_stability$stock %in% c("HER.IIIan", "HER.IIIas", "HER.2224"), "stock"] <- 'HER.3a22'
  relative_stability[relative_stability$stock %in% c("SPR.2224", "SPR.2532"), "stock"] <- 'SPR.2232'
  relative_stability[relative_stability$stock %in% c("PLE.2224", "PLE.2532"), "stock"] <- 'PLE.2232'
  relative_stability[relative_stability$stock %in% c("FLE.2224", "FLE.2532"), "stock"] <- 'FLE.2232'
  relative_stability[relative_stability$stock %in% c("TUR.2224", "TUR.2532"), "stock"] <- 'TUR.2232'
  relative_stability[relative_stability$stock %in% c("DAB.2224", "DAB.2532"), "stock"] <- 'DAB.2232'
  relative_stability[grep('IIIa', relative_stability$stock), "stock"] <- # for all other species, correct kask
     paste( relative_stability[grep('IIIa', relative_stability$stock),'stock'], ".kask", sep='')



  # then aggregate again for correctness
  relative_stability           <- aggregate(relative_stability$totland, list(relative_stability$ctry, relative_stability$stock), sum, na.rm=TRUE)
  colnames(relative_stability) <- c("ctry", "stock", "totland")


  totland                      <- as.matrix(tapply(relative_stability$totland, relative_stability$stock, sum))

  relative_stability           <- cbind.data.frame (relative_stability, totland_all=totland[as.character(relative_stability$stock),])

  relative_stability$percent   <- round(relative_stability$totland/relative_stability$totland_all*100,3)

  # keep relevant species and order according to pop_name
  pop_names                               <- read.table(file.path(general$main.path, "popsspe", paste("pop_names_", general$case_study,".txt", sep='')))
  relative_stability$stock                <- factor(relative_stability$stock)
  levels(relative_stability$stock )       <- pop_names[,2][ match(levels(relative_stability$stock), as.character(pop_names[,1]))] # map the name to integer
  relative_stability$mapped_stk_code      <- as.numeric(as.character(relative_stability$stock))
  relative_stability                      <- relative_stability[!is.na(relative_stability$mapped_stk_code),] # remove NA stocks

  library(doBy)
  relative_stability                 <- orderBy(~ctry+mapped_stk_code, data=relative_stability) # library(doBy) # order from 0 to nbstock



  ## merge with all combi to get all pop informed even if percent at 0.(required fro Cpp multimap)
  all.combi             <- expand.grid(ctry=unique(relative_stability[, c('ctry')]),
                                       mapped_stk_code=pop_names[,2] )
  relative_stability           <- merge(all.combi, relative_stability , all=TRUE)
  relative_stability[is.na( relative_stability$percent ), 'percent' ]  <- 0   # replace NA  by 0


  for (pop in unique(relative_stability$mapped_stk_code)){
     # copy/paste the same for semester because we don´t care right now
     semester <- "semester1"
     write.table(relative_stability[relative_stability$mapped_stk_code==pop, c('ctry', 'percent')], # for c++ multimap<vid,percent> with stock implicit
           file=file.path(general$main.path, "popsspe",
             paste(pop,"ctrysspe_relative_stability_",semester,".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE, append=FALSE)

     semester <- "semester2"
     write.table(relative_stability[relative_stability$mapped_stk_code==pop, c('ctry', 'percent')], # for c++ multimap<vid,percent> with stock implicit
           file=file.path(general$main.path, "popsspe",
             paste(pop,"ctrysspe_relative_stability_", semester,".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE, append=FALSE)

 }
