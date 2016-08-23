 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!LINK METIERS TO TARGET STOCKS!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!met_target_names.txt!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##


 ## METIER SPE
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

  if(general$case_study=="canadian_paper"){
      year      <- "2010"
      years     <- "2005_2010"
      method    <- "maximum"
      threshold <- "50"
      }
  if(general$case_study=="baltic_only"){
      year      <- "2012"
      years     <- "2008_2012"
      method    <- "inverse"
      threshold <- "25"
      }

  if(general$case_study=="myfish"){
      year      <- "2012"
      years     <- "2008_2012"
      method    <- "inverse"
      threshold <- "25"
      }


     
      
      
     if('DEN' %in% general$case_study_countries) {
       load(file.path(general$main.path,"merged_tables", general$case_study, paste("x.agg.DEN.",year,".igraph",general$igraph,".RData",sep='')))
       x_agg_den <- x.agg ; x_agg_den$country <- "DEN"
       load(file.path(general$main.path,"merged_tables", general$case_study, paste("metier_names.", general$a.country,".", general$a.year,".igraph",general$igraph,".RData",sep='')))
       metier_names_den           <- metier_names
       colnames(metier_names_den) <- c("met", "idx")
       combined_met_names         <-  data.frame(metier_names_den)
       }
       
     if('DEU' %in% general$case_study_countries) {
       load(file.path(general$main.path,"merged_tables", general$case_study, paste("x.agg.DEU.",year,".igraph",general$igraph,".RData",sep='')))
       x_agg_deu <- x.agg ; x_agg_deu$country <- "DEU"
       load(file.path(general$main.path,"merged_tables", general$case_study, paste("metier_names.DEU.",year,".igraph",general$igraph,".RData",sep='')))
       metier_names_deu <- metier_names  ; colnames(metier_names_deu) <- c("met", "idx")
       combined_met_names <-  data.frame(metier_names_deu)
       }
       
       
     if('SWE' %in% general$case_study_countries) {
       load(file.path(general$main.path,"merged_tables", general$case_study, paste("x.agg.SWE.",year,".igraph",general$igraph,".RData",sep='')))
       x_agg_swe <- x.agg ; x_agg_swe$country <- "SWE"
       load(file.path(general$main.path,"merged_tables", general$case_study, paste("metier_names.SWE.",year,".igraph",general$igraph,".RData",sep='')))
       metier_names_swe <- metier_names  ; colnames(metier_names_swe) <- c("met", "idx")
       combined_met_names <-  data.frame(metier_names_swe)
       }
       
     # rename the metiers because of the combination of dnk and ger
     if('DEN' %in% general$case_study_countries &&
          'DEU' %in% general$case_study_countries &&
            'SWE' %in% general$case_study_countries) {
       combined_met_names           <- merge(metier_names_den, metier_names_deu, by="met", all=TRUE, suffixes = c(".den",".deu"))
       combined_met_names           <- merge(combined_met_names, metier_names_swe, by="met", all=TRUE)
       colnames(combined_met_names) <- c('met', 'idx.den', 'idx.deu', 'idx.swe')
       combined_met_names$idx       <- 0:(nrow(combined_met_names)-1) # dont forget the c++ offset...
     
     # rename for den
     levels(x_agg_den$LE_MET_level6) <-
        combined_met_names$idx[ match(levels(x_agg_den$LE_MET_level6), as.character(combined_met_names$idx.den))]
     # rename for deu
     levels(x_agg_deu$LE_MET_level6) <-
       combined_met_names$idx[ match(levels(x_agg_deu$LE_MET_level6), as.character(combined_met_names$idx.deu))]
     # rename for swe
     levels(x_agg_swe$LE_MET_level6) <-
       combined_met_names$idx[ match(levels(x_agg_swe$LE_MET_level6), as.character(combined_met_names$idx.swe))]
     
     # save
     # rbind Germany, Denmark and Sweden
     combined_columns   <- unique(c(names(x_agg_den),names(x_agg_deu),names(x_agg_swe)))
     idx_col            <- grep("KG", combined_columns)
     combined_columns   <- c(combined_columns[!(1:length(combined_columns)) %in% idx_col], sort(combined_columns[idx_col]))
     x_agg_den[,combined_columns[is.na(match(combined_columns,names(x_agg_den)))]] <- NA
     x_agg_deu[,combined_columns[is.na(match(combined_columns,names(x_agg_deu)))]] <- NA
     x_agg_swe[,combined_columns[is.na(match(combined_columns,names(x_agg_swe)))]] <- NA
     x_agg_den          <- x_agg_den[, combined_columns] # order column names
     x_agg_deu          <- x_agg_deu[, combined_columns] # order column names
     x_agg_swe          <- x_agg_swe[, combined_columns] # order column names
     x.agg              <- rbind(x_agg_den, x_agg_deu, x_agg_swe) # rbind
     head(x.agg )
     
     } else{
      x.agg  <- x_agg_den           
     }


 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
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

 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##


 # find out areas
 #load(file.path(general$main.path, "igraph", paste(general$igraph, "_graphibm.RData",sep=''))) # built from the R code
 coord <- read.table(file=file.path(general$main_path_input, "graphsspe", paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
 coord <- as.matrix(as.vector(coord))
 coord <- matrix(coord, ncol=3)
 colnames(coord) <- c('x', 'y', 'idx.port')
 #plot(coord[,1], coord[,2])

 
 coord_pt_graph <- coord[x.agg$pt_graph,] # replace coord of vms point by the coord of the graph node before finding out the stock area
 
 source(file=file.path(general$main.path.code,"IBM_param_utils_longlat_to_ICESareas.r"))
 x.agg$x      <- as.numeric(as.character(coord_pt_graph[,'x']))
 x.agg$y      <- as.numeric(as.character(coord_pt_graph[,'y']))
 x.agg$area   <- longlat_to_ICESareas(x.agg)
 x.agg[is.na(x.agg$area) | !(x.agg$area %in% c('22', '23', '24', '25', '26', '27', '28-1', '28-2', '29', '30', '31', '32',
                         'IIIan', 'IIIas') ), 'area'] <- 'nsea' # if out of range, assign all these nodes to North Sea stocks...
 x.agg[(x.agg$area %in% c('22', '23', '24')), 'area'] <- '2224'
 x.agg[(x.agg$area %in% c('25', '26', '27', '28-1', '28-2', '29','30', '31', '32')), 'area'] <- '2532'


 # aggregate per metier
 library(data.table)
 nm               <- names(x.agg)
 idx.col.w        <- grep('KG', nm) # index columns with species weight
 idx.col          <- c(idx.col.w) # index columns with species weight
 DT               <- data.table(x.agg) # library data.table for fast grouping replacing aggregate()
 # AGGREGATE WEIGHT AND EFFORT PER SPECIES
 eq1              <- c.listquote( paste ("sum(",nm[idx.col],",na.rm=TRUE)",sep="") )
 x.agg2           <- DT[,eval(eq1),by=list(LE_MET_level6, area)]
 x.agg2           <- data.frame( x.agg2)
 colnames(x.agg2) <- c("LE_MET_level6", "area", nm[idx.col])




 # reshape
 x.agg2$id               <- paste(x.agg2$LE_MET_level6, '.', x.agg2$area, sep='')
 x.agg2.long             <- reshape(x.agg2, direction="long", ids="id",
                              times=nm[idx.col.w], timevar="species",
                               v.names="LE_KG_", varying=3:(ncol(x.agg2)-1))  # be patient....
 x.agg2.long             <- x.agg2.long[,c("LE_MET_level6",  "area",  "species", "LE_KG_")]
 rownames(x.agg2.long)   <- NULL
 colnames(x.agg2.long)   <- c("LE_MET_level6",  "area", "species", "KG")


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


  # then re-aggregate because of the areas
  x.agg2.long <- aggregate(x.agg2.long$KG, list(LE_MET_level6=x.agg2.long$LE_MET_level6, stock=x.agg2.long$stock), sum, na.rm=TRUE)
  colnames(x.agg2.long) <- c("LE_MET_level6",   "stock", "KG")

  # subset for relevant populations
  pop_names                   <- read.table(file.path(general$main.path, "popsspe",paste("pop_names_",general$case_study,".txt", sep='')))
  x.agg2.long                 <- x.agg2.long[x.agg2.long$stock %in% pop_names[,1],] # keep simulated stocks only
  x.agg2.long$stock           <- factor(x.agg2.long$stock)
  levels(x.agg2.long$stock )  <- pop_names[,2][ match(levels(x.agg2.long$stock), as.character(pop_names[,1]))] # map the name to integer
  x.agg2.long$mapped_stk_code <- as.numeric(as.character(x.agg2.long$stock))
  library(doBy)
  x.agg2.long                 <- orderBy(~LE_MET_level6+mapped_stk_code, data=x.agg2.long) # library(doBy) # order from 0 to nbstock

  x.agg2.long                 <- x.agg2.long[,c("LE_MET_level6",   "mapped_stk_code", "KG")]
  x.agg2.long                 <- x.agg2.long[!is.na(x.agg2.long$mapped_stk_code),] # remove NA stocks



  # HERE WE ARE: retain max 5 target species per metier
   x.agg2.long                 <- orderBy(~LE_MET_level6-KG, data=x.agg2.long) # library(doBy) # order from 0 to nbstock
   x.agg2.long                 <- do.call ("rbind", lapply( split(x.agg2.long, f=x.agg2.long$LE_MET_level6), function(x) {res <- x[1:5,]; if(all(res$KG!=0)) res else x[1,] } ) )


  
  # export to c++ as a multimap
  write.table(x.agg2.long[, c("LE_MET_level6", "mapped_stk_code")],
          file=file.path(general$main.path, "metiersspe",
           "met_target_names.txt"),
            col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)


