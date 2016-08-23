

 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!LOAD & COMPUTE FOR OBTAINING!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## metier_discards_ogives.dat          !!!!!!!!!!!!!!!!!!!!!!!!##
 ## metierspe_mls_cat_semesterXX.dat    !!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 
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


 # load the combined graph with the "merged" table for DNK
 load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.fgrounds.","DEN",".",general$a.year,".igraph",general$igraph,".RData",sep='')))
 ping.fgrounds.den  <- ping.fgrounds
 load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.harbours.","DEN",".",general$a.year,".igraph",general$igraph,".RData",sep='')))
 ping.harbours.den  <- ping.harbours


 if("DEN" %in% general$case_study_countries &&
      "DEU" %in% general$case_study_countries &&
       "SWE" %in% general$case_study_countries ){

 # load the combined graph with the "merged" table for DEU
 load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.fgrounds.","DEU",".",general$a.year,".igraph",general$igraph,".RData",sep='')))
 ping.fgrounds.deu  <- ping.fgrounds[, !colnames(ping.fgrounds) %in% c("QUARTER")]
 load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.harbours.","DEU",".",general$a.year,".igraph",general$igraph,".RData",sep='')))
 ping.harbours.deu  <- ping.harbours
 ping.harbours.deu <- ping.harbours[, !colnames(ping.harbours) %in% c("QUARTER")]


  # load the combined graph with the "merged" table for SWE
 load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.fgrounds.","SWE",".",general$a.year,".igraph",general$igraph,".RData",sep='')))
 ping.fgrounds.swe  <- ping.fgrounds[, !colnames(ping.fgrounds) %in% c("QUARTER")]
 load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.harbours.","SWE",".",general$a.year,".igraph",general$igraph,".RData",sep='')))
 ping.harbours.swe  <- ping.harbours
 ping.harbours.swe <- ping.harbours[, !colnames(ping.harbours) %in% c("QUARTER")]

 # small adjustement for SWE data
 ping.fgrounds.swe$VE_FLT   <- NA
 ping.fgrounds.swe$SI_SP    <- NA
 ping.fgrounds.swe$SI_HE    <- NA
 ping.fgrounds.swe$KW_HOURS <- NA
 ping.harbours.swe$VE_FLT   <- NA
 ping.harbours.swe$SI_SP    <- NA
 ping.harbours.swe$SI_HE    <- NA
 ping.harbours.swe$KW_HOURS <- NA
 ping.harbours.swe$KW_HOURS <- NA


 # combined DNK and GER (removing useless info on kg landing)
  ping.fgrounds <- rbind(
                        ping.fgrounds.den[,-grep("KG", colnames(ping.fgrounds.den))],
                         ping.fgrounds.deu[,-grep("KG", colnames(ping.fgrounds.deu))],
                          ping.fgrounds.swe[, colnames(ping.fgrounds.den[,-grep("KG", colnames(ping.fgrounds.den))])]
                         )
  # keep only DNK and DEU, and SWE vessels
  ping.fgrounds <- ping.fgrounds[c(grep("DNK", ping.fgrounds$VE_REF), grep("DEU", ping.fgrounds$VE_REF), grep("SWE", ping.fgrounds$VE_REF)),]

  # combined DNK, DEU, and SWE (removing useless info on kg landing)
  ping.harbours <- rbind(
                        ping.harbours.den[,-grep("KG", colnames(ping.harbours.den))],
                         ping.harbours.deu[,-grep("KG", colnames(ping.harbours.deu))],
                          ping.harbours.swe[,colnames(ping.harbours.den[,-grep("KG", colnames(ping.harbours.den))])]
                         )
  # keep only DNK and DEU, and SWE vessels
  ping.harbours <- ping.harbours[c(grep("DNK", ping.harbours$VE_REF), grep("DEU", ping.harbours$VE_REF), grep("SWE", ping.harbours$VE_REF)),]


  # save the combined datasets
  save(ping.fgrounds,  file=file.path(general$main.path, "merged_tables", general$case_study,
             paste("ping.fgrounds.","DEN.DEU.SWE",".",general$a.year,".igraph",general$igraph,".RData", sep='')))
      cat(paste("save the combined ping.fgrounds', this year...OK\n\n",sep=""))
  save(ping.harbours,  file=file.path(general$main.path, "merged_tables", general$case_study,
             paste("ping.harbours.","DEN.DEU.SWE",".",general$a.year,".igraph",general$igraph,".RData", sep='')))
     cat(paste("save the combined 'ping.harbours', this year...OK\n\n",sep=""))

 } else{ # end combine countries
 ping.fgrounds <- ping.fgrounds.den
 ping.harbours <- ping.harbours.den
 }



 
  # add quarters, and then semesters
 ping.fgrounds$quarter          <- quarters(as.POSIXct(ping.fgrounds$SI_DATE, tz="GMT"))
 ping.fgrounds$semester         <- factor(ping.fgrounds$quarter)
 levels(ping.fgrounds$semester) <- c(1,1,2,2)

 # remove the No_Matrix6 metier...
 ping.fgrounds               <- ping.fgrounds[ping.fgrounds$LE_MET_level6!= "No_Matrix6",]
 ping.fgrounds               <- ping.fgrounds[ping.fgrounds$LE_MET_level6!= "NA",]
 ping.fgrounds$LE_MET_level6 <- factor(ping.fgrounds$LE_MET_level6)
 #...and slightly modify the name of metiers
 #levels(ping.fgrounds$LE_MET_level6) <-
 #   paste("met", 0:(length(levels(ping.fgrounds$LE_MET_level6))-1), "_", levels(ping.fgrounds$LE_MET_level6), "_",general$a.country, sep='')
 # NEED JUST INTEGERS!, replaced by:
 ping.fgrounds$LE_MET_char          <- ping.fgrounds$LE_MET_level6
 ping.fgrounds$LE_MET_level6         <- unlist(lapply(strsplit(as.character(ping.fgrounds$LE_MET_level6), split="_"), function(x) paste(x[1],'_',x[2],sep='')))   # remove mesh size and useless selective grid info
 # rename metiers into INTEGER metiers
 ping.fgrounds$LE_MET_level6         <- factor(ping.fgrounds$LE_MET_level6)
  
 #load(file.path(general$main.path, "merged_tables", general$case_study,
 #      paste("combined_met_names.",general$a.year,".igraph", general$igraph,".RData", sep='')))    # get 'combined_met_names'
 # levels(ping.fgrounds$LE_MET_level6) <- combined_met_names$idx[ match(levels(ping.fgrounds$LE_MET_level6), as.character(combined_met_names$met))]
 
 # get metier_names
 load(file.path(general$main.path,"merged_tables", general$case_study, paste("metier_names.", general$a.country,".", general$a.year,".igraph",general$igraph,".RData",sep='')))
  
 # NEED JUST INTEGERS! for c++, 
 levels(ping.fgrounds$LE_MET_level6) <- metier_names [match(levels(ping.fgrounds$LE_MET_level6), metier_names[,1]), 2]





 ## DISCARDS ###################
 # create discards ogive e.g. all at 0 (and not species-specific...)
 dis <- NULL
 for (met in levels(ping.fgrounds$LE_MET_level6) ) {

    #the_met <- combined_met_names[combined_met_names[, 'idx']==met, 'met']
    the_met <- metier_names[metier_names[, 2]==met, 1]
    
    L50         <- 5  # fake but conservative.
    L75         <- 10 # fake but conservative.
    if(general$case_study=="canadian_paper"){
       a_size_group_bin_in_cm <- 10
       mid                    <- 5
       }
    if(general$case_study=="baltic_only"){
       a_size_group_bin_in_cm <- 5
       mid                    <- 2.5
       }
    if(general$case_study=="myfish"){
       a_size_group_bin_in_cm <- 5
       mid                    <- 2.5
       }
    l           <-  c(0,1,2,3,4,5,6,7,8,9,10,11,12,1000) *a_size_group_bin_in_cm  # vector of 14 length groups of eg 10 cm bin or 5 cm
    length.fish <-  l + mid # i.e. mid size in cm
    equ.dis     <-  paste("1- (1/(1+exp(S1-S2*length.fish)))")  # cf. Rasmus paper
    S1          <-  L50*log(3) / (L75 - L50)      # L75=L50+(1/2*SR)
    S2          <-  S1/L50

    ogive       <- rep(met, 14)
    dis         <-  rbind (dis, cbind(ogive, round(eval(parse("",text=equ.dis)),3)  )      )    # ...assuming 14 szgroup bins
 }
 colnames(dis) <- c("met_name","ogive")
 #=> parameterize as the proportion discarded per szgroup.
 # CAUTION THIS IS VERY SENSITIVE AS THE CATCHES ARE INFERED FROM THE MODELED LANDINGS AND THIS OGIVE......
 # HOWEVER THIS IS DEPRECATED AND NOT LONGER USED AS DISCARDS ARE NOW DEDUCED FROM LANDINGS, SELECTIVITY AND MLS IN DISPLACE.


 # save the .dat file
 write.table(dis,
               file=file.path(general$main.path, "metiersspe",
                 paste("metier_discards_ogives.dat",sep='')),
                   col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)





 ## MLS ###################
# pop parameters
 pa <- read.csv(file=file.path(general$main.path,
                  paste("IBM_datainput_stockdata_", general$case_study,".csv", sep='')),
                    sep=',', header=TRUE)
  ## CAUTION WITH THE NB OF POPS HERE!!!!                  
                    

 filename_met_Q1 <- file.path(general$main.path, "metiersspe",
                  paste("metierspe_mls_cat_semester1.dat", sep=''))
 filename_met_Q2 <- file.path(general$main.path, "metiersspe",
                  paste("metierspe_mls_cat_semester2.dat", sep=''))


 # load the combined graph with the "merged" table
 load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.fgrounds.",general$a.country,".",general$a.year,".igraph",
                general$igraph,".RData",sep='')))

 ping.fgrounds$LE_MET_level6         <- unlist(lapply(strsplit(as.character(ping.fgrounds$LE_MET_level6), split="_"), function(x) paste(x[1],'_',x[2],sep='')))   #
 ping.fgrounds$LE_MET_level6         <- factor(ping.fgrounds$LE_MET_level6)
 
 #load(file.path(general$main.path, "merged_tables", general$case_study,
 #       paste("combined_met_names.",general$a.year,".igraph", general$igraph,".RData", sep='')))    # get 'combined_met_names'
 # levels(ping.fgrounds$LE_MET_level6) <- combined_met_names$idx[ match(levels(ping.fgrounds$LE_MET_level6), as.character(combined_met_names$met))]
  
 # replaced by: 
 # get metier_names
 load(file.path(general$main.path,"merged_tables", general$case_study, paste("metier_names.", general$a.country,".", general$a.year,".igraph",general$igraph,".RData",sep='')))  
 # NEED JUST INTEGERS! for c++, 
 levels(ping.fgrounds$LE_MET_level6) <- metier_names [match(levels(ping.fgrounds$LE_MET_level6), metier_names[,1]), 2]


  
  
 

 mls <- data.frame(NULL)
 for (met in levels(ping.fgrounds$LE_MET_level6) ) {
    mls     <- rbind.data.frame(mls, cbind.data.frame(metier=rep(met, length(0:(nrow(pa)-1))), pop= 0:(nrow(pa)-1), mls=pa[,"mls_cat"]))
    }

  library(doBy)
  mls <- orderBy(~metier+pop, data=mls)

 # save the .dat file
  write.table(mls[,c("metier", "mls")],
                file=filename_met_Q1,
                  append=FALSE, sep=" ", col.names=TRUE, row.names=FALSE, quote=FALSE)
  write.table(mls[,c("metier", "mls")],
                file=filename_met_Q2,
                  append=FALSE, sep=" ", col.names=TRUE, row.names=FALSE, quote=FALSE)
 






