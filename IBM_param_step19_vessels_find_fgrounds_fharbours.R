 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!LOAD & COMPUTE FOR OBTAINING!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## vesselsspe_fgrounds_quarter[xx].dat          !!!!!!!!!!!!!!!##
 ## vesselsspe_freq_fgrounds_quarter[xx].dat    !!!!!!!!!!!!!!!!##
 ## vesselsspe_harbours_quarter[xx].dat         !!!!!!!!!!!!!!!!##
 ## vesselsspe_freq_harbours_quarter[xx].dat     !!!!!!!!!!!!!!!##
 ## DNK00[xx]_possible_metiers_quarter2.dat     !!!!!!!!!!!!!!!!##
 ## DNK00[xx]_freq_possible_metiers_quarter[xx].dat !!!!!!!!!!!!##
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


 # load the graph
 #load(file.path(general$main.path, "igraph", paste(general$igraph,  "_graphibm.RData",sep=''))) # built from the R code
 coord <- read.table(file=file.path(general$main_path_input, "graphsspe", paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
 coord <- as.matrix(as.vector(coord))
 coord <- matrix(coord, ncol=3)
 colnames(coord) <- c('x', 'y', 'idx.port')
 #plot(coord[,1], coord[,2])

 
 coord            <-  cbind(coord, idx=1:nrow(coord))  ## CAUTION: start at 1
 coord.fgrounds   <-  coord[coord[,'idx.port']==0,]
 coord.harbours   <-  coord[coord[,'idx.port']!=0,]



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

  
 

 # (optional) load outputs from glm and then remove here potential few records without any info
 # from avai to make the input .dat files very robust for Cpp...
 load(file.path(general$main.path, "popsspe", paste("betas",general$case_study,"_INTEGER_",general$case_study,".RData", sep='')) )
 nrow( ping.fgrounds)
 ping.fgrounds                <- ping.fgrounds[ping.fgrounds$VE_REF %in% unique(all.betas.vid$VE_REF),]
 ping.fgrounds                <- ping.fgrounds[ping.fgrounds$LE_MET_level6 %in% unique(all.betas.met$LE_MET_level6),]
 ping.fgrounds$VE_REF         <- factor(ping.fgrounds$VE_REF)
 ping.fgrounds$LE_MET_level6  <- factor(ping.fgrounds$LE_MET_level6)
 nrow( ping.fgrounds)


 # add quarters, and then semesters
 ping.harbours$quarter          <- quarters(as.POSIXct(ping.harbours$SI_DATE, tz="GMT"))
 ping.harbours$semester         <- factor(ping.harbours$quarter)
 levels(ping.harbours$semester) <- c(1,1,2,2)

 # then, aggregate...
 # FGROUNDS: AGGREGATE PER VE_REF
 nm                       <- names(ping.fgrounds)
 idx.col.e                <- grep('LE_EFF_VMS', nm)
 ping.fgrounds$LE_EFF_VMS <- as.numeric(as.character(ping.fgrounds$LE_EFF_VMS))
 idx.col                  <- c(idx.col.e)

 library(data.table)
 DT              <- data.table(ping.fgrounds) # library data.table for fast grouping replacing aggregate()
 eq1             <- c.listquote( paste ("sum(",nm[idx.col],",na.rm=TRUE)",sep="") )
 x.agg           <- DT[,eval(eq1), by=list(VE_REF, quarter, pt_graph)]
 x.agg           <- data.frame( x.agg)
 colnames(x.agg) <- c("VE_REF", "quarter", "pt_graph",nm[idx.col])

 # crucial to be consistent with cpues on node
 library(doBy)
 x.agg                    <- orderBy(~VE_REF +quarter +pt_graph, data=x.agg)


 # AGGREGATE PER VE_REF, PER METIER
 nm                       <- names(ping.fgrounds)
 idx.col.e                <- grep('LE_EFF_VMS', nm)
 ping.fgrounds$LE_EFF_VMS <- as.numeric(as.character(ping.fgrounds$LE_EFF_VMS))
 idx.col                  <- c(idx.col.e)

 library(data.table)
 DT                       <- data.table(ping.fgrounds) # library data.table for fast grouping replacing aggregate()
 eq1                      <- c.listquote( paste ("sum(",nm[idx.col],",na.rm=TRUE)",sep="") )
 x.agg.met                <- DT[,eval(eq1),by=list(VE_REF, LE_MET_level6, quarter, pt_graph)]
 x.agg.met                <- data.frame( x.agg.met)
 colnames(x.agg.met)      <- c("VE_REF", "LE_MET_level6", "quarter", "pt_graph", nm[idx.col])

 # crucial to be consistent with cpues on node
 library(doBy)
 x.agg.met                <- orderBy(~VE_REF +quarter +pt_graph, data=x.agg.met)



 ####-------
 for (a.quarter in c("Q1","Q2","Q3","Q4")){

    #-----------
    # vesselsspe_fgrounds_quarter[xx].dat
    # vesselsspe_freq_fgrounds_quarter[xx].dat
    x.agg.Q        <- x.agg[x.agg$quarter==a.quarter,]
    x.agg.Q$VE_REF <- factor( x.agg.Q$VE_REF )
    tot            <- tapply(x.agg.Q$LE_EFF_VMS, x.agg.Q$VE_REF, sum, na.rm=TRUE  )
    x.agg.Q$tot    <- tot[match(x.agg.Q$VE_REF, names(tot))] # map
    x.agg.Q$freq   <- round(x.agg.Q$LE_EFF_VMS /  x.agg.Q$tot,4)

    # save .dat files
    x.agg.Q$pt_graph <-  x.agg.Q$pt_graph - 1 ##!!! OFFSET FOR C++ !!!##
        vesselsspe_fgrounds_quarter <- x.agg.Q[,c('VE_REF','pt_graph')]
        write.table(vesselsspe_fgrounds_quarter,
            file=file.path(general$main.path, "vesselsspe",
              paste("vesselsspe_fgrounds_quarter",gsub("Q","",a.quarter),".dat",sep='')),
                  col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)
        vesselsspe_freq_fgrounds_quarter <- x.agg.Q[,c('VE_REF','freq')]
        write.table(vesselsspe_freq_fgrounds_quarter,
          file=file.path(general$main.path, "vesselsspe",
            paste("vesselsspe_freq_fgrounds_quarter",gsub("Q","",a.quarter),".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)
    #-----------



    #-----------
    # vesselsspe_harbours_quarter[xx].dat
    # vesselsspe_freq_harbours_quarter[xx].dat
    h.Q         <- ping.harbours[ping.harbours$quarter==a.quarter, c("VE_REF","pt_graph")]
    h.Q         <- h.Q[!is.na(h.Q$pt_graph),]
    h.Q$VE_REF  <- factor(h.Q$VE_REF )
    h.Q         <- h.Q[!duplicated(h.Q),]
    nb          <- tapply(h.Q$pt_graph, h.Q$VE_REF , length)
    h.Q$nb      <-  nb[match(h.Q$VE_REF, names(nb))] # map
    h.Q$freq    <- round(1 / h.Q$nb , 2)

    # save .dat files
        h.Q$pt_graph <-  h.Q$pt_graph - 1 ##!!! FOR C++ !!!##
        vesselsspe_harbours_quarter <-h.Q[,c('VE_REF','pt_graph')]
        write.table(vesselsspe_harbours_quarter,
            file=file.path(general$main.path, "vesselsspe",
              paste("vesselsspe_harbours_quarter",gsub("Q","",a.quarter),".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)
        vesselsspe_freq_harbours_quarter <- h.Q[,c('VE_REF','freq')]
        write.table(vesselsspe_freq_harbours_quarter,
            file=file.path(general$main.path, "vesselsspe",
              paste("vesselsspe_freq_harbours_quarter",gsub("Q","",a.quarter),".dat",sep='')),
                col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)

        # check for NAs
        dd<- vesselsspe_harbours_quarter [is.na(vesselsspe_harbours_quarter[,c('pt_graph')]) , ]
        if(nrow(dd)!=0) {print ("NAs in vesselsspe_harbours_quarter"); browser()}
        dd<- vesselsspe_freq_harbours_quarter [is.na(vesselsspe_freq_harbours_quarter[,c('freq')]) , ]
        if(nrow(dd)!=0) {print ("NAs in vesselsspe_freq_possible_metiers_quarter"); browser()}
   #----------



   #-----------
   # DNK00[xx]_possible_metiers_quarter2.dat
   # DNK00[xx]_freq_possible_metiers_quarter[xx].dat
    x.agg.met.Q         <-  x.agg.met[ x.agg.met$quarter==a.quarter,]
    x.agg.met.Q$VE_REF  <- factor(x.agg.met.Q$VE_REF)
    for(vid in unique(x.agg.met$VE_REF)){
       x.agg.met.Q.vid                <-   x.agg.met.Q[  x.agg.met.Q$VE_REF %in% vid,]
       x.agg.met.Q.vid$VE_REF         <- factor(x.agg.met.Q.vid$VE_REF)
       x.agg.met.Q.vid$pt_graph       <- factor( x.agg.met.Q.vid$pt_graph )
       x.agg.met.Q.vid$LE_MET_level6  <- factor(x.agg.met.Q.vid$LE_MET_level6)
       tot                            <- tapply( x.agg.met.Q.vid$LE_EFF_VMS, list( x.agg.met.Q.vid$pt_graph) , sum, na.rm=TRUE)
       x.agg.met.Q.vid$tot            <- tot[match(x.agg.met.Q.vid$pt_graph, names(tot))] # map
       x.agg.met.Q.vid$freq           <-  round( x.agg.met.Q.vid$LE_EFF_VMS /x.agg.met.Q.vid$tot ,4)

       # save .dat files (vessel-spe .dat files)
       x.agg.met.Q.vid$pt_graph            <- as.numeric(as.character(x.agg.met.Q.vid$pt_graph)) - 1 ##!!! FOR C++ !!!##
       vesselsspe_possible_metiers_quarter <- x.agg.met.Q.vid[,c('pt_graph', 'LE_MET_level6')]

       write.table(vesselsspe_possible_metiers_quarter,
           file=file.path(general$main.path, "vesselsspe",
             paste(vid,"_possible_metiers_quarter",gsub("Q","",a.quarter),".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)
       vesselsspe_freq_possible_metiers_quarter <- x.agg.met.Q.vid[,c('pt_graph', 'freq')]
       write.table(vesselsspe_freq_possible_metiers_quarter,
           file=file.path(general$main.path, "vesselsspe",
             paste(vid,"_freq_possible_metiers_quarter",gsub("Q","",a.quarter),".dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)

      # check for NAs
      dd<- vesselsspe_possible_metiers_quarter [is.na(vesselsspe_possible_metiers_quarter[,c('LE_MET_level6')]) , ]
        if(nrow(dd)!=0) {print ("NAs in vesselsspe_possible_metiers_quarter"); browser()}
      dd <- vesselsspe_freq_possible_metiers_quarter [is.na(vesselsspe_freq_possible_metiers_quarter[,c('freq')]) , ]
        if(nrow(dd)!=0){print ("NAs in vesselsspe_freq_possible_metiers_quarter"); browser()}

     } # end vid
     #----------

 } # end a.quarter



