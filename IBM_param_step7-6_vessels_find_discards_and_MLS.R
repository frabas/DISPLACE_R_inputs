

 # load the combined graph with the "merged" table
 load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.fgrounds.",general$a.country,".",general$a.year,".igraph",
                general$igraph,".RData",sep='')))


 # remove the No_Matrix6 metier...
 ping.fgrounds               <- ping.fgrounds[ping.fgrounds$LE_MET_level6!= "No_Matrix6",]
 ping.fgrounds               <- ping.fgrounds[ping.fgrounds$LE_MET_level6!= "NA",]
 ping.fgrounds$LE_MET_level6 <- factor(ping.fgrounds$LE_MET_level6)
 #...and slightly modify the name of metiers
 #levels(ping.fgrounds$LE_MET_level6) <-
 #   paste("met", 0:(length(levels(ping.fgrounds$LE_MET_level6))-1), "_", levels(ping.fgrounds$LE_MET_level6), "_",general$a.country, sep='')
 # NEED JUST INTEGERS!, replaced by:
  ping.fgrounds$LE_MET_char          <- ping.fgrounds$LE_MET_level6
  load(file.path(general$main.path, "merged_tables", general$case_study,
        paste("combined_met_names.",general$a.year,".igraph", general$igraph,".RData", sep='')))    # get 'combined_met_names'
  ping.fgrounds$LE_MET_level6         <- unlist(lapply(strsplit(as.character(ping.fgrounds$LE_MET_level6), split="_"), function(x) paste(x[1],'_',x[2],sep='')))   # remove mesh size and useless selective grid info
  # rename metiers into INTEGER metiers
   ping.fgrounds$LE_MET_level6         <- factor(ping.fgrounds$LE_MET_level6)
  levels(ping.fgrounds$LE_MET_level6) <- combined_met_names$idx[ match(levels(ping.fgrounds$LE_MET_level6), as.character(combined_met_names$met))]


 # (optional) load outputs from glm and then remove here potential few records without any info
 # from avai to make the input .dat files very robust for Cpp...
 load(file.path(general$main.path, "popsspe", paste("betas_DEN_DEU_SWE_INTEGER_",general$case_study ,".RData", sep='')) )
 ping.fgrounds                <- ping.fgrounds[ping.fgrounds$VE_REF %in% unique(all.betas.vid$VE_REF),]
 ping.fgrounds                <- ping.fgrounds[ping.fgrounds$LE_MET_level6 %in% unique(all.betas.met$LE_MET_level6),]
 ping.fgrounds$VE_REF         <- factor(ping.fgrounds$VE_REF)
 ping.fgrounds$LE_MET_level6  <- factor(ping.fgrounds$LE_MET_level6)



 ## DISCARDS ###################
 # create discards ogive e.g. all at 0 (and not species-specific...)
 dis <- NULL
 for (met in levels(ping.fgrounds$LE_MET_level6) ) {

    the_met <- combined_met_names[combined_met_names[, 'idx']==met, 'met']

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
    l           <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,1000) *a_size_group_bin_in_cm  # vector of 14 length groups of eg 10 cm bin or 5 cm
    length.fish <-  l + mid # i.e. mid size in cm
    equ.dis     <- paste("1- (1/(1+exp(S1-S2*length.fish)))")  # cf. Rasmus paper
    S1          <- L50*log(3) / (L75 - L50)      # L75=L50+(1/2*SR)
    S2          <-  S1/L50

    ogive              <- rep(met, 14)
    dis <-  rbind (dis, cbind(ogive, round(eval(parse("",text=equ.dis)),3)  )      )    # ...assuming 14 szgroup bins
 }
 colnames(dis) <- c("met_name","ogive")
 #=> parameterize as the proportion discarded per szgroup.
 # CAUTION THIS IS VERY SENSISTIVE AS THE CATCHES ARE INFERED FROM THE MODELED LANDINGS AND THIS OGIVE......


 # save the .dat file
 if(FRANCOIS) {
            write.table(dis,
               file=file.path(general$main.path, "metiersspe",
                 paste("metier_discards_ogives.dat",sep='')),
                   col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)
       }




 ## MLS ###################
# pop parameters
 pa <- read.csv(file=file.path(general$main.path,
                  paste("IBM_datainput_stockdata_",case_study,".csv", sep='')),
                    sep=';', header=TRUE)

 filename_met_Q1 <- file.path(general$main.path, "metiersspe",
                  paste("metierspe_mls_cat_semester1.dat", sep=''))
 filename_met_Q2 <- file.path(general$main.path, "metiersspe",
                  paste("metierspe_mls_cat_semester2.dat", sep=''))


 # load the combined graph with the "merged" table
 load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.fgrounds.",general$a.country,".",general$a.year,".igraph",
                general$igraph,".RData",sep='')))

 load(file.path(general$main.path, "merged_tables", general$case_study,
        paste("combined_met_names.",general$a.year,".igraph", general$igraph,".RData", sep='')))    # get 'combined_met_names'
  ping.fgrounds$LE_MET_level6         <- unlist(lapply(strsplit(as.character(ping.fgrounds$LE_MET_level6), split="_"), function(x) paste(x[1],'_',x[2],sep='')))   #
  ping.fgrounds$LE_MET_level6         <- factor(ping.fgrounds$LE_MET_level6)
  levels(ping.fgrounds$LE_MET_level6) <- combined_met_names$idx[ match(levels(ping.fgrounds$LE_MET_level6), as.character(combined_met_names$met))]


 mls <- data.frame(NULL)
 for (met in levels(ping.fgrounds$LE_MET_level6) ) {
    mls     <- rbind.data.frame(mls, cbind.data.frame(metier=rep(met, length(0:(nrow(pa)-1))), pop= 0:(nrow(pa)-1), mls=pa[,"mls_cat"]))
    }

  mls <- orderBy(~metier+pop, data=mls)

 # save the .dat file
 if(FRANCOIS) {
  write.table(mls[,c("metier", "mls")],
                file=filename_met_Q1,
                  append=FALSE, sep=" ", col.names=TRUE, row.names=FALSE, quote=FALSE)
  write.table(mls[,c("metier", "mls")],
                file=filename_met_Q2,
                  append=FALSE, sep=" ", col.names=TRUE, row.names=FALSE, quote=FALSE)
 }







