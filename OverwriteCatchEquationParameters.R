 
 
 # GENERAL SETTINGS
  general <- list()
  general$main.path             <- file.path("C:", "Users", "fbas", "Documents", "GitHub", "DISPLACE_input_raw")
  general$main.path.code        <- file.path("C:", "Users", "fbas", "Documents", "GitHub", "DISPLACE_R_inputs")
  general$main_path_input       <- file.path("C:", "Users", "fbas", "Documents", "GitHub", "DISPLACE_input")
  general$inPath                <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis","balticRTI","FISHERIES")
  general$outPath               <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis","balticRTI","FISHERIES")
  general$inPathGraph           <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis","balticRTI","GRAPH")
  general$inPathManagement      <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis","balticRTI","MANAGEMENT")
  general$inPathPop             <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis","balticRTI","POPULATIONS")

  
  
  general$application            <- "balticRTI"
  
  general$main.path.ibm         <- file.path("C:","Users","fbas","Documents","GitHub", paste("DISPLACE_input_" , general$application, sep=""))
  dir.create(file.path(general$inPathPop , "CatchRates")) 
  dir.create(file.path(general$main.path.ibm, paste("popsspe_", general$application, sep='')))
  dir.create(file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep='')))
  dir.create(file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep='')))

  general$case_study_countries  <- c("DEN", "SWE", "DEU")    # for the Baltic only
  general$a.year                <- "2015"
  general$igraph                <- 56
  
  
  if(general$application=="balticRTI"){
      year      <- "2015"
      years     <- "2013_2014_2015"
      method    <- "inverse"
      threshold <- "25"
      p         <- 0.1
      }


 
 
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!! THE MERGED TABLE!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

  ctry <- "DNK"
  load(file=file.path(general$inPath,"all_merged_weight_DEN_2015.RData"))
  tacsatp_den <- all.merged
  tacsatp_den <- tacsatp_den[!is.na(as.numeric(as.character(tacsatp_den$SI_LONG))) &  !is.na(as.numeric(as.character(tacsatp_den$SI_LATI))), ]
  tacsatp_den$SI_LONG <- as.numeric(as.character(tacsatp_den$SI_LONG))
  tacsatp_den$SI_LATI <- as.numeric(as.character(tacsatp_den$SI_LATI))
  tacsatp_den$SI_STATE <- as.numeric(as.character(tacsatp_den$SI_STATE))
  tacsatp_den$LE_EFF_VMS <- as.numeric(as.character(tacsatp_den$LE_EFF_VMS)) /60
  tacsatp_den <- tacsatp_den[tacsatp_den$SI_STATE==1, ]  # keep fishing positions only
  tacsatp_den$ctry <- "DNK"


  ctry <- "SWE"
  load(file=file.path(general$inPath,"Displace2015_tacsat_swe_v2.RData"))
  tacsatp_swe <- tacsat.swe
  tacsatp_swe$SI_LONG <- as.numeric(as.character(tacsatp_swe$SI_LONG))
  tacsatp_swe$SI_LATI <- as.numeric(as.character(tacsatp_swe$SI_LATI))
  tacsatp_swe$SI_STATE <- as.numeric(as.character(tacsatp_swe$SI_STATE))
  tacsatp_swe$LE_MET_level6 <- tacsatp_swe$VE_MET  
  format_date <- "%Y-%m-%d %H:%M:%S" 
  tacsatp_swe$SI_DATIM <- as.POSIXct( tacsatp_swe$SI_DATIM, tz='GMT',   format_date)
  library(doBy)
  tacsatp_swe <- orderBy(~VE_REF + SI_DATIM, tacsatp_swe)
  tacsatp_swe$LE_EFF_VMS <- abs(c(0, as.numeric( tacsatp_swe[-nrow( tacsatp_swe),"SI_DATIM"] - tacsatp_swe[-1,"SI_DATIM"], units="mins") /60)) 
  start.trip <- c(1,diff( tacsatp_swe[,"FT_REF"]))
  tacsatp_swe$all_effort <- tacsatp_swe$LE_EFF_VMS  # save...
  tacsatp_swe[start.trip!=0, "LE_EFF_VMS"] <- 0  # just correct for the trip change points
  tacsatp_swe$LE_EFF_VMS <- as.numeric(as.character(tacsatp_swe$LE_EFF_VMS))
  tacsatp_swe <- tacsatp_swe[!is.na(as.numeric(as.character(tacsatp_swe$SI_LONG))) &  !is.na(as.numeric(as.character(tacsatp_swe$SI_LATI))), ]
  tacsatp_swe <- tacsatp_swe[tacsatp_swe$SI_STATE==1, ]  # keep fishing positions only
  tacsatp_swe$ctry <- "SWE"

   
  nm <- intersect(colnames(tacsatp_den), colnames(tacsatp_swe))
  tacsatp <- rbind.data.frame(
                        tacsatp_den[,nm],
                         tacsatp_swe[,nm] #,
                         # tacsatp_ger[, nm]
                         )
  tacsatp <- tacsatp[c(grep("DNK", tacsatp$VE_REF), grep("DEU",tacsatp$VE_REF), grep("SWE", tacsatp$VE_REF)),]
  tacsatp$ctry <- factor(tacsatp$ctry)

   
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  # relevant vessels only i.e. active in the area....
  
  # keep only the vessels fishnig in the western Baltic  (and kattegat because her.3a22, and East baltic because spr.2232)
  library(maptools)
  handmade            <- readShapePoly(file.path(general$inPathManagement, "wbaltic_wgs84"))  # build in ArcGIS 10.1
  the_area            <- sapply(slot(handmade, "polygons"), function(x) lapply(slot(x, "Polygons"), function(x) x@coords)) # tricky there...
  in_area             <- point.in.polygon(tacsatp[,'SI_LONG'],tacsatp[,'SI_LATI'], the_area[[1]][,1],the_area[[1]][,2])

  # then subset here...
  vid_this_case_study <- as.character(unique(tacsatp$VE_REF[in_area>0]))
  cat(paste(length(vid_this_case_study), " vessels in the area over ", length(unique(tacsatp$VE_REF)), " in total" , "\n"))
  tacsatp      <- tacsatp[tacsatp$VE_REF %in% vid_this_case_study,]
  
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  # relevant vessels only i.e. with some recorded catches on the subsetted stocks....
  
  # keep only the vessels fishnig in the western Baltic  (and kattegat because her.3a22, and East baltic because spr.2232)
   
  agg     <- aggregate(tacsatp[, grep('LE_KG_', colnames(tacsatp))], list(tacsatp$VE_REF), sum, na.rm=TRUE)
  agg$tot <- apply(agg[,-1], 1, sum, na.rm=TRUE)
  vid_with_no_landings_for_these_stocks <- as.character(agg[agg$tot<=0, 1])
    
  # then subset here...
  cat(paste(length(vid_with_no_landings_for_these_stocks), " vessels to remove over ", length(unique(tacsatp$VE_REF)), " in total" , "\n"))
  tacsatp      <- tacsatp[!tacsatp$VE_REF %in% vid_with_no_landings_for_these_stocks,]
   


 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!FIND THE CLOSED IGRAPH NODE FOR EACH FISHING PING!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

  # load the graph
  #load(file.path(general$main.path.igraph, paste(general$igraph, "_graphibm.RData",sep=''))) # built from the R code
  coord <- read.table(file=file.path(general$inPathGraph, 
             paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
  coord <- as.matrix(as.vector(coord))
  coord <- matrix(coord, ncol=3)
  coord <- cbind(coord, 1:nrow(coord))
  colnames(coord) <- c('x', 'y', 'harb', 'pt_graph')
  plot(coord[,1], coord[,2])

  saved_coord <- coord

  graph <- read.table(file=file.path(general$inPathGraph,
           paste("graph", general$igraph, ".dat", sep=""))) # build from the c++ gui
  graph <- as.matrix(as.vector(graph))
  graph <- matrix(graph, ncol=3)
  segments(coord[graph[,1]+1,1], coord[graph[,1]+1,2], coord[graph[,2]+1,1], coord[graph[,2]+1,2], col=4) # CAUTION: +1, because c++ to R


  if(FALSE){
    #memory.limit(4000)
    library(spatstat)
    an <- function(x)  as.numeric(as.character(x))
  
    tacsatp_f <- tacsatp[tacsatp$SI_STATE==1,]  # fishing pings only
    coord_f   <- coord[coord[,'harb']==0,]
  
    # for fishing pings
    X     <- ppp(x=an(tacsatp_f$SI_LONG), y=an(tacsatp_f$SI_LATI),
                         xrange=range(an(tacsatp_f$SI_LONG)), yrange=range(an(tacsatp_f$SI_LATI)))
    Y     <- ppp(x=coord_f[,"x"], y=coord_f[,"y"],
                                xrange=range(coord_f[,"x"]), yrange=range(coord_f[,"y"]))
    unloadNamespace("igraph") # CAUTION:to avoid conflicts with the spatstat library e.g. diameter()
    N <- nncross (X=X, Y=Y)$which # caution: just euclidean distance on coord
    # visual check
    if(FALSE){
      plot(superimpose(X=X, Y=Y), main="nncross", cols=c("red","blue"))
      arrows(X$x, X$y, Y[N]$x, Y[N]$y, length=0.15)
      }
    # add
    ping_fgrounds <- cbind(tacsatp_f, pt_graph= coord_f[N, 'pt_graph'])
    # export to save time
    save(ping_fgrounds, file=file.path(general$outPath, "ping_fgrounds.RData"))
  
  } else{
       load(file=file.path(general$outPath, "ping_fgrounds.RData"))
  }

   # check one pt
   #ff <- tail(ping_fgrounds)
   #plot(ff$SI_LONG, ff$SI_LATI)
   #points(an(coord[,'x']), an(coord[,'y']), pch="+")
   #points(an(coord[ff$pt_graph,'x']), an(coord[ff$pt_graph,'y']), pch="+", col=2)

  
   # reuse the exported metier names in GenerateVesselConfigFiles.R
    metier_names <-  read.table(
       file=file.path(general$inPath, "metier_names.dat"),
          header=TRUE)
    
   # debug metier names
   levels(ping_fgrounds$LE_MET_level6) <- gsub(">=", "o", levels(ping_fgrounds$LE_MET_level6))  
   levels(ping_fgrounds$LE_MET_level6) <- gsub("<",  "u", levels(ping_fgrounds$LE_MET_level6))  
   levels(ping_fgrounds$LE_MET_level6) <- gsub(">",  "o", levels(ping_fgrounds$LE_MET_level6))  
   levels(ping_fgrounds$LE_MET_level6) <- gsub("-",  "_", levels(ping_fgrounds$LE_MET_level6))  

   # link up
   levels(ping_fgrounds$LE_MET_level6)[!levels(ping_fgrounds$LE_MET_level6) %in% as.character(metier_names[,"name"])]  <- "other"
  
   ## debug to avoid 0/0
   ping_fgrounds <- ping_fgrounds[ping_fgrounds$LE_EFF_VMS!=0,]
   ## other debug...
   idx_non0_weight <- apply(ping_fgrounds[,grep('KG',colnames(ping_fgrounds))], 1, sum, na.rm=TRUE) !=0  # because maybe the relvant bunch of species for this record has not been kept
   ping_fgrounds   <- ping_fgrounds[idx_non0_weight,] # keep only records with some weight for some species

   # add a semester
   ping_fgrounds$quarter   <- quarters(as.Date(ping_fgrounds$SI_DATE))
   ping_fgrounds$semester  <- factor(ping_fgrounds$quarter) # init 
   levels(ping_fgrounds$semester) <- c("1", "1", "2", "2")
 
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!MERGE WITH AVAI!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!AND DO THE GLMs!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!PER POP AND SEMESTER!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##


  # load avai object 
  load(file.path(general$inPathPop, "avai",
        paste("lst_avai_igraph",general$igraph,"_",years,"_",method,"_",threshold,".RData",sep=""))) ##!!e.g.  2005-2010 !!##

  # (caution: give the order for naming stocks in integer from 0 to n-1)
  spp_table <-  read.table(file=file.path(general$inPathPop, paste("pop_names_",general$application ,".txt",sep='')),
              header=TRUE)
  spp                        <- as.character(spp_table$spp)

  all.betas.vid      <- NULL  # VE_REF mean estimate
  all.gammas.met     <- NULL  # LE_MET_level6 mean estimate
  all.betas.vid.se   <- NULL  # VE_REF standard error
  all.gammas.met.se  <- NULL  # LE_MET_level6 standard error

  range_szgroup <-  c('0', '2', '3', '5', '7')   # this actually cannot be changed because this naming is used in DISPLACE c++
  the_selected_szgroups <- NULL
  write(c('nm2',  'selected_szgroups'), # init
                   file=file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=''),
                       paste("the_selected_szgroups.dat",sep=' ')), append=FALSE,  ncol=2,
                          sep=" ")


  for(rg in range_szgroup){
    assign(paste('all.deltas' ,rg, sep=''),  NULL)   # init
    assign(paste('all.deltas.se' ,rg, sep=''),  NULL)   # init
    }

  res1 <- NULL  # goodness of fit glm1
  res2 <- NULL   # goodness of fit glm2
  #----------------------------------------------
  # subset the x.agg to keep the distribution area of
  # the relevant pop and during the relevant semester only
  nm        <-  names(lst.avai)  # names of stocks
  # PER STOCK
  for (i in 1: length(spp)){  
     nm2 <- spp[i]  # name of a given stock
     # PER SEMESTER
     for (j in 1: length(lst.avai[[nm2]])){  
       nm3        <- names(lst.avai[[nm2]])[j]
       cat(paste("stock ",    nm2,    "\n"))
       cat(paste("semester ", nm3, "\n"))
       avai       <- lst.avai[[nm2]][[nm3]]
       name.sp    <- substr(nm2, 1,3) # FAO sp code
       a_pop      <- paste("LE_KG_", name.sp, sep='')
       if(a_pop %in% colnames(ping_fgrounds) && name.sp!="MZZ"){
            xx <- ping_fgrounds[
                       ping_fgrounds$pt_graph %in% avai[,4] &
                         ping_fgrounds$semester==as.numeric(nm3),  # subset according to pt_graph and semester
                          c("VE_REF","LE_MET_level6", "LE_EFF_VMS", "semester", "pt_graph", a_pop)
                        ]  # subset the pop
     

       # Instead of making the code very complicated and furthermore refactor the c++ side,
       # hereafter an easy and DANGEROUS shortcut (dangerous because confusing...) as a way for changing the szgroups used for the catch rate equation....
       # i.e. select the 5 most relevant ones for this species.
       # e.g. try to contrast the spatial avai data for small vs. larger fish (i.e. >minimum landing size)
  
  
  
       selected_szgroups <-  range_szgroup #default
       if(nm2 %in% "COD.2224") selected_szgroups <-   c(2,5,7,8,9)
       if(nm2 %in% "COD.2532") selected_szgroups <-   c(2,5,7,8,9)
       if(nm2 %in% "SPR.2232") selected_szgroups <-   c(0,1,2,3,4)
       if(nm2 %in% "HER.3a22") selected_szgroups <-   c(2,3,4,5,6)
       avai[,paste(name.sp,'.nb_indiv.',range_szgroup, sep='')] <- avai[,paste(name.sp,'.nb_indiv.', selected_szgroups, sep='')] ## CAUTION HERE!
       if(nm3=="1") the_selected_szgroups <- rbind(the_selected_szgroups, cbind.data.frame(nm2, selected_szgroups))   # store for later use....

      
       # replace the selected groups in the catch rate equation                         
       write.table(cbind(nm2=nm2, selected_szgroups=selected_szgroups,
                   file=file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=''),
                       paste("the_selected_szgroups.dat",sep=' ')), append=TRUE,
                         quote = FALSE, sep=" ", col.names=FALSE, row.names=FALSE)
 
     

       # map avai according to pt_graph
       xx$idx                                  <- match( xx[,"pt_graph"], avai[,4])  # avai[,4] is the pt_graph
       xx                                      <- xx[!is.na(xx$idx),]
       col.avai.to.keep                        <- paste (name.sp,  paste('.nb_indiv.',range_szgroup, sep=''), sep='')
       xx                                      <- cbind(xx, avai[xx$idx, col.avai.to.keep]) #=> keep only some sz group bins...
       xx[,colnames(xx) %in% col.avai.to.keep] <- xx[,colnames(xx) %in% col.avai.to.keep]*1000
       # ==> !!CAUTION!! change of scale here i.e. *1000 to improve the glm.

       # debug in case of not informed avai for the range of szgroup chosen...
       for (a.col in col.avai.to.keep) xx[,a.col] <- replace( xx[,a.col],  is.na(xx[,a.col]), 0)

       # make the col names generic by removing the FAO species code...
       colnames(xx)[colnames(xx) %in% col.avai.to.keep] <-  paste('nb_indiv.', range_szgroup, sep='')

       ## STRONG ASSUMPTION (just to test the effect of some cpue on pt_graph with 0 availability)
       #idx <- apply(xx[,c('nb_indiv.2', 'nb_indiv.3', 'nb_indiv.4')], 1, sum, na.rm=T)==0
       #xx[idx,a_pop] <- 0 # remove the catches!

       # EXPERT KNOWLEDGE: remove records if 0 landings
       idx  <- which(xx[,a_pop]<=0)
       xx   <- xx[-idx, ] ## EXPERT KNOWLEDGE


       # compute cpue kg per hour
       xx$cpue <- ceiling( xx[,a_pop] / (xx$LE_EFF_VMS) )
       # EXPERT KNOWLEDGE: remove records if avai say 0 on this node
       idx <- which(apply(xx[,paste('nb_indiv.',range_szgroup, sep='')], 1, sum, na.rm=TRUE) <=0)  ## EXPERT KNOWLEDGE
       if(length(idx)!=0){
                 cat(paste("percent records with avai at 0: ", round(nrow(xx[idx, ])/ nrow(xx[, ])*100),"%\n",sep=''))
                 # avai at 0 while some landings is explained by insufficient coverage of the bits and ibts surveys (in 2010)
                 # (or too small threshold distance for interpolation...) and should be removed to make the glm significant...
                 # in the particualr case of the SPR-2232, this leads to also remove an entire metier! so need to be avoided:
                # if(nm2=="SPR.2232") xx[idx, 'nb_indiv.0'] <- 1 else xx <- xx[-idx, ] # to avoid removing a metier (met 34)!
                xx <- xx[-idx, ]
                 ##if(nm2=="SPR.2232") browser()
                  }

       if(nrow(xx)>5){
       # clean up
       xx <- xx[! (is.na(xx$cpue) | is.infinite(xx$cpue)),]
       for(a.col in  paste('nb_indiv.',range_szgroup, sep='') ) xx <- xx[!is.na(xx[,a.col]),]

       # EXPERT KNOWLEDGE: clean up unrealistic cpue for DEMERSAL spp
       if(!name.sp %in% c("HER","SPR","NOP","MAC", "MUS", "OYF")) xx <- xx[xx$cpue<5000,] # 5000 kg a hour

       # EXPERT KNOWLEDGE: a lot of variables for a very ugly outcome response cpue? filter out low cpue for these species...
       #if(name.sp %in% c("HER","SPR","NOP","MAC")) xx <- xx[xx$cpue>5,] # 5 kg a hour
        xx <- xx[xx$cpue>1,] # 1 kg a hour

       if(nrow(xx)>2){
       # get the vessel effect (beta), the metier effect (gamma) and the avai effect (delta) for this species
       # but first, decide on the metier of ref
       # ( e.g. arbitrary take the metier with highest landings for this species given the area)
       ref.metier        <- ""
       sum_this_sp       <- tapply(xx[,paste("LE_KG_",name.sp,sep='')], xx$LE_MET_level6, sum, na.rm=T)
       sum_this_sp       <- sum_this_sp[!is.na(sum_this_sp)]
       ref.metier        <- names(which.max(round(sum_this_sp[order(sum_this_sp)])))
       if(length(sum_this_sp[sum_this_sp>1]) <= 1){
          # only one metier for some species, so need to contrast the glm by fake landings for a closer metier
          # then look at combined_met_names and find another one to contrast upon:
          if(name.sp %in% c('NEP')){  xx$LE_MET_level6 <- as.character(xx$LE_MET_level6) ;  xx$LE_MET_level6[1] <- "9";  xx$LE_MET_level6 <- factor( xx$LE_MET_level6)} #fake, to contrast
       }
 
       # check again...
       sum_this_sp       <- tapply(xx[,paste("LE_KG_",name.sp,sep='')], xx$LE_MET_level6, sum, na.rm=T)
       sum_this_sp       <- sum_this_sp[!is.na(sum_this_sp)]
       if(length(sum_this_sp[sum_this_sp>1]) <= 1){
          print("only one met: need some fake (but relevant) metier to contrast the glm!!!!")
          browser()
       }
       
       
       #if(nm2=="PRA.kask") { xx$VE_REF <- as.character(xx$VE_REF) ;  xx$VE_REF[1] <-  "SWE_39";   xx$VE_REF <- factor( xx$VE_REF) } # fake to contrast!
       #if(nm2=="SOL.nsea") { xx$VE_REF <- as.character(xx$VE_REF) ;  xx$VE_REF[1] <-  "DNK000011752";   xx$VE_REF <- factor( xx$VE_REF) } # fake to contrast!
       #if(nm2=="MUS.kask") { xx$VE_REF <- as.character(xx$VE_REF) ;  xx$VE_REF[1] <-  "DNK000033712";   xx$VE_REF <- factor( xx$VE_REF) } # fake to contrast!



       if(length(ref.metier)==0) ref.metier <- levels(factor(xx$LE_MET_level6))[1] # if unusual fleet for this sp
       xx$LE_MET_level6                     <-  relevel(factor(xx$LE_MET_level6), ref=ref.metier)

       # choose the reference VE_REF with median cpue (removing 0 cpue before)
       ref.vid       <-  ""
       ref.vid       <-  xx[as.numeric(xx[,a_pop]) >=   median(xx[xx [,a_pop] !=0,a_pop],na.rm=T) , "VE_REF"][1]
       xx$VE_REF     <-  relevel(factor(xx$VE_REF), ref=as.character(ref.vid))


       # refactorize
       xx$VE_REF        <- as.factor(xx$VE_REF)
       xx$LE_MET_level6 <- as.factor( xx$LE_MET_level6)


      ## VISUALIZE THE SPATIAL AVAI
      if(FALSE){
       coord           <- read.table(file=file.path(general$inPathGraph, paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
       coord           <- as.matrix(as.vector(coord))
       coord           <- matrix(coord, ncol=3)
       colnames(coord) <- c('x', 'y', 'idx.port')
       
       a_xx            <- xx[!duplicated(xx$pt_graph),]
       xy              <- coord[a_xx$pt_graph, 1:2]
       a_col           <- "nb_indiv.5"   # for a given szgroup
       a_sum           <- sum(a_xx[,a_col])
       plot(xy[,1], xy[,2], pch=16, cex=sqrt((a_xx[,a_col])/a_sum)*30, xlim=c(1,15), ylim=c(52,60))
       map(add=TRUE)
       a_xx            <- xx[xx$LE_MET_level6=="PTM_SPF_16_31_0_0",] # color for a given metier to see the spatial coverage...
       xy              <- coord[a_xx$pt_graph, 1:2]
       points(xy[,1], xy[,2], pch=16, cex=sqrt((a_xx[,a_col])/a_sum)*30, col=2)
       # CAUTION: it is likely that the spatial avai is lacking of contrast
         # 1- when the ad hoc inverse-distance used to interpolate is not optimal (taking the "maximum" seems actually perform better)
         # 2- when a given vessel metier occur in a limited spatial coverage where the avai is likely to be almost equal!!
       # => as the point 2 is likely then main part of the effect on the catch rates is actually explained by the vessel-metier combination and not by the avai.....
       }



      # multiply avai with selectivity ogive for the catch equation model
      # multiply avai with absolute number at szgroup for the catch equation model
      # Log(E(CPUE (s,VES,MET,n))) =  beta(s,VES)+ gamma(s,MET)  + sum(z in szgroups) delta(s,z)×SEL(MET,s,z)  × RAV(n,s,z)     × N(s,z)
      # load
      # reuse the exported metier names in GenerateVesselConfigFiles.R
  
      xx$idx_met         <- factor(xx$LE_MET_level6)
      levels(xx$idx_met) <- metier_names[match( levels(xx$idx_met), metier_names$name), "idx"]
      
      xx$idx_spp         <- (which(nm2 %in% spp)  -1) 
      
      xx <- lapply (split(xx, f=xx$idx_met),
                function(x) {
                # browser()
                            sel <- read.table(
                                    file=file.path(general$main.path, "metiersspe",
                                         paste(x$idx_met[1], "metier_selectivity_per_stock_ogives.dat",sep=''))
                                        )
                            sel_this_met_this_sp <- sel[x$idx_spp[1] +1,]
                            sels <- matrix(sel_this_met_this_sp, ncol=length(sel_this_met_this_sp), nrow=nrow(x), byrow=TRUE)
                            colnames(sels)  <- paste("sel_", 0:13, sep='') # 14 szgroups


                            cbind.data.frame(x, data.frame(sels))
                            }
              )
       xx <-  do.call("rbind.data.frame", xx)       
       
       dd <- as.matrix(xx[, paste("nb_indiv.", range_szgroup, sep='')]) * data.matrix(xx[, paste("sel_", selected_szgroups, sep='')])       
       colnames (dd) <-  paste("sel_and_avai_", range_szgroup, sep='')
       xx <- cbind.data.frame(xx,dd)
                              
   

      ### DO A GLM ###
      # to relate cpue to N (a proxy of N) also accounting for vessel and metier effect.

     #library(pscl)
      # glm3  <- zeroinfl(cpue ~ VE_REF + LE_MET_level6   | 1 , data=xx, dist = "negbin")
      # to run on xx with 0s still present!  but optim fails....


      # start with a poisson glm
      glm1  <- glm(
                   as.formula(paste("(ceiling(cpue)) ~ VE_REF + LE_MET_level6 +",
                               paste('sel_and_avai_',range_szgroup, sep='', collapse="+"), "-1"  )),
                   family=poisson, # offset=log(LE_EFF_VMS),
                    data=xx, x=TRUE
                   )
      # then do a negative binomial glm because poisson likely to does not properly fit due to overdispersed response
      library(MASS) # for glm.nb()
       er <- try({glm.nb(
                   as.formula(paste("(ceiling(cpue)) ~ VE_REF + LE_MET_level6 +",
                               paste('sel_and_avai_',range_szgroup, sep='', collapse="+"), "-1" )),
                    data=xx, x=TRUE, control=list(epsilon=1e-05, maxit=100, trace=5), init.theta=2,
                   )  # careful intepretation of the significance needed here because no intercept
      }, silent=TRUE)
      if(class(er)!="try-error"){
      glm2 <- er
      }  else{
      glm2 <- glm1   # but use the poisson one in case the glm.nb does not converge!
      }

       #http://www.unc.edu/courses/2006spring/ecol/145/001/docs/lectures/lecture27.htm


      #The Poisson regression results indicated that WSSTA has a significant relationship to prevalence. The estimated coefficient of WSSTA is 0.268846. Since our link function is a log, this coefficient reflects changes in the log mean prevalence. The reported model is: log ? = 2.198 + 0.269*WSSTA. To understand the effect of WSSTA on the original scale we need to exponentiate this equation.
      # mu= exp(2.198+0.269*WSSTA)=exp(2.198)exp(0.269*WSSTA)=exp(2.198)exp(0.269)^WSSTA=exp(2.198)*1.308^WSSTA
      # Thus we see that each one unit increase in WSSTA multiplies the predicted mean prevalence by 1.3

      #Unfortunately we can also see from the output that this model does not fit.
      #For a Poisson model the residual deviance can be used as a goodness of fit statistic.
      #The reported residual deviance is 2716.8 on 45 degrees of freedom. For a good-fitting model the ratio of the residual deviance to its degrees of freedom should be approximately 1.
      #Since the ratio here is far greater than 1, we conclude the data are overdispersed.
      #This is additional evidence that a negative binomial error distribution would be a more appropriate choice here.


      #Observe that the AIC has decreased tremendously, from 2914.9 in the Poisson model to 413.98 in the negative binomial model.
      #Interestingly there has been little change in the parameter estimates.
      # Thus our interpretation of the effect of WSSTA on the mean prevalence is roughly the same as it was for Poisson model.

      #=> poisson regression i.e. mixing categorical to continuous variables
                        # e.g. nb_indiv.1 + nb_indiv.2 + nb_indiv.3 are actually proxies for N
       vars <- c("VE_REF","LE_MET_level6",  paste('sel_and_avai_',range_szgroup, sep='') )


       ## by hand:
       ##beta <- coefficients(glm2)
       ##beta[is.na(beta)] <- 0
       ##eta <- as.numeric(glm2$x %*% beta )
       ##mu <- exp(eta)
       ##all.equal(mu, predict(glm2, type = "response"))

       #Pearson goodness of fit (should be different from 0 in case the model fits)
       # We test for goodness-of-fit of the model with a chi-square test based on the residual deviance and degrees of freedom.
       # The GOF test indicates that the Poisson model fits the data if p > 0.05.
       res1        <- rbind(res1,c(nm2, nm3, pchisq(glm1$deviance, glm1$df.resid, lower=FALSE))) # i.e. 1- pchisq(glm1$deviance, glm1$df.resid, lower=TRUE)
       pearson.nb  <- residuals(object = glm2, type="pearson")
       res2        <- rbind(res2, c(stock=nm2,
                             semester=nm3,
                             p_from_deviance_residuals=pchisq(summary(glm2)$deviance,summary(glm2)$df.residual, lower=FALSE),
                             p_from_pearson_residuals  =pchisq(sum(pearson.nb^2),summary(glm2)$df.residual, lower=FALSE)
                               ))
      #The information on deviance is also provided. We can use the residual deviance to perform a goodness of fit test for the overall model. The residual deviance is the difference between the deviance of the current model and the maximum deviance of the ideal model where the predicted values are identical to the observed. Therefore, if the residual difference is small enough, the goodness of fit test will not be significant, indicating that the model fits the data. We conclude that the model fits reasonably well because the goodness-of-fit chi-squared test is not statistically significant. If the test had been statistically significant, it would indicate that the data do not fit the model well. In that situation, we may try to determine if there are omitted predictor variables, if our linearity assumption holds and/or if there is an issue of over-dispersion.
       with(glm1, cbind(res.deviance = deviance, df = df.residual, p = pchisq(deviance,
          df.residual, lower.tail = FALSE)))




   # get the decomposition
    pred <- predict(glm2,type="terms") # caution: in terms of cpue => not the betas
    #check exp( 1.411228-0.1094064+1.172626) = response 1

    # get back the Beta factors one by one on the right scale => e.g. exp(metier effect)
    ## SHOULD WE KEEP THE SIGNIFICANT ONES ONLY AND THEN PUT THE OTHERS TO 0??
     smry.hat         <- summary(glm2)$coefficients[,"Estimate"]
     smry.hat.se      <- summary(glm2)$coefficients[,"Std. Error"]
      for (a.var in vars){
       idx1 <-  which(regexpr(a.var, names(smry.hat))==1) # idx to keep only the setofvessels estimates
       idx2 <-  which(regexpr(a.var, names(smry.hat.se))==1) # idx to keep only the setofvessels estimates
       ref  <-  paste(a.var, levels(xx[,a.var])[1],sep="")  # get the reference level for this factor
       if(!a.var %in%  paste('sel_and_avai_',range_szgroup, sep='')  ) {
           smry.var.hat           <-  c(smry.hat[idx1], 0)
           smry.var.hat.se        <-  c(smry.hat.se[idx2], 0)
           names(smry.var.hat)    <-  c(names(smry.hat)[idx1], ref)
           names(smry.var.hat.se) <-  c(names(smry.hat.se)[idx2], ref)
           beta.                  <-  smry.var.hat
           beta.se.               <-  smry.var.hat.se
           an_order               <-  order( beta. , decreasing=TRUE)
           beta.                  <-  beta. [an_order]
           beta.se.               <-  beta.se. [an_order]
           nms                    <-  names( beta. )
           nms.se                 <-  names( beta.se. )
           beta.                  <-  matrix( beta. )
           beta.se.               <-  matrix( beta.se. )
           rownames( beta. )      <-  sub(a.var, "", nms)
           rownames( beta.se. )   <-  sub(a.var, "", nms.se)
           if(length(beta.)==0) {  beta. <- 0 }
           if(length(beta.se.)==0) {  beta.se. <- 0 }
           
          if(a.var %in%  "VE_REF"){
            assign( paste("beta.",a.var, sep=''), beta. )
            assign( paste("beta.se.",a.var, sep=''), beta.se. )
          } else{
          assign( paste("gamma.",a.var, sep=''), beta. )
          assign( paste("gamma.se.",a.var, sep=''), beta.se. )
          }

       
       } else{
       delta.     <- smry.hat[idx1]
       delta.se. <- smry.hat.se[idx2]
       assign( paste("delta.",a.var, sep=''), delta. )
       assign( paste("delta.se.",a.var, sep=''), delta.se. )
       }
     }


    # then map on xx and predict response to compare with observed data
    idx                      <- which(!xx$LE_MET_level6 %in%   rownames(gamma.LE_MET_level6))
    if(length(idx)>0) xx     <- xx[- idx ,]   ## caution: some levels removed becase singularities
    xx$beta.VE_REF           <- beta.VE_REF[as.character(xx$VE_REF), 1]
    xx$beta.se.VE_REF        <- beta.se.VE_REF[as.character(xx$VE_REF), 1]
    xx$gamma.LE_MET_level6    <- gamma.LE_MET_level6[as.character(xx$LE_MET_level6), 1]
    xx$gamma.se.LE_MET_level6 <- gamma.se.LE_MET_level6[as.character(xx$LE_MET_level6), 1]
    ## NO intecept...xx$beta.intercept <- summary(glm1)$coefficients[1]
    for(rg in range_szgroup){
       a.name <- paste('delta.sel_and_avai_',rg,sep='')
       if(length(get(a.name))>0) xx[,a.name] <- get(a.name)     else xx[,a.name] <- 0
       a.name <- paste('delta.se.sel_and_avai_',rg,sep='')
       if(length(get(a.name))>0) xx[,a.name] <- get(a.name)     else xx[,a.name] <- 0
    }


    xx$response <-  exp(xx$beta.VE_REF*1 +             
                            xx$gamma.LE_MET_level6*1 +
                              xx[,paste("delta.sel_and_avai_",range_szgroup[1], sep='')] * xx[,paste("sel_and_avai_",range_szgroup[1], sep='')] +
                              xx[,paste("delta.sel_and_avai_",range_szgroup[2], sep='')] * xx[,paste("sel_and_avai_",range_szgroup[2], sep='')] +
                              xx[,paste("delta.sel_and_avai_",range_szgroup[3], sep='')] * xx[,paste("sel_and_avai_",range_szgroup[3], sep='')] +
                              xx[,paste("delta.sel_and_avai_",range_szgroup[4], sep='')] * xx[,paste("sel_and_avai_",range_szgroup[4], sep='')] +
                              xx[,paste("delta.sel_and_avai_",range_szgroup[5], sep='')] * xx[,paste("sel_and_avai_",range_szgroup[5], sep='')] 
                             )


    # should be the same as returned by:
    xx$response1 <- predict(glm1, xx, type="response")   # poisson
    xx$response2 <- predict(glm2, xx, type="response")   # negative binomial
    xx$se2       <- predict(glm2, xx, type="response", se.fit=TRUE)$se.fit   # standart error negative binomial
 #browser()


     ## then , fit a normal distrubtion on the residuals....
     ## (should be reused for stochastic catch rate in the c++ code)

     #  residus <- round(xx$response1 - xx$cpue)
     #  norm.nll <- function(par,data) -sum(dnorm(data, par[1],par[2],log=T))
     #  opt <- optim(c(1,1), norm.nll,data=residus, method='BFGS')
     #  print(rnorm(100, mean=opt$par[1], sd = opt$par[2]) )



     #     lnorm.nll <- function(par,data) {
     # sum(dlnorm(data, par[1],par[2],log=T))
     #   }
     #       opt <- optim(c(0.1,0.1), lnorm.nll,data=abs(residus)+1,control=list(fnscale=-1),hessian=T)
     #       print(rlnorm(100, mean=opt$par[1], sd = opt$par[2]) )


    # plot
    graphics.off()
    for(met in levels(xx$LE_MET_level6)) {  # plot per metier
    print(met)
       xxx <- xx[xx$LE_MET_level6==met,]   # keep only the start of the curve
       if(nrow(xxx)>0){
           plot(xxx$cpue, xxx$response1, ylim=c(0, max(xxx$cpue)), xlim=c(0, max(xxx$cpue)))
           coeffs1 <- coefficients(lm(xxx$response1~xxx$cpue))
           if(!is.na(coeffs1[1]) && !is.na(coeffs1[2]) ) abline(a=coeffs1[1], b=coeffs1[2])
           points(xxx$cpue, xxx$response2, col=2)
           coeffs2 <- coefficients(lm(xxx$response2~xxx$cpue))
           if(!is.na(coeffs2[1]) && !is.na(coeffs2[2]) ) abline(a=coeffs2[1], b=coeffs2[2], col=2)
           abline(a=0, b=1, lty=2)
           title(nm2)
          }
   # browser()
  #anova(glm1,test = "Chisq")

    }
 #browser()
    # plot all
   plot(xx$cpue, xx$response2, ylim=range(xx$cpue), xlim=range(xx$cpue))
          coeffs <- coefficients(lm(xx$response1~xx$cpue))
          if(!is.na(coeffs[1]) && !is.na(coeffs[2]) ) abline(a=coeffs[1], b=coeffs[2])
          abline(a=0, b=1, lty=2)
          title(nm2)

    #=> in case of a perfect model, the two lines should overlap...
    # save the plot
    savePlot(filename=file.path(general$inPathPop, "CatchRates",
       paste("cpue_vs_response_from_glm_on_",nm2,"_semester", nm3,
         "_",years,"_",method, "_", threshold, "_",general$application,".jpeg",sep='')), type="jpeg")


    # The betas coeffs should then be used in the IBM catch equation
    # i.e. catch= exp(beta1*vid + beta2*metier +beta3*szgroup2*1000 + beta4*szgroup3*1000 + beta5*szgroup4*1000)
    # neglecting the intercept (which should be 0 in the ideal case anyway)
    # Would be ideal to also draw the predictions in the c++ code using the confidence intervals around the estimates from mean+/-2*beta.se.
    betas.vid            <- xx[!duplicated(data.frame(xx$VE_REF, xx$beta.VE_REF)), c("VE_REF", "beta.VE_REF")]
    betas.vid            <- cbind(betas.vid,  pop=nm2, semester=nm3 ) # add the pop name + semester
    all.betas.vid        <- rbind(all.betas.vid, betas.vid)
    all.betas.vid[,2]    <- round(all.betas.vid[,2], 5)
    betas.vid.se         <- xx[!duplicated(data.frame(xx$VE_REF, xx$beta.se.VE_REF)), c("VE_REF", "beta.se.VE_REF")]
    betas.vid.se         <- cbind(betas.vid.se,  pop=nm2, semester=nm3 ) # add the pop name + semester
    all.betas.vid.se     <- rbind(all.betas.vid.se, betas.vid.se)
    all.betas.vid.se[,2] <- round(all.betas.vid.se[,2], 5)
    gammas.met            <- xx[!duplicated(data.frame(xx$LE_MET_level6, xx$gamma.LE_MET_level6)), c("LE_MET_level6", "gamma.LE_MET_level6")]
    gammas.met            <- cbind(gammas.met,  pop=nm2, semester=nm3 ) # add the pop name + semester
    all.gammas.met        <- rbind(all.gammas.met, gammas.met)
    all.gammas.met[,2]    <- round(all.gammas.met[,2], 5)
    gammas.met.se         <- xx[!duplicated(data.frame(xx$LE_MET_level6, xx$gamma.se.LE_MET_level6)), c("LE_MET_level6", "gamma.se.LE_MET_level6")]
    gammas.met.se         <- cbind(gammas.met.se,  pop=nm2, semester=nm3 ) # add the pop name + semester
    all.gammas.met.se     <- rbind(all.gammas.met.se, gammas.met.se)
    all.gammas.met.se[,2] <- round(all.gammas.met.se[,2], 5)

    # particular cases because continuous variables...
    for (rg in range_szgroup){
      # mean estimate
      deltas         <- data.frame(delta.sel_and_avai_=xx[1, paste('delta.sel_and_avai_',rg,sep='')], pop=nm2, semester= nm3 ) # add the pop name  + semester
      all.deltas     <- get(paste('all.deltas',rg,sep=''))
      all.deltas     <- rbind(all.deltas, deltas)
      all.deltas[,1] <- round(all.deltas[,1], 5)
      assign (paste('all.deltas',rg,sep=''), all.deltas) # send back...
      # se
      deltas.se         <- data.frame(delta.sel_and_avai_=xx[1, paste('delta.se.sel_and_avai_',rg,sep='')], pop=nm2, semester= nm3 ) # add the pop name  + semester
      all.deltas.se     <- get(paste('all.deltas.se',rg,sep=''))
      all.deltas.se     <- rbind(all.deltas.se, deltas.se)
      all.deltas.se[,1] <- round(all.deltas.se[,1], 5)
      assign (paste('all.deltas.se',rg,sep=''), all.deltas.se) # send back...
      }

    } else{ cat("this stock has been filtered out...\n")}
    } else{ cat("this stock has been filtered out...\n")}
    } else{ cat("this stock is not found in the merged table...\n")}
    }  # end semester

  } # end for loop spp
   #----------------------------------------------

   # the goodness of fit table
   res2 <- as.data.frame(res2)
   res2 <- cbind.data.frame(res2, "significance (p>0.05)"=NA)
   res2 <- cbind.data.frame(res2, "significance (p>0.05)"=NA)
   res2 <- cbind.data.frame(res2, "significance (p>0.05)"=NA)
   res2 <- cbind.data.frame(res2, "p"=NA)
   res2[as.numeric(as.character(res2[,3])) >0.05, 5] <- "signif."
   res2[as.numeric(as.character(res2[,4])) >0.05, 6] <- "signif."
   res2[ !(is.na(res2[,5]) & is.na(res2[,6]) ) , 7] <- "s."
   res2[ is.na(res2[,7]) , 7] <- "n.s."
   res2[  , 8] <- round( as.numeric( apply(res2, 1, function (x) { max(c(as.numeric(as.character(x[3])), as.numeric(as.character(x[4]))), na.rm=TRUE)}) ) , 3)


  
    
    ## merge with all combi to get all pop informed even if no betas...(required fro Cpp)
    all.combi           <- expand.grid(VE_REF=levels(factor(ping_fgrounds$VE_REF)),
                                pop=spp,
                                 semester=levels(factor(ping_fgrounds$semester)))
    all.betas.vid       <- merge(all.combi, all.betas.vid, all=TRUE)
    all.betas.vid.se    <- merge(all.combi, all.betas.vid.se, all=TRUE)
    all.combi           <- expand.grid(LE_MET_level6=metier_names$name,
                                pop=spp,
                                  semester=levels(factor(ping_fgrounds$semester)))
    all.gammas.met       <- merge(all.combi, all.gammas.met, all=TRUE)
    all.gammas.met.se    <- merge(all.combi, all.gammas.met.se, all=TRUE)
    
    
    
    ## convert string name into integer name to speed up the c++
    # mean estimates
    levels(all.betas.vid$pop)  <- 0: (length(spp)-1)
    levels(all.gammas.met$pop) <- 0: (length(spp)-1)
    rownames(metier_names) <- metier_names$name
    levels(all.gammas.met$LE_MET_level6) <- metier_names[levels(all.gammas.met$LE_MET_level6), 'idx']
    for (obj_name in paste('all.deltas', range_szgroup,sep='')){obj <- get(obj_name); levels(obj$pop) <- 0: (length(spp)-1); assign (obj_name, obj, .GlobalEnv)}
  
  
    
    # order to get some multimap for Cpp
    #   i.e. vessel/beta per pop i.e metier/beta per pop
    #   i.e. avai_szgroup2/beta per pop, avai_szgroup3/beta per pop, avai_szgroup4/beta per pop
    library(doBy)
    all.betas.vid       <- orderBy(~semester+VE_REF+pop, data=all.betas.vid)
    all.betas.vid.se    <- orderBy(~semester+VE_REF+pop, data=all.betas.vid.se)
    all.gammas.met       <- orderBy(~semester+LE_MET_level6+pop, data=all.gammas.met)
    all.gammas.met.se    <- orderBy(~semester+LE_MET_level6+pop, data=all.gammas.met.se)

      #replace NA by 0
    all.betas.vid        <- replace(all.betas.vid, is.na(all.betas.vid), 0)
    all.gammas.met       <- replace(all.gammas.met, is.na(all.gammas.met), 0)
    all.betas.vid.se     <- replace(all.betas.vid.se, is.na(all.betas.vid.se), 0)
    all.gammas.met.se    <- replace(all.gammas.met.se, is.na(all.gammas.met.se), 0)

 
   # ...do the same for the continous variables
    for (rg in range_szgroup){
      # mean estimate
      all.deltas         <- get(paste('all.deltas',rg,sep=''))
      all.deltas         <- replace(all.deltas, is.na(all.deltas), 0)
      all.deltas$pop     <- factor(all.deltas$pop)
      all.deltas         <- orderBy(~semester+pop, data=all.deltas)
     assign (paste('all.deltas',rg,sep=''), all.deltas) # send back...
      # se
      all.deltas.se      <- get(paste('all.deltas.se',rg,sep=''))
      all.deltas.se      <- replace(all.deltas.se, is.na(all.deltas.se), 0)
      all.deltas.se$pop  <- factor(all.deltas.se$pop)
      all.deltas.se      <- orderBy(~semester+pop, data=all.deltas.se)
     assign (paste('all.deltas.se',rg,sep=''), all.deltas.se) # send back...
    }




  
    # save and export temporarly
    save(list=c('all.betas.vid','all.gammas.met', paste('all.deltas', range_szgroup,sep=''),
                'all.betas.vid.se','all.gammas.met.se', paste('all.deltas.se', range_szgroup,sep='')),
              file=file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=''), paste("betas", general$application,"_INTEGER_",
                                  general$application ,".RData", sep='')))


    # the goodness of fit table
    write.table(res2,
                   file=file.path(general$inPathPop, "CatchRates",
                       paste("goodness_of_fit_table_", general$application,".csv",sep=' ')),
                         quote = FALSE, sep=" ", col.names=FALSE, row.names=FALSE)
 

     # then, export PER SEMESTER:
    for(semester in c(1,2)){

      ## VESSEL SPE----------
      # export betas specific to the vessel given this pop
      # mean estimates
      all.betas.vid <-  replace(all.betas.vid, is.na(all.betas.vid), 0)
      all.betas.vid <-  all.betas.vid[order(all.betas.vid$VE_REF,all.betas.vid$semester, all.betas.vid$pop),] # order
          write.table(all.betas.vid[all.betas.vid$semester==semester, c('VE_REF','beta.VE_REF')],
                   file=file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep=''),
                       paste("vesselsspe_betas_semester",semester,".dat",sep='')),
                         quote = FALSE, sep=" ", col.names=TRUE, row.names=FALSE)
      # s.e.
      all.betas.vid.se <-  replace(all.betas.vid.se, is.na(all.betas.vid.se), 0)
      all.betas.vid.se <-  all.betas.vid.se[order(all.betas.vid.se$VE_REF,all.betas.vid.se$semester, all.betas.vid.se$pop),] # order
          write.table(all.betas.vid.se[all.betas.vid.se$semester==semester, c('VE_REF','beta.se.VE_REF')],
                   file=file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep=''),
                       paste("vesselsspe_betas_se_semester",semester,".dat",sep='')),
                         quote = FALSE, sep=" ", col.names=TRUE, row.names=FALSE)






      ## METIER SPE----------
      # export betas specific to the metier given this pop
      # mean estimates
      all.gammas.met <- replace(all.gammas.met, is.na(all.gammas.met), -20)
        #  better to replace NA by an arbitrary value (eg -20 because exp(-20) close to 0)
        # so that no catch when the metier is with NA cpue (because no observed landings for this metier this species)
        # otherwise: risk of unrealistic cacth when eg the beta_vid is very high for certain vessel...eg MAC.nsea and DNK000012028
        write.table(all.gammas.met[all.gammas.met$semester==semester, c('LE_MET_level6','gamma.LE_MET_level6')],
                    file=file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep=''),
                      paste("metierspe_betas_semester",semester,".dat",sep='')),
                         quote = FALSE, sep=" ", col.names=TRUE, row.names=FALSE)
      # s.e.
      all.gammas.met.se <- replace(all.gammas.met.se, is.na(all.gammas.met.se), 0)
        write.table(all.gammas.met.se[all.gammas.met.se$semester==semester, c('LE_MET_level6','gamma.se.LE_MET_level6')],
                    file=file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep=''),
                      paste("metierspe_betas_se_semester",semester,".dat",sep='')),
                         quote = FALSE, sep=" ", col.names=TRUE, row.names=FALSE)


      ## POP SPE----------
      # (caution: give the order for naming stocks in integer from 0 to n-1)
      for(rg in  c('0', '2', '3', '5', '7')){  # caution the naming  c('0', '2', '3', '5', '7') cannot be negotiate...but could correspond to other sizes ()
        # export betas specific to the avai szgroup given this pop (caution: remenber the scaling i.e *1000)
        # mean estimates
        all.deltas <- get(paste('all.deltas',rg,sep=''))
        all.deltas <- replace(all.deltas, is.na(all.deltas), 0)
           write.table(all.deltas[all.deltas$semester==semester, c('pop','delta.sel_and_avai_')],
                    file=file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=''),
                       paste("avai",rg,"_betas_semester",semester,".dat",sep='')),
                         quote = FALSE, sep=" ", col.names=TRUE, row.names=FALSE)
        # s.e.
        all.deltas.se <- get(paste('all.deltas.se',rg,sep=''))
        all.deltas.se <- replace(all.deltas.se, is.na(all.deltas.se), 0)
           write.table(all.deltas.se[all.deltas.se$semester==semester, c('pop','delta.sel_and_avai_')],
                    file=general$main.path.ibm, paste("popsspe_", general$application, sep=''),
                       paste("avai",rg,"_betas_se_semester",semester,".dat",sep='')),
                         quote = FALSE, sep=" ", col.names=TRUE, row.names=FALSE)
       }
      }




