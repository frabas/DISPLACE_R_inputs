 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!MERGE WITH AVAI!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!AND DO THE GLMs!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!PER POP AND SEMESTER!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##


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


  # load avai object built using get_spatial_avai_keys_on_igraph_nodes_from_surveys_per_size_group.r
  load(file.path(general$main.path, "avai",
        paste("lst_avai_igraph",general$igraph,"_",year,"_",method,"_",threshold,".RData",sep=""))) ##!! e.g. 2010 !!##
  lst.avai1      <- lst.avai

  # load avai object built using get_spatial_avai_keys_on_igraph_nodes_from_surveys_per_size_group.r
  load(file.path(general$main.path, "avai",
        paste("lst_avai_igraph",general$igraph,"_",years,"_",method,"_",threshold,".RData",sep=""))) ##!!e.g.  2005-2010 !!##
  lst.avai2     <- lst.avai

  #lst.avai  <-  lst.avai1 # one year...
  lst.avai  <-  lst.avai2 # ...or a period.



  # load for Denmark
  if("DEN" %in% general$case_study_countries){
     load(file.path(general$main.path,"merged_tables", general$case_study, paste("x.agg.DEN.",year,".igraph",general$igraph,".RData",sep='')))
     x_agg_den <- x.agg ; x_agg_den$country <- "DEN"
     load(file.path(general$main.path,"merged_tables", general$case_study, paste("metier_names.DEN.",year,".igraph",general$igraph,".RData",sep='')))
     metier_names_den           <- metier_names
     colnames(metier_names_den) <- c("met", "idx")
      combined_met_names <-  data.frame(metier_names_den)
     # save
     save(combined_met_names,  file=file.path(general$main.path, "merged_tables", general$case_study, ## SAVE
             paste("combined_met_names.", general$a.year,".igraph",
              general$igraph,".RData", sep='')))
     write.table(combined_met_names[,c(1,2)],
                 paste(general$main.path, "\\metiersspe\\combined_met_names.txt",sep=''),
                  quote=FALSE, col.names=FALSE, row.names=FALSE)
     }


  # load for Germany
  if("DEU" %in% general$case_study_countries){
     load(file.path(general$main.path,"merged_tables", general$case_study, paste("x.agg.DEU.",year,".igraph",general$igraph,".RData",sep='')))
     x_agg_deu <- x.agg ; x_agg_deu$country <- "DEU"
     load(file.path(general$main.path,"merged_tables", general$case_study, paste("metier_names.DEU.",year,".igraph",general$igraph,".RData",sep='')))
     metier_names_deu <- metier_names  ; colnames(metier_names_deu) <- c("met", "idx")
     combined_met_names <-  data.frame(metier_names_deu)
    # save
     save(combined_met_names,  file=file.path(general$main.path, "merged_tables", general$case_study, ## SAVE
             paste("combined_met_names.", general$a.year,".igraph",
              general$igraph,".RData", sep='')))
    write.table(combined_met_names[,c(1,2)],
                 paste(general$main.path, "\\metiersspe\\combined_met_names.txt",sep=''),
                  quote=FALSE, col.names=FALSE, row.names=FALSE)
    }


  # load for SWE
  if("SWE" %in% general$case_study_countries){
     load(file.path(general$main.path,"merged_tables", general$case_study, paste("x.agg.SWE.",year,".igraph",general$igraph,".RData",sep='')))
     x_agg_swe <- x.agg ; x_agg_swe$country <- "SWE"
     load(file.path(general$main.path,"merged_tables", general$case_study, paste("metier_names.SWE.",year,".igraph",general$igraph,".RData",sep='')))
     metier_names_swe <- metier_names  ; colnames(metier_names_swe) <- c("met", "idx")
     combined_met_names <-  data.frame(metier_names_swe)
    # save
     save(combined_met_names,  file=file.path(general$main.path, "merged_tables", general$case_study, ## SAVE
             paste("combined_met_names.", general$a.year,".igraph",
              general$igraph,".RData", sep='')))
    write.table(combined_met_names[,c(1,2)],
                 paste(general$main.path, "\\metiersspe\\combined_met_names.txt",sep=''),
                  quote=FALSE, col.names=FALSE, row.names=FALSE)
    }


  if("DEN" %in% general$case_study_countries &&
      "DEU" %in% general$case_study_countries &&
        "SWE" %in% general$case_study_countries  ){
     # rename the metiers because of the combination of dnk and ger
     combined_met_names        <- merge(metier_names_den, metier_names_deu, by="met", all=TRUE, suffixes = c(".den",".deu"))
     combined_met_names        <- merge(combined_met_names, metier_names_swe, by="met", all=TRUE)
     colnames(combined_met_names) <- c('met', 'idx.den', 'idx.deu', 'idx.swe')
     combined_met_names$idx    <- 0:(nrow(combined_met_names)-1) # dont forget the c++ offset...
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
     save(combined_met_names,  file=file.path(general$main.path, "merged_tables", general$case_study, ## SAVE
             paste("combined_met_names.", general$a.year,".igraph",
              general$igraph,".RData", sep='')))
     write.table(combined_met_names[,c('met','idx')],
                 paste(general$main.path, "\\metiersspe\\combined_met_names.txt",sep=''),
                  quote=FALSE, col.names=FALSE, row.names=FALSE)


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
   } else{
    x.agg  <- x_agg_den
   }# end combine




   ## SELECTIVITY ###################
  # by default, create a fake selectivity ogive i.e. all at 1 (and not species-specific...)
  sel <- NULL
  for (met in unique(combined_met_names$idx) ) {

    the_met <- as.character(combined_met_names[combined_met_names[, 'idx']==met, 'met'])

    clupeid <- FALSE  ; gadoid <- FALSE; trawl <- FALSE ; gillnet <- FALSE
    if (length (grep("SPF", the_met))!=0)  clupeid <- TRUE
    if ((length (grep("DEF", the_met))!=0 || length (grep("MCD", the_met))!=0 || length (grep("CRU", the_met))!=0 || length (grep("FWS", the_met))!=0) && clupeid==FALSE)
                    {trawl <- TRUE; gillnet <- FALSE ; gadoid <- TRUE}
    if (length (grep("GNS", the_met))!=0 && clupeid==FALSE)  {trawl <- FALSE; gillnet <- TRUE ; gadoid <- TRUE}
    if (length (grep("FPO", the_met))!=0 || length (grep("LHP", the_met))!=0 || length (grep("LLS", the_met))!=0)  {trawl <- FALSE; gillnet <- TRUE ; gadoid <- TRUE}

    #### TO DO : PER METIER
    L50 <- 1   # default: will generate ogive at 1
    L75 <- 1.1  # default: will generate ogive at 1
    if(clupeid){
     L50         <- 16 # 36mm trawl, Suuronen and millar 1992
     L75         <- 18 # 36mm trawl, Suuronen and millar 1992
    }
    if(gadoid && trawl){
     L50         <- 38
     L75         <- 41
    }
    if(gadoid && gillnet ){
     L50         <- 44 # gillnet Madsen 2007
     L75         <- 46 # gillnet Madsen 2007
    }
    cat(paste('the_met is ', the_met, ' then clupeid is ',
                 clupeid,', gadoid is ',gadoid,
                  ', trawl is ', trawl, ', gillnet is ',gillnet, '\n', sep=''))

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
    l           <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,1000) *a_size_group_bin_in_cm  # vector of 14 length groups of eg 10 cm bin or 5 cm
    length.fish <-  l + mid # i.e. mid size in cm
    equ.sel     <- paste("1/(1+exp(S1-S2*length.fish))")  # cf. Rasmus paper
    S1          <- L50*log(3) / (L75 - L50)      # L75=L50+(1/2*SR)
    S2          <-  S1/L50
    # eval(parse("",text=equ.sel)) # a sigmoid....
    ogive              <- rep(met, 14)
    sel <-  rbind (sel, cbind(ogive, eval(parse("",text=equ.sel))  )  )  # ...assuming 14 szgroup bins
  }
  colnames(sel) <- c("met_name","ogive")



  # save the .dat file
  write.table(sel,
          file=file.path(general$main.path, "metiersspe",
                 paste("metier_selectivity_ogives.dat",sep='')),
                   col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)


  #-------------------------------
  #-------------------------------


  ## create metier specific gear widths files by hand
  ## from our BENTHIS analyses

  ##   metiersspe\metier_gear_widths_param_a.dat
  ##     metiersspe\metier_gear_widths_param_b.dat
  ##     metiersspe\metier_gear_widths_model_type.dat    i.e. lm or nls power curve


   gear_param_per_metier <- data.frame(
a_metier=c('OT_CRU','OT_CRU','OT_DMF','OT_DMF','OT_MIX','OT_MIX','OT_MIX_ARA','OT_MIX_ARA','OT_MIX_DMF_BEN','OT_MIX_DMF_BEN','OT_MIX_DMF_PEL','OT_MIX_DMF_PEL','OT_MIX_DPS','OT_MIX_DPS','OT_MIX_NEP','OT_MIX_NEP','OT_MIX_TGS_CTC','OT_MIX_TGS_CTC','OT_MIX_TGS_OCC','OT_MIX_TGS_OCC','OT_SPF','OT_SPF','TBB_CRU','TBB_CRU','TBB_DMF','TBB_DMF','TBB_MOL','TBB_MOL','DRB_MOL','DRB_MOL','SDN_DEM','SDN_DEM','SSC_DEM','SSC_DEM'),
param=c('a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b'),
Estimate=c(5.10393560454806,0.468985756915913,9.6053549509854,0.433672763959314,10.6607888271164,0.292055014993337,37.5271604597435,0.149004797319136,3.21410379943408,77.981158829069,6.63707197355847,0.770594580782091,26.6738247840508,0.210221545999405,3.92727763464472,35.8253721834011,6.23686411376723,0.767375050454527,0.0192465419797634,119.140335982507,0.965238378524667,68.3889717127507,1.48117115311386,0.457788539321641,0.660086393453441,0.507845311175148,0.953001905566232,0.709356826689359,0.314245137194503,1.24544036138755,1948.83466676682,0.236271746198865,4461.27004311913,0.117589220782479),
Std..Error=c(1.81527145191998,0.0597519960969362,3.98228885098937,0.067572002767068,6.69386377505425,0.104413257104915,10.6717875588847,0.044963446750424,1.67854244656697,40.9297885227685,2.69086696344053,0.126123213329976,5.37466576335144,0.030829495804396,0.928442484509969,21.0228522096513,1.46159830273852,0.0732116002636393,0.000552819642352548,0.510207569180525,0.205245990518183,7.45180177818494,0.278399892100703,0.0346555048025894,0.172902115850281,0.0388684340513048,0.315715856194751,0.138412196798781,0.110027479611801,0.10614681568516,637.25152416296,0.0636712369543136,1665.50234108383,0.118756519107319),
t.value=c(2.81166521907769,7.84887179593252,2.41201864314765,6.41793562718951,1.59262112068153,2.79710664230959,3.51648308708138,3.31390958851994,1.91481830322951,1.90524216331315,2.46651806415295,6.10985527910701,4.96288066244663,6.81884476260001,4.22996329893018,1.70411568450042,4.26715336360309,10.4816046595234,34.8152281598731,233.513462322532,4.70283670871103,9.17750817164227,5.32030074414718,13.2096918492278,3.81768835047121,13.0657517744299,3.01854305657162,5.12495894939517,2.8560604887363,11.733186279291,3.05818753329251,3.71080816866175,2.67863330664306,0.990170658978435),
Pr...t..=c(0.00613312535554725,1.21619365805854e-11,0.021410083292817,2.48114253493853e-07,0.114790848188445,0.00631861326022122,0.000513087659147687,0.0010462790834138,0.0692370736030276,0.0705334706657513,0.0147045751318625,7.39218704723967e-09,1.2637878625965e-05,2.97113026239585e-08,0.000166717383514359,0.097483711710908,0.000314181622785133,5.0948672020349e-10,9.05842416252619e-12,5.10054218622276e-20,0.000204968683311441,5.36482029322678e-08,0.00313939649832079,4.44157761915604e-05,0.000458495488420268,5.11509704563588e-16,0.00678642704689924,5.16047183433098e-05,0.0075895814688592,6.18091407283774e-13,0.00391206507124518,0.000614325243514857,0.0438919330122769,0.367557330382699),
equ=c('DoS=a*(kW^b)','DoS=a*(kW^b)','DoS=a*(kW^b)','DoS=a*(kW^b)','DoS=a*(kW^b)','DoS=a*(kW^b)','DoS=a*(kW^b)','DoS=a*(kW^b)','DoS=(a*LOA)+b','DoS=(a*LOA)+b','DoS=a*(LOA^b)','DoS=a*(LOA^b)','DoS=a*(kW^b)','DoS=a*(kW^b)','DoS=(a*LOA)+b','DoS=(a*LOA)+b','DoS=a*(LOA^b)','DoS=a*(LOA^b)','DoS=(a*kW)+b','DoS=(a*kW)+b','DoS=(a*LOA)+b','DoS=(a*LOA)+b','beamw=a*(kW^b)','beamw=a*(kW^b)','beamw=a*(kW^b)','beamw=a*(kW^b)','beamw=a*(LOA^b)','beamw=a*(LOA^b)','dredgew=a*(LOA^b)','dredgew=a*(LOA^b)','seineropel=a*(kW^b)','seineropel=a*(kW^b)','seineropel=a*(LOA^b)','seineropel=a*(LOA^b)'),
nb_records=c(124,124,39,39,94,94,271,271,48,48,190,190,45,45,53,53,24,24,12,12,19,19,7,7,42,42,22,22,33,33,47,47,8,8)
)








  #-------------------------------
  #-------------------------------
  # keep only DEN (caution: DNK in VE_REF), DEU and SWE vessels
  x.agg <- x.agg[c(grep("DNK", x.agg$VE_REF), grep("DEU", x.agg$VE_REF), grep("SWE", x.agg$VE_REF)),]

  all.betas.vid    <- NULL  # VE_REF mean estimate
  all.betas.met    <- NULL  # LE_MET_level6 mean estimate
  all.betas.vid.se <- NULL  # VE_REF standard error
  all.betas.met.se <- NULL  # LE_MET_level6 standard error

  range_szgroup <- c(0,2,3,5,7)   # canadian journal - size group to be included in the glm e.g 0: 0-10cm, 1: 10-20cm, etc.
  the_selected_szgroups <- NULL


  for(rg in range_szgroup){
    assign(paste('all.betas' ,rg, sep=''),  NULL)   # init
    assign(paste('all.betas.se' ,rg, sep=''),  NULL)   # init
    }

  res1 <- NULL  # goodness of fit glm1
  res2 <- NULL   # goodness of fit glm2
  #----------------------------------------------
  # subset the x.agg to keep the distribution area of
  # the relevant pop and during the relevant semester only
  nm        <-  names(lst.avai)  # names of stocks
  for (i in 1: length(lst.avai)){  # PER STOCK
     nm2 <- names(lst.avai)[[i]]  # name of a given stock
     for (j in 1: length(lst.avai[[i]])){  # PER SEMESTER
       nm3        <- names(lst.avai[[i]])
       cat(paste("stock ",    nm2,    "\n"))
       cat(paste("semester ", nm3[j], "\n"))
       avai       <- lst.avai[[i]][[j]]
       name.sp    <- substr(nm2, 1,3) # FAO sp code
       a_pop      <- paste("LE_KG_", name.sp, sep='')
       if(a_pop %in% colnames(x.agg) && name.sp!="MZZ"){
            xx <- x.agg[
                       x.agg$pt_graph %in% avai[,4] &
                         x.agg$semester==as.numeric(nm3[[j]]),  # subset according to pt_graph and semester
                          c("VE_REF","LE_MET_level6", "LE_EFF_VMS", "semester", "pt_graph", a_pop)
                        ]  # subset the pop
       if(name.sp=="SAN") thrhold <- 200000 else  thrhold <- 5000  # unfortunate hardcoding...


       # Instead of making the code very complicated and furthermore refactor the c++ side,
       # hereafter an easy and DANGEROUS shortcut (dangerous because confusing...) as a way for changing the szgroups used for the catch rate equation....
       # i.e. select the 5 most relevant ones for this species.
       # e.g. try to contrast the spatial avai data for small vs. larger fish (i.e. >minimum landing size)
       selected_szgroups <-  range_szgroup #default
       if(nm2 %in% "COD.2224") selected_szgroups <-   c(2,6,7,8,9)
       if(nm2 %in% "COD.2532") selected_szgroups <-   c(2,6,7,8,9)
       if(nm2 %in% "SPR.2232") selected_szgroups <-   c(0,1,2,3,4)
       if(nm2 %in% "HER.3a22") selected_szgroups <-   c(2,3,4,5,6)
       avai[,paste(name.sp,'.nb_indiv.',range_szgroup, sep='')] <- avai[,paste(name.sp,'.nb_indiv.', selected_szgroups, sep='')] ## CAUTION HERE!
       if(nm3[j]==1) the_selected_szgroups <- rbind(the_selected_szgroups, cbind.data.frame(nm2, selected_szgroups))   # store for later use....



       if( table(xx[,a_pop]!=0)["TRUE"] >5  && # on more than 5 nodes
             !all(is.na(avai[,-c(1:4)]))   &&  # avai informed
               sum(xx[,a_pop], na.rm=TRUE)> thrhold ){  # at least 5 tons landed this semester!




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
       xx$cpue <- ceiling( xx[,a_pop] / (xx$LE_EFF_VMS/60) )
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
       # get the vessel effect, the metier effect and the avai effect for this species
       # but first, decide on the metier of ref
       # ( e.g. arbitrary take the metier with highest landings for this species given the area)
       ref.metier        <- ""
       sum_this_sp       <- tapply(xx[,paste("LE_KG_",name.sp,sep='')], xx$LE_MET_level6, sum, na.rm=T)
       sum_this_sp       <- sum_this_sp[!is.na(sum_this_sp)]
       ref.metier        <- names(which.max(round(sum_this_sp[order(sum_this_sp)])))
       cat(paste("ref metier is taken ", combined_met_names[combined_met_names$idx==ref.metier,'met'],"\n"))
       if(length(sum_this_sp[sum_this_sp>1]) <= 1){
          # only one metier for some species, so need to contrast the glm by fake landings for a closer metier
          # then look at combined_met_names and find another one to contrast upon:
          if(name.sp %in% c('NEP')){  xx$LE_MET_level6 <- as.character(xx$LE_MET_level6) ;  xx$LE_MET_level6[1] <- "9";  xx$LE_MET_level6 <- factor( xx$LE_MET_level6)} #fake, to contrast
          if(name.sp %in% c('MON', 'SOL', 'TUR')){  xx$LE_MET_level6 <- as.character(xx$LE_MET_level6) ;  xx$LE_MET_level6[1] <- "10";  xx$LE_MET_level6 <- factor( xx$LE_MET_level6)} #fake, to contrast
          if(name.sp %in% c('PRA')){  xx$LE_MET_level6 <- as.character(xx$LE_MET_level6) ;  xx$LE_MET_level6[1] <- "11";  xx$LE_MET_level6 <- factor( xx$LE_MET_level6)} #fake, to contrast
          if(name.sp %in% c('CSH')){  xx$LE_MET_level6 <- as.character(xx$LE_MET_level6) ;  xx$LE_MET_level6[1] <- "24";  xx$LE_MET_level6 <- factor( xx$LE_MET_level6)} #fake, to contrast
          if(name.sp %in% c('MUS')){  xx$LE_MET_level6 <- as.character(xx$LE_MET_level6) ;  xx$LE_MET_level6[1] <- "23";  xx$LE_MET_level6 <- factor( xx$LE_MET_level6)} #fake, to contrast
          #   if(name.sp %in% c('CSH','SOL', 'MON','HKE', 'HAD')){  xx$LE_MET_level6 <- as.character(xx$LE_MET_level6) ;  xx$LE_MET_level6[1] <- "17";  xx$LE_MET_level6 <- factor( xx$LE_MET_level6)} #fake, to contrast
          #   if(name.sp %in% c('SAN')){              xx$LE_MET_level6 <- as.character(xx$LE_MET_level6) ;  xx$LE_MET_level6[1] <- "51" ;  xx$LE_MET_level6 <- factor( xx$LE_MET_level6)} #fake, to contrast
          #    if(name.sp %in% c('MUS','OYF')){        xx$LE_MET_level6 <- as.character(xx$LE_MET_level6) ;  xx$LE_MET_level6[1] <-  "8";   xx$LE_MET_level6 <- factor( xx$LE_MET_level6)} #fake, to contrast
          #    if(name.sp %in% c('PRA','CSH')){        xx$LE_MET_level6 <- as.character(xx$LE_MET_level6) ;  xx$LE_MET_level6[1] <-  "9";   xx$LE_MET_level6 <- factor( xx$LE_MET_level6)} #fake, to contrast
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
       coord           <- read.table(file=file.path(general$main_path_input, "graphsspe", paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
       coord           <- as.matrix(as.vector(coord))
       coord           <- matrix(coord, ncol=3)
       colnames(coord) <- c('x', 'y', 'idx.port')
       
       a_xx            <- xx[!duplicated(xx$pt_graph),]
       xy              <- coord[a_xx$pt_graph, 1:2]
       a_col           <- "nb_indiv.0"   # for a given szgroup
       a_sum           <- sum(a_xx[,a_col])
       plot(xy[,1], xy[,2], pch=16, cex=sqrt((a_xx[,a_col])/a_sum)*30, xlim=c(1,15), ylim=c(52,60))
       map(add=TRUE)
       a_xx            <- xx[xx$LE_MET_level6=="11",] # color for a given metier to see the spatial coverage...
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
       sel <- read.table(file.path(general$main.path, "metiersspe",
                 paste("metier_selectivity_ogives.dat",sep='')), sep= ' ', header=TRUE)

       sel <- cbind.data.frame(sel, szgroup=rep(0:13, length=nrow(sel)))  # 14 szgroups
       sel <- sel[sel$szgroup %in%   selected_szgroups,]
       sel <- cbind.data.frame(sel, szgroup_alias=rep(range_szgroup, length=nrow(sel)))  # caution here: keep the 5 szgroup and relabel as agreed for avai in c++
       for(ii in 1:nrow(xx)){ # brute force (to avoid mistakes...)
          xx[ii, paste("nb_indiv.",range_szgroup, sep='')] <- xx[ii, paste("nb_indiv.",range_szgroup, sep='')] * sel[ sel$met_name==xx[ii, "LE_MET_level6"], "ogive"]                                                          }




      ### DO A GLM ###
      # to relate cpue to N (a proxy of N) also accounting for vessel and metier effect.

     #library(pscl)
      # glm3  <- zeroinfl(cpue ~ VE_REF + LE_MET_level6   | 1 , data=xx, dist = "negbin")
      # to run on xx with 0s still present!  but optim fails....


      # start with a poisson glm
      glm1  <- glm(
                   as.formula(paste("(ceiling(cpue)) ~ VE_REF + LE_MET_level6 +",
                               paste('nb_indiv.',range_szgroup, sep='', collapse="+"), "-1"  )),
                   family=poisson, # offset=log(LE_EFF_VMS),
                    data=xx, x=TRUE
                   )
      # then do a negative binomial glm because poisson likely to does not properly fit due to overdispersed response
      library(MASS) # for glm.nb()
       er <- try({glm.nb(
                   as.formula(paste("(ceiling(cpue)) ~ VE_REF + LE_MET_level6 +",
                               paste('nb_indiv.',range_szgroup, sep='', collapse="+"), "-1" )),
                    data=xx, x=TRUE, control=list(epsilon=1e-05, maxit=100, trace=5), init.theta=2,
                   )  # careful intepretation of the significance needed here because no intercept
      }, silent=TRUE)
      if(class(er)!="try-error"){
      glm2 <- er
      }  else{
      glm2 <- glm1   # but use the poisson one in case the glm.nb does not converge!
      }



     #if(nm2=="NOP.nsea") browser()

     #glm2 <- glm.nb(
     #              as.formula(paste("(ceiling(cpue)) ~ VE_REF -1" )),
     #               data=xx, x=TRUE, control=list(epsilon=1e-05, maxit=100, trace=5), init.theta=2,
     #              )


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
       vars <- c("VE_REF","LE_MET_level6",  paste('nb_indiv.',range_szgroup, sep='') )


       ## by hand:
       ##beta <- coefficients(glm2)
       ##beta[is.na(beta)] <- 0
       ##eta <- as.numeric(glm2$x %*% beta )
       ##mu <- exp(eta)
       ##all.equal(mu, predict(glm2, type = "response"))

       #Pearson goodness of fit (should be different from 0 in case the model fits)
       # We test for goodness-of-fit of the model with a chi-square test based on the residual deviance and degrees of freedom.
       # The GOF test indicates that the Poisson model fits the data if p > 0.05.
       res1        <- rbind(res1,c(nm2, nm3[j], pchisq(glm1$deviance, glm1$df.resid, lower=FALSE))) # i.e. 1- pchisq(glm1$deviance, glm1$df.resid, lower=TRUE)
       pearson.nb  <- residuals(object = glm2, type="pearson")
       res2        <- rbind(res2, c(stock=nm2,
                             semester=nm3[j],
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
       if(!a.var %in%  paste('nb_indiv.',range_szgroup, sep='')  ) {
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
       } else{
       beta. <- smry.hat[idx1]
       beta.se. <- smry.hat.se[idx2]
       }
       assign( paste("beta.",a.var, sep=''), beta. )
       assign( paste("beta.se.",a.var, sep=''), beta.se. )
      }


    # then map on xx and predict response to compare with observed data
    idx                      <- which(!xx$LE_MET_level6 %in%   rownames(beta.LE_MET_level6))
    if(length(idx)>0) xx     <- xx[- idx ,]   ## caution: some levels removed becase singularities
    xx$beta.VE_REF           <- beta.VE_REF[as.character(xx$VE_REF), 1]
    xx$beta.se.VE_REF        <- beta.se.VE_REF[as.character(xx$VE_REF), 1]
    xx$beta.LE_MET_level6    <- beta.LE_MET_level6[as.character(xx$LE_MET_level6), 1]
    xx$beta.se.LE_MET_level6 <- beta.se.LE_MET_level6[as.character(xx$LE_MET_level6), 1]
    ## NO intecept...xx$beta.intercept <- summary(glm1)$coefficients[1]
    for(rg in range_szgroup){
       a.name <- paste('beta.nb_indiv.',rg,sep='')
       if(length(get(a.name))>0) xx[,a.name] <- get(a.name)     else xx[,a.name] <- 0
       a.name <- paste('beta.se.nb_indiv.',rg,sep='')
       if(length(get(a.name))>0) xx[,a.name] <- get(a.name)     else xx[,a.name] <- 0
    }


    xx$response <-  exp(xx$beta.VE_REF*1 +              # canadian paper
                            xx$beta.LE_MET_level6*1 +
                              xx$beta.nb_indiv.0*xx$nb_indiv.0 +
                                xx$beta.nb_indiv.2*xx$nb_indiv.2 +
                                  xx$beta.nb_indiv.3*xx$nb_indiv.3  +
                                   xx$beta.nb_indiv.5*xx$nb_indiv.5  +
                                     xx$beta.nb_indiv.7*xx$nb_indiv.7)



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
   plot(xx$cpue, xx$response1, ylim=range(xx$cpue), xlim=range(xx$cpue))
          coeffs <- coefficients(lm(xx$response1~xx$cpue))
          if(!is.na(coeffs[1]) && !is.na(coeffs[2]) ) abline(a=coeffs[1], b=coeffs[2])
          abline(a=0, b=1, lty=2)
          title(nm2)

    #=> in case of a perfect model, the two lines should overlap...
    # save the plot
    savePlot(filename=file.path(general$main.path, "jpeg_glm",
       paste("cpue_vs_response_from_glm_on_",nm2,"_semester", nm3[j],
         "_",years,"_",method, "_", threshold, "_",general$case_study,".jpeg",sep='')), type="jpeg")


    # The betas coeffs should then be used in the IBM catch equation
    # i.e. catch= exp(beta1*vid + beta2*metier +beta3*szgroup2*1000 + beta4*szgroup3*1000 + beta5*szgroup4*1000)
    # neglecting the intercept (which should be 0 in the ideal case anyway)
    # Would be ideal to also draw the predictions in the c++ code using the confidence intervals around the estimates from mean+/-2*beta.se.
    betas.vid            <- xx[!duplicated(data.frame(xx$VE_REF, xx$beta.VE_REF)), c("VE_REF", "beta.VE_REF")]
    betas.vid            <- cbind(betas.vid,  pop=nm2, semester=nm3[j] ) # add the pop name + semester
    all.betas.vid        <- rbind(all.betas.vid, betas.vid)
    all.betas.vid[,2]    <- round(all.betas.vid[,2], 5)
    betas.vid.se         <- xx[!duplicated(data.frame(xx$VE_REF, xx$beta.se.VE_REF)), c("VE_REF", "beta.se.VE_REF")]
    betas.vid.se         <- cbind(betas.vid.se,  pop=nm2, semester=nm3[j] ) # add the pop name + semester
    all.betas.vid.se     <- rbind(all.betas.vid.se, betas.vid.se)
    all.betas.vid.se[,2] <- round(all.betas.vid.se[,2], 5)
    betas.met            <- xx[!duplicated(data.frame(xx$LE_MET_level6, xx$beta.LE_MET_level6)), c("LE_MET_level6", "beta.LE_MET_level6")]
    betas.met            <- cbind(betas.met,  pop=nm2, semester=nm3[j] ) # add the pop name + semester
    all.betas.met        <- rbind(all.betas.met, betas.met)
    all.betas.met[,2]    <- round(all.betas.met[,2], 5)
    betas.met.se         <- xx[!duplicated(data.frame(xx$LE_MET_level6, xx$beta.se.LE_MET_level6)), c("LE_MET_level6", "beta.se.LE_MET_level6")]
    betas.met.se         <- cbind(betas.met.se,  pop=nm2, semester=nm3[j] ) # add the pop name + semester
    all.betas.met.se     <- rbind(all.betas.met.se, betas.met.se)
    all.betas.met.se[,2] <- round(all.betas.met.se[,2], 5)

    # particular cases because continuous variables...
    for (rg in range_szgroup){
      # mean estimate
      betas         <- data.frame(beta.nb_indiv=xx[1, paste('beta.nb_indiv.',rg,sep='')], pop=nm2, semester= nm3[j] ) # add the pop name  + semester
      all.betas     <- get(paste('all.betas',rg,sep=''))
      all.betas     <- rbind(all.betas, betas)
      all.betas[,1] <- round(all.betas[,1], 5)
      assign (paste('all.betas',rg,sep=''), all.betas) # send back...
      # se
      betas.se         <- data.frame(beta.nb_indiv=xx[1, paste('beta.se.nb_indiv.',rg,sep='')], pop=nm2, semester= nm3[j] ) # add the pop name  + semester
      all.betas.se     <- get(paste('all.betas.se',rg,sep=''))
      all.betas.se     <- rbind(all.betas.se, betas.se)
      all.betas.se[,1] <- round(all.betas.se[,1], 5)
      assign (paste('all.betas.se',rg,sep=''), all.betas.se) # send back...
      }

    } else{ cat("this stock has been filtered out...\n")}
    } else{ cat("this stock has been filtered out...\n")}
    } else{ cat("this stock is irrelevant here...\n")}
    } else{ cat("this stock is not found in the merged table...\n")}
    }

  } # end for loop
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


    # subset for relevant populations
   pop.to.keep <- c(   "COD.nsea",  "COD.kat",    "COD.2224", "COD.2532",
                       "HER.nsea",      "HER.3a22",           "HER.2532",
                       "SPR.nsea",   "SPR.kask",                          "SPR.2232",
                       "NEP.nsea",   "NEP.kask",
                       "SOL.nsea",   "SOL.kask",  "SOL.2224",
                                     "LEM.kask",
                       "PLE.nsea",   "PLE.kask",                          "PLE.2232",
                                                                          "FLE.2232",
                       "HAD.nsea",
                       "HOM.nsea",
                       "MON.nsea",
                       "NOP.nsea",
                       "HKE.nsea",
                       "DAB.nsea",                                        "DAB.2232",
                                                                          "TUR.2232",
                       "CSH.nsea",
                       "PRA.nsea",   "PRA.kask",
                       "SAN.nsea",
                       "POK.nsea",
                       "MAC.nsea",
                                     "WHG.kask",  "WHG.2224",
                                     "MUS.kask",
                                                  "MUS.2224"
                      )


    #other.pops <- pop.to.keep[!pop.to.keep%in%all.betas.vid$pop]

    all.betas.vid     <- all.betas.vid[all.betas.vid$pop %in%  pop.to.keep,]
    all.betas.met     <- all.betas.met[all.betas.met$pop %in%  pop.to.keep,]
    all.betas.vid$pop <- factor(all.betas.vid$pop)
    all.betas.met$pop <- factor(all.betas.met$pop)
    all.betas.vid.se     <- all.betas.vid.se[all.betas.vid.se$pop %in%  pop.to.keep,]
    all.betas.met.se     <- all.betas.met.se[all.betas.met.se$pop %in%  pop.to.keep,]
    all.betas.vid.se$pop <- factor(all.betas.vid.se$pop)
    all.betas.met.se$pop <- factor(all.betas.met.se$pop)

    #replace NA by 0
    all.betas.vid       <- replace(all.betas.vid, is.na(all.betas.vid), 0)
    all.betas.met       <- replace(all.betas.met, is.na(all.betas.met), 0)
    all.betas.vid.se    <- replace(all.betas.vid.se, is.na(all.betas.vid.se), 0)
    all.betas.met.se    <- replace(all.betas.met.se, is.na(all.betas.met.se), 0)
    ## merge with all combi to get all pop informed even if no betas...(required fro Cpp)
    all.combi           <- expand.grid(VE_REF=levels(factor(all.betas.vid$VE_REF)),
                                pop=levels(factor(all.betas.vid$pop)),
                                 semester=levels(factor(all.betas.vid$semester)))
    all.betas.vid       <- merge(all.combi, all.betas.vid, all=TRUE)
    all.betas.vid.se    <- merge(all.combi, all.betas.vid.se, all=TRUE)
    all.combi           <- expand.grid(LE_MET_level6=levels(factor(all.betas.met$LE_MET_level6)),
                                pop=levels(factor(all.betas.met$pop)),
                                  semester=levels(factor(all.betas.met$semester)))
    all.betas.met       <- merge(all.combi, all.betas.met, all=TRUE)
    all.betas.met.se    <- merge(all.combi, all.betas.met.se, all=TRUE)
    # order to get some multimap for Cpp
    #   i.e. vessel/beta per pop i.e metier/beta per pop
    #   i.e. avai_szgroup2/beta per pop, avai_szgroup3/beta per pop, avai_szgroup4/beta per pop
    library(doBy)
    all.betas.vid       <- orderBy(~semester+VE_REF+pop, data=all.betas.vid)
    all.betas.vid.se    <- orderBy(~semester+VE_REF+pop, data=all.betas.vid.se)
    all.betas.met       <- orderBy(~semester+LE_MET_level6+pop, data=all.betas.met)
    all.betas.met.se    <- orderBy(~semester+LE_MET_level6+pop, data=all.betas.met.se)

    # ...do the same for the continous variables
    for (rg in range_szgroup){
      # mean estimate
      all.betas         <- get(paste('all.betas',rg,sep=''))
      all.betas         <- all.betas[all.betas$pop %in%  pop.to.keep,]
      all.betas         <- replace(all.betas, is.na(all.betas), 0)
      all.betas$pop     <- factor(all.betas$pop)
      all.betas         <- orderBy(~semester+pop, data=all.betas)
     assign (paste('all.betas',rg,sep=''), all.betas) # send back...
      # se
      all.betas.se      <- get(paste('all.betas.se',rg,sep=''))
      all.betas.se      <- all.betas.se[all.betas.se$pop %in%  pop.to.keep,]
      all.betas.se      <- replace(all.betas.se, is.na(all.betas.se), 0)
      all.betas.se$pop  <- factor(all.betas.se$pop)
      all.betas.se      <- orderBy(~semester+pop, data=all.betas.se)
     assign (paste('all.betas.se',rg,sep=''), all.betas.se) # send back...
    }

    ## convert string name into integer name to speed up the c++
    # mean estimates
    all.betas.vid$pop <- factor(all.betas.vid$pop)
    all.betas.met$pop <- factor(all.betas.met$pop)
    for (obj_name in paste('all.betas', range_szgroup,sep='')){obj <- get(obj_name); obj$pop <- factor(obj$pop); assign (obj_name, obj, .GlobalEnv)}
    pop_names         <- cbind(levels(all.betas.vid$pop), 0:(length(levels(all.betas.vid$pop))-1))
    # se
    all.betas.vid.se$pop <- factor(all.betas.vid.se$pop)
    all.betas.met.se$pop <- factor(all.betas.met.se$pop)
    for (obj_name in paste('all.betas.se', range_szgroup,sep='')){obj <- get(obj_name); obj$pop <- factor(obj$pop); assign (obj_name, obj, .GlobalEnv)}
    if(TRUE ){  #!#!#! CAUTION   #!#!#!
       write.table(pop_names, file.path(general$main.path,"popsspe",paste("pop_names_",general$case_study,".txt",sep="")), quote=FALSE, col.names=FALSE, row.names=FALSE)
       # cution: name of species can change across the different case studies
       # because some species can be filtered out...e.g. less than 10 tons, etc.
       }
    levels(all.betas.vid$pop)  <-  0:(length(levels(all.betas.vid$pop))-1) # change level names
    levels(all.betas.met$pop)  <-  0:(length(levels(all.betas.vid$pop))-1) # change level names
    levels(all.betas.vid.se$pop)  <-  0:(length(levels(all.betas.vid.se$pop))-1) # change level names
    levels(all.betas.met.se$pop)  <-  0:(length(levels(all.betas.vid.se$pop))-1) # change level names
    for (obj_name in paste('all.betas', range_szgroup,sep='')){obj <- get(obj_name); levels(obj$pop)   <-  0:(length(levels(obj$pop))-1); assign (obj_name, obj, .GlobalEnv)} # change level names
    for (obj_name in paste('all.betas.se', range_szgroup,sep='')){obj <- get(obj_name); levels(obj$pop)   <-  0:(length(levels(obj$pop))-1); assign (obj_name, obj, .GlobalEnv)} # change level names


    ## check that we do not have lost metiers along the process....
    ## if yes, then add the metier with NA (..should have a very little impact)
    dd<- combined_met_names[!combined_met_names$idx %in%  unique(as.character(all.betas.met$LE_MET_level6)),]
    if(nrow(dd)!=0){
    cat(paste('lost metier: ',dd$idx,  "\n", sep=''))
    all.betas.met$LE_MET_level6     <- as.character(all.betas.met$LE_MET_level6)
    all.betas.met                   <- rbind.data.frame(all.betas.met, expand.grid(LE_MET_level6=dd$idx, pop=0: (length(unique(all.betas.met$pop))-1), semester=1:2, beta.LE_MET_level6=NA ))
    all.betas.met$LE_MET_level6     <- as.factor(all.betas.met$LE_MET_level6)
    all.betas.met.se$LE_MET_level6  <- as.character(all.betas.met.se$LE_MET_level6)
    all.betas.met.se                <- rbind.data.frame(all.betas.met.se, expand.grid(LE_MET_level6=dd$idx, pop=0: (length(unique(all.betas.met$pop))-1), semester=1:2, beta.se.LE_MET_level6=NA ))
    all.betas.met.se$LE_MET_level6  <- as.factor(all.betas.met.se$LE_MET_level6)
    }

    ## check that we do not have lost some vessels along the process....
    dd <- x.agg[!x.agg$VE_REF %in%  unique(as.character(all.betas.vid$VE_REF)),]
    if(nrow(dd)!=0){
     cat("CHECK LOST VESSELS!")
     all.betas.vid$VE_REF  <- as.character(all.betas.vid$VE_REF)
     all.betas.vid         <- rbind.data.frame(all.betas.vid, expand.grid(VE_REF=dd$VE_REF, pop=0: (length(unique(all.betas.met$pop))-1), semester=1:2, beta.VE_REF=NA ))
     all.betas.vid$VE_REF  <- as.factor(all.betas.vid$VE_REF)
     }

    # order
    all.betas.vid     <- orderBy(~semester+VE_REF+pop, data=all.betas.vid)
    all.betas.met     <- orderBy(~semester+LE_MET_level6+pop, data=all.betas.met)
    all.betas.vid.se     <- orderBy(~semester+VE_REF+pop, data=all.betas.vid.se)
    all.betas.met.se     <- orderBy(~semester+LE_MET_level6+pop, data=all.betas.met.se)


    # save and export temporarly
    save(list=c('all.betas.vid','all.betas.met', paste('all.betas', range_szgroup,sep=''),
                'all.betas.vid.se','all.betas.met.se', paste('all.betas.se', range_szgroup,sep='')),
              file=file.path(general$main.path, "popsspe", paste("betas", general$case_study,"_INTEGER_",
                                  general$case_study ,".RData", sep='')))


    # the goodness of fit table
    res2 <- res2[res2[,1]%in%pop.to.keep,]
    write.table(res2,
                   file=file.path(general$main.path, "popsspe",
                       paste("goodness_of_fit_table_", general$case_study,".csv",sep=' ')),
                         quote = FALSE, sep=" ", col.names=FALSE, row.names=FALSE)
  
    # the selected groups in the catch rate equation
    the_selected_szgroups              <- the_selected_szgroups[the_selected_szgroups[,1]%in%pop.to.keep,]
    pop_names                          <- read.table(file.path(general$main.path, "popsspe", paste("pop_names_",general$case_study,".txt", sep='')))           
    the_selected_szgroups$nm2          <- factor(the_selected_szgroups$nm2)
    levels(the_selected_szgroups$nm2)  <- pop_names[,2][ match(levels(the_selected_szgroups$nm2), as.character(pop_names[,1]))] # map the name to integer

    write.table(the_selected_szgroups,
                   file=file.path(general$main.path,"popsspe",
                       paste("the_selected_szgroups.dat",sep=' ')),
                         quote = FALSE, sep=" ", col.names=TRUE, row.names=FALSE)


     # then, export PER SEMESTER:
    for(semester in c(1,2)){

      ## VESSEL SPE----------
      # export betas specific to the vessel given this pop
      # mean estimates
      all.betas.vid <-  replace(all.betas.vid, is.na(all.betas.vid), 0)
      all.betas.vid <-  all.betas.vid[order(all.betas.vid$VE_REF,all.betas.vid$semester, all.betas.vid$pop),] # order
          write.table(all.betas.vid[all.betas.vid$semester==semester, c('VE_REF','beta.VE_REF')],
                   file=file.path(general$main.path,"vesselsspe",
                       paste("vesselsspe_betas_semester",semester,".dat",sep='')),
                         quote = FALSE, sep=" ", col.names=TRUE, row.names=FALSE)
      # s.e.
      all.betas.vid.se <-  replace(all.betas.vid.se, is.na(all.betas.vid.se), 0)
      all.betas.vid.se <-  all.betas.vid.se[order(all.betas.vid.se$VE_REF,all.betas.vid.se$semester, all.betas.vid.se$pop),] # order
          write.table(all.betas.vid.se[all.betas.vid.se$semester==semester, c('VE_REF','beta.se.VE_REF')],
                   file=file.path(general$main.path,"vesselsspe",
                       paste("vesselsspe_betas_se_semester",semester,".dat",sep='')),
                         quote = FALSE, sep=" ", col.names=TRUE, row.names=FALSE)






      ## METIER SPE----------
      # export betas specific to the metier given this pop
      # mean estimates
      all.betas.met <- replace(all.betas.met, is.na(all.betas.met), -20)
        #  better to replace NA by an arbitrary value (eg -20 because exp(-20) close to 0)
        # so that no catch when the metier is with NA cpue (because no observed landings for this metier this species)
        # otherwise: risk of unrealistic cacth when eg the beta_vid is very high for certain vessel...eg MAC.nsea and DNK000012028
        write.table(all.betas.met[all.betas.met$semester==semester, c('LE_MET_level6','beta.LE_MET_level6')],
                    file=file.path(general$main.path, "metiersspe",
                      paste("metierspe_betas_semester",semester,".dat",sep='')),
                         quote = FALSE, sep=" ", col.names=TRUE, row.names=FALSE)
      # s.e.
      all.betas.met.se <- replace(all.betas.met.se, is.na(all.betas.met.se), 0)
        write.table(all.betas.met.se[all.betas.met.se$semester==semester, c('LE_MET_level6','beta.se.LE_MET_level6')],
                    file=file.path(general$main.path, "metiersspe",
                      paste("metierspe_betas_se_semester",semester,".dat",sep='')),
                         quote = FALSE, sep=" ", col.names=TRUE, row.names=FALSE)


      ## POP SPE----------
      for(rg in range_szgroup){
        # export betas specific to the avai szgroup given this pop (caution: remenber the scaling i.e *1000)
        # mean estimates
        all.betas <- get(paste('all.betas',rg,sep=''))
        all.betas <- replace(all.betas, is.na(all.betas), 0)
           write.table(all.betas[all.betas$semester==semester, c('pop','beta.nb_indiv')],
                    file=file.path(general$main.path, "popsspe",
                       paste("avai",rg,"_betas_semester",semester,".dat",sep='')),
                         quote = FALSE, sep=" ", col.names=TRUE, row.names=FALSE)
        # s.e.
        all.betas.se <- get(paste('all.betas.se',rg,sep=''))
        all.betas.se <- replace(all.betas.se, is.na(all.betas.se), 0)
           write.table(all.betas.se[all.betas.se$semester==semester, c('pop','beta.nb_indiv')],
                    file=file.path(general$main.path, "popsspe",
                       paste("avai",rg,"_betas_se_semester",semester,".dat",sep='')),
                         quote = FALSE, sep=" ", col.names=TRUE, row.names=FALSE)
       }
      }




