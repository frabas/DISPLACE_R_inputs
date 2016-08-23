 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!GET THE FILES FOR AVAI ON NODE        !!!!!!!!!!!!!!!!!!!!##
 ##!!!! e.g. pop0spe_avai_szgroup_nodes_semester1.dat !!!!!!!!!!##
 ##!!!!!and list of node with presence           !!!!!!!!!!!!!!!##
 ##!!!!!!!!!lst_idx_nodes_per_pop.dat     !!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

 ## POP SPE
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
  general$case_study_countries  <- c("DEN")    # for the myfish app
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



 # load avai built using get_spatial_avai_keys_on_igraph_nodes_from_surveys_per_size_group.r
  load(file.path(general$main.path, "avai",
        paste("lst_avai_igraph",general$igraph,"_",years,"_",method,"_",threshold,".RData",sep=""))) 
                       # HERE WE use several years for smoothing the spatial distribution 

 # load outputs from glm  to get the pop to keep
 load(file.path(general$main.path, "popsspe", paste("betas",general$case_study,"_INTEGER_",general$case_study,".RData", sep='')) )
 pops      <- as.character(unique(all.betas0$pop))
 pop_names <- read.table(file.path(general$main.path, "popsspe",paste("pop_names_",general$case_study,".txt", sep='')))

 # load shape file
 library(sp)
 library(maptools)
 sh1 <- readShapePoly(file.path(general$main.path,"shp","francois_EU"))


 #-----------
 call_make_avai_files <-
    function (lst.avai, add_stochastic_variation=FALSE, num="", general=general){

  # save in .dat files
 idx_node_semester1 <- NULL
 idx_node_semester2 <- NULL
 for (sp in 1: length(lst.avai)){
  nm <- names(lst.avai)
  if(nm[sp] %in% pop_names[,1]) for (j in 1: length(lst.avai[[sp]])){
    avai <- lst.avai[[sp]][[j]]
    nm2 <- names(lst.avai[[sp]])
    options(scipen=999) # suppress the scientific notation
    avai         <- signif(avai,6)
    avai         <- replace(avai, is.na(avai),0)
    avai[,4]     <- avai[,4] -1 ## CAUTION for Cpp: idx_node must start at 0
    idx_with_0s  <- which (apply(avai[,-c(1:4)], 1, sum)==0)
    # plot
    for(szgroup in 0:7){
        if(any(  avai[,grep(paste('nb_indiv.',szgroup,sep=''), colnames(avai))[1]  ] !=0 )){

          if(add_stochastic_variation==FALSE){
           library(PBSmapping)  # addBubbles()
           plot(avai[,1],avai[,2], xlab="Longitude", ylab="Latitude", col=2, type="n")
           addBubbles(data.frame(EID=1:nrow(avai), X=avai[,1], Y=avai[,2], Z=avai[,grep(paste('nb_indiv.',szgroup,sep=''), colnames(avai))[1] ]),
                   legend.pos="bottomleft", legend.type=c("nested"),
                        legend.title= paste(nm[sp], " availability", szgroup, " semest.", j), legend.cex=0.8, symbol.zero="+")
          plot(sh1, add=TRUE)
          savePlot(file.path(general$main.path,"popsspe", "jpeg_avai",
                paste("presence_area_",nm[sp],"_sz",szgroup, " semest.", j,".jpeg",sep='')), type="jpeg")
           } else{
             ## add a multivariate lognormal error on availability key
             ## to enjoy a stcohastic variation in spatial distribution.
             ## call it several time and store stochastic output files somewhere so that we do not have to bother
             ## doing it in c++ i.e. we´ll just pick up a file randomly from c++ afterward
             avai_per_szgroup_per_quarter <-  avai[,grep(paste('nb_indiv.',szgroup,sep=''), colnames(avai))[1]]

             #note that the stochastic variation can only add on node where avai!=0
             # i.e. cannot create some new zones for the distrubtion of the stocks!!
             # more elaborated process needed if required e.g. erosion/dilatation knowing neighbouring nodes, etc.
             # eg add a fake epsilon to enable avai on 0 nodes as well,
             # but only those node that are on the frontiers of the current distrib:
             # those can be identified using the "graph" object i.e. all the links with avai!=0 in dep and avai==0 in arr  (dilation order 1)

             for(i in 1: nrow(avai)) {points(as.numeric(as.character(avai[i,"x"])), as.numeric(as.character(avai[i,"y"])), col=2) ; for(j in 1: 1000000) {} }

             library(compositions)
             corr <- diag(length(avai_per_szgroup_per_quarter))
             corr[lower.tri(corr)] <- 1 # i.e. correlated
             corr[upper.tri(corr)] <- 1  # i.e. correlated
             diag(corr ) <-1.2 # corr*sd    ## HERE WE ARE.....
             stochast_avai <- as.vector(rlnorm.rplus(1,log(avai_per_szgroup_per_quarter),corr)  )
             stochast_avai <-  stochast_avai/sum(stochast_avai) # rescale the avai key to 1
             # a plot to check the magnitude of the change:
             # drawns <- rlnorm.rplus(1,log(avai_per_szgroup_per_quarter),corr)
             #plot(avai_per_szgroup_per_quarter, as.vector(drawns)/  sum(as.vector(drawns)) )
             #abline(a=0,b=1 )


             # then, replace back:
             avai[,grep(paste('nb_indiv.',szgroup,sep=''), colnames(avai))] <- stochast_avai

           }
         }
    }
    # then remove node if pop not present...
    if(length(idx_with_0s)!=0) avai         <- avai[-idx_with_0s, ]  # remove null avai

    # brute force to fill in the 0s i.e. for the size groups where the avai is not informed
    sp_name                                <- strsplit(nm[sp], split="\\.")[[1]][1]
    the_avai                               <-  avai[,-c(1:4)]
    szgroups_with_all_0_avai               <-  which(apply(the_avai, 2, sum)==0)-1
    szgroups_with_some_avai                <-  which(apply(the_avai, 2, sum)!=0)-1
    closer_szgroup                         <-  sapply(szgroups_with_all_0_avai, function (x) {  which.min(abs(x-szgroups_with_some_avai))    })
    if(length(szgroups_with_all_0_avai)!=0)
       the_avai[, szgroups_with_all_0_avai+1] <-  the_avai[, paste(sp_name,".nb_indiv.",closer_szgroup, sep='') ]

    # then, reshape for c++
    full_avai2               <- cbind(rep(as.numeric(as.character(avai[,4])), each=14), c(t(the_avai)))   # CAUTION HARDCODING 14 szgroup





    colnames(full_avai2) <- c("idx_node","avai")
    if(add_stochastic_variation==TRUE){
       is_stoch <- paste("_",as.character(num),sep='') ; stoch_folder <- "stochast_avai"
    } else{
       is_stoch <- ""; stoch_folder <- "static_avai"
       }
    write.table(full_avai2, file = file.path(general$main.path,"popsspe", stoch_folder,
                            paste(pop_names[ pop_names[,1]==nm[sp], 2], ## caution: convert string name into integer
                              "spe_full_avai_szgroup_nodes_semester",nm2[j],is_stoch,".dat",sep="")),
                               row.names = FALSE, col.names = TRUE, quote=FALSE )
                               ##=> pop0spe_full_avai_szgroup_nodes_semester1 => a c++ multimap

    ## change 6sep11: for speedup c++: get rid of the szgroups that are actually not used for the do_catch caluclation
    ## so that the multimap for availability will be smaller!
    ## cf. also range_szgroup for the glm poisson model

    the_selected_szgroups  <- read.table(file=file.path(general$main.path,"popsspe", paste("the_selected_szgroups.dat",sep=' ')), header=TRUE)
    idx                    <- the_selected_szgroups [ the_selected_szgroups[,1]== nm[sp] , "selected_szgroups"]
    if(length(idx)==0) idx <- c(0,2,3,5,7) # default

    # select the 5 relevant szgroup for this stock  (to feed the catch rate equation only)
    sp_name      <- strsplit(nm[sp], split="\\.")[[1]][1]
    avai2        <- cbind(rep(as.numeric(as.character(avai[,4])), each=5), c(t(avai[, paste(sp_name,".nb_indiv.", idx, sep="") ])))   # CAUTION 5 szgroups ONLY i.e. coded 0,2,3,5,7 in c++ but note that could correpond to other sz according to the_selected_szgroups table!!!


    colnames(avai2) <- c("idx_node","avai")
    write.table(avai2, file = file.path(general$main.path,"popsspe",  stoch_folder,
                            paste(pop_names[ pop_names[,1]==nm[sp], 2], ## caution: convert string name into integer
                              "spe_avai_szgroup_nodes_semester",nm2[j],is_stoch,".dat",sep="")),
                               row.names = FALSE, col.names = TRUE, quote=FALSE )
                               ##=> pop0spe_avai_szgroup_nodes_semester1 => a c++ multimap



    # inform list of presence nodes
    assign( paste("idx_node_semester",nm2[j],sep=''),
               rbind(get(paste("idx_node_semester",nm2[j],sep='')), cbind(pop_names[ pop_names[,1]==nm[sp], 2], avai[,4]))
        ) # bind each time PER SEMESTER


 }}

   # export lst_idx_nodes_per_pop_quarterXX.dat
  for(se in c('semester1', 'semester2') ){
     idx_node <- get(paste("idx_node_",se,sep=''))
     colnames(idx_node) <- c("stock", "idx_node")
     write.table(idx_node, file = file.path(general$main.path,"popsspe", stoch_folder,
                            paste("lst_idx_nodes_per_pop_",se,is_stoch,".dat",sep='')),
                               row.names = FALSE, col.names = TRUE, quote=FALSE )
     }


  return()
  }

  # calls------
   # baseline
   cat("if it still doesn't exist, 'static_avai' folder is created in ",
                      file.path(general$main.path,"popsspe","\n"))
   dir.create(file.path(general$main.path,"popsspe","static_avai"),
                      showWarnings = TRUE, recursive = TRUE, mode = "0777")
   call_make_avai_files(lst.avai, add_stochastic_variation=FALSE, num="", general=general)

   
   if(general$case_study!= "myfish"){
   
   # add a multivariate lognormal error
   cat("if it still doesn't exist, 'stochast_avai' folder is created in ",
                      file.path(general$main.path,"popsspe","\n"))
   dir.create(file.path(general$main.path,"popsspe","stochast_avai"),
                      showWarnings = TRUE, recursive = TRUE, mode = "0777")
   n<-50
   increment <- sprintf("%03d", 1:n)
   for(i in 1:n)  call_make_avai_files(lst.avai, add_stochastic_variation=TRUE, num=increment[i], general=general)
   }






