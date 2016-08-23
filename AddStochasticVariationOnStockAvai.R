
   # GENERAL SETTINGS
   general <- list()
   if(.Platform$OS.type == "windows") {
     general$application           <- "balticRTI" # ...or myfish etc.
     general$main.path.param       <- file.path("C:","Users","fbas","Documents","GitHub",paste("DISPLACE_input_", general$application, sep=''))
     general$main.path.ibm         <- file.path("C:","Users","fbas","Documents","GitHub",paste("DISPLACE_input_", general$application, sep=''))
   }

    dir.create(file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=''), "stochastic_avai"))


##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
call_make_avai_files <- function(pid=0, num="1", general=general){


   an <- function(x) as.numeric(as.character(x))
   options(scipen=999)
   for (a.semester in c("1", "2")){

    # input (a static)
     popsspe_avai_semester_this_pop <-
         read.table(file=file.path( general$main.path.ibm, paste("popsspe_", general$application, sep=''), "static_avai",
              paste(pid, "spe_full_avai_szgroup_nodes_semester",gsub("Q","",a.semester),".dat",sep='')),
                  header=TRUE)


       ## do it szgroup per szgroup
       avai_this_szgroup <- NULL
       for (a.szgroup in 1:14){

             popsspe_avai_semester_this_pop_lst      <- split(popsspe_avai_semester_this_pop, popsspe_avai_semester_this_pop$pt_graph)
             lst_this_szgroup                        <- lapply(popsspe_avai_semester_this_pop_lst, function (x, szgroup) x[szgroup,], a.szgroup)
             this_szgroup                            <- cbind(do.call("rbind", lst_this_szgroup), szgroup=(a.szgroup-1))

             ## add a multivariate lognormal error on availability key
             ## to enjoy a stcohastic variation in spatial distribution.
             ## call it several time and store stochastic output files somewhere so that we do not have to bother
             ## doing it in c++ i.e. we´ll just pick up a file randomly from c++ afterward
             #note that the stochastic variation can only add on node where avai!=0
             # i.e. cannot create some new zones for the distrubtion of the stocks!!
             # more elaborated process needed if required e.g. erosion/dilatation knowing neighbouring nodes, etc.
             # eg add a fake epsilon to enable avai on 0 nodes as well,
             # but only those node that are on the frontiers of the current distrib:
             # those can be identified using the "graph" object i.e. all the links with avai!=0 in dep and avai==0 in arr  (dilation order 1)


             if(FALSE){
             library(compositions)
             corr <- diag(length(this_szgroup[, "abundance"]))
             corr[lower.tri(corr)] <- 1 # i.e. correlated
             corr[upper.tri(corr)] <- 1  # i.e. correlated
             diag(corr ) <-1.2 # corr*sd    ## HERE WE ARE.....
             stochast_avai <- as.vector(rlnorm.rplus(1,log(this_szgroup[, "abundance"]),corr)  )
             } else{
             res <- NULL
             for (i in 1:length(this_szgroup[, "abundance"])) {
                                    res <- c( res, rlnorm(1,log(this_szgroup[i, "abundance"]), sdlog=1.2)  )
                                    }
             stochast_avai <- res
             }



             stochast_avai <-  stochast_avai/sum(stochast_avai) # rescale the avai key to 1



             # a plot to check the magnitude of the change:
              #plot(density (this_szgroup[, "abundance"]), col=2)
              #lines(density (stochast_avai), col=2)

              # plot(this_szgroup[, "abundance"],stochast_avai, xlim=c(0,0.005), ylim=c(0,0.005))

             # drawns <- rlnorm.rplus(1,log(this_szgroup),corr)
             #plot(this_szgroup, as.vector(drawns)/  sum(as.vector(drawns)) )
             #abline(a=0,b=1 )


             # then, replace back:
             this_szgroup[,2] <- stochast_avai

        avai_this_szgroup <- rbind(avai_this_szgroup , this_szgroup)
        } # end szgroup

        # order back with pt_graph, szgroup
         library(doBy)
         avai_this_szgroup <- orderBy(~pt_graph, data=avai_this_szgroup)

        # output (a stochastic draw)
          write.table(avai_this_szgroup[,c('pt_graph', 'abundance')],  # the szgroup dim is implicit....
            file=file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=''),  "stochast_avai",
              paste(pid, "spe_full_avai_szgroup_nodes_semester",gsub("Q","",a.semester),paste("_",as.character(num),sep=''),".dat",sep='')),
                  col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE, append=FALSE)


        the_selected_szgroups  <- read.table(file=file.path(general$main.path.param, paste("popsspe_", general$application, sep=''),
                                    paste("the_selected_szgroups.dat",sep=' ')), header=TRUE)
        idx                    <- the_selected_szgroups [ the_selected_szgroups[,1]== pid , "selected_szgroups"]

        # output 2
        write.table(avai_this_szgroup[avai_this_szgroup$szgroup %in% idx, c('pt_graph', 'abundance')],  # the szgroup dim is implicit....
            file=file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=''),  "stochast_avai",
              paste(pid, "spe_avai_szgroup_nodes_semester",gsub("Q","",a.semester),paste("_",as.character(num),sep=''),".dat",sep='')),
                  col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE, append=FALSE)




   } # end semester


return()
}


##--CALLS------------


    # add a multivariate lognormal error
   cat("if it still doesn't exist, 'stochast_avai' folder is created in ",
                      file.path(general$main.path,"popsspe","\n"))
   dir.create(file.path(general$main.path.ibm, paste("popsspe_", general$application, sep='') ,"stochast_avai"),
                      showWarnings = TRUE, recursive = TRUE, mode = "0777")
   n<-50
   increment <- sprintf("%03d", 1:n)
   #for(i in 1:n)  call_make_avai_files(pid=0, num=increment[i], general=general)
   for(i in 1:n)  call_make_avai_files(pid=1, num=increment[i], general=general)
   for(i in 1:n)  call_make_avai_files(pid=2, num=increment[i], general=general)
   #for(i in 1:n)  call_make_avai_files(pid=3, num=increment[i], general=general)


