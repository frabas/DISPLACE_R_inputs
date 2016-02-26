
 
 
 
     general <- list()
     general$main.path      <- file.path("C:","displace-project.org","repository", "ibm_vessels_param")
     general$main.path.code <- file.path("C:","displace-project.org","repository", "ibm_vessels_param_R")


     # check the file given the metier names
     dd <- read.table(file=file.path(general$main.path,"metiersspe",
       "metier_selectivity_ogives.dat"), header=TRUE, sep=" ")
     forgotten_metiers <- (0:max(unique(dd[,1])) )[!  0:max(unique(dd[,1])) %in% unique(dd[,1]) ]
     if(length( forgotten_metiers )!=0) { cat(paste("some forgotten metiers found!!......\n")) }
     #=> likely to lead to the wrong size of the c++ 'metiers' object, then lead to deadly c++ bugs


     ## TO DO : change in c++: use the met_names_txt file instead of gussing nbmetier from selectivity ogive...
     
     

     ## TO DO: check presence of all pop in the avai files
    ## if not present then forced to use as implicit pop.....
    
     ## TO DO: check for absence of ports for given vessel given quarter while some fgrounds are found....why?
       ## (see magic numbers in c++ to fix it)

    

        ####::::::::::TO DO!!
      # TO DO: N-at-sizgroup => // DEADLY BUG: MAKE SURE THAT NO BLANK IS LEFT IN THE VERY END OF THE .DAT FILE...
      # maybe use write.table with eol="\r\n" will produce Windows' line endings on a Unix-alike OS,


      # :::::::::::::TO DO!!
      correct to remove the x at the begining of the file for
      code_area_for_graph_points_graph






     ## check for NAs in .dat files in vesselspe
     lst_files <- list.files (file.path(general$main.path, "vesselsspe"))
     for (fi in lst_files){
        if(length(grep("features",fi))!=0) a.sep="|" else a.sep=" "
        dd <- read.table(file=file.path(general$main.path,"vesselsspe", fi), header=FALSE, sep=a.sep)
        #print(head(dd,2))
        if( any(is.na(dd)) ) cat(paste("NAs found in ",fi,"......\n"))

     }




          ## check for NAs in .dat files in harboursspe
     lst_files <- list.files (file.path(general$main.path, "harboursspe"))
     for (fi in lst_files){
        er <- try(dd <- read.table(file=file.path(general$main.path,"harboursspe", fi), header=FALSE), silent=TRUE)
        if(class(er)=="try-error"){
         print(fi)
         print(er[[1]])
        }else{
         #print(head(dd,2))
         if( any(is.na(dd)) ) cat(paste("NAs found in ",fi,"......\n"))
         if( any(dd=="Inf") ) cat(paste("Inf found in ",fi,"......\n"))
         }
     }
     
     

              ## check for NAs in .dat files in popsspe
     lst_files <- list.files (file.path(general$main.path, "popsspe"),recursive = TRUE)
     for (fi in lst_files){
        if(length(grep("spe_SSB_R_parameters",fi))==0){
        dd <- read.table(file=file.path(general$main.path,"popsspe", fi), header=FALSE)
        #print(head(dd,2))
        if( any(is.na(dd)) ) cat(paste("NAs found in ",fi,"......\n"))
        }
     }

                  ## check for NAs in .dat files in metiersspe
     lst_files <- list.files (file.path(general$main.path, "metiersspe"), include.dirs = FALSE)
     for (fi in lst_files){
        dd <- read.table(file=file.path(general$main.path,"metiersspe", fi), header=FALSE)
        #print(head(dd,2))
        if( any(is.na(dd)) ) cat(paste("NAs found in ",fi,"......\n"))

     }


     # if deadly bug when cpue files are loaded then check:
     lastly, check the number of species in  vesselsspe_percent_tacs_per_pop_semester1
     and     [vid]_cpue_per_stk_on_nodes_quarter[]   and DNK000013712_gshape_cpue_per_stk_on_nodes_quarter1
     because pop_name definition is circular....that can lead to mix up the pop!
     #=> we should have multimap with the right number of pop !!!
     # if not then reparameterize step 4 for cpue files....
     
    
    
    graph <- read.table("C:\\displace-project.org\\repository\\ibm_vessels_input\\graphsspe\\graph111.dat")
    graph <- as.matrix(graph)
    graph <- matrix(graph, ncol=3)
    dd <- unique(c(graph[graph[,3]>900, c(1,2)])) # should be nodes in polygons
    dd
    
    
    nodes_in_polygons <- read.table("C:\\displace-project.org\\repository\\ibm_vessels_input\\graphsspe\\nodes_in_polygons_a_graph111_quarter1.dat", header=TRUE)
    dd2 <-  nodes_in_polygons[,2]
    
    nodes_in_polygons[nodes_in_polygons[,1]=="11",2] %in% dd

   
    coord111 <- read.table("C:\\displace-project.org\\repository\\ibm_vessels_input\\graphsspe\\coord111.dat", header=TRUE)
      coord111 <- as.matrix(coord111)
    coord111 <- matrix(coord111, ncol=3)
     ddd<- coord111[dd,]
     points(ddd[,1],ddd[,2], pch=".")
     ddd2<- coord111[dd2,]
     points(ddd2[,1],ddd2[,2], pch=".", col="2")
     
     
    
    # a posteriori correction for graph111 !!!
    idx <- (graph[,1] %in% nodes_in_polygons[,2] | graph[,2] %in% nodes_in_polygons[,2]) & graph[,3]<900
    graph[idx,]
    graph[idx, 3] <-  graph[idx, 3]+900 
    graph[idx,]
    graph_c <- matrix(graph, ncol=1) 
    write.table(graph_c, file="C:\\displace-project.org\\repository\\ibm_vessels_input\\graphsspe\\graph111.dat", row.names=FALSE, col.names=FALSE)
   
     
   
    graph <- read.table("C:\\displace-project.org\\repository\\ibm_vessels_input\\graphsspe\\graph112.dat")
    graph <- as.matrix(graph)
    graph <- matrix(graph, ncol=3)
    dd <- unique(c(graph[graph[,3]>900, c(1,2)])) # should be nodes in polygons
    dd
    
    
    nodes_in_polygons <- read.table("C:\\displace-project.org\\repository\\ibm_vessels_input\\graphsspe\\nodes_in_polygons_a_graph112_quarter1.dat", header=TRUE)
    nodes_in_polygons[,2]
    
    nodes_in_polygons[nodes_in_polygons[,1]=="11",2] %in% dd

     
 
   
   
     