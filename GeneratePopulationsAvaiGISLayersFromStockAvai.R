
   args <- commandArgs(trailingOnly = TRUE)

   general <- list()

   if (length(args) < 2) {
     if(.Platform$OS.type == "windows") {
       general$application           <- "testexample" # ...or myfish
       general$main_path_gis         <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis", general$application)
       general$main.path.ibm         <- file.path("C:","Users","fbas","Documents","GitHub",paste("DISPLACE_input_", general$application, sep=''))
       general$igraph                <- 56  # caution: should be consistent with existing objects already built upon a given graph
       general$main_path_R_inputs    <- file.path("C:", "Users", "fbas", "Documents", "GitHub", "DISPLACE_R_inputs")
       do_plot                       <- TRUE
     }
  } else {
       general$application           <- args[1]
       general$main_path_gis         <- args[2]
       general$main.path.ibm         <- args[3]
       general$igraph                <- args[4]  # caution: should be consistent with existing vessels already built upon a given graph
       general$main_path_R_inputs    <- args[5]
       do_plot                       <- FALSE
      

  }
  cat(paste("START \n"))


  if(general$application=="testexample"){
  
      general$casestudy           <- "testexample"             
      species.to.keep             <- c("COD", "PLE", "WHG", "TUR", "FLE", "DAB", "SOL", "SPR", "HER")
      general$method              <- "maximum" # for the canadian paper
      general$threshold           <- 50
      general$method              <- "inverse" # for the baltic only CS. because the grid mesh size if much finer
      general$threshold           <- 25
      general$p                   <- 0.1
      years <- c(2013:2015)
 
  
  } else{
      stop("to be defined for this app")
  }


  
  dir.create(file.path( general$main.path.ibm))
  dir.create(file.path( general$main.path.ibm, paste("pop_names_",general$application ,".txt",sep='')))
  
  
  # load
  load(file = file.path(general$main_path_gis, "POPULATIONS", "avai",
                            paste("lst_avai_igraph", general$igraph,"_",general$method,"_", general$threshold, ".RData",sep="")) )

 
 
 
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## STOCK NAMES !!
  
  # (caution: give the order for naming stocks in integer from 0 to n-1)
  spp_table <-  read.table(file=file.path(general$main_path_gis, "POPULATIONS", 
                           paste("pop_names_", general$application,".txt",sep='')), header=TRUE)
  spp                        <- as.character(spp_table$spp)
 
   
  


 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

 dir.create(path=file.path(general$main_path_gis,   "POPULATIONS", "SpatialLayers"))
 

  get_contour_this_avai <- function (
                          a.semester = "1",
                          a.stock    = "COD.nsea",
                          a.szgroup  = "4",
                          a.sigma=0.3,
                          a.nlevels=10,
                          a.xrange= c(-10,27),
                          a.yrange=c(50,66),
                          general=general){

      ## format and retrieve X, Y from coord ##
     avai           <- lst.avai[[a.stock]][[a.semester]] [, c( 'x', 'y', paste(substr(a.stock, 1,3), '.nb_indiv.', a.szgroup, sep='')) ]
     if(length(a.szgroup)>1) avai[,3] <-    apply(avai[, c(paste(substr(a.stock, 1,3), '.nb_indiv.', a.szgroup, sep=''))], 1, sum, na.rm=TRUE)   # if more than one szgroup requested then sum up
     avai           <- avai [, c(1:3)]
     colnames(avai) <- c("X", "Y", "Z")

      avai <- avai[!duplicated(avai),]

     ## apply a kernel density plot on a point pattern, and get the countour lines ##
     library(spatstat)

     # customize the function contour.im
     # (which is the specfic contour() function called on density.ppp object)
     # by replacing contour.defaut() by contourLines() i.e. the one returning the list of coordinates!!
     contour.im <-
        function (x, ..., main, axes = TRUE, add = FALSE)
          {
          defaultmain <- deparse(substitute(x))
          sop <- spatstat.options("par.contour")
          if (missing(main))
             main <- resolve.defaults(sop, list(main = defaultmain))$main
          if (missing(add))
             add <- resolve.defaults(sop, list(add = FALSE))$add
          if (missing(axes))
             axes <- resolve.defaults(sop, list(axes = TRUE))$axes
          if (!add) {
             do.call.matched("contourLines", resolve.defaults(list(x = x$xcol,
               y = x$yrow, z = t(x$v)), list(add = TRUE), list(...)))
            }
          }



  contour1 <- contour(density(ppp(avai[,1], avai[,2], xrange=a.xrange, yrange=a.yrange ), sigma=a.sigma, weights=avai[,3]),
                ylim=a.yrange, xlim=a.xrange, xlab="", ylab="", main="", add=FALSE, nlevels = a.nlevels)

  return(contour1)

  }




     


 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  export_contour_in_shapefile <- function (contourobj, nameobj, general){
    lst <- list(NULL)
    count <- 0
    for(i in 1: length(contourobj)){
    
      x <- contourobj[[i]]$x
      y <- contourobj[[i]]$y
     if(contourobj[[i]]$level>0){
      count <- count+1
      library(sp)
      assign(paste("poly", i, sep=""),
        Polygon(cbind(  c(x,x[1]),
                   c(y, y[1])
                    )))
      assign  (paste("Pol", i, sep=""),  Polygons(list(get(paste("poly", i, sep=""))), ID=i) )
      lst[[count]] <- get(paste("Pol", i, sep=""))
      }
     }

    sp              <- SpatialPolygons(lst, 1:length(lst))
    proj4string(sp) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
    # export in .shp attaching the df with the levels
    IDs  <- sapply(sp@polygons, function(x) x@ID)    
    a_df <-  data.frame(GRIDCODE=sapply(contourobj, function(x) x$level) [as.numeric(IDs)], row.names=IDs)
    spdf <- SpatialPolygonsDataFrame(sp, a_df)
    library(maptools)
    writePolyShape(spdf, file.path(general$main_path_gis,  "POPULATIONS", "SpatialLayers", nameobj))
   cat(paste("Save the GIS layer in /POPULATIONS/SpatialLayers folder....done \n"))                         
  
   return()
   }
  
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 
 
  ## CALLS
  # per body size (assuming implicit ONTOGENIC migration)
  names(lst.avai)  
  if(general$application=="testexample"){
  size_cats <- list()
  size_cats[["COD.2532"]] [["small"]]  <- c("0", "1", "2", "3","4","5")
  size_cats[["COD.2532"]] [["medium"]] <- c("6","7","8")
  size_cats[["COD.2532"]] [["large"]]  <- c("9","10","11", "12", "13")
  size_cats[["COD.2224"]] [["small"]]  <- c("0", "1", "2", "3","4","5")
  size_cats[["COD.2224"]] [["medium"]] <- c("6","7","8")
  size_cats[["COD.2224"]] [["large"]]  <- c("9","10","11", "12", "13")
  size_cats[["FLE.2223"]] [["small"]]  <- c("0", "1", "2", "3","4","5")
  size_cats[["FLE.2223"]] [["medium"]] <- c("6","7","8")
  size_cats[["FLE.2223"]] [["large"]]  <- c("9","10","11", "12", "13")
  size_cats[["FLE.2425"]] [["small"]]  <- c("0", "1", "2", "3","4","5")
  size_cats[["FLE.2425"]] [["medium"]] <- c("6","7","8")
  size_cats[["FLE.2425"]] [["large"]]  <- c("9","10","11", "12", "13")
  size_cats[["PLE.2123"]] [["small"]]  <- c("0", "1", "2", "3","4","5")
  size_cats[["PLE.2123"]] [["medium"]] <- c("6","7","8")
  size_cats[["PLE.2123"]] [["large"]]  <- c("9","10","11", "12", "13")
  size_cats[["PLE.2432"]] [["small"]]  <- c("0", "1", "2", "3","4","5")
  size_cats[["PLE.2432"]] [["medium"]] <- c("6","7","8")
  size_cats[["PLE.2432"]] [["large"]]  <- c("9","10","11", "12", "13")
  size_cats[["SOL.IIIa2223"]] [["small"]]  <- c("3","4")
  size_cats[["SOL.IIIa2223"]] [["medium"]] <- c("5","6")
  size_cats[["SOL.IIIa2223"]] [["large"]]  <- c("7","8","9","10","11")
  size_cats[["WHG.2232"]] [["small"]]  <- c("3","4","5")
  size_cats[["WHG.2232"]] [["medium"]] <- c("6","7","8")
  size_cats[["WHG.2232"]] [["large"]]  <- c("9","10","11", "12", "13")
  size_cats[["DAB.2232"]] [["small"]]  <- c("3","4","5")
  size_cats[["DAB.2232"]] [["medium"]] <- c("6","7","8")
  size_cats[["DAB.2232"]] [["large"]]  <- c("9","10","11", "12", "13")
  size_cats[["TUR.2232"]] [["small"]]  <- c("3","4")
  size_cats[["TUR.2232"]] [["medium"]] <- c("5","6")
  size_cats[["TUR.2232"]] [["large"]]  <- c("7","8","9","10","11", "12", "13")
  size_cats[["HER.IIIa22"]] [["small"]]  <- c("1")
  size_cats[["HER.IIIa22"]] [["medium"]] <- c("2")
  size_cats[["HER.IIIa22"]] [["large"]]  <- c("3")
  size_cats[["HER.2532"]] [["small"]]  <- c("1")
  size_cats[["HER.2532"]] [["medium"]] <- c("2")
  size_cats[["HER.2532"]] [["large"]]  <- c("3")
  size_cats[["SPR.2232"]] [["small"]]  <- c("1")
  size_cats[["SPR.2232"]] [["medium"]] <- c("2")
  size_cats[["SPR.2232"]] [["large"]]  <- c("3")
   } else{
      stop("size_cats - to be defined for this app - adapt within the R script")
  }


 
 # the selected groups in the catch rate equation
 write(c('stock',  'comcat_per_szgroup'),
                   file=file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=''),
                       paste("comcat_per_szgroup_done_by_hand.dat",sep=' ')), append=FALSE,  ncol=2,
                          sep=" ") 
                          
 count <-0
 for (sp in 1: length(spp)){
    count <- count+1
       dd <- unlist(lapply(size_cats[[sp]], length))
       comcat_per_szgroup <- c(rep(0, dd[1]), rep(1, dd[2]), rep(2, 14-(dd[1]+dd[2])))
       write.table(cbind(rep(sp-1, 14), comcat_per_szgroup),
                   file=file.path(general$main.path.ibm, paste("popsspe_", general$application, sep=''),
                       paste("comcat_per_szgroup_done_by_hand.dat",sep=' ')), append=TRUE,
                         quote = FALSE, sep=" ", col.names=FALSE, row.names=FALSE)
 
 }
   cat(paste("Save the selected groups for the catch rate equation....done \n"))                         
  
 
   
  cols <- c(
            rgb(0,1,0,0.5),
            rgb(1,1,0,0.5),
            rgb(0,1,1,0.5),
            rgb(0.75,0.75,0.75,0.5),
            rgb(0.25,0.5,0.75,0.5),
            rgb(1,0,0,0.5),
            rgb(0.8,0.8,0,0.5),
            rgb(1,0.3,0.3,0.5),
            rgb(1,0.3,0.1,0.5),
            rgb(1,0.3,0.5,0.5),
            rgb(1,0.9,0.5,0.5),
            rgb(1,0.3,0.9,0.5),
            rgb(1,0.9,0.9,0.5)
             )
  if(do_plot) {
     windows(5,8)
     plot(0,0, xlim=c(9,16), ylim=c(53,60), xlab="Longitude", ylab="Latitude", col=2, type="n")
  }
  
  for (pid in 1:length(spp)){
    for (sz_cat in c('small', 'medium', 'large')){
  
    layername <- paste("contour", pid-1,"_", sz_cat, "_", paste(size_cats[[ spp[pid] ]][[sz_cat]], collapse="-"), sep="")
    assign(layername, get_contour_this_avai (
                          a.semester = "1",
                          a.stock    = spp[pid],
                          a.szgroup  = size_cats[[ spp[pid] ]][[sz_cat]],
                          a.sigma=0.1,
                          a.nlevels=5,
                          a.xrange= c(-10,27),
                          a.yrange=c(50,66),
                          general=general)
        )
 
 
     # plot it
     if(do_plot) lapply(get(layername), function(obj, a_level) {
       polygon(obj$x, obj$y, col=ifelse(obj$level>0, cols[pid], rgb(0,1,1,0.0)), border = NA)
       })
  
     # export it
     export_contour_in_shapefile (contourobj=get(layername), nameobj=layername, general)
    cat(paste("contour shape file for", layername, "exported....done \n"))                         

  }}      
           
  # additional info on the plot         
  if(do_plot){
     sh1 <- readShapePoly(file.path(general$main_path_gis,  "MANAGEMENT", "francois_EU"))
     plot(sh1, add=TRUE, col=grey(0.5))
     legend("bottomright", legend=paste(c(spp)),
         fill=cols, ncol=2, cex=0.8, bg="white", box.col="white")
     savePlot(file.path(general$main_path_gis,  "POPULATIONS", "avai", "avai_distrib_small_medium_large.jpeg"), type="jpeg")
  }


cat(paste("....done \n"))




