## TRANSFORM IN GIS shape files......

get_contour_this_avai <- function (
                          a.semester = "1",
                          a.stock    = "COD.nsea",
                          a.szgroup  = "4",
                          a.sigma=0.3,
                          a.nlevels=10,
                          a.xrange= c(-10,27),
                          a.yrange=c(50,66),
                          general=general){

     load(file = file.path(general$main_path, "avai",
                            paste("lst_avai_igraph", general$igraph,"_", general$nmy,"_",general$method,"_", general$threshold, ".RData",sep="")) )  # get lst.avai

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




  general                     <- list()
  general$casestudy           <- "balticRTI"
  general$main_path           <- file.path("C:", "Users", "fbas", "Documents", "GitHub", "DISPLACE_input_raw")
  general$main_path_input     <- file.path("C:", "Users", "fbas", "Documents", "GitHub", "DISPLACE_input")
  general$main_path_R_inputs  <- file.path("C:", "Users", "fbas", "Documents", "GitHub", "DISPLACE_R_inputs")
  general$main_path_gis       <- file.path("C:", "Users", "fbas", "Documents", "GitHub", "DISPLACE_input_gis")
  general$igraph              <- "56"   # for the baltic only CS.
  general$method              <- "inverse" # for the baltic only CS.
  general$threshold           <- 25
  general$p                   <- 0.1
  general$nmy                 <- "2014_2015"

  # a call
  names(lst.avai)
  contour1 <- get_contour_this_avai (
                          a.semester = "1",
                          a.stock    = "COD.2224",
                          a.szgroup  = c("6","7","8","9","10","11"),
                          a.sigma=0.1,
                          a.nlevels=5,
                          a.xrange= c(-10,27),
                          a.yrange=c(50,66),
                          general=general)

  contour2 <- get_contour_this_avai (
                          a.semester = "1",
                          a.stock    = "PLE.2123",
                          a.szgroup  = c("6","7","8","9","10","11"),
                          a.sigma=0.1,
                          a.nlevels=5,
                          a.xrange= c(-10,27),
                          a.yrange=c(50,66),
                          general=general)

  contour3 <- get_contour_this_avai (
                          a.semester = "1",
                          a.stock    = "PLE.2432",
                          a.szgroup  = c("6","7","8","9","10","11"),
                          a.sigma=0.1,
                          a.nlevels=5,
                          a.xrange= c(-10,27),
                          a.yrange=c(50,66),
                          general=general)

  contour4 <- get_contour_this_avai (
                          a.semester = "1",
                          a.stock    = "FLE.2223",
                          a.szgroup  = c("6","7","8","9","10","11"),
                          a.sigma=0.1,
                          a.nlevels=5,
                          a.xrange= c(-10,27),
                          a.yrange=c(50,66),
                          general=general)

  contour5 <- get_contour_this_avai (
                          a.semester = "1",
                          a.stock    = "FLE.2425",
                          a.szgroup  = c("6","7","8","9","10","11"),
                          a.sigma=0.1,
                          a.nlevels=5,
                          a.xrange= c(-10,27),
                          a.yrange=c(50,66),
                          general=general)
  contour6 <- get_contour_this_avai (
                          a.semester = "1",
                          a.stock    = "DAB.2232",
                          a.szgroup  = c("3","4"),
                          a.sigma=0.1,
                          a.nlevels=5,
                          a.xrange= c(-10,27),
                          a.yrange=c(50,66),
                          general=general)

   cols <- c(
            rgb(0,1,0,0.5),
            rgb(1,1,0,0.5),
            rgb(0,1,1,0.5),
            rgb(0.75,0.75,0.75,0.5),
            rgb(0.25,0.5,0.75,0.5),
            rgb(1,0,0,0.5)
             )




   # plot it
   windows(5,8)
   plot(0,0, xlim=c(9,16), ylim=c(53,60), xlab="Longitude", ylab="Latitude", col=2, type="n")
   lapply(contour1, function(obj, a_level) {
       polygon(obj$x, obj$y, col=ifelse(obj$level>0, cols[1], rgb(0,1,1,0.0)), border = NA)
       })
   lapply(contour2, function(obj, a_level) {
       polygon(obj$x, obj$y, col=ifelse(obj$level>0, cols[2], rgb(0,1,1,0.0)), border = NA)
       })
   lapply(contour3, function(obj, a_level) {
       polygon(obj$x, obj$y, col=ifelse(obj$level>0, cols[3], rgb(0,1,1,0.0)), border = NA)
       })
   lapply(contour4, function(obj, a_level) {
       polygon(obj$x, obj$y, col=ifelse(obj$level>0, cols[4], rgb(0,1,1,0.0)), border = NA)
       })
   lapply(contour5, function(obj, a_level) {
       polygon(obj$x, obj$y, col=ifelse(obj$level>0, cols[5], rgb(0,1,1,0.0)), border = NA)
       })
   lapply(contour6, function(obj, a_level) {
       polygon(obj$x, obj$y, col=ifelse(obj$level>0, cols[6], rgb(0,1,1,0.0)), border = NA)
       })
  sh1 <- readShapePoly(file.path(general$main_path_input, "graphsspe", "shp","francois_EU"))
  plot(sh1, add=TRUE, col=grey(0.5))
  legend("bottomright", legend=paste(c('COD.2224','PLE.2123','PLE.2432','FLE.2223','FLE.2425', 'DAB.2232')),
         fill=cols, ncol=2, cex=0.8, bg="white", box.col="white")

  # and convert it in GIS shape file
  for (id in 1:6){

  nameobj    <- paste("contour", id, sep='')
  contourobj <- get(nameobj)
  lst <- list(NULL)
  count <- 0
  for(i in 1: length(contourobj)){
      x <- contourobj[[i]]$x
      y <- contourobj[[i]]$y
     if(contourobj[[i]]$level>0){
      count <- count+1
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
  IDs  <- sapply(slot(sp, "polygons"), function(x) slot(x, "ID"))
  a_df <-  data.frame(GRIDCODE=sapply(contourobj, function(x) x$level) [as.numeric(IDs)], row.names=IDs)
  spdf <- SpatialPolygonsDataFrame(sp, a_df)
  writePolyShape(spdf, file.path(general$main_path_gis, general$casestudy, nameobj))

  } # end for



