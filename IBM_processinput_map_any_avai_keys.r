

  general <- list()
  general$main.path          <- file.path("C:","ibm_vessels_outputs") ## ON H: OR C: CAUTION!
  general$main.path.input    <- file.path("C:","displace-project.org","repository","ibm_vessels_input") 
  general$main.path.param    <- file.path("C:","displace-project.org","repository","ibm_vessels_param") 

  #general$namefolderinput   <- "final"
  general$namefolderinput   <- "balticonly"
 
 if(general$namefolderinput=="final"){
  general$igraph <- 4
  general$a.year <- "2010"
  general$a.country <- "DNK"
 }
 
 if(general$namefolderinput=="balticonly"){
  general$igraph            <- 11
  general$a.year            <- "2012"
  general$a.country         <- c("DNK", "DEU", "SWE")
 }
   
##--------------------------------------------------------------------------##
##-------------UTILS--------------------------------------------------------##
##--------------------------------------------------------------------------##
a.shortest.path <- function (from = 6, to = 1001, g=g, is.plot=FALSE, a.color=2) {
from <- from - 1; to <- to - 1 # CAUTION! idx from 0 in g (!)
path <- unlist(get.shortest.paths(g, from, to , weights= g[[9]][[4]]$dist.km)) # dijkstra´s algorithm in library(igraph)
x.path <- g[[9]][[3]]$x[path+1]
y.path <- g[[9]][[3]]$y[path+1]
if(is.plot){
  lines(x.path,y.path,col=a.color, lwd=1.6)
  points(x.path,y.path,col=a.color, pch=16, cex=0.6)
  # i.e start point is points(coord[from,1], coord[from,2], col=3,pch=16)
  # i.e stop point is points(coord[to,1], coord[to,2],col=3, pch=16)
  }
return(path+1) # index pts compatible with coord
}

 ### CAUTION ONLY FOR igraph under <<R.2.14.0
 ### TO DO: update...

  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 distAB.f <- function(A,B, .unit="km"){
 # CAUTION : LONGLAT
 # return the dist in km or nautical miles.
 # formula = cos (gamma) = sin LatA sin LatB + cos LatA cos LatB cos delta
 # if gamma in degree, knowing that 1 degree = 60 minutes = 60 nm, then gamma in degree * 60 = dist in nm
 # we can also use the radius R of the earth: the length of the arc AB is given by R*gamma with gamma in radians
        p = 180/pi;  Rearth = 6378.388 # in km
        nm <- 1852e-3 # 1852 m for 1 nm
        res <- switch(.unit,
           km = Rearth * acos(sin(A[,2]/p)*sin(B[,2]/p) + (cos(A[,2]/p) * cos(B[,2]/p)*cos(A[,1]/p - B[,1]/p))),
           nm = Rearth * acos(sin(A[,2]/p)*sin(B[,2]/p) + (cos(A[,2]/p) * cos(B[,1]/p)*cos(A[,1]/p - B[,1]/p))) / nm
           )
       names(res) <- NULL
        return(res)
        }

dist.paths <- function(from=1, to=c(100,200), g=g, coord=coord){
   short.path.dists <- rep(0, length(to))
   for(i in 1: length(to)){
     a.sh.path <- a.shortest.path  (from = from, to = to[i], g=g, is.plot=FALSE, a.color=4)
     mat <- coord[a.sh.path,]
     if(length(mat)>3){
         short.path.dists[i] <-  sum (distAB.f(A=mat, B=rbind(mat[-1,],mat[nrow(mat),]), .unit="km"  ))
       } else{short.path.dists[i] <- 0} # in that case, dep pt = arr pt!
     }

return(short.path.dists)
}



##--------------------------------------------------------------------------##
##-------------LOAD THE GRAPH ----------------------------------------------##
##--------------------------------------------------------------------------##

## FILES FOR BUILDING A IGRAPH
load(file.path(general$main.path.input,"graphsspe", paste(general$igraph,"_graphibm.RData",sep=''))) # built from the R code

 #  a plot
 library(maptools)
 library(maps)
 library(mapdata)
 windows(5,5)
 plot(coord[,1], coord[,2], pch=".", xlab="Longitude", ylab="Latitude", xlim=c(-8,23), ylim=c(52,65))
 ices_areas <- readShapeSpatial("C:\\displace-project.org\\repository\\ibm_vessels_param\\ices_areas\\ices_areas")
 plot(ices_areas, col="white",  add=TRUE)
 points(coord[,1], coord[,2], pch=".")
 box()
  map("worldHires", add=TRUE)
 library(igraph)
 #a.shortest.path  (from = 2, to = 3170, g=g, is.plot=TRUE, a.color=5)
#a.shortest.path  (from = 725, to = 2341, g=g, is.plot=TRUE, a.color=3)



##--------------------------------------------------------------------------##
##-------------MAIN---------- ----------------------------------------------##
##--------------------------------------------------------------------------##



get_contour_this_avai <- function (
                          a.number   = "052",
                          a.semester = "1",
                          a.stock    = "12",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "4",
                          general=general,
                          do_PBSmap =FALSE){
  
  if(general$namefolderinput=="final")      a.file     <- paste(a.stock,"spe_full_avai_szgroup_nodes_semester",a.semester,"_",a.number,".dat", sep='')
  if(general$namefolderinput=="balticonly") a.file     <- paste(a.stock,"spe_full_avai_szgroup_nodes_semester",a.semester,".dat", sep='')
  avai       <- read.table(file=file.path(general$main.path.input, paste("popsspe_", general$namefolderinput, sep=''), a.folder, a.file), header=TRUE)
  #avai       <- read.table(file=file.path('C:','Users','fba','Dropbox','ibm_vessels_param','popsspe', a.folder, a.file), header=TRUE)
  
  ## format and retrieve X, Y from coord ##
  nodes <- avai$idx_node +1 # convert from C++ index to R
  avai <- matrix(avai$avai, ncol=14, byrow=TRUE) # 14 age groups
  avai <- cbind(coord[unique(nodes), 1:2], avai)
  colnames(avai) <- c("X", "Y", paste("nb_szgroup_", 0:13, sep=""))

  avai <- avai[, c(1:2, grep(paste("nb_szgroup_",a.szgroup, sep="") , colnames(avai)) )  ] # subset a szgroup
  colnames(avai) <- c("X", "Y", "Z")

  ## DO THE PBSmapping plot (for fun...) ##
  # load shape file
  
 
  if(do_PBSmap){
  library(maptools)
  sh1 <- readShapePoly(file.path(general$main.path.param,"shp","francois_EU"))
  library(PBSmapping)  # addBubbles()
  plot(avai[,1], avai[,2], xlab="Longitude", ylab="Latitude", col=2, type="n")
  addBubbles(data.frame(EID=1:nrow(avai), X=avai[,1], Y=avai[,2], Z=avai[,3]),
                legend.pos="bottomleft", legend.type=c("nested"), 
                legend.title= paste(a.stock, " availability", a.szgroup, " semest.", a.semester), 
                legend.cex=0.8, symbol.zero="+")
  plot(sh1, add=TRUE, col=grey(0.5))
  }
  
  ##apply a kernel density plot on a point pattern, and get the countour lines ##
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
    #    if (axes) 
   #         do.call.matched("plot.default", resolve.defaults(list(x = range(x$xcol), 
   #             y = range(x$yrow), type = "n"), list(...), list(asp = 1, 
   #             xlab = "x", ylab = "y", main = main)))
    #    else {
   #         rec <- owin(x$xrange, x$yrange)
   #         do.call.matched("plot.owin", resolve.defaults(list(x = rec), 
   #             list(...), list(main = main)))
    #    }
    }
    #do.call.matched("contour.default", resolve.defaults(list(x = x$xcol, 
    #    y = x$yrow, z = t(x$v)), list(add = TRUE), list(...)))
  do.call.matched("contourLines", resolve.defaults(list(x = x$xcol, 
        y = x$yrow, z = t(x$v)), list(add = TRUE), list(...)))
  }

               
  contour1 <- contour(density(ppp(avai[,1], avai[,2], xrange=c(-10,25), yrange=c(50,65) ), sigma=0.3, weights=avai[,3]),
                ylim=c(53,60), xlim=c(-1,16), xlab="", ylab="", main="", add=FALSE, nlevels = 10)  
  
  return(contour1)
  
  }
  
  
  ### CALLS AND PLOTS
 
  ## e.g. different avai key
   ## e.g. different stocks EXPLICIT NORTH SEA STOCKS
  contour1 <- get_contour_this_avai ( 
                          a.number   = "001",
                          a.semester = "1",
                          a.stock    = "20",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "0",
                          general=general,
                          do_PBSmap =FALSE)
  contour2 <- get_contour_this_avai ( 
                          a.number   = "002",
                          a.semester = "1",
                          a.stock    = "20",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "0",
                          general=general,
                          do_PBSmap =FALSE)
     windows(5,5)
   #plot(0,0, xlim=c(10,20), ylim=c(53,60), xlab="Longitude", ylab="Latitude", col=2, type="n")
   plot(0,0, xlim=c(-10,10), ylim=c(53,60), xlab="Longitude", ylab="Latitude", col=2, type="n")
   lapply(contour1, function(obj, a_level) {
       polygon(obj$x, obj$y, col=ifelse(obj$level>0,rgb(0.9,0.9,0,0.5),rgb(0,1,1,0.0)), border = NA)
       })
   lapply(contour1, function(obj, a_level) {
       polygon(obj$x, obj$y, col=1, border = NA)
       })
   lapply(contour2, function(obj, a_level) {
       polygon(obj$x, obj$y, col=ifelse(obj$level>0,rgb(0,0.9,1,0.5),rgb(0,1,1,0.0)), border = NA)
       })
     sh1 <- readShapePoly(file.path(general$param.path,"shp","francois_EU"))
  plot(sh1, add=TRUE, col=grey(0.5))
  mtext(side=3, "(a)", adj=0, line=1, cex=2)

  #---------------------------------------------------
  
 # or different szgroups........
   ## e.g. different stocks
  contour2 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "1",
                          a.stock    = "12",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "2",
                          general=general,
                          do_PBSmap =FALSE)
  contour3 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "1",
                          a.stock    = "12",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "3",
                          general=general,
                          do_PBSmap =FALSE)
  contour4 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "1",
                          a.stock    = "12",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "4",
                          general=general,
                          do_PBSmap =FALSE)
   contour5 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "1",
                          a.stock    = "12",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "5",
                          general=general,
                          do_PBSmap =FALSE)

  # plot 
   plot(0,0, xlim=c(5,22), ylim=c(53,60), xlab="Longitude", ylab="Latitude", col=2, type="n")
  
    dd <- unlist(lapply(contour2, function(obj) {obj$level}) ) 
    the_min <- min(dd[dd!=0]) # should correspond to the larger polygon...
    lapply(contour2, function(obj, a_level) {
       polygon(obj$x, obj$y, col=ifelse(obj$level==a_level,rgb(0.9,0.9,1,0.5),rgb(0,1,1,0.0)), border = NA)
      # browser()
       }, a_level=the_min)
       
   dd <- unlist(lapply(contour3, function(obj) {obj$level}) ) 
    the_min <- min(dd[dd!=0]) # should correspond to the larger polygon...
    lapply(contour3, function(obj, a_level) {
       polygon(obj$x, obj$y, col=ifelse(obj$level==a_level,rgb(0.75,0.75,1,0.5),rgb(0,1,1,0.0)), border = NA)
      # browser()
       }, a_level=the_min)
  
   dd <- unlist(lapply(contour4, function(obj) {obj$level}) ) 
    the_min <- min(dd[dd!=0]) # should correspond to the larger polygon...
    lapply(contour4, function(obj, a_level) {
       polygon(obj$x, obj$y, col=ifelse(obj$level==a_level,rgb(0.5,0.5,1,0.5),rgb(0,1,1,0.0)), border = NA)
      # browser()
       }, a_level=the_min)
  
   dd <- unlist(lapply(contour5, function(obj) {obj$level}) ) 
    the_min <- min(dd[dd!=0]) # should correspond to the larger polygon...
    lapply(contour5, function(obj, a_level) {
       polygon(obj$x, obj$y, col=ifelse(obj$level==a_level,rgb(0.25,0.25,1,0.5),rgb(0,1,1,0.0)), border = NA)
      # browser()
       }, a_level=the_min)
  
  sh1 <- readShapePoly(file.path(general$param.path,"shp","francois_EU"))
  plot(sh1, add=TRUE, col=grey(0.5))


  
  
  
  
  
  
  
  
     
  #---------------------------------------------------
  #---------------------------------------------------
  #---------------------------------------------------
  ## e.g. different stocks EXPLICIT NORTH SEA STOCKS
  
  # save plot
  outputfile <- file.path(general$output.path, "main_colored_bulk_avai_per_explicit_stock_per_semester_NS_BS.pdf")
  pdf(file = outputfile, width=8, height=8)
  
  #windows(10,10)
  par(mfrow=c(2,2))
  par(mar=c(4,4,2.5,1))
  
  
  contour2 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "1",
                          a.stock    = "2",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "0",
                          general=general,
                          do_PBSmap =FALSE)
  contour5 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "1",
                          a.stock    = "5",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "0",
                          general=general,
                          do_PBSmap =FALSE)
  contour9 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "1",
                          a.stock    = "9",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "4",
                          general=general,
                          do_PBSmap =FALSE)
  contour13 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "1",
                          a.stock    = "13",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "4",
                          general=general,
                          do_PBSmap =FALSE)
 contour14 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "1",
                          a.stock    = "14",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "2",
                          general=general,
                          do_PBSmap =FALSE)
   contour15 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "1",
                          a.stock    = "15",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "0",
                          general=general,
                          do_PBSmap =FALSE)
  contour20 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "1",
                          a.stock    = "20",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "0",
                          general=general,
                          do_PBSmap =FALSE)
 contour21 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "1",
                          a.stock    = "21",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "0",
                          general=general,
                          do_PBSmap =FALSE)
 contour26 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "1",
                          a.stock    = "26",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "3",
                          general=general,
                          do_PBSmap =FALSE)
 contour29 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "1",
                          a.stock    = "29",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "3",
                          general=general,
                          do_PBSmap =FALSE)
                         


  # plot 
   #windows(5,5)
   plot(0,0, xlim=c(-4,12), ylim=c(50,62), xlab="Longitude", ylab="Latitude", col=2, type="n")
   cols <- c(
            rgb(0,1,0,0.5),
            rgb(1,1,0,0.5),
            rgb(0,1,1,0.5),
            rgb(0.75,0.75,0.75,0.5),
            rgb(0.75,1,1,0.5),
            rgb(0.75,1,0,0.5),
            rgb(0.25,0.5,0.75,0.5),
            rgb(0.5,0.0,0.5,0.5),  
            rgb(1,0.25,0.25,0.5),
            rgb(0.0,0.7,0.7,0.5)    
             ) #  plot(1:9,col=cols, pch=16,cex=3)
   
   count <-0
   #for(i in c(2,5,9,13,14,15, 20, 21, 26, 29)){
   for(i in c(2,5,9,13,14,15, 20, 21, 26)){ #revision 2 without 29
   count <- count+1
     a_contour_obj <- get(paste("contour",i,sep=''))
     dd            <- unlist(lapply(a_contour_obj, function(obj) {obj$level}) ) 
     the_min       <- min(dd[dd!=0]) # should correspond to the larger polygon...
     lapply(a_contour_obj, function(obj, a_level, a_col) {
       polygon(obj$x, obj$y, col=ifelse(obj$level==a_level, a_col, rgb(0,1,0,0.0)), border = NA)
      # browser()
       }, a_level=the_min, a_col=cols[count])
   }
  
  sh1 <- readShapePoly(file.path(general$param.path,"shp","francois_EU"))
  plot(sh1, add=TRUE, col=grey(0.7))
  mtext(side=3, "(a)", adj=0, line=1, cex=1.8)
 # legend("bottomright", legend=paste('st.', c(2,5,9,13,14,15, 20, 21, 26, 29)), fill=cols, ncol=2, cex=0.8, bg="white", box.col="white")  
  legend("bottomright", legend=paste(c('her.nsea','spr.nsea','cod.nsea','pok.nsea','had.nsea','nop.nsea', 'san.nsea', 'mac.nsea', 'ple.nsea')), fill=cols, ncol=2, cex=0.8, bg="white", box.col="white")  
  box()
  


 
 
 


   ## e.g. different stocks EXPLICIT BALTIC SEA
  contour3 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "1",
                          a.stock    = "3",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "0",
                          general=general,
                          do_PBSmap =FALSE)
  contour4 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "1",
                          a.stock    = "4",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "0",
                          general=general,
                          do_PBSmap =FALSE)
  contour7 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "1",
                          a.stock    = "7",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "0",
                          general=general,
                          do_PBSmap =FALSE)
  contour10 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "1",
                          a.stock    = "10",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "4",
                          general=general,
                          do_PBSmap =FALSE)
   contour11 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "1",
                          a.stock    = "11",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "4",
                          general=general,
                          do_PBSmap =FALSE)
  contour12 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "1",
                          a.stock    = "12",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "4",
                          general=general,
                          do_PBSmap =FALSE)
 contour30 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "1",
                          a.stock    = "30",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "4",
                          general=general,
                          do_PBSmap =FALSE)
   plot(0,0, xlim=c(8,22), ylim=c(53,60), xlab="Longitude", ylab="Latitude", col=2, type="n")
   cols <- c(
            rgb(0,1,0,0.5),
            rgb(1,1,0,0.5),
            rgb(0,1,1,0.5),
            rgb(0.75,0.75,0.75,0.5),
            rgb(0.75,1,0,0.5),
            rgb(0.25,0.5,0.75,0.5),
            rgb(0.5,0.0,0.5,0.5),  
            rgb(1,0.25,0.25,0.5),
            rgb(0.0,0.7,0.7,0.5)    
             ) #  plot(1:9,col=cols, pch=16,cex=3)
 count <- 0
 for(i in c(3,4,7,10,11,12,30)){
     count <- count+1
     a_contour_obj <- get(paste("contour",i,sep=''))
     dd            <- unlist(lapply(a_contour_obj, function(obj) {obj$level}) ) 
     the_min       <- min(dd[dd!=0]) # should correspond to the larger polygon...
     lapply(a_contour_obj, function(obj, a_level, a_col) {
       polygon(obj$x, obj$y, col=ifelse(obj$level==a_level, a_col, rgb(0,1,0,0.0)), border = NA)
      # browser()
       }, a_level=the_min, a_col=cols[count])
   }
  
  sh1 <- readShapePoly(file.path(general$param.path,"shp","francois_EU"))
  plot(sh1, add=TRUE, col=grey(0.7))
  mtext(side=3, "(b)", adj=0, line=1, cex=1.8)
   # legend("bottomright", legend=paste('st.', c(3,4,7,10,11,12,30)), fill=cols, ncol=3, cex=0.8, bg="white", box.col="white") 
    legend("bottomright", legend=paste(c('her.3a22','her.2532','spr.2232','cod.kat','cod.2224','cod.2532','sol.kask')), fill=cols, ncol=3, cex=0.8, bg="white", box.col="white") 
  box()
  

  ## e.g. different stocks EXPLICIT NORTH SEA STOCKS
  contour2 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "2",
                          a.stock    = "2",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "0",
                          general=general,
                          do_PBSmap =FALSE)
  contour5 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "2",
                          a.stock    = "5",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "0",
                          general=general,
                          do_PBSmap =FALSE)
  contour9 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "2",
                          a.stock    = "9",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "4",
                          general=general,
                          do_PBSmap =FALSE)
  contour13 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "2",
                          a.stock    = "13",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "4",
                          general=general,
                          do_PBSmap =FALSE)
 contour14 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "2",
                          a.stock    = "14",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "2",
                          general=general,
                          do_PBSmap =FALSE)
   contour15 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "2",
                          a.stock    = "15",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "0",
                          general=general,
                          do_PBSmap =FALSE)
  contour20 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "2",
                          a.stock    = "20",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "0",
                          general=general,
                          do_PBSmap =FALSE)
 contour21 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "2",
                          a.stock    = "21",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "0",
                          general=general,
                          do_PBSmap =FALSE)
 contour26 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "2",
                          a.stock    = "26",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "3",
                          general=general,
                          do_PBSmap =FALSE)
 contour29 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "2",
                          a.stock    = "29",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "3",
                          general=general,
                          do_PBSmap =FALSE)
                         


  # plot 
   #windows(5,5)
   plot(0,0, xlim=c(-4,12), ylim=c(50,62), xlab="Longitude", ylab="Latitude", col=2, type="n")
   cols <- c(
            rgb(0,1,0,0.5),
            rgb(1,1,0,0.5),
            rgb(0,1,1,0.5),
            rgb(0.75,0.75,0.75,0.5),
            rgb(0.75,1,1,0.5),
            rgb(0.75,1,0,0.5),
            rgb(0.25,0.5,0.75,0.5),
            rgb(0.5,0.0,0.5,0.5),  
            rgb(1,0.25,0.25,0.5),
            rgb(0.0,0.7,0.7,0.5)    
             ) #  plot(1:9,col=cols, pch=16,cex=3)
   
   count <-0
  # for(i in c(2,5,9,13,14,15, 20, 21, 26, 29)){
   for(i in c(2,5,9,13,14,15, 20, 21, 26)){ # revision 2 without 29
   count <- count+1
     a_contour_obj <- get(paste("contour",i,sep=''))
     dd            <- unlist(lapply(a_contour_obj, function(obj) {obj$level}) ) 
     the_min       <- min(dd[dd!=0]) # should correspond to the larger polygon...
     lapply(a_contour_obj, function(obj, a_level, a_col) {
       polygon(obj$x, obj$y, col=ifelse(obj$level==a_level, a_col, rgb(0,1,0,0.0)), border = NA)
      # browser()
       }, a_level=the_min, a_col=cols[count])
   }
  
  sh1 <- readShapePoly(file.path(general$param.path,"shp","francois_EU"))
  plot(sh1, add=TRUE, col=grey(0.7))
  mtext(side=3, "(c)", adj=0, line=1, cex=1.8)
  #legend("bottomright", legend=paste('st.', c(2,5,9,13,14,15, 20, 21, 26, 29)), fill=cols, ncol=2, cex=0.8, bg="white", box.col="white")  
  legend("bottomright", legend=paste(c('her.nsea','spr.nsea','cod.nsea','pok.nsea','had.nsea','nop.nsea', 'san.nsea', 'mac.nsea', 'ple.nsea')), fill=cols, ncol=2, cex=0.8, bg="white", box.col="white")  
  box()
  


   ## e.g. different stocks EXPLICIT BALTIC SEA
  contour3 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "2",
                          a.stock    = "3",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "0",
                          general=general,
                          do_PBSmap =FALSE)
  contour4 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "2",
                          a.stock    = "4",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "0",
                          general=general,
                          do_PBSmap =FALSE)
  contour7 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "2",
                          a.stock    = "7",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "0",
                          general=general,
                          do_PBSmap =FALSE)
  contour10 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "2",
                          a.stock    = "10",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "4",
                          general=general,
                          do_PBSmap =FALSE)
   contour11 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "2",
                          a.stock    = "11",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "4",
                          general=general,
                          do_PBSmap =FALSE)
  contour12 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "2",
                          a.stock    = "12",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "4",
                          general=general,
                          do_PBSmap =FALSE)
 contour30 <- get_contour_this_avai ( 
                          a.number   = "045",
                          a.semester = "2",
                          a.stock    = "30",
                          a.folder   = "stochast_avai",
                          a.szgroup  = "4",
                          general=general,
                          do_PBSmap =FALSE)
   plot(0,0, xlim=c(8,22), ylim=c(53,60), xlab="Longitude", ylab="Latitude", col=2, type="n")
   cols <- c(
            rgb(0,1,0,0.5),
            rgb(1,1,0,0.5),
            rgb(0,1,1,0.5),
            rgb(0.75,0.75,0.75,0.5),
            rgb(0.75,1,0,0.5),
            rgb(0.25,0.5,0.75,0.5),
            rgb(0.5,0.0,0.5,0.5),  
            rgb(1,0.25,0.25,0.5),
            rgb(0.0,0.7,0.7,0.5)    
             ) #  plot(1:9,col=cols, pch=16,cex=3)
 count <- 0
 for(i in c(3,4,7,10,11,12,30)){
     count <- count+1
     a_contour_obj <- get(paste("contour",i,sep=''))
     dd            <- unlist(lapply(a_contour_obj, function(obj) {obj$level}) ) 
     the_min       <- min(dd[dd!=0], na.rm=TRUE) # should correspond to the larger polygon...
     lapply(a_contour_obj, function(obj, a_level, a_col) {
       polygon(obj$x, obj$y, col=ifelse(obj$level==a_level, a_col, rgb(0,1,0,0.0)), border = NA)
      # browser()
       }, a_level=the_min, a_col=cols[count])
   }
  
  sh1 <- readShapePoly(file.path(general$param.path,"shp","francois_EU"))
  plot(sh1, add=TRUE, col=grey(0.7))
  mtext(side=3, "(d)", adj=0, line=1, cex=1.8)
  #  legend("bottomright", legend=paste('st.', c(3,4,7,10,11,12,30)), fill=cols, ncol=3, cex=0.8, bg="white", box.col="white") 
    legend("bottomright", legend=paste(c('her.3a22','her.2532','spr.2232','cod.kat','cod.2224','cod.2532','sol.kask')), fill=cols, ncol=3, cex=0.8, bg="white", box.col="white") 

  box()
  
      
   
  dev.off()
  
  




  ###################
  ###################
   # balticonly
   
  namefile       <- paste(paste("map_avai_balticonly", sep=""))
 output.folder  <- file.path(general$main.path, general$namefolderinput)
  
 tiff(filename=file.path(output.folder, paste(namefile, ".tiff", sep="" )),
                                   width = 2500, height = 1400, 
                                   units = "px", pointsize = 12,  res=300)
 
   # plot 
   # semester1
    contour1 <- get_contour_this_avai ( 
                          a.number   = "000",
                          a.semester = "1",
                          a.stock    = "3",
                          a.folder   = "static_avai",
                          a.szgroup  = "2",
                          general=general,
                          do_PBSmap =FALSE)
   contour2 <- get_contour_this_avai ( 
                          a.number   = "000",
                          a.semester = "1",
                          a.stock    = "7",
                          a.folder   = "static_avai",
                          a.szgroup  = "2",
                          general=general,
                          do_PBSmap =FALSE)
   contour3 <- get_contour_this_avai ( 
                          a.number   = "000",
                          a.semester = "1",
                          a.stock    = "10",
                          a.folder   = "static_avai",
                          a.szgroup  = "6",
                          general=general,
                          do_PBSmap =FALSE)
 # semester2 
    contour4 <- get_contour_this_avai ( 
                          a.number   = "000",
                          a.semester = "2",
                          a.stock    = "3",
                          a.folder   = "static_avai",
                          a.szgroup  = "2",
                          general=general,
                          do_PBSmap =FALSE)
   contour5 <- get_contour_this_avai ( 
                          a.number   = "000",
                          a.semester = "2",
                          a.stock    = "7",
                          a.folder   = "static_avai",
                          a.szgroup  = "2",
                          general=general,
                          do_PBSmap =FALSE)
   contour6 <- get_contour_this_avai ( 
                          a.number   = "000",
                          a.semester = "2",
                          a.stock    = "10",
                          a.folder   = "static_avai",
                          a.szgroup  = "6",
                          general=general,
                          do_PBSmap =FALSE)

 par(mfrow=c(1,2))
   plot(0,0, xlim=c(9, 15), ylim=c(53.5,57), xlab="Longitude", ylab="Latitude", col=2, type="n")
lapply(contour1, function(obj, a_level) {
       polygon(obj$x, obj$y, col=ifelse(obj$level>0, rgb(0.75,1,0,0.5),rgb(0,1,1,0.0)), border = NA)
       })
 lapply(contour2, function(obj, a_level) {
       polygon(obj$x, obj$y, col=ifelse(obj$level>0, rgb(1,0.25,0.25,0.5),rgb(0,1,1,0.0)), border = NA)
       })
 lapply(contour3, function(obj, a_level) {
       polygon(obj$x, obj$y, col=ifelse(obj$level>0,rgb(0,1,1,0.5),rgb(0,1,1,0.0)), border = NA)
       })
  sh1 <- readShapePoly(file.path(general$main.path.param,"shp","francois_EU"))
  plot(sh1, add=TRUE, col=grey(0.5))
  mtext(side=3, "(a)", adj=0, line=1, cex=1.8)
  legend("bottomright", legend=paste(c('HER3a22', 'SPR2232', 'COD2224')), fill=c( rgb(0.75,1,0,0.5),rgb(1,0.25,0.25,0.5),rgb(0,1,1,0.5) ), ncol=2, cex=0.8, bg="white", box.col="white")  
  box()
  # semester2
  plot(0,0, xlim=c(9, 15), ylim=c(53.5,57), xlab="Longitude", ylab="Latitude", col=2, type="n")
lapply(contour4, function(obj, a_level) {
       polygon(obj$x, obj$y, col=ifelse(obj$level>0, rgb(0.75,1,0,0.5),rgb(0,1,1,0.0)), border = NA)
       })
 lapply(contour5, function(obj, a_level) {
       polygon(obj$x, obj$y, col=ifelse(obj$level>0, rgb(1,0.25,0.25,0.5),rgb(0,1,1,0.0)), border = NA)
       })
 lapply(contour6, function(obj, a_level) {
       polygon(obj$x, obj$y, col=ifelse(obj$level>0,rgb(0,1,1,0.5),rgb(0,1,1,0.0)), border = NA)
       })
  sh1 <- readShapePoly(file.path(general$main.path.param,"shp","francois_EU"))
  plot(sh1, add=TRUE, col=grey(0.5))
  mtext(side=3, "(b)", adj=0, line=1, cex=1.8)
 
  dev.off()




 ##---------------------------------------------------------------------------##
 ##---------------------------------------------------------------------------##
 ##---------------------------------------------------------------------------##
 ##---------------------------------------------------------------------------##
 ##---------------------------------------------------------------------------##
 ##---------------------------------------------------------------------------##
 ##---------------------------------------------------------------------------##
 ##---------------------------------------------------------------------------##
 ##---------------------------------------------------------------------------##
 ##---------------------------------------------------------------------------##
 ##---------------------------------------------------------------------------##
 ##---------------------------------------------------------------------------##

 # STRIKING PICTURE FOR PLOS ONE
 # 1-avai
 
  # plot 
   # semester1
    contour1 <- get_contour_this_avai ( 
                          a.number   = "000",
                          a.semester = "1",
                          a.stock    = "3",
                          a.folder   = "static_avai",
                          a.szgroup  = "2",
                          general=general,
                          do_PBSmap =FALSE)
   contour2 <- get_contour_this_avai ( 
                          a.number   = "000",
                          a.semester = "1",
                          a.stock    = "7",
                          a.folder   = "static_avai",
                          a.szgroup  = "2",
                          general=general,
                          do_PBSmap =FALSE)
   contour3 <- get_contour_this_avai ( 
                          a.number   = "000",
                          a.semester = "1",
                          a.stock    = "10",
                          a.folder   = "static_avai",
                          a.szgroup  = "6",
                          general=general,
                          do_PBSmap =FALSE)
 # 2-zonation
        
         # What has been used for the WMF graph definition
          library(maptools)
          library(rgdal)
         # planned nearshore windmills  # http://www.ens.dk/en/supply/renewable-energy/wind-power/offshore-wind-power/new-nearshore-wind-tenders
          windmills_KriegersFlak                      <<- readShapePoly(file.path(general$main.path.param,"shp","Windmills", "November_2013", "Havvindmoeller", "KriegersFlak_polygon.shp"),
                                   proj4string=CRS("+proj=utm  +ellps=intl +zone=32 +towgs84=-81,-89,-115,0,0,0,0,0"))
          windmills_KriegersFlak                      <<- spTransform(windmills_KriegersFlak, CRS("+proj=longlat +ellps=WGS84"))    # convert to longlat

          windmills_Kystmølle_zoner2012                      <<- readShapePoly(file.path(general$main.path.param,"shp","Windmills", "November_2013", "Havvindmoeller", "Kystmølle_zoner2012.shp"),
                                   proj4string=CRS("+proj=utm  +ellps=intl +zone=32 +towgs84=-81,-89,-115,0,0,0,0,0"))
          windmills_Kystmølle_zoner2012                      <<- spTransform(windmills_Kystmølle_zoner2012, CRS("+proj=longlat +ellps=WGS84"))    # convert to longlat

          windmills_U_omr_StoreMiddelgrund                      <<- readShapePoly(file.path(general$main.path.param,"shp","Windmills", "November_2013", "Havvindmoeller", "U_omr_StoreMiddelgrund.shp"),
                                   proj4string=CRS("+proj=utm  +ellps=intl +zone=32 +towgs84=-81,-89,-115,0,0,0,0,0"))
          windmills_U_omr_StoreMiddelgrund                      <<- spTransform(windmills_U_omr_StoreMiddelgrund, CRS("+proj=longlat +ellps=WGS84"))    # convert to longlat

          windmills_U_omr_Ronne                      <<- readShapePoly(file.path(general$main.path.param,"shp","Windmills", "November_2013", "Havvindmoeller", "U_omr_Ronne.shp"),
                                   proj4string=CRS("+proj=utm  +ellps=intl +zone=32 +towgs84=-81,-89,-115,0,0,0,0,0"))
          windmills_U_omr_Ronne                      <<- spTransform(windmills_U_omr_Ronne, CRS("+proj=longlat +ellps=WGS84"))    # convert to longlat

          windmills_U_omr_Ringkobing                      <<- readShapePoly(file.path(general$main.path.param,"shp","Windmills", "November_2013", "Havvindmoeller", "U_omr_Ringkobing.shp"),
                                   proj4string=CRS("+proj=utm  +ellps=intl +zone=32 +towgs84=-81,-89,-115,0,0,0,0,0"))
          windmills_U_omr_Ringkobing                      <<- spTransform(windmills_U_omr_Ringkobing, CRS("+proj=longlat +ellps=WGS84"))    # convert to longlat

          windmills_U_omr_Jammmerbugt                      <<- readShapePoly(file.path(general$main.path.param,"shp","Windmills", "November_2013", "Havvindmoeller", "U_omr_Jammmerbugt.shp"),
                                   proj4string=CRS("+proj=utm  +ellps=intl +zone=32 +towgs84=-81,-89,-115,0,0,0,0,0"))
          windmills_U_omr_Jammmerbugt                      <<- spTransform(windmills_U_omr_StoreMiddelgrund, CRS("+proj=longlat +ellps=WGS84"))    # convert to longlat
 
  
          nb_areas <- 7
          # plot polygons retrieved from the map 'windmills_OWP_Ostsee_30102013' (Leyre Goti) using locator()!!
          # sent by Leyre Goti 7 Jan14
          #windmills_OWP_Ostsee_30102013                      <<- readShapePoly(file.path(general$main.path.ibm,"shp","Windmills", "January_2014_ger", "OWP_Ostsee_30102013.shp"),
          #                               proj4string=CRS("+proj=utm  +ellps=intl +zone=32 +towgs84=-81,-89,-115,0,0,0,0,0"))
          #plot(windmills_OWP_Ostsee_30102013, add=TRUE, border=3)
          assign("pol1", list(x=c(11.41674, 11.53503, 11.55322, 11.60175, 11.56839),
             y=c(54.46500, 54.40737, 54.41344, 54.41647, 54.43770))) # ger  from locator()
          assign("pol2", list(x=c(11.38338, 11.41978, 11.42281, 11.38338),
             y=c(54.24056, 54.25573, 54.30729, 54.29516))) # ger from locator()
          assign("pol3", list(x=c(13.55495, 13.79758, 13.95226, 13.89160, 13.79758, 13.80061, 13.73692, 13.61561),
             y=c(54.83501, 54.77132, 54.76222, 54.90174, 54.89567, 54.80772, 54.80468, 54.85624))) # ger from locator()
          assign("pol4", list(x=c(13.98562, 14.04325, 14.15243, 14.17669, 14.02505),
             y=c( 54.92297, 54.76222, 54.75009, 54.80468, 54.92903))) # ger from locator()   
          # plot polygons retrieved from the map 'windmillmap1' using locator()!!
          assign("pol5", list(x=c( 15.37101, 14.73002, 15.00666, 14.98642, 15.18883, 15.78258, 15.50595, 15.39125),
             y=c( 54.58778, 54.52937, 54.49433, 54.42424, 54.33858, 54.44760, 54.51380, 54.52158))) # poland from locator()
          assign("pol6", list(x=c(  12.20245, 12.06557, 12.02824, 12.16512),
             y=c(  56.58406, 56.68640, 56.65911, 56.53972))) # swe kattegat, Stora middelgrund
          assign("pol7", list(x=c(  12.38911, 12.33311, 12.29578, 12.33311),
             y=c( 56.83308, 56.86719, 56.84672, 56.80238))) # swe kattegat, kattegat offshore skottarevprojektet

          # NATURA 2000 zonation
          NATURA2000          <<- readShapePoly(file.path(general$main.path.param,"shp", "Natura2000_end2012_rev1_9_15_53_60","Natura2000_end_2012_rev1_9_15_53_60_water_1.shp"), 
                                    proj4string= CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"))
          NATURA2000_proj       <<- spTransform(NATURA2000,CRS("+proj=longlat +ellps=WGS84")) # actually,  already in WGS


  # 3-individual vessels
 
 #obs
 #load( file=file.path(general$main.path,"merged_tables", general$case_study,
 #          paste("lst_per_date_", paste(general$case_study_countries, collapse="_"),"_",general$a.year,".RData",sep='')))      # get "lst_per_date"

 #sim
 sce <- "area_closure_WMF_NAT2000"
 general$case_study           <- "baltic_only"
 general$case_study_countries <- "DEN_DEU_SWE"
 load( file=file.path(general$main.path.param,"merged_tables", general$case_study,
           paste("lst_per_date", paste(general$case_study_countries, collapse="_"),"_",general$a.year,"_",sce,"_", sim, ".RData",sep='')))      # get "lst_per_date"

  




   
################
################
### THE PLOT ###
################
################
 # STRIKING PICTURE FOR PLOS ONE
  tiff(filename=file.path(general$main.path, general$namefolderinput, paste("DISPLACE_a_striking_plot", ".tiff", sep="" )),
                                   width = 2500, height = 2500, 
                                   units = "px", pointsize = 12,  res=300)

   plot(0,0, xlim=c(9, 15), ylim=c(53.5,57), xlab="Longitude", ylab="Latitude", col=2, type="n")
   lapply(contour1, function(obj, a_level) {
       polygon(obj$x, obj$y, col=ifelse(obj$level>0, rgb(0.75,1,0,0.5),rgb(0,1,1,0.0)), border = NA)
       })
   lapply(contour2, function(obj, a_level) {
       polygon(obj$x, obj$y, col=ifelse(obj$level>0, rgb(1,0.25,0.25,0.5),rgb(0,1,1,0.0)), border = NA)
       })
   lapply(contour3, function(obj, a_level) {
       polygon(obj$x, obj$y, col=ifelse(obj$level>0,rgb(0,1,1,0.5),rgb(0,1,1,0.0)), border = NA)
       })
   #sh1 <- readShapePoly(file.path(general$main.path.param,"shp","francois_EU"))
   #plot(sh1, add=TRUE, col=rgb(0.6,0.4,0.3,1))
   map("worldHires", add=TRUE, fill=TRUE, col=rgb(0.6,0.4,0.3,1))
   box()

  # plot the grid
  points(coord[,'x'], coord[,'y'], col=grey(0.8), pch=".")
  
   # planned
   plot(windmills_KriegersFlak,add=TRUE, border="purple", lwd=1, density=30, angle=90 )
   plot(windmills_Kystmølle_zoner2012,add=TRUE, border="purple", lwd=1, density=30, angle=90 )
   plot(windmills_U_omr_StoreMiddelgrund,add=TRUE, border="purple", lwd=1, density=30, angle=90 )
   plot(windmills_U_omr_Ronne,add=TRUE, border="purple", lwd=1, density=30, angle=90 )
   plot(windmills_U_omr_Ringkobing,add=TRUE, border="purple", lwd=1, density=30, angle=90 )
   plot(windmills_U_omr_Jammmerbugt,add=TRUE, border="purple", lwd=1, density=30, angle=90 )

  
   # plot WMF polygons retrieved from the map 
   polygon(pol1, border="purple", lwd=1, density=30, angle=90 ) 
   polygon(pol2, border="purple", lwd=1, density=30, angle=90 ) 
   polygon(pol3, border="purple", lwd=1, density=30, angle=90 ) 
   polygon(pol4, border="purple", lwd=1, density=30, angle=90 ) 
   polygon(pol5, border="purple", lwd=1, density=30, angle=90 ) 
   polygon(pol6, border="purple", lwd=1, density=30, angle=90 ) 
   polygon(pol7, border="purple", lwd=1, density=30, angle=90 ) 
     
     
          
   plot(NATURA2000_proj,add=TRUE, density=30, col="red", border="red", lwd=1)

     
   # the vessels 
   the.colors <- c(grey(0.0), grey(0.3), grey(0.5))
   a.df <- lst_per_date[[ 47 ]]
 
   a.df$ctry <- factor(a.df$ctry)
   levels(a.df$ctry)  <- c(1:3)
   an <- function(x) as.numeric(as.character(x))
   if(!is.null(a.df)){
     for(i in 1: nrow(a.df)){
        segments(an(a.df$SI_LONG.1[i]), an(a.df$SI_LATI.1[i]), an(a.df$SI_LONG[i]), an(a.df$SI_LATI[i]),
                      lty=1, lwd=2, col=the.colors[as.numeric(a.df[i, "ctry" ])])                 
        points(an(a.df$SI_LONG[i]), an(a.df$SI_LATI[i]), col=the.colors[as.numeric(a.df[i, "ctry"])],cex=0.8, pch=as.numeric(as.character(a.df[, 'vsize'])))
      
        library(shape) # to get Arrows()
 
          Arrows(an(a.df$SI_LONG.1[i]), an(a.df$SI_LATI.1[i]), an(a.df$SI_LONG[i]), an(a.df$SI_LATI[i]), col=the.colors[as.numeric(a.df[i, "ctry"])] ,
           length=0.05, lwd=2.5, code=2,  segment = FALSE, arr.length = 0.3)
 
       }
     }


    
  # the legends
  legend("topleft", legend=paste(c('DEN', 'GER', 'SWE')), fill=c( the.colors ), ncol=1, cex=1, bg="white", box.col="white")
  legend("bottomright", legend=paste(c('HER3a22', 'SPR2232', 'COD2224')),
            fill=c( rgb(0.75,1,0,0.5),rgb(1,0.25,0.25,0.5),rgb(0,1,1,0.5) ), ncol=3, cex=1, bg="white", box.col="white")
  legend("topright", legend=paste(c('Natura2000', 'Windmill farms')),
            border=c( "red", "purple" ), fill=c( "red", "purple" ), density=c(30,30), angle=c(45, 180), ncol=1, cex=1, bg="white", box.col="white")


  dev.off()
  
  
