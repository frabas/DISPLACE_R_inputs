

  general           <- list()
  general$main.path <- file.path("C:","Users", "fba", "Dropbox", "ibm_vessels_param")
  general$igraph    <- 4
  general$a.year    <- "2010"
  general$a.country <- "DNK"



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
load(file.path(general$main.path,"igraph","4_graphibm.RData")) # built from the R code

 #  a plot
 library(maptools)
 library(maps)
 library(mapdata)
 windows(5,5)
 plot(coord[,1], coord[,2], pch=".", xlab="Longitude", ylab="Latitude", xlim=c(-8,23), ylim=c(52,65))
 ices_areas <- readShapeSpatial("C:\\Users\\fba\\Dropbox\\ibm_vessels_param\\ices_areas\\ices_areas")
 plot(ices_areas, col="white",  add=TRUE)
 points(coord[,1], coord[,2], pch=".")
 box()
  map("worldHires", add=TRUE)
 library(igraph)
 #a.shortest.path  (from = 2, to = 3170, g=g, is.plot=TRUE, a.color=5)
#a.shortest.path  (from = 725, to = 2341, g=g, is.plot=TRUE, a.color=3)



##--------------------------------------------------------------------------##
##-------------READ THE FILES ----------------------------------------------##
##--------------------------------------------------------------------------##

a.quarter <- "1"
fgrounds <- read.table( file=file.path(general$main.path, "vesselsspe",
              paste("vesselsspe_fgrounds_quarter",gsub("Q","",a.quarter),".dat",sep='')),
                header=TRUE)
fgrounds$pt_graph <- fgrounds$pt_graph +1 ## convert from C++ file to R
harbours <- read.table( file=file.path(general$main.path, "vesselsspe",
              paste("vesselsspe_harbours_quarter",gsub("Q","",a.quarter),".dat",sep='')),
                header=TRUE)
harbours$pt_graph <- harbours$pt_graph +1 ## convert from C++ file to R

freq_fgrounds <- read.table( file=file.path(general$main.path, "vesselsspe",
              paste("vesselsspe_freq_fgrounds_quarter",gsub("Q","",a.quarter),".dat",sep='')),
                header=TRUE)

##--------------------------------------------------------------------------##
##-------------COMPUTE DISTANCE---------------------------------------------##
##--------------------------------------------------------------------------##

fgrounds <- cbind(fgrounds, distance_to_closest_port=0)
for (a_line in 1: nrow(fgrounds)) {
   this_vid_harbours <-  harbours[harbours$VE_REF==as.character(fgrounds[a_line,'VE_REF']), 'pt_graph']
   if(length(this_vid_harbours)!=0){
     fgrounds [a_line,'distance_to_closest_port'] <-
      min(dist.paths (from=fgrounds[a_line, 'pt_graph'], to=this_vid_harbours, g=g, coord=coord), na.rm=TRUE)
    # a.shortest.path  (from = fgrounds[a_line, 'pt_graph'], to = this_vid_harbours[1], g=g, is.plot=TRUE, a.color=3)
     }
}


##--------------------------------------------------------------------------##
##-------------DO THE PLOT--- ----------------------------------------------##
##--------------------------------------------------------------------------##


 plot(fgrounds$distance_to_closest_port, freq_fgrounds$freq,  pch="+", lwd=2)

 count <-0
 for(vid in unique(fgrounds$VE_REF)) {
 count <- count+1
     plot(fgrounds[freq_fgrounds$VE_REF==vid,]$distance_to_closest_port,
                 freq_fgrounds[freq_fgrounds$VE_REF==vid,]$freq,  xlim=c(0,400), pch="+", lwd=2, col=count)
     browser()
     }



