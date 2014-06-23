
library(maptools)
general                        <- list()
general$lim.lat                <- c(49,70) # default
general$lim.long               <- c(-8.5,25) # default
general$main_path_ibm_param    <- file.path("C:","displace-project.org","repository","ibm_vessels_param")


sh_coastline      <- readShapePoly(file.path(general$main_path_ibm_param,"shp","francois_EU"),  proj4string=CRS("+proj=longlat +ellps=WGS84"))
sh_coastline_proj <- spTransform(sh_coastline, CRS("+proj=utm  +ellps=intl +zone=32 +towgs84=-84,-107,-120,0,0,0,0,0"))


dd <-sapply(slot(sh_coastline_proj, "polygons"), function(x) lapply(slot(x, "Polygons"), function(x) x@coords)) # tricky there...




 iLand <- 2
 library(sp)
pts <- SpatialPoints(cbind(dd[[iLand]][[1]][,1], dd[[iLand]][[1]][,2]))

#
# rgeos package
#

library(rgeos) # for gBuffer()


# create polygons buffering each point by 1km, then unioning all
# generated polygons together
pts.1nm <- gBuffer(pts, width=1853)
pts.2nm <- gBuffer(pts, width=1853*2)
pts.3nm <- gBuffer(pts, width=1853*3)
pts.4nm <- gBuffer(pts, width=1853*4)
plot(pts)
plot(pts.1nm)
plot(pts.2nm, add=TRUE)
plot(pts.3nm, add=TRUE)
plot(pts.4nm, add=TRUE)
plot(pts, add=TRUE)

# an attempt at a broder scale
plot(sh_coastline_proj, xlim=c(300000, 700000), ylim=c(5750000, 7290000)) # utm 32
for(iLand in 1:length(dd)){
  if(sh_coastline_proj@data$C_NAME[iLand]=="DENMARK") {
       p1 <- readWKT(paste("POLYGON((",paste(dd[[iLand]][[1]][,1], dd[[iLand]][[1]][,2], collapse=","),"))", sep=''))
       buff.1nm <- gBuffer(p1,width=1853)
       buff.10nm <- gBuffer(p1,width=1853*10)
       buff.20nm <- gBuffer(p1,width=1853*20)
       plot(buff.1nm, add=TRUE)
       plot(buff.10nm, add=TRUE)
       plot(buff.20nm, add=TRUE)
    }
}



##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

# e.g. from IBM_param_mapping_cpue_in_out_closure.R


# a BETTER attempt at a broader scale
plot(sh_coastline_proj, xlim=c(300000, 700000), ylim=c(5750000, 7290000)) # utm 32
all_polys_x <- NULL
all_polys_y <- NULL
for(iLand in 1:length(dd)){
  if(sh_coastline_proj@data$C_NAME[iLand]=="DENMARK") {
       all_polys_x <- c(all_polys_x, dd[[iLand]][[1]][,1])
       all_polys_y <- c(all_polys_y, dd[[iLand]][[1]][,2])
  }
 }
p1 <- SpatialPoints(cbind(all_polys_x, all_polys_y))
buff.0nm <- gBuffer(p1,width=1)
buff.10nm <- gBuffer(p1,width=1853*10)
buff.20nm <- gBuffer(p1,width=1853*20)
buff.30nm <- gBuffer(p1,width=1853*30)
buff.40nm <- gBuffer(p1,width=1853*40)
buff.50nm <- gBuffer(p1,width=1853*50)
buff.100nm <- gBuffer(p1,width=1853*100)
buff.200nm <- gBuffer(p1,width=1853*200)
plot(buff.10nm, add=TRUE)
plot(buff.20nm, add=TRUE)
plot(buff.30nm, add=TRUE)
plot(buff.40nm, add=TRUE)
plot(buff.50nm, add=TRUE)
plot(buff.100nm, add=TRUE)
plot(buff.200nm, add=TRUE)
plot(sh_coastline_proj, add=TRUE, col=grey(0.5))





# then build the buffer strips!
buff.10_20nm <- gDifference(buff.20nm, buff.10nm, drop_not_poly=FALSE)
buff.20_30nm <- gDifference(buff.30nm, buff.20nm, drop_not_poly=FALSE)
buff.30_40nm <- gDifference(buff.40nm, buff.30nm, drop_not_poly=FALSE)
buff.40_50nm <- gDifference(buff.50nm, buff.40nm, drop_not_poly=FALSE)
buff.50_100nm <- gDifference(buff.100nm, buff.50nm, drop_not_poly=FALSE)
buff.100_200nm <- gDifference(buff.200nm, buff.100nm, drop_not_poly=FALSE)

buff.10_20nm <- gDifference(buff.10_20nm, buff.0nm, drop_not_poly=FALSE)
buff.20_30nm <- gDifference(buff.20_30nm, buff.0nm, drop_not_poly=FALSE)
buff.30_40nm <- gDifference(buff.30_40nm , buff.0nm, drop_not_poly=FALSE)
buff.40_50nm <- gDifference(buff.40_50nm, buff.0nm, drop_not_poly=FALSE)
buff.50_100nm <- gDifference(buff.50_100nm, buff.0nm, drop_not_poly=FALSE)
buff.100_200nm <- gDifference(buff.100_200nm, buff.0nm, drop_not_poly=FALSE)
plot(buff.10_20nm, col=grey(0.9), add=TRUE)
plot(buff.20_30nm, col=grey(0.8), add=TRUE)
plot(buff.30_40nm, col=grey(0.7), add=TRUE)
plot(buff.40_50nm, col=grey(0.6), add=TRUE)
plot(buff.50_100nm, col=grey(0.5), add=TRUE)
plot(buff.100_200nm, col=grey(0.4), add=TRUE)
plot(sh_coastline_proj, add=TRUE, col=grey(0.5))



# apply to cpue data
agg_cpue_st <- agg_cpue[agg_cpue$st%in%c(8,9,10,11),]   # 10: western cod (but also Kattegat cod as seems to be a mistake when defining area of stocks?)

 # load the graph
 load(file.path(general$main.path, "igraph", paste(11, "_graphibm.RData",sep=''))) # built from the R code

# retreive coordinates from the graph
agg_cpue_st$x <- coord[agg_cpue_st$pt_graph, 'x']
agg_cpue_st$y <- coord[agg_cpue_st$pt_graph, 'y']

SP <- SpatialPoints(cbind(agg_cpue_st$x , agg_cpue_st$y ),proj4string=CRS("+proj=longlat +datum=WGS84"))
SP_proj <- spTransform(SP, CRS("+proj=utm  +ellps=intl +zone=32 +towgs84=-84,-107,-120,0,0,0,0,0"))    # convert to UTM
agg_cpue_st <- cbind(agg_cpue_st, SP_proj)


# plot
plot(sh_coastline_proj, xlim=c(400000, 600000), ylim=c(5800000, 6500000)) # utm 32

plot(buff.10_20nm, col=grey(0.9), add=TRUE)
plot(buff.20_30nm, col=grey(0.8), add=TRUE)
plot(buff.30_40nm, col=grey(0.7), add=TRUE)
plot(buff.40_50nm, col=grey(0.6), add=TRUE)
plot(buff.50_100nm, col=grey(0.5), add=TRUE)
plot(buff.100_200nm, col=grey(0.4), add=TRUE)
points(agg_cpue_st$coords.x1, agg_cpue_st$coords.x2, cex=sqrt(agg_cpue_st$cpue_kghour)/30, pch=16,xlim=c(9,15), ylim=c(53.5,57.5))
plot(sh_coastline_proj, add=TRUE, col=grey(0.5))


 for(a_buff in c("buff.10_20nm", 'buff.20_30nm','buff.30_40nm','buff.40_50nm','buff.50_100nm','buff.100_200nm')){
 
    buff              <- get(a_buff)
    in_out            <- matrix (over(SpatialPoints(agg_cpue_st[,c('coords.x1','coords.x2')]),  buff), ncol=1)
    colnames(in_out)  <- a_buff
    agg_cpue_st       <- cbind.data.frame(agg_cpue_st, in_out) 
 
 
 }
 
 # check
 pts <- agg_cpue_st[agg_cpue_st$buff.10_20nm==1, c('coords.x1','coords.x2')]
 points(pts[,1], pts[,2] , col="green")
 
 pts <- agg_cpue_st[agg_cpue_st$buff.50_100nm==1, c('coords.x1','coords.x2')]
 points(pts[,1], pts[,2] , col="purple")



 ## then aggregate the data per buffer zone.
 cpue_buff.10_20nm <- mean(agg_cpue_st[agg_cpue_st$buff.10_20nm==1, "cpue_kghour"], na.rm=TRUE)
   cpue_buff.20_30nm <- mean(agg_cpue_st[agg_cpue_st$buff.20_30nm==1, "cpue_kghour"], na.rm=TRUE)
 cpue_buff.30_40nm <- mean(agg_cpue_st[agg_cpue_st$buff.30_40nm==1, "cpue_kghour"], na.rm=TRUE)
 cpue_buff.40_50nm <- mean(agg_cpue_st[agg_cpue_st$buff.40_50nm==1, "cpue_kghour"], na.rm=TRUE)
 cpue_buff.50_100nm <- mean(agg_cpue_st[agg_cpue_st$buff.50_100nm==1, "cpue_kghour"], na.rm=TRUE)
 cpue_buff.100_200nm <- mean(agg_cpue_st[agg_cpue_st$buff.100_200nm==1, "cpue_kghour"], na.rm=TRUE)

 ## barplot
barplot(c('10_20nm'=cpue_buff.10_20nm, 
          '20_30nm'=cpue_buff.20_30nm,
          '30_40nm'=cpue_buff.30_40nm,
          '40_50nm'=cpue_buff.40_50nm,
          '50_100nm'=cpue_buff.50_100nm,
          '100_200nm' =cpue_buff.100_200nm ), main="cpue vs. distance to shore") 
 




