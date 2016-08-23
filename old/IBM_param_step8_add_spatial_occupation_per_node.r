
## INFORM THE SPATIAL OCCUPATION OF OTHER SEA USERS
# should be useful to understand how much Danish fishermen are constrained
# in their movements by the shipping routes, etc. around Denmark.

 general <- list()
 general$lim.lat <- c(49,70) # default
 general$lim.long <-  c(-8.5,25) # default
 general$main.path <- file.path("C:","displace-project.org","repository","ibm_vessels_param")


#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
# LOOK AT THE OPERATIONAL CODE AT THE VERY END
# (OTHER THINGS ARE JUST EXPLORATORY....) 
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
 
 


#---------------------------------------
#---------------------------------------
# utils
convertWGS2UTM <- function(){
   ## convert coord WGS into UTM
   library(rgdal)

  y.wgs <- c(41.4036923115558, 41.38010926480547, 41.41148169051035,
  41.409534433320914)
  x.wgs <- c(2.145810127258301, 2.1807003021240234, 2.2183799743652344,
  2.1170568466186523)
  SP <- SpatialPoints(cbind(x.wgs, y.wgs),
  proj4string=CRS("+proj=longlat +datum=WGS84"))
  spTransform(SP,
     CRS("+proj=utm  +ellps=intl +zone=31 +towgs84=-84,-107,-120,0,0,0,0,0"))    # convert to UTM

  #spTransform(SP,CRS("+proj=longlat +datum=WGS84")) # convert back to WGS

  #for setting the +towgs84 string arg:
  # see Grids & Datums at http://www.asprs.org/resources/grids/
  return()
  }

#---------------------------------------
# from DTU-Aqua (Kirsten) server: \\aqua-gisfil01\gis
# from DTU-Aqua now it is : \\aqua-pfil01\gis
#Y:\GEOdata\BasicLayers\ProtectedAreas\Natura2000_Områder.shp
#Y:\GEOdata\BasicLayers\ProtectedAreas\Sverige\natura2000_sverige.shp
#Y:\GEOdata\BasicLayers\ProtectedAreas\Germany\habitat_omraader_BS_region.shp
#Y:\GEOdata\BasicLayers\ProtectedAreas\Poland\wgs84\oso.shp
#Y:\GEOdata\BasicLayers\ProtectedAreas\Poland\wgs84\soo.shp
#Y:\GEOdata\BasicLayers\Items\Windmills\Store_vindmoelleparker_polygon.shp
#Y:\GEOdata\BasicLayers\Items\Windmills\Windmills_oct_2011_offshore.shp
#Y:\GEOdata\BasicLayers\Military\fareomraader.shp
#Y:\GEOdata\BasicLayers\Military\Skydeomraader_2012.shp
#Y:\GEOdata\BasicLayers\Military\Military.shp

#http://www.danskehavnelods.dk/indexkort/danskesoekort.html




 ## TO DO: proj to be adapted (seems to be UTM)
 ## proj ok for poland

 library(maptools)
 sh_coastlines            <- readShapePoly(file.path(general$main.path,"shp","francois_EU"))
 ProtectedAreas1          <- readShapePoly(file.path("C:","shp","ProtectedAreas","Denmark","Natura2000_Områder.shp"),  proj4string=CRS("+proj=utm  +ellps=intl +zone=32 +towgs84=-81,-89,-115,0,0,0,0,0"))
 ProtectedAreas2          <- readShapePoly(file.path("C:","shp","ProtectedAreas","Sverige", "natura2000_sverige.shp"),  proj4string=CRS("+proj=tmerc +lat_0=0 +lon_0=15.8082777778 +k=1 +x_0=1500000 +y_0=0 +ellps=bessel +units=m +no_defs"))
 ProtectedAreas3          <- readShapePoly(file.path("C:","shp","ProtectedAreas","Germany", "habitat_omraader_BS_region.shp"),  proj4string=CRS("+proj=longlat +ellps=WGS84"))
 ProtectedAreas4          <- readShapePoly(file.path("C:","shp","ProtectedAreas","Poland","wgs84","oso.shp"),  proj4string=CRS("+proj=longlat +ellps=WGS84"))
 ProtectedAreas5          <- readShapePoly(file.path("C:","shp","ProtectedAreas","Poland","wgs84","soo.shp"),  proj4string=CRS("+proj=longlat +ellps=WGS84"))
 Military1                <- readShapePoly(file.path("C:","shp","Military","fareomraader.shp"),  proj4string=CRS("+proj=longlat +ellps=WGS84"))
 Military2                <- readShapePoly(file.path("C:","shp","Military","Skydeomraader_2012.shp"),  proj4string=CRS("+proj=merc +ellps=WGS84 +units=m "))
 Military3                <- readShapePoly(file.path("C:","shp","Military","Military.shp"),  proj4string=CRS("+proj=longlat +ellps=WGS84"))

 # from Leyre Goti, further processed by Kerstin G. to restrict to western baltic
 NATURA2000          <- readShapePoly(file.path(general$main.path,"shp", "Natura2000_end2012_rev1_9_15_53_60","Natura2000_end_2012_rev1_9_15_53_60_water_1.shp"), 
                                    proj4string=CRS("+proj=utm  +ellps=intl +zone=32 +towgs84=-81,-89,-115,0,0,0,0,0"))

 
 # harmonize the geo. projections
 # (for setting the +towgs84 string arg:
 # see Grids & Datums at http://www.asprs.org/resources/grids/)

 library(rgdal)
 ProtectedAreas1_proj <- spTransform(ProtectedAreas1,CRS("+proj=longlat +ellps=WGS84"))  # convert fro UTM to lomglat WGS
 ProtectedAreas2_proj <- spTransform(ProtectedAreas2,CRS("+proj=longlat +ellps=WGS84"))  # convert fro UTM to lomglat WGS
 ProtectedAreas3_proj <- spTransform(ProtectedAreas3,CRS("+proj=longlat +ellps=WGS84"))  # convert fro UTM to lomglat WGS
 ProtectedAreas4_proj <- spTransform(ProtectedAreas4,CRS("+proj=longlat +ellps=WGS84"))  # actually,  already in WGS
 ProtectedAreas5_proj <- spTransform(ProtectedAreas5,CRS("+proj=longlat +ellps=WGS84"))  # actually,  already in WGS
 Military1_proj       <- spTransform(Military1,CRS("+proj=longlat +ellps=WGS84"))  # actually,  already in WGS
 Military2_proj       <- spTransform(Military2,CRS("+proj=longlat +ellps=WGS84"))  # convert fro UTM to lomglat WGS
 Military3_proj       <- spTransform(Military3,CRS("+proj=longlat +ellps=WGS84")) # actually,  already in WGS

 NATURA2000_proj       <- spTransform(NATURA2000,CRS("+proj=longlat +ellps=WGS84")) # actually,  already in WGS


 # plot the graph
 library(maps)
 library(mapdata)
 windows(15,15)
 #map("worldHires", xlim=general$lim.long, ylim=general$lim.lat)
 #zoom:
 map("worldHires", xlim=c(10,15), ylim=c(53.5,56.2))
 #zoom: map("worldHires", xlim=c(9,13), ylim=c(53,56.2))
 #zoom: map("worldHires", xlim=c(5,14), ylim=c(53,56))
#zoom:  map("worldHires", xlim=c(7,15), ylim=c(53,58))

 plot(0,0, type="n", xlim=c(10,15), ylim=c(53.5,56.2), xlab="Longitude", ylab="Latitude")
 plot(sh_coastlines, add=TRUE)  # better for plotting the western baltic sea coastline!
 plot(NATURA2000_proj, add=TRUE, col=2)
 
 
 plot(ProtectedAreas1_proj, add=TRUE, col=1)
 plot(ProtectedAreas2_proj, add=TRUE, col=2)
 plot(ProtectedAreas3_proj, add=TRUE, col=3)
 plot(ProtectedAreas4_proj, add=TRUE, col=4)
 plot(ProtectedAreas5_proj, add=TRUE, col=4)
 plot(Military1_proj, add=TRUE, col=4)
 plot(Military2_proj, add=TRUE, col=6)
 plot(Military3_proj, add=TRUE, col=7)

 savePlot(filename=file.path(general$main.path,"igraph",
       paste("map_graph6_protected_and_military_areas_by_polygons.jpeg",sep='')), type="jpeg")

 ##----------------------------------
 ##  do a nicer plot using ggmap
 require(ggmap)
 require(mapproj)
 map.center <- geocode("copenhaguen")
 SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom=7)
 SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="stamen",maptype="toner",zoom=7)
 print(SHmap)


 ncmap = get_map(location=as.vector(bbox(cbind(c(7,15), c(53,58)))),source="stamen",maptype="toner",zoom=7)
 print(ggmap(ncmap))

 # transform in df for ggmap...
 polys1 <- fortify(ProtectedAreas1_proj)
 polys2 <- fortify(ProtectedAreas2_proj)
 polys3 <- fortify(ProtectedAreas3_proj)
 polys4 <- fortify(ProtectedAreas4_proj)
 polys5 <- fortify(ProtectedAreas5_proj)

 #ggplot()+ geom_polygon( aes(x = long, y=lat, group =  group), colour = "red", fill = "red", data=polys) # works ok

 # ggmap(ncmap, extent = "device") +          # no exact match.....
  SHmap + # while this works ok....
    geom_polygon( aes(x = long, y=lat, group =  group, colour = "red", fill = "red"), data=polys1) +
     geom_polygon( aes(x = long, y=lat, group =  group, colour = "red", fill = "blue"), data=polys4) +
   geom_point(aes(x=12.42478, y=55.81565), col=3, pch=16)
 ggmap(ncmap)+ geom_point( aes(x = long, y=lat, colour = "red"), data=polys)  # works ok




 ##-----------------------------------------------------------------------------
 ##-----------------------------------------------------------------------------

 # read the graph
 ## FILES FOR BUILDING A IGRAPH
 load(file.path(general$main.path,"igraph","6_graphibm.RData")) # built from the R code

 #----------------------------
 ## 1. in case of polygon-oriented data,
 ##  match each layer with the node coordinates
 ## using point.in.polygon facilities
 #----------------------------
 #----------------------------
 #--------------------------- 
 # loop over the SpatialPoly
 detectCoordInPolygonsFromSH <- function (sh, coord, name_column="poly1"){

     coord <- eval(parse(text=paste("cbind(coord, ",name_column,"= 0)", sep='')))
     dd                   <- sapply(slot(sh, "polygons"), function(x) lapply(slot(x, "Polygons"), function(x) x@coords)) # tricky there...
     library(sp)
     for(iLand in 1:length(dd)){
      if(length(dd)>1){
       for(i in 1:length(dd[[iLand]])){
          print(iLand)
          # Points on land are 1 or 2 and not is 0
          er <- try({res   <- point.in.polygon(coord[,1],coord[,2],dd[[iLand]][[i]][,1],dd[[iLand]][[i]][,2])}, silent=TRUE)
          if(class(er)=="try-error") res   <- point.in.polygon(coord[,1],coord[,2],dd[[iLand]][[i]][,1],dd[[iLand]][[i]][,2])
          coord[which(res!=0), name_column] <- 1
       }
      } else{
          er <- try({res   <- point.in.polygon(coord[,1],coord[,2],dd[[1]][,1],dd[[1]][,2])}, silent=TRUE)
          if(class(er)=="try-error") res   <- point.in.polygon(coord[,1],coord[,2],dd[[1]][,1],dd[[1]][,2])
          coord[which(res!=0), name_column] <- 1
      
      } 
      
     }
 return(coord)
 }


# a more efficient function?...need first to transform with e.g. fortify(ProtectedAreas1_proj)
detectCoordInPolygonsFromFortify <- function (df_fortify, coord, name_column="poly1"){

     coord <- eval(parse(text=paste("cbind(coord, ",name_column,"= 0)", sep='')))
     lst <- lapply(split(df_fortify, f="id"),
        function(x, coord){
         point.in.polygon(coord[,1],coord[,2],x$long, x$lat)
     }, coord=coord)
     coord[,name_column] <- unsplit(lst, f="id")
 return(coord)
 }

 # calls
 coord <-  detectCoordInPolygonsFromSH (ProtectedAreas1_proj, coord, name_column="PA1")
 coord <-  detectCoordInPolygonsFromSH (ProtectedAreas2_proj, coord, name_column="PA2")
 coord <-  detectCoordInPolygonsFromSH (ProtectedAreas3_proj, coord, name_column="PA3")
 coord <-  detectCoordInPolygonsFromSH (ProtectedAreas4_proj, coord, name_column="PA4")
 coord <-  detectCoordInPolygonsFromSH (ProtectedAreas5_proj, coord, name_column="PA5")
 coord <-  detectCoordInPolygonsFromSH (Military1_proj, coord, name_column="M1")
 coord <-  detectCoordInPolygonsFromSH (Military2_proj, coord, name_column="M2")
 coord <-  detectCoordInPolygonsFromSH (Military3_proj, coord, name_column="M3")
 #or...
 #coord <- detectCoordInPolygonsFromFortify (df_fortify=ProtectedAreas1_proj, coord=coord, name_column="poly1")


 plot(0,0, type="n", xlim=c(10,15), ylim=c(53.5,56.2), xlab="Longitude", ylab="Latitude")
 plot(sh_coastlines, add=TRUE)  # better for plotting the western baltic sea coastline!
 # all regulated areas
 the_columns <- c(grep("PA",colnames(coord)), grep("M",colnames(coord)) )
 the_cols    <- apply(coord[, the_columns],1, sum)+1
 the_cexs    <- 1+ apply(coord[,the_columns],1, sum)/2
 points(coord[,"x"], coord[,"y"],  col=the_cols, pch=".", cex=the_cexs)

 savePlot(filename=file.path(general$main.path,"igraph",
       paste("map_graph6_protected_and_military_areas_by_nodes.jpeg",sep='')), type="jpeg")




 #----------------------------
 #----------------------------
 ## 2.  for raster layer
 ## e.g. AIS density map (from Joni Kaitaranta [mailto:joni.kaitaranta@helcom),
 ## use point-raster overlay.......
 #----------------------------
 #----------------------------
  library(raster)
  ais <- raster(file.path("C:","shp","AIS_2011_HELCOM", "_ags_AIS_Montly_Ave_Density_2011.tif"))  # +proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
  newproj <- "+proj=longlat +datum=WGS84"
  ais_proj <- projectRaster(ais, crs=newproj)
  
  dd <- extract (ais_proj, coord[,1:2]) # get the value of AIS on the coord points!

  coord <- cbind(coord,  ais_code=cut(dd, breaks=c(0,50,100,200,400,100000)))


  # plot and save...
  plot(0,0, type="n", xlim=c(10,15), ylim=c(53.5,56.2), xlab="Longitude", ylab="Latitude")
  plot(sh_coastlines, add=TRUE)  # better for plotting the western baltic sea coastline!
  points(coord[,"x"], coord[,"y"], col=coord[,"ais_code"], pch=".", cex=1+ coord[,"ais_code"]/5)

  savePlot(filename=file.path(general$main.path,"igraph",
       paste("map_graph6_ais.jpeg",sep='')), type="jpeg")


    # plot and save...
  plot(0,0, type="n", xlim=c(8,15), ylim=c(53.5,60), xlab="Longitude", ylab="Latitude")
  plot(sh_coastlines, add=TRUE)  # better for plotting the western baltic sea coastline!
  points(coord[,"x"], coord[,"y"], col=coord[,"ais_code"], pch=".", cex=1+ coord[,"ais_code"]/5)


  ##-----------------------------
  ## to do: save coord and do a c++ input file
  # export in txt format for use with C
  # THIS WAY IS BETTER BECAUSE C IS ROW ORIENTED
  write(signif(coord, 6),file=file.path(general$main.path,"igraph", "coord6_with_occupation.dat"), ncol=1)
  nrow(coord)
  ncol(coord)
  # remnber that the idx.port is related to the one in harbour.list



  ##-----------------------------
  # QGIS testing
  # to plot with seaCharts
  SP <- SpatialPoints(cbind(coord[,"x"], coord[,"y"]), proj4string=CRS("+proj=longlat +datum=WGS84"))
  coord_UTM <- spTransform(SP,  # transform to the same projection of the sea charts
     CRS("+proj=merc +lon_0=9 +lat_ts=56 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))    # convert to UTM  zone denamrk
  write.table(coord_UTM, file=file.path(general$main.path,"igraph", "coord6_UTM.csv"),
                 col.names=TRUE, row.names=FALSE)  # import in QGIS with layer>add delimited text


  # a shortset path
  path <- a.shortest.path (from = 20, to = 9750, g=g, is.plot=TRUE, a.color=2)
  SP <- SpatialPoints(cbind(coord[path,"x"], coord[path,"y"]), proj4string=CRS("+proj=longlat +datum=WGS84"))
  path_coord_UTM <- spTransform(SP,  # transform to the same projection of the sea charts
     CRS("+proj=merc +lon_0=9 +lat_ts=56 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))    # convert to UTM  zone denamrk
  path_coord_UTM <- cbind(coordinates(path_coord_UTM), group=1,  order=1:length(path)) # for the Python plugin  Points to Paths
  colnames(path_coord_UTM) <- c('x','y','group','order')
  write.table(path_coord_UTM, file=file.path(general$main.path,"igraph", "path_coord6_UTM.csv"),
                 col.names=TRUE, row.names=FALSE)

   # or for a given vessel...
   load(file.path(general$main.path,"merged_tables",
           paste("all_merged_weight_",general$a.country,"_",general$a.year,".RData",sep='')))
  this_vid   <- all.merged [all.merged$VE_REF %in% "DNK000001744", c('SI_LONG','SI_LATI', 'SI_DATE','SI_TIME')]
  ctime      <- strptime(  paste(this_vid$SI_DATE, " ", this_vid$SI_TIME, sep="") ,  "%e/%m/%Y %H:%M" )
  this_vid   <- cbind.data.frame( this_vid, date.in.R=ctime)
  this_vid   <- this_vid[order(this_vid$date.in.R),] # caution here: need to reorder in chrological time
  SP         <- SpatialPoints(cbind(an(this_vid[,"SI_LONG"]), an(this_vid[,"SI_LATI"])), proj4string=CRS("+proj=longlat +datum=WGS84"))
  this_vid_path_coord_UTM <- spTransform(SP,  # transform to the same projection of the sea charts
                               CRS("+proj=merc +lon_0=9 +lat_ts=56 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))    # convert to UTM  zone denamrk
  this_vid_path_coord_UTM <- cbind.data.frame(coordinates(this_vid_path_coord_UTM), group=1,  order=1:nrow(this_vid)) # for the Python plugin  Points to Paths
  colnames(this_vid_path_coord_UTM) <- c('x','y','group','order')
  write.table(this_vid_path_coord_UTM, file=file.path(general$main.path,"igraph", "this_vid_path_coord6_UTM.csv"),
                 col.names=TRUE, row.names=FALSE)

  # in QGIS, create a new polygon vector layer and use the toogle menu to drawn some polygons on it,
  # then use the 'spatial query' QGIS plugin to define intersections with the underlying node layer...
  # (i.e. install the python plugin and then click on the icon in the bottom task bar)
  # e.g. http://www.youtube.com/watch?v=ARqpBuEQ3rM
  # for example the nodes lying in the user defined polygons can be selected and then
  # a new layer created from the selection (caution: via the node layer menu: save selection as...)
  # what is useful is also this give the index of the selected nodes!!!
  
  
  
  
  
  
  
  ##-----------------------------
  ##-----------------------------
  ##-----------------------------
  ##---OPERATIONAL CODE----------
  ##-----------------------------
  ##----------------------------- 
  ##-----------------------------
  
  general <- list()
  general$lim.lat <- c(49,70) # default
  general$lim.long <-  c(-8.5,25) # default
  general$main.path <- file.path("C:","displace-project.org","repository","ibm_vessels_param")

  # from Leyre Goti, further processed by Kerstin G. to restrict to western baltic
  # European Environment Agency http://www.eea.europa.eu/data-and-maps/data/natura-4/natura-2000-tabular-data-12-tables
  # NOTE THAT THESE AREAS ARE PROTECTED 'ON THE PAPER'
  # BUT NOTHING SPECIFY THAT THE FISHING ACTIVITIES ARE RESTRICTED INSIDE THEM...
  # BECAUSE THE USUAL (EU or NATIONAL) PROCEDURE IS TO CONDUCT AN IMPACT ASSESSEMENT FOR EACH OF THEM...WHICH IS NOT DONE YET!
  
  # first, understand which projection has been used...
  #NATURA2000<-readOGR(dsn=file.path(general$main.path,"shp", "Natura2000_end2012_rev1_9_15_53_60","Natura2000_end_2012_rev1_9_15_53_60_water_1.shp"), layer="Natura2000_end_2012_rev1_9_15_53_60_water_1") #read in file
  #proj4string(NATURA2000) 

  png(filename = file.path(general$main.path,"shp", paste("NATURA2000.png",sep="")),
                                   width = 2400, height = 2400, 
                                   units = "px", pointsize = 12,  res=300)   # high resolution plot

  NATURA2000            <- readShapePoly(file.path(general$main.path,"shp", "Natura2000_end2012_rev1_9_15_53_60","Natura2000_end_2012_rev1_9_15_53_60_water_1.shp"), 
                                    proj4string= CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"))
  NATURA2000_proj       <- spTransform(NATURA2000,CRS("+proj=longlat +ellps=WGS84")) # actually,  already in WGS

  plot(0,0, type="n", xlim=c(10,15), ylim=c(53.5,56.2), xlab="Longitude", ylab="Latitude")
  plot(sh_coastlines, add=TRUE)  # better for plotting the western Baltic Sea coastline!
  plot(NATURA2000_proj, add=TRUE, col=2)

 
  load(file.path(general$main.path,"igraph","11_graphibm.RData")) # built from the R code
   
  coord <-  detectCoordInPolygonsFromSH (NATURA2000_proj, coord, name_column="Natura2000")

  #points(coord[,1], coord[,2], col=coord[,4], pch=".")
  
  write(signif(coord, 6),file=file.path(general$main.path,"igraph", "coord11_with_occupation.dat"), ncol=1)
  nrow(coord)
  ncol(coord)
 
  dev.off()
 #----------------------------


  
  
  
  
  
  
  
  
