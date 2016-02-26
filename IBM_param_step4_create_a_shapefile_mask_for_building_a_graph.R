 library(maptools)
 library(rgeos)
 library(raster)

 ices_areas             <<- readShapePoly(file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input", "graphsspe", "shp", "ices_areas","ices_areas.shp"),
                                    proj4string= CRS("+proj=longlat +ellps=WGS84"))

 # e.g.
 ices_areas[ices_areas$ICES_area == "29",]

 # zone to include in the mask
 ices_areas$ICES_area
 zones    <- c("IIIa", "22", "23", "24", "25")
 a_shape <- ices_areas[ices_areas$ICES_area %in% zones,]
 a_negative_shape <- ices_areas[!ices_areas$ICES_area %in% zones,]


 #polys <- SpatialPolygons(list(
 # Polygons(list(Polygon(), ID[1]))
 #))
 #proj4string(polys) <- CRS(proj4string(ices_areas))



 # reduce the complexity of the shapefile
 a_smoothed_shape <- gSimplify(a_shape, tol=0.01, topologyPreserve=TRUE)
 sauv <- a_smoothed_shape

 area <- lapply(a_smoothed_shape@polygons, function(x) sapply(x@Polygons, function(y) y@area))
 mainPolys <- lapply(area, function(x) which(x > 0.001))

     for(i in 1:length(mainPolys)){
      if(length(mainPolys[[i]]) >= 1 && mainPolys[[i]][1] >= 1){
        a_smoothed_shape@polygons[[i]]@Polygons  <- a_smoothed_shape@polygons[[i]]@Polygons[mainPolys[[i]]]
        a_smoothed_shape@polygons[[i]]@plotOrder <- 1:length(a_smoothed_shape@polygons[[i]]@Polygons)
      }
    }

 a_smoothed_shape <- SpatialPolygonsDataFrame(a_smoothed_shape, data=data.frame(1:length(mainPolys)), match.ID = FALSE)

 writeSpatialShape(a_smoothed_shape, file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input","graphsspe","shp","ices_areas_mask_for_myfish.shp")
                                   )



# reduce the complexity of the shapefile
 a_smoothed_negative_shape <- gSimplify(a_negative_shape, tol=0.01, topologyPreserve=TRUE)
 sauv <- a_smoothed_negative_shape

 area <- lapply(a_smoothed_negative_shape@polygons, function(x) sapply(x@Polygons, function(y) y@area))
 mainPolys <- lapply(area, function(x) which(x > 0.001))

     for(i in 1:length(mainPolys)){
      if(length(mainPolys[[i]]) >= 1 && mainPolys[[i]][1] >= 1){
        a_smoothed_negative_shape@polygons[[i]]@Polygons  <- a_smoothed_negative_shape@polygons[[i]]@Polygons[mainPolys[[i]]]
        a_smoothed_negative_shape@polygons[[i]]@plotOrder <- 1:length(a_smoothed_negative_shape@polygons[[i]]@Polygons)
      }
    }

 a_smoothed_negative_shape <- SpatialPolygonsDataFrame(a_smoothed_negative_shape, data=data.frame(1:length(mainPolys)), match.ID = FALSE)

 writeSpatialShape(a_smoothed_negative_shape, file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input","graphsspe","shp","ices_areas_negative_mask_for_myfish.shp")
                                   )
