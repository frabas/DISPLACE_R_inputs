
  general <- list()
  general$main.path <- file.path("C:","displace-project.org","repository","ibm_vessels_param")
  general$main.path.ibm <- file.path("C:","displace-project.org","repository","ibm_vessels_param")


##------------------------------------------------------##
##------------------------------------------------------##
##------------------------------------------------------##
##------------------------------------------------------##
##------------------------------------------------------##

 list_files <- list.files ("C:\\displace-project.org\\repository\\ibm_vessels_input\\vesselsspe_balticonly")
 list_files <- list_files[grep("cpue_per_stk_on_nodes_quarter1", list_files)  ]
 list_files <- list_files[grep("gscale", list_files, invert=TRUE) ]
 list_files <- list_files[grep("gshape", list_files, invert=TRUE) ]
 # save .dat files
   cpue_all <- NULL
   for (a_file in list_files){
        cpue <- read.table(file=paste("C:\\displace-project.org\\repository\\ibm_vessels_input\\vesselsspe_balticonly\\",a_file, sep=""), header=TRUE)
        cpue <- cbind(cpue, st=rep(0:30, length=nrow(cpue)))
      cpue_all <- rbind(cpue_all, cpue)
      }
 list_files <- list.files ("C:\\displace-project.org\\repository\\ibm_vessels_input\\vesselsspe_balticonly")
 list_files <- list_files[grep("cpue_per_stk_on_nodes_quarter2", list_files)  ]
 list_files <- list_files[grep("gscale", list_files, invert=TRUE) ]
 list_files <- list_files[grep("gshape", list_files, invert=TRUE) ]
 # save .dat files
   for (a_file in list_files){
        cpue <- read.table(file=paste("C:\\displace-project.org\\repository\\ibm_vessels_input\\vesselsspe_balticonly\\",a_file, sep=""), header=TRUE)
        cpue <- cbind(cpue, st=rep(0:30, length=nrow(cpue)))
      cpue_all <- rbind(cpue_all, cpue)
      }
 list_files <- list.files ("C:\\displace-project.org\\repository\\ibm_vessels_input\\vesselsspe_balticonly")
 list_files <- list_files[grep("cpue_per_stk_on_nodes_quarter3", list_files)  ]
 list_files <- list_files[grep("gscale", list_files, invert=TRUE) ]
 list_files <- list_files[grep("gshape", list_files, invert=TRUE) ]
 # save .dat files
   for (a_file in list_files){
        cpue <- read.table(file=paste("C:\\displace-project.org\\repository\\ibm_vessels_input\\vesselsspe_balticonly\\",a_file, sep=""), header=TRUE)
        cpue <- cbind(cpue, st=rep(0:30, length=nrow(cpue)))
      cpue_all <- rbind(cpue_all, cpue)
      }
 list_files <- list.files ("C:\\displace-project.org\\repository\\ibm_vessels_input\\vesselsspe_balticonly")
 list_files <- list_files[grep("cpue_per_stk_on_nodes_quarter4", list_files)  ]
 list_files <- list_files[grep("gscale", list_files, invert=TRUE) ]
 list_files <- list_files[grep("gshape", list_files, invert=TRUE) ]
 # save .dat files
   for (a_file in list_files){
        cpue <- read.table(file=paste("C:\\displace-project.org\\repository\\ibm_vessels_input\\vesselsspe_balticonly\\",a_file, sep=""), header=TRUE)
        cpue <- cbind(cpue, st=rep(0:30, length=nrow(cpue)))
      cpue_all <- rbind(cpue_all, cpue)
      }



cpue_all_1 <- cpue_all[cpue_all$cpue_kghour>10,]
agg_cpue <- aggregate(cpue_all_1$cpue_kghour, list(cpue_all_1$st, cpue_all_1$pt_graph), mean)
colnames(agg_cpue) <- c( 'st','pt_graph', 'cpue_kghour')



##------------------------------------------------------##
##------------------------------------------------------##
##------------------------------------------------------##
##------------------------------------------------------##
##------------------------------------------------------##
agg_cpue_st <- agg_cpue[agg_cpue$st==7,]   # 10: western cod (but also Kattegat cod as seems to be a mistake when defining area of stocks?)

## CAUTION: STATIC CPUEs here

 # load the graph
 load(file.path(general$main.path, "igraph", paste(11, "_graphibm.RData",sep=''))) # built from the R code

# retreive coordinates from the graph
agg_cpue_st$x <- coord[agg_cpue_st$pt_graph, 'x']
agg_cpue_st$y <- coord[agg_cpue_st$pt_graph, 'y']

# plot
 plot(agg_cpue_st$x, agg_cpue_st$y, cex=sqrt(agg_cpue_st$cpue_kghour)/15, pch=16,xlim=c(9,15), ylim=c(53.5,57.5), )

 library(maptools)
 sh1 <- readShapePoly(file.path(general$main.path,"shp","francois_EU"))

 plot(sh1, add=TRUE, col=grey(0.5))



##------------------------------------------------------##
##------------------------------------------------------##
##------------------------------------------------------##
##------------------------------------------------------##
##------------------------------------------------------##
## TEST INCLUSION
##------------------------------------------------------##
##------------------------------------------------------##
##------------------------------------------------------##
##------------------------------------------------------##

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


   library(sp)
   library(rgdal)
   windmills_KriegersFlak                      <<- readShapePoly(file.path(general$main.path.ibm,"shp","Windmills", "November_2013", "Havvindmoeller", "KriegersFlak_polygon.shp"),
                                   proj4string=CRS("+proj=utm  +ellps=intl +zone=32 +towgs84=-81,-89,-115,0,0,0,0,0"))
  windmills_KriegersFlak                      <<- spTransform(windmills_KriegersFlak, CRS("+proj=longlat +ellps=WGS84"))    # convert to longlat

  windmills_Kystmølle_zoner2012                      <<- readShapePoly(file.path(general$main.path.ibm,"shp","Windmills", "November_2013", "Havvindmoeller", "Kystmølle_zoner2012.shp"),
                                   proj4string=CRS("+proj=utm  +ellps=intl +zone=32 +towgs84=-81,-89,-115,0,0,0,0,0"))
  windmills_Kystmølle_zoner2012                      <<- spTransform(windmills_Kystmølle_zoner2012, CRS("+proj=longlat +ellps=WGS84"))    # convert to longlat

  windmills_U_omr_StoreMiddelgrund                      <<- readShapePoly(file.path(general$main.path.ibm,"shp","Windmills", "November_2013", "Havvindmoeller", "U_omr_StoreMiddelgrund.shp"),
                                   proj4string=CRS("+proj=utm  +ellps=intl +zone=32 +towgs84=-81,-89,-115,0,0,0,0,0"))
  windmills_U_omr_StoreMiddelgrund                      <<- spTransform(windmills_U_omr_StoreMiddelgrund, CRS("+proj=longlat +ellps=WGS84"))    # convert to longlat

  windmills_U_omr_Ronne                      <<- readShapePoly(file.path(general$main.path.ibm,"shp","Windmills", "November_2013", "Havvindmoeller", "U_omr_Ronne.shp"),
                                   proj4string=CRS("+proj=utm  +ellps=intl +zone=32 +towgs84=-81,-89,-115,0,0,0,0,0"))
  windmills_U_omr_Ronne                      <<- spTransform(windmills_U_omr_Ronne, CRS("+proj=longlat +ellps=WGS84"))    # convert to longlat

  windmills_U_omr_Ringkobing                      <<- readShapePoly(file.path(general$main.path.ibm,"shp","Windmills", "November_2013", "Havvindmoeller", "U_omr_Ringkobing.shp"),
                                   proj4string=CRS("+proj=utm  +ellps=intl +zone=32 +towgs84=-81,-89,-115,0,0,0,0,0"))
  windmills_U_omr_Ringkobing                      <<- spTransform(windmills_U_omr_Ringkobing, CRS("+proj=longlat +ellps=WGS84"))    # convert to longlat

  windmills_U_omr_Jammmerbugt                      <<- readShapePoly(file.path(general$main.path.ibm,"shp","Windmills", "November_2013", "Havvindmoeller", "U_omr_Jammmerbugt.shp"),
                                   proj4string=CRS("+proj=utm  +ellps=intl +zone=32 +towgs84=-81,-89,-115,0,0,0,0,0"))
  windmills_U_omr_Jammmerbugt                      <<- spTransform(windmills_U_omr_StoreMiddelgrund, CRS("+proj=longlat +ellps=WGS84"))    # convert to longlat




    # quantify what is included in the polygon
  plot(windmills_KriegersFlak,add=TRUE, border="red", lwd=2)
  plot(windmills_Kystmølle_zoner2012,add=TRUE, border="red", lwd=2)
  plot(windmills_U_omr_StoreMiddelgrund,add=TRUE, border="red", lwd=2)
  plot(windmills_U_omr_Ronne,add=TRUE, border="red", lwd=2)
  plot(windmills_U_omr_Ringkobing,add=TRUE, border="red", lwd=2)
  plot(windmills_U_omr_Jammmerbugt,add=TRUE, border="red", lwd=2)


    # plot polygons retrieved from the map
    polygon(pol1, border="blue", lwd=2)
    polygon(pol2, border="blue", lwd=2)
    polygon(pol3, border="blue", lwd=2)
    polygon(pol4, border="blue", lwd=2)
    polygon(pol5, border="blue", lwd=2)
    polygon(pol6, border="blue", lwd=2)
    polygon(pol7, border="blue", lwd=2)


    # NATURA 2000 special case
    NATURA2000          <<- readShapePoly(file.path(general$main.path,"shp", "Natura2000_end2012_rev1_9_15_53_60","Natura2000_end_2012_rev1_9_15_53_60_water_1.shp"),
                                    proj4string= CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"))
    NATURA2000_proj       <<- spTransform(NATURA2000,CRS("+proj=longlat +ellps=WGS84")) # actually,  already in WGS

    plot(NATURA2000_proj, add=TRUE)

    # unwrap the complex polygon object
    the_names <- NULL
    for(i in 1: length(NATURA2000_proj@polygons)){
    for(j in 1: length(NATURA2000_proj@polygons[[i]]@Polygons)){
     assign(paste("a_poly_", i,"_",j, sep=''),
              NATURA2000_proj@polygons[[i]]@Polygons[[j]]@coords
              )
     the_names <- c(the_names, paste("a_poly_", i,"_",j, sep=''))

    } }



   ## BUILD A TABLE WITH PERCENTAGE EFFORT PER POLYGON
   polygons <- c("windmills_KriegersFlak", "windmills_Kystmølle_zoner2012",
                    "windmills_U_omr_StoreMiddelgrund", "windmills_U_omr_Ronne",
                     "windmills_U_omr_Ringkobing", "windmills_U_omr_Jammmerbugt",
                      "pol1", "pol2", "pol3", "pol4", "pol5", "pol6", "pol7", the_names)

   agg_cpue_st <- cbind.data.frame(agg_cpue_st, inclusion=rep(0,nrow(agg_cpue_st)))
   count <- 0
   for(pol in polygons){
   print(pol)
   count <- count+1
      a_poly <- get(pol)
      if(class(a_poly)=="SpatialPolygonsDataFrame"){
         dd     <- a_poly@polygons[[1]]@Polygons[[1]]@coords
         dd     <- list(x=dd[,1], y=dd[,2])
      } else{
         if(!is.list(a_poly)) a_poly <- list(x=a_poly[,1], y=a_poly[,2])
         dd     <- a_poly
         }


      agg_cpue_st[,"inclusion"] <- pmax(agg_cpue_st[,"inclusion"], point.in.polygon( agg_cpue_st[,'x'], agg_cpue_st[,'y'],dd$x,dd$y))
    }







##------------------------------------------------------##
##------------------------------------------------------##
##------------------------------------------------------##
##------------------------------------------------------##
##------------------------------------------------------##
## TEST
##------------------------------------------------------##
##------------------------------------------------------##
##------------------------------------------------------##
##------------------------------------------------------##

agg_cpue_st <-  agg_cpue_st[agg_cpue_st$y<56,] # remove Kattegat!! ## CAUTION!

summary(lm(cpue_kghour~inclusion, data=agg_cpue_st))
anova(lm(cpue_kghour~inclusion, data=agg_cpue_st))

 t.test(
       agg_cpue_st[agg_cpue_st$inclusion==0,"cpue_kghour"],
       agg_cpue_st[agg_cpue_st$inclusion==1,"cpue_kghour"],
        )
plot(density(agg_cpue_st[agg_cpue_st$inclusion==0,"cpue_kghour"]))
lines(density(agg_cpue_st[agg_cpue_st$inclusion==1,"cpue_kghour"]), add=TRUE, col=2)

 plot(agg_cpue_st$x, agg_cpue_st$y, cex=1, pch=16, col=agg_cpue_st$inclusion+1, xlim=c(9,15), ylim=c(53.5,57.5))
     # quantify what is included in the polygon
  plot(windmills_KriegersFlak,add=TRUE, border="red", lwd=2)
  plot(windmills_Kystmølle_zoner2012,add=TRUE, border="red", lwd=2)
  plot(windmills_U_omr_StoreMiddelgrund,add=TRUE, border="red", lwd=2)
  plot(windmills_U_omr_Ronne,add=TRUE, border="red", lwd=2)
  plot(windmills_U_omr_Ringkobing,add=TRUE, border="red", lwd=2)
  plot(windmills_U_omr_Jammmerbugt,add=TRUE, border="red", lwd=2)


    # plot polygons retrieved from the map
    polygon(pol1, border="blue", lwd=2)
    polygon(pol2, border="blue", lwd=2)
    polygon(pol3, border="blue", lwd=2)
    polygon(pol4, border="blue", lwd=2)
    polygon(pol5, border="blue", lwd=2)
    polygon(pol6, border="blue", lwd=2)
    polygon(pol7, border="blue", lwd=2)

    plot(NATURA2000_proj, add=TRUE)


##------------------------------------------------------##
##------------------------------------------------------##
##------------------------------------------------------##
##------------------------------------------------------##

   Satellite.Palette <-colorRampPalette(c("blue3","cyan","aquamarine","yellow","orange","red"))

   agg_cpue_st$bins <-  cut(agg_cpue_st[,"cpue_kghour"],
                                 breaks=quantile(agg_cpue_st[,"cpue_kghour"], probs=c(0,.05,0.1,0.15,.2,0.25,.3,0.35,.4,.45,.5,.55,.6,.65,.7,.75,.8,.85,.9,0.95,1)))
   plot(agg_cpue_st$x, agg_cpue_st$y, cex=1, pch=16, col=Satellite.Palette(21)[agg_cpue_st[,"bins"]], xlim=c(9,15), ylim=c(53.5,57.5))
     # quantify what is included in the polygon
  plot(windmills_KriegersFlak,add=TRUE, border="red", lwd=2)
  plot(windmills_Kystmølle_zoner2012,add=TRUE, border="red", lwd=2)
  plot(windmills_U_omr_StoreMiddelgrund,add=TRUE, border="red", lwd=2)
  plot(windmills_U_omr_Ronne,add=TRUE, border="red", lwd=2)
  plot(windmills_U_omr_Ringkobing,add=TRUE, border="red", lwd=2)
  plot(windmills_U_omr_Jammmerbugt,add=TRUE, border="red", lwd=2)


    # plot polygons retrieved from the map
    polygon(pol1, border="blue", lwd=2)
    polygon(pol2, border="blue", lwd=2)
    polygon(pol3, border="blue", lwd=2)
    polygon(pol4, border="blue", lwd=2)
    polygon(pol5, border="blue", lwd=2)
    polygon(pol6, border="blue", lwd=2)
    polygon(pol7, border="blue", lwd=2)

    plot(NATURA2000_proj, add=TRUE)
