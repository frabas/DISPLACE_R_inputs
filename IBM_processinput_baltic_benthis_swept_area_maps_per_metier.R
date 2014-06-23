
  library(vmstools)
  outPath   <- "C:/BENTHIS/outputs/"

  load("C:\\BENTHIS\\outputs\\ALL_AggregatedSweptArea.RData")

 ## subset for the Western Baltic Sea and Kattegat
 ## because the coverage is not that good for Eastern Baltic Sea
 idx_pts <- point.in.polygon(point.x = aggResult[,'CELL_LONG'], point.y = aggResult[,'CELL_LATI'],
        pol.x = c(12, 12, 11.94, 11.97, 12, 12, 15, 15, 14.78,
            14.2, 13.7, 12.81, 12.44), pol.y = c(55.3, 54.75,
            54.67, 54.56, 54.56, 53.5, 53.5, 55, 55.3, 55.4,
            55.5, 55.38, 55.33)) > 0  | point.in.polygon(point.x = aggResult[,'CELL_LONG'], point.y = aggResult[,'CELL_LATI'],
       pol.x = c(8.648336, 7.034822, 7.357525, 9.083985, 9.608377,
            10.22958, 10.689431, 11.084742, 11.617201, 12.068985,
            11.972174, 10.59262, 9.971417, 9.39862, 8.648336),
       pol.y = c(57.08073, 57.99182, 58.20964, 58.87187, 59.32015,
            59.86417, 59.99375, 59.8804, 58.96783, 58.0774, 57.4653, #sd2224
            57.74247, 57.50441, 57.10708, 57.08073)) > 0  |
           point.in.polygon(point.x = aggResult[,'CELL_LONG'], point.y = aggResult[,'CELL_LATI'], #sd2224
       pol.x = c(12.297, 12.13, 12.45, 12.81, 12.94, 13.21,
            12.5, 12.448),
       pol.y = c(56.13, 55.48, 55.31, 55.38,
            55.41, 55.71, 56.29, 56.305)) > 0 |
            point.in.polygon(point.x = aggResult[,'CELL_LONG'], point.y = aggResult[,'CELL_LATI'],
       pol.x = c(10.1, 10.75, 10.71, 11.58, 11.58, 11.99, 11.94,
            11.97, 12, 12, 9.3, 9.3), pol.y = c(56.6, 56.3, 56.15,
            55.9, 55.65, 55, 54.67, 54.56, 54.56, 53.75, 53.75,   #sd2224
            56.6)) > 0 |
            point.in.polygon(point.x = aggResult[,'CELL_LONG'], point.y = aggResult[,'CELL_LATI'],
       pol.x = c(10.59262, 11.97217, 12.15883, 12.70796, 13.12992,
            12.80622, 12.95073, 12.72185, 12.45127, 12.29556,
            12.13384, 11.99063, 11.58487, 11.58487, 11.63281,
            11.49492, 11.3094, 11.27652, 10.71374, 10.70218,  #kask
            10.24553, 10.19351, 10.42472, 10.59262),
       pol.y = c(57.74247,
            57.4653, 57.48032, 56.94085, 56.46389, 56.36135,
            56.19091, 56.16918, 56.29535, 56.12728, 55.49119,
            55.28764, 55.63113, 55.91101, 55.90623, 55.94866,
            55.97965, 56.00988, 56.14253, 56.25853, 56.49587,
            57.11107, 57.63566, 57.74247)) > 0   # kask

  aggResult <- aggResult[idx_pts,]

xrange <- c(9,14)
yrange <- c(54,59)


 #- Plot all of it
 library(RColorBrewer)
 colintens             <- brewer.pal(9,"YlOrRd")


 #- Create polygon set of gridcells to plot (takes a bit longer)
 uniqueCells           <- aggResult[!duplicated(aggResult$grID),c("grID","CELL_LONG","CELL_LATI")]
 resx                  <- 1/60 #1 minute
 resy                  <- 1/60 #1 minute
 grdc2plot             <- lonLat2SpatialPolygons(lst=lapply(as.list(1:nrow(uniqueCells)),
                                                function(x){
                                                  data.frame(SI_LONG=c(uniqueCells[x,"CELL_LONG"]-resx/2,
                                                                       rep(uniqueCells[x,"CELL_LONG"]+resx/2,2),uniqueCells[x,"CELL_LONG"]-resx/2),
                                                             SI_LATI=c(rep(uniqueCells[x,"CELL_LATI"]-resy/2,2),rep(uniqueCells[x,"CELL_LATI"]+resy/2,2)))}))
 save(grdc2plot,file=file.path(outPath,"ALL_grdc2plot.RData"))


 #- Add any subset to plot: First, combine all metiers over all years
 subAggResult          <- aggResult
 data2plot             <- aggregate(subAggResult[,c("SWEPT_AREA_KM2",
                                                   "SWEPT_AREA_KM2_LOWER",
                                                   "SWEPT_AREA_KM2_UPPER")],by=list(subAggResult$grID),FUN=sum,na.rm=T)



 colnames(data2plot)[1]<- "grID"
 idx                   <- match(uniqueCells$grID, data2plot$grID)
 col2plot              <- c("white",colintens)[cut(data2plot[idx,"SWEPT_AREA_KM2"],breaks=c(-1,0,0.25,2,4,8,16,32, 10000))]


 # do the plot
 tiff(filename=file.path(outPath, paste("KASK_WBALTIC", ".tiff", sep="" )),
                                   width = 3400, height = 3400,
                                   units = "px", pointsize = 12,  res=500)

 plot(grdc2plot,add=FALSE,col=col2plot, border=0,xlim=xrange, ylim=yrange, asp=1/lonLatRatio(mean(xrange),mean(yrange)),
     xlab="Longitude",ylab="Latitude",las=1)   # sp:::plot.SpatialPolygons
   map("worldHires",  add=TRUE, col=rgb(0,0,0), fill=TRUE)

 axis(1) ; axis(2, las=2)
 box()

 # save in tiff
 dev.off()




 #- PER METIER
 for (a_metier in c("OT_CRU", "OT_DMF", "OT_MIX_NEP", "OT_SPF", "OT_MIX_DMF_PEL", "SDN_DEM")){
 subAggResult          <- aggResult [aggResult$LE_MET %in% a_metier, ]
 data2plot             <- aggregate(subAggResult[,c("SWEPT_AREA_KM2",
                                                   "SWEPT_AREA_KM2_LOWER",
                                                   "SWEPT_AREA_KM2_UPPER")],by=list(subAggResult$grID),FUN=sum,na.rm=T)



 colnames(data2plot)[1]<- "grID"
 idx                   <- match(uniqueCells$grID, data2plot$grID)
 col2plot              <- c("white",colintens)[cut(data2plot[idx,"SWEPT_AREA_KM2"],breaks=c(-1,0,0.25,2,4,8,16,32, 10000))]


 # do the plot
 tiff(filename=file.path(outPath, paste("KASK_WBALTIC_", a_metier, ".tiff", sep="" )),
                                   width = 3400, height = 3400,
                                   units = "px", pointsize = 12,  res=500)

 plot(grdc2plot,add=FALSE,col=col2plot, border=0,xlim=xrange, ylim=yrange, asp=1/lonLatRatio(mean(xrange),mean(yrange)),
     xlab="Longitude",ylab="Latitude",las=1)   # sp:::plot.SpatialPolygons
   map("worldHires",  add=TRUE, col=rgb(0,0,0), fill=TRUE)

 axis(1) ; axis(2, las=2)
 box()

 # save in tiff
 dev.off()

 }
