
predict_fuel_cons_from_kw <- function(x){
   # predict fuel cons per hour from kW
   table.fuelcons.per.engine <- read.table(file= file.path("C:","Users", "fba", "Dropbox","ibm_vessels_param",
             "IBM_datainput_engine_consumption.txt"), header=TRUE,sep="")
   linear.model <- lm(calc_cons_L_per_hr_max_rpm~ kW2, data=table.fuelcons.per.engine)  # conso = a*Kw +b
    #=> so, it is assumed here that knowing the kW of the vessel is enough
    # to guess its fuel consumption at maximal speed
   an  <- function(x) as.numeric(as.character(x))
   x$kW2            <- an(x$KW_HOURS) / (an(x$LE_EFF_VMS)/60) # retrieve info on KW
   x$fuel_cons_rate <- predict(linear.model, newdata=data.frame(kW2=an(x$kW2))) # Liter per hour
return(x)
}

  ICESarea2 <-
function (tacsat, string = TRUE)
{
    library(sp)
    ICES.area <- rep(NA, dim(tacsat)[1])
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(8.648336, 7.034822, 7.357525, 9.083985, 9.608377,
            10.22958, 10.689431, 11.084742, 11.617201, 12.068985,
            11.972174, 10.59262, 9.971417, 9.39862, 8.648336),
        pol.y = c(57.08073, 57.99182, 58.20964, 58.87187, 59.32015,
            59.86417, 59.99375, 59.8804, 58.96783, 58.0774, 57.4653,
            57.74247, 57.50441, 57.10708, 57.08073)) > 0] <- ifelse(string, 'kask', '0')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(10.59262, 11.97217, 12.15883, 12.70796, 13.12992,
            12.80622, 12.95073, 12.72185, 12.45127, 12.29556,
            12.13384, 11.99063, 11.58487, 11.58487, 11.63281,
            11.49492, 11.3094, 11.27652, 10.71374, 10.70218,
            10.24553, 10.19351, 10.42472, 10.59262), pol.y = c(57.74247,
            57.4653, 57.48032, 56.94085, 56.46389, 56.36135,
            56.19091, 56.16918, 56.29535, 56.12728, 55.49119,
            55.28764, 55.63113, 55.91101, 55.90623, 55.94866,
            55.97965, 56.00988, 56.14253, 56.25853, 56.49587,
            57.11107, 57.63566, 57.74247)) > 0] <-ifelse(string,'kask', '0')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(-4, 7, 8, 7, 7, 7.906163, -4, -4.6, -4.6, -4),
        pol.y = c(62, 62, 61.5, 60, 58, 57.5, 57.5, 57.3, 58.2,
            58.4)) > 0] <- ifelse(string,'nsea', '1')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(7.906163, 8.6488368, 8.5, 10.3, 10.3, 9.2,
            9.2, 7.11, -1, -4, -1.78), pol.y = c(57.5, 57.08073,
            57, 57.3, 57, 56.2, 52.7, 53.5, 53.5, 56.1, 57.5)) >
        0] <- ifelse(string,'nsea', '1')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(0, 0.5, 7.5, 7.5), pol.y = c(53.5, 51, 51,
            53.5)) > 0] <- ifelse(string,'nsea', '1')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(12, 12, 11.94, 11.97, 12, 12, 15, 15, 14.78,
            14.2, 13.7, 12.81, 12.44), pol.y = c(55.3, 54.75,
            54.67, 54.56, 54.56, 53.5, 53.5, 55, 55.3, 55.4,
            55.5, 55.38, 55.33)) > 0] <- ifelse(string,'2224', '2')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(14.2, 14.78, 15, 15, 18, 18, 14.2), pol.y = c(55.4,
            55.3, 55, 53, 53, 56.5, 56.5)) > 0] <- ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(18, 18, 22, 22), pol.y = c(56.5, 53.5, 53.5,
            56.5)) > 0] <- ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(18, 18, 18.32, 18.32, 19, 19, 16, 16), pol.y = c(56.5,
            57, 57, 57.5, 57.925, 59.762, 59.762, 56.5)) > 0] <- ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(19, 19, 18.45, 18.3, 18, 18, 21.5, 21.72, 21.98,
            22.17, 22.24, 21.93), pol.y = c(58.5, 57.9, 57.58,
            57, 57, 56.5, 56.5, 57.57, 57.97, 58.04, 58.15,
            58.5)) > 0] <- ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(21.5, 21.72, 21.98, 22.17, 22.24, 22.24, 23,
            25, 25), pol.y = c(56.5, 57.57, 57.97, 58.04, 58.15,
            58.35, 58.5, 58.5, 56.5)) > 0] <- ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(19, 17.975, 21.6, 21.8, 23.325, 23.325, 23.191,
            23, 23, 23.5, 23.6, 24, 23.692, 22.5, 22.1, 21.92,
            19), pol.y = c(59.762, 60.5, 60.5, 60.7, 60.5, 59.965,
            59.867, 59.827, 59, 59, 59.05, 58.75, 59.5, 59.5,
            58.35, 58.5, 58.5)) > 0] <- ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(16.5, 16.5, 19.7, 19.7, 22.6, 21.4), pol.y = c(60.5,
            63.7, 63.7, 63.5, 63.5, 60.5)) > 0] <- ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(19.7, 19.7, 25.7, 25.7, 19.7), pol.y = c(63.7,
            63.5, 63.5, 67, 67)) > 0] <-ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(23.325, 23.325, 23.191, 23, 23, 30.5, 30.5),
        pol.y = c(60.5, 59.965, 59.867, 59.827, 59, 59, 60.5)) >
        0] <- ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(12.297, 12.13, 12.45, 12.81, 12.94, 13.21,
            12.5, 12.448), pol.y = c(56.13, 55.48, 55.31, 55.38,
            55.41, 55.71, 56.29, 56.305)) > 0] <- ifelse(string,'2224', '2')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
        pol.x = c(10.1, 10.75, 10.71, 11.58, 11.58, 11.99, 11.94,
            11.97, 12, 12, 9.3, 9.3), pol.y = c(56.6, 56.3, 56.15,
            55.9, 55.65, 55, 54.67, 54.56, 54.56, 53.75, 53.75,
            56.6)) > 0] <- ifelse(string,'2224', '2')
    return(ICES.area)
}


##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!MAP FUEL CONS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

library(vmstools)

## sligtly modified:
 vmsGridCreate  <-
function (dF, nameLon = "Longitude", nameLat = "Latitude", nameVarToSum = "", nameVarToAverage = "",
    cellsizeX = 0.5, cellsizeY = 0.5, we = "", ea = "", so = "",
    no = "", gridValName = "fishing", plotMap = TRUE, plotTitle = "",
    numCats = 5, paletteCats = "heat.colors", addLegend = TRUE, cols="",
    legendx = "bottomleft", legendncol = 1, legendtitle = "fishing activity",
    plotPoints = TRUE, legPoints = FALSE, colPoints = 1, colLand = "sienna",
    addICESgrid = FALSE, addScale = TRUE, outGridFile = "", outPlot = "",
    ...)
{
    require(sp)
    require(maptools)
    lstargs <- list(...)
    if (nrow(dF) > 0) {
        if (we == "") {
            we = min(dF[[nameLon]], na.rm = TRUE)
            we = we - we%%cellsizeX
        }
        if (ea == "") {
            ea = max(dF[[nameLon]], na.rm = TRUE)
            ea = ea - ea%%cellsizeX + cellsizeX
        }
        if (so == "") {
            so = min(dF[[nameLat]], na.rm = TRUE)
            so = so - so%%cellsizeY
        }
        if (no == "") {
            no = max(dF[[nameLat]], na.rm = TRUE)
            no = no - no%%cellsizeY + cellsizeY
        }
        numXcells <- ceiling((ea - we)/cellsizeX)
        numYcells <- ceiling((no - so)/cellsizeY)
        numXcells <- abs(numXcells)
        numYcells <- abs(numYcells)
        gridTopology <- GridTopology(c(we + (cellsizeX/2), so +
            (cellsizeY/2)), c(cellsizeX, cellsizeY), c(numXcells,
            numYcells))
        spatialGrid <- SpatialGrid(grid = gridTopology)
        gridded(spatialGrid) = TRUE
        coords <- cbind(x = dF[[nameLon]], y = dF[[nameLat]])
        sPDF <- SpatialPointsDataFrame(coords, data = dF)
        gridCellIndexPerVMSpoint <- overlay(spatialGrid, sPDF)
        sPDF$gridCellIndex <- gridCellIndexPerVMSpoint
        if (nameVarToSum != "") {
            perCell <- tapply(sPDF[[nameVarToSum]], gridCellIndexPerVMSpoint,
                sum)
            # to inform better the breaks....
            print(round(quantile (perCell[perCell>5], probs=seq(0,1,by=0.1)),3))    # here modified

        }
        else {
         if (nameVarToAverage != "") {    # here modified
            perCell <-                       # here modified
            tapply(sPDF[[ nameVarToAverage[1] ]], gridCellIndexPerVMSpoint, sum, na.rm=TRUE)/   # here modified
                 tapply(sPDF[[ nameVarToAverage[2] ]], gridCellIndexPerVMSpoint, sum, na.rm=TRUE)    # here modified

        # to inform better the breaks....
           print(round(quantile (perCell[perCell!=0], probs=seq(0,1,by=0.1))))   # here modified


        }else{
            sPDF$ones <- 1
            perCell <- tapply(sPDF$ones, gridCellIndexPerVMSpoint,
                sum)
        }
        }
        dFdataEmpty <- data.frame(seqID = (1:(numXcells * numYcells)))
        spatialGridDataFrame <- SpatialGridDataFrame(grid = gridTopology,
            data = dFdataEmpty, proj4string = CRS("+init=epsg:9820"))  # here modified
        spatialGridDataFrame[[gridValName]] <- NA
        spatialGridDataFrame[[gridValName]][as.integer(names(perCell))] <- c(perCell)
        if (plotMap) {
            mapGrid(spatialGridDataFrame, sPDF, we = we, ea = ea,
                so = so, no = no, gridValName = gridValName,
                plotTitle = plotTitle, numCats = numCats, paletteCats = paletteCats,
                addLegend = addLegend, legendx = legendx, legendncol = legendncol,
                legendtitle = legendtitle, plotPoints = plotPoints,
                legPoints = legPoints, colPoints = colPoints, cols=cols,
                colLand = colLand, addICESgrid = addICESgrid,
                addScale = addScale, outGridFile = outGridFile,
                outPlot = outPlot, breaks0 = lstargs$breaks0)
        }
        if (outGridFile != "") {
            writeAsciiGrid(spatialGridDataFrame, outGridFile,
                na.value = -99.99, attr = gridValName, dec = ".")
        }
        if (outPlot != "") {
            savePlot(outPlot, type = "png")
        }
        invisible(spatialGridDataFrame)
    }
}

mapGrid  <-
function (sGDF, sPDF, we = "", ea = "", so = "", no = "", gridValName = "fishing",
    plotTitle = "", numCats = 5, paletteCats = "heat.colors",
    addLegend = TRUE, legendx = "bottomleft", legendncol = 1,
    legendtitle = "fishing activity", plotPoints = FALSE, colPoints = 1, cols=cols,
    legPoints = FALSE, colLand = "sienna", addICESgrid = FALSE,
    addScale = TRUE, outGridFile = "", outPlot = "", ...)
{
    require(sp)
    require(maptools)
    #par(mar = c(4, 6, 1, 1))
    xlim0 = c(we, ea)
    ylim0 = c(so, no)
    lstargs <- list(...)
    if (length(lstargs$breaks0) == 0) {
        breaks0 <- pretty(sGDF[[gridValName]], n = numCats)
    }
    else {
        breaks0 <- lstargs$breaks0
    }

    if(length(cols)<2) {
      cols <- rev(do.call(paletteCats, list(length(breaks0) - 1)))
      } else{
      cols <- cols [1:(length(breaks0) - 1)]     # here modified
      }

    library(mapdata)
     #library(mapproj)
     #map("worldHires", add=FALSE, project="albers", par=c(50, 65),  xlim = xlim0 + c(+0.1, -0.1), ylim = ylim0 + c(+0.1, -0.1))
     #map.grid(c(xlim0, ylim0), lty=1, col=grey(0.5))
   library(mapdata)

map("worldHires", add=FALSE,col=colLand,fill=TRUE, bg="white",  xlim=xlim0 + c(+0.1,-0.1), ylim=ylim0 + c(+0.1,-0.1),
regions=c('uk','ireland','france','germany','netherlands', 'norway','belgium', asp=4,
'spain','luxembourg','denmark', 'sweden','iceland', 'portugal','italy','sicily','ussr','sardinia','albania','monaco','turkey','austria',
'switzerland','czechoslovakia','finland','libya', 'hungary','yugoslavia','poland','greece','romania','bulgaria', 'slovakia','morocco',
'tunisia','algeria','egypt' ),  mar=c(5,5,1,1))      #     border=c(-1.0,-1.0)

    im <- as.image.SpatialGridDataFrame(sGDF, attr = 2)
    image(im$x, im$y, im$z, axes = FALSE, col = cols, breaks = breaks0,
        xlim = xlim0, ylim = ylim0, add = TRUE)


    if (addICESgrid) {
    #    for (i in seq(xlim0[1], xlim0[2], by = 1)) lines (mapproject(expand.grid(x=i, y = seq(ylim0[1], ylim0[2], by = 0.5))), col=grey(0.5))
    #    for (i in seq(ylim0[1], ylim0[2], by = 0.5)) lines (mapproject(expand.grid(x=seq(xlim0[1], xlim0[2], by = 1), y = i)), col=grey(0.5))
    }
#add ICES rectangles
if(addICESgrid){
  for(i in seq(-15,50, by=1)) abline(v=i, col=grey(0.9), lty=1, lwd=0.5)
  for(i in seq(0, 75, by=0.5)) abline(h=i, col=grey(0.9) , lty=1, lwd=0.5)
  }

map("worldHires", add=TRUE, col=colLand, fill=TRUE, bg="white",  xlim=xlim0 , ylim=ylim0 ,
regions=c('uk','ireland','france','germany','netherlands', 'norway','belgium',
'spain','luxembourg','denmark', 'sweden','iceland', 'portugal','italy','sicily','ussr','sardinia','albania','monaco','turkey','austria',
'switzerland','czechoslovakia','finland','libya', 'hungary','yugoslavia','poland','greece','romania','bulgaria', 'slovakia','morocco',
'tunisia','algeria','egypt' ),  mar=c(5,5,1,1))

    box()
    axis(1)
    axis(2, las = 2)
    if (we > 0) {
        mtext("Longitude", side = 1, line = 2)
    }
    else {
        mtext("Longitude", side = 1, line = 2)
    }
    if (no > 0) {
        mtext("Latitude", side = 2, line = 2)
    }
    else {
        mtext("Latitude", side = 2, line = 2)
    }
    if (addScale)
        map.scale(x = xlim0[2] - (xlim0[2] - xlim0[1])/2, y = ylim0[1],
            ratio = FALSE)
    if (plotPoints) {
        if (length(colPoints) != 1) {
            colPoints <- factor(colPoints)
            a.legPoints <- levels(colPoints)
            levels(colPoints) <- colors()[(1:length(levels(colPoints))) *
                10]
            points(sPDF, pch = 16, col = as.character(colPoints),
                cex = 0.5)
            if (length(legPoints) != 0) {
                legend(x = "bottomright", legend = a.legPoints,
                  pch = 16, col = levels(colPoints), title = "",
                  ncol = 2, bg = "white", pt.cex = 1)
            }
        }
        else {
            points(sPDF, pch = ".", col = colPoints, cex = 0.1)
        }
    }
    legend(x = legendx, legend = breaks0[1:(length(breaks0) -
        1)], pch = 22, pt.bg = cols, title = legendtitle, ncol = legendncol,
        bg = "white", cex=0.7, pt.cex = 2)
    if (plotTitle != "")
        mtext(plotTitle)
}


#----------------------
#----------------------
#----------------------
#----------------------

do_map <-
 function(x=y09v, x2=y09v_othvid, year="2009", take_all=TRUE){

  graphics.off()

  anf <- function(x) as.numeric(as.character(x))



   if(year=="2009"){ # a year for which we have Tech I data
   x$VE_REF                                <- factor(x$VE_REF)
   ti                                      <- sub_ti[sub_ti$year %in% year,]
   data_and_fuel                           <- merge(x, ti[,c('eu.ident', 'af_fuel_forbrug')],
                                                    by.x="VE_REF", by.y="eu.ident")
   remove(x); gc(reset=TRUE)
   if(take_all){
   x2                                      <- cbind(x2, af_fuel_forbrug=NA)
   data_and_fuel                           <- rbind.data.frame(data_and_fuel,x2)
   }
   } else{
   x2$VE_REF                               <- factor(x2$VE_REF)
   x2                                      <- cbind(x2, af_fuel_forbrug=NA)
   data_and_fuel                           <- x2
   }
   remove(x2); gc(reset=TRUE)
   eff_per_vid                             <- tapply(anf(data_and_fuel$LE_EFF_VMS)/60, data_and_fuel$VE_REF, sum, na.rm=TRUE)
   data_and_fuel$tot_eff_hours_vid         <- data_and_fuel$VE_REF # init
   levels(data_and_fuel$tot_eff_hours_vid) <- eff_per_vid[match( levels(data_and_fuel$tot_eff_hours_vid), names(eff_per_vid) )]
   data_and_fuel$liter_per_hour            <- anf(data_and_fuel$af_fuel_forbrug) / anf(data_and_fuel$tot_eff_hours_vid)
   # for not imformed vessels: use the linear prediction from kw:
   idx                                     <- is.na(data_and_fuel$liter_per_hour)
   data_and_fuel[idx, 'liter_per_hour']    <- data_and_fuel[idx, 'fuel_cons_rate']

   # correlation??
   # plot( data_and_fuel$liter_per_hour[!idx], data_and_fuel$fuel_cons_rate[!idx])
   # summary(lm(data_and_fuel$fuel_cons_rate~data_and_fuel$liter_per_hour))

   # deal with species info
   dd    <- data_and_fuel[,grep("EURO",  colnames(data_and_fuel))]
   data_and_fuel <- data_and_fuel[,-grep("EURO",  colnames(data_and_fuel))]
   data_and_fuel$set_of_species_cum_euro <- apply(dd, 1, sum, na.rm=TRUE)
   remove(dd) ; gc(reset=TRUE)

  data_and_fuel$VE_REF_FT_REF             <- factor(paste(data_and_fuel$VE_REF,"_",data_and_fuel$FT_REF, sep=''))
  conso_per_trip                          <- tapply(anf(data_and_fuel$liter_per_hour), data_and_fuel$VE_REF_FT_REF, sum, na.rm=TRUE)
  data_and_fuel$conso_per_trip            <- data_and_fuel$VE_REF_FT_REF # init
  levels(data_and_fuel$conso_per_trip)    <- conso_per_trip[match( levels(data_and_fuel$conso_per_trip), names(conso_per_trip) )]
  data_and_fuel$conso_per_trip            <- anf(data_and_fuel$conso_per_trip)
  #
  data_and_fuel <- data_and_fuel[grep("DNK", data_and_fuel$VE_REF),]
  #
   data_and_fuel_vid <- data_and_fuel[!duplicated(data_and_fuel$VE_REF), "liter_per_hour"]
   hist(data_and_fuel_vid, nclass=10)


  ## compare the different maps having different meanings
  data_and_fuel$SI_LONG           <- anf(data_and_fuel$SI_LONG)
  data_and_fuel$SI_LATI           <- anf(data_and_fuel$SI_LATI)
  data_and_fuel[,"LE_EFF_VMS"]    <- anf(data_and_fuel[,"LE_EFF_VMS"] )
  data_and_fuel[, "LE_EFF_VMS"]   <- replace(data_and_fuel[,"LE_EFF_VMS"], is.na(data_and_fuel[, "LE_EFF_VMS"]) | data_and_fuel[, "LE_EFF_VMS"] < 0, 0)
  data_and_fuel$effort_hours      <-   anf(data_and_fuel$LE_EFF_VMS)/ 60 # in hours

  # ----------------------------------------
  # Figure xx a,b---------------------------
  # ----------------------------------------
  windows(6,8)
  #layout(rbind(c(1,2),
  #            c(3,4),
  #            c(5,6)))
  #layout.show(6)
  par(mfrow=c(2,1))
  par(mar=c(4,4,1,1))


  #1. map  effort-----
  #data_and_fuel$effort_thousand_hours_percent <-  data_and_fuel$effort_thousand_hours /
  #                                                    sum(data_and_fuel$effort_thousand_hours )*100
  dnk_1 <- vmsGridCreate(data_and_fuel[data_and_fuel$SI_STATE=="1",],
                           nameVarToSum="effort_hours",   plotPoints =FALSE,
                            colLand="darkolivegreen4",  addICESgrid=TRUE, paletteCats= "heat.colors",  legendx="bottomright",
                             nameLon="SI_LONG", nameLat="SI_LATI", cellsizeX =0.2, cellsizeY =0.2, we = -8, ea = 25, so = 51, no = 62,
                              #if take_all=FALSE: breaks0=c( 1,2,4,7,13,21,34,57, 117,  1000000), legendncol=5,
                              #breaks0=c( 1,2,3,8,17,33,59,103,180,426, 1000000), legendncol=5,
                              breaks0=c( 5,9,18,32,57,93,136,224,435,  819, 1000000), legendncol=5,
                                legendtitle="Fishing effort (hours)",
                                 outGridFile= paste("dnk",year,"_1.asc",sep='') )
   mtext(side=3, "(a)", line=1, cex=2, adj=0)

   #2. map  euros--------
   dnk_1 <- vmsGridCreate(data_and_fuel[data_and_fuel$SI_STATE=="1",],
              nameVarToSum="set_of_species_cum_euro",   plotPoints =FALSE,
               colLand="darkolivegreen4",  addICESgrid=TRUE, paletteCats= "heat.colors",  legendx="bottomright",
                nameLon="SI_LONG", nameLat="SI_LATI", cellsizeX =0.2, cellsizeY =0.2, we = -8, ea = 25, so = 51, no = 62,
                #if take_all=FALSE: breaks0=c(36, 750, 2144, 4328, 7386, 12324, 19958, 30489, 47071, 82479,  1000000), legendncol=5,
                # breaks0=round(c(0, 1283,  4429,    9985,   18737,   31916,   51676,   80235,  136304,  260902,  1000000),-2), legendncol=5,
                  legendtitle="Landings (Euro)",
                   outGridFile= paste("dnk",year,"_1.asc",sep='') )
   mtext(side=3, "(b)", line=1, cex=2, adj=0)

   # save plot
   savePlot(filename = file.path("C:","wamp", "www", "fisher_questionnaire" ,
                       "Rcode","merge_vesselactivity_to_eflalo",
                          paste("mapping_figure_xx_ab_", year, sep="")),
                            type = "png", device = dev.cur())

  # ----------------------------------------
  # Figure xx c,d---------------------------
  # ----------------------------------------
  windows(6,8)
  #layout.show(6)
  par(mfrow=c(2,1))
  par(mar=c(4,4,1,1))

  #3. map (relative) fuel consumption per cell for informed vessels
  library(vmstools)
  #data_and_fuel$fuelcons_liter                   <-  (data_and_fuel$liter_per_hour*data_and_fuel$effort_hours)
  data_and_fuel$fuelcons_thousand_liter          <-  (data_and_fuel$liter_per_hour*data_and_fuel$effort_hours)/1e3
  data_and_fuel$fuelcons_thousand_liter_percent  <-  (data_and_fuel$fuelcons_thousand_liter )/
                                                         sum(data_and_fuel$fuelcons_thousand_liter )*100
  dnk_1 <- vmsGridCreate(data_and_fuel, nameVarToSum="fuelcons_thousand_liter_percent",   plotPoints =FALSE,
                          colLand="darkolivegreen4",  addICESgrid=TRUE, paletteCats= "heat.colors",  legendx="bottomright",
                           nameLon="SI_LONG", nameLat="SI_LATI", cellsizeX =0.2, cellsizeY =0.2, we = -8, ea = 25, so = 51, no = 62,
                            #if take_all=FALSE: breaks0=c(0, 0.001, 0.003, 0.006, 0.009, 0.014, 0.023, 0.035, 0.057, 0.108, 1000000), legendncol=5,
                            breaks0=c(0.000, 0.001, 0.002, 0.004, 0.006, 0.010, 0.016, 0.024, 0.041 ,0.087 , 1000000), legendncol=5,
                             legendtitle="Fuel cons. (%)",
                              outGridFile= paste("dnk",year,"_1.asc",sep='') )
  mtext(side=3, "(c)", line=1, cex=2, adj=0)


  #4. map fuel use per unit of effort per cell
  data_and_fuel$effort_thousand_hours         <-  (data_and_fuel$effort_hours)/1e3
  dnk_1 <- vmsGridCreate(data_and_fuel,
                          nameVarToAverage= c("fuelcons_thousand_liter", "effort_thousand_hours"),
                            plotPoints =FALSE,
                             colLand="darkolivegreen4",  addICESgrid=TRUE, paletteCats= "heat.colors",  legendx="bottomright",
                              nameLon="SI_LONG", nameLat="SI_LATI", cellsizeX =0.2, cellsizeY =0.2, we = -8, ea = 25, so = 51, no = 62,
                               #if take_all=FALSE: breaks0=c(11,49,   74,   93,  112,  125,  135,  148,  179,  523, 1000000),
                                breaks0=c(11,64,92,119,145,168,188,234,435,764, 1000000),
                               legendncol=5,   legendtitle="Fuel per unit hour (l.h-1)",
                                 outGridFile= paste("dnk",year,"_1.asc",sep='') )
  mtext(side=3, "(d)", line=1, cex=2, adj=0)

  #4. map VPUF in euro per lirwe
 #     ramp <- colorRamp(c("firebrick1", "mediumaquamarine"), interpolate="spline")
 #  cols <- rgb( ramp(seq(0, 1, length = 8)), max = 255)
 #    dd  <- data_and_fuel[,grep("EURO",  colnames(data_and_fuel))]
 #     data_and_fuel$set_of_species_cum_euro <- apply(dd, 1, sum, na.rm=TRUE)
 #    dnk_1 <- vmsGridCreate(data_and_fuel [data_and_fuel$SI_STATE=="1",],
 #              nameVarToAverage= c("set_of_species_cum_euro", "fuelcons_liter"),
 #               plotPoints =FALSE,  cols=cols,
 #         colLand="darkolivegreen4",  addICESgrid=TRUE, paletteCats= "heat.colors",  legendx="bottomright",
 #             nameLon="SI_LONG", nameLat="SI_LATI", cellsizeX =0.2, cellsizeY =0.2, we = -8, ea = 25, so = 51, no = 62,
 #              breaks0=c(0,0.25,0.5, 1, 2,4,8,16, 1000000), legendncol=3,   legendtitle="CPUF (Euro per l)",
 #                outGridFile= paste("dnk",y,"_1.asc",sep='') )

   # save plot
   savePlot(filename = file.path("C:","wamp", "www", "fisher_questionnaire" ,
                       "Rcode","merge_vesselactivity_to_eflalo",
                          paste("mapping_figure_xx_cd_", year, sep="")),
                            type = "png", device = dev.cur())
  # ----------------------------------------
  # Figure xx e,f---------------------------
  # ----------------------------------------
  windows(6,8)
  #layout.show(6)
  par(mfrow=c(2,1))
  par(mar=c(4,4,1,1))

  met_to_keep <- grep('OTB', unique(data_and_fuel$LE_MET_level6))
  met_to_keep <- c(met_to_keep, grep('SDN', unique(data_and_fuel$LE_MET_level6)))
  data_and_fuel_trawlers_seiners <- data_and_fuel[data_and_fuel$LE_MET_level6 %in% met_to_keep,]

  #5. map VPUE in euro per hour
   ramp  <- colorRamp(c("firebrick1", "mediumaquamarine"), interpolate="spline")
   cols  <- rgb( ramp(seq(0, 1, length = 10)), max = 255)
    dnk_1 <- vmsGridCreate(data_and_fuel [data_and_fuel$SI_STATE=="1",],
               nameVarToAverage= c("set_of_species_cum_euro", "effort_hours"),
                plotPoints =FALSE,  #cols=cols,
                 colLand="darkolivegreen4",  addICESgrid=TRUE, paletteCats= "heat.colors",  legendx="bottomright",
                  nameLon="SI_LONG", nameLat="SI_LATI", cellsizeX =0.2, cellsizeY =0.2, we = -8, ea = 25, so = 51, no = 62,
                   #if take_all=FALSE: breaks0=c(9,149,222,297,367,462,586,799,1334,2441, 1000000),
                   breaks0=c(9,149,222,297,367,462,586,799,1334,2441, 1000000),
                    legendncol=5,   legendtitle="CPUE (Euro per h)",
                     outGridFile= paste("dnk",year,"_1.asc",sep='') )
   mtext(side=3, "(a)", line=1, cex=2, adj=0)


  met_to_keep <- grep('OTB', unique(data_and_fuel$LE_MET_level6))
  met_to_keep <- c(met_to_keep, grep('SDN', unique(data_and_fuel$LE_MET_level6)))
  met_to_keep <- unique(data_and_fuel$LE_MET_level6)[ met_to_keep]
  data_and_fuel_trawlers_seiners <- data_and_fuel[data_and_fuel$LE_MET_level6 %in% met_to_keep,]

  #5. map VPUE in euro per hour
   ramp  <- colorRamp(c("firebrick1", "mediumaquamarine"), interpolate="spline")
   cols  <- rgb( ramp(seq(0, 1, length = 10)), max = 255)
    dnk_1 <- vmsGridCreate(data_and_fuel_trawlers_seiners [data_and_fuel_trawlers_seiners$SI_STATE=="1",],
               nameVarToAverage= c("set_of_species_cum_euro", "effort_hours"),
                plotPoints =FALSE,  #cols=cols,
                 colLand="darkolivegreen4",  addICESgrid=TRUE, paletteCats= "heat.colors",  legendx="bottomright",
                  nameLon="SI_LONG", nameLat="SI_LATI", cellsizeX =0.2, cellsizeY =0.2, we = -8, ea = 25, so = 51, no = 62,
                   #if take_all=FALSE: breaks0=c(9,149,222,297,367,462,586,799,1334,2441, 1000000),
                   breaks0=c(9,149,222,297,367,462,586,799,1334,2441, 1000000),
                    legendncol=5,   legendtitle="CPUE (Euro per h)",
                     outGridFile= paste("dnk",year,"_1.asc",sep='') )
   mtext(side=3, "(a)", line=1, cex=2, adj=0)


   #6. map CPUF in euro per litre
   ramp   <- colorRamp(c("firebrick1", "mediumaquamarine"), interpolate="spline")
   cols   <- rgb( ramp(seq(0, 1, length = 10)), max = 255)
   data_and_fuel$cpuf_trip                <-  data_and_fuel$set_of_species_cum_euro/data_and_fuel$conso_per_trip
   dnk_1 <- vmsGridCreate(data_and_fuel [data_and_fuel$SI_STATE=="1",],
               nameVarToSum= c("cpuf_trip"),
                plotPoints =FALSE,  #cols=cols,
                 colLand="darkolivegreen4",  addICESgrid=TRUE, paletteCats= "heat.colors",  legendx="bottomright",
                  nameLon="SI_LONG", nameLat="SI_LATI", cellsizeX =0.2, cellsizeY =0.2, we = -8, ea = 25, so = 51, no = 62,
                   breaks0=c(0, 0.05,   0.14,   0.33,   0.62,   1.11,  1.97,   3.78,   8.01,  17.63, 1000000),
                    legendncol=5,   legendtitle="CPUF (Euro per l)",
                     outGridFile= paste("dnk",year,"_1.asc",sep='') )
   mtext(side=3, "(b)", line=1, cex=2, adj=0)

   # save plot
   savePlot(filename = file.path("C:","wamp", "www", "fisher_questionnaire" ,
                       "Rcode","merge_vesselactivity_to_eflalo",  "mapping_plot",
                          paste("mapping_figure_xx_ef_", year, sep="")),
                            type = "png", device = dev.cur())

 return()
 }



##---------------------------
##---------------------------
##---------------------------
##---------------------------
##---------------------------
##---------------------------

if(FALSE){
# calls for particular 2009
do_map (x=y09v, x2=y09v_othvid, year="2009", take_all=TRUE)

# shortcut calls for other years (simpler because not to be merged with Tech I data)
load(file=file.path("C:", "merging", "redo4merged2010", "all_merged_value_2010.RData"))
y0Xv         <- all.merged
remove(all.merged); gc(reset=TRUE)
y0Xv         <- predict_fuel_cons_from_kw(y0Xv)
y0Xv$SI_LONG <- as.numeric(as.character(y0Xv$SI_LONG))
y0Xv$SI_LATI <- as.numeric(as.character(y0Xv$SI_LATI))
y0Xv$area    <- ICESarea2 (y0Xv, string = TRUE)
y0Xv         <- y0Xv[!is.na(y0Xv$area) & y0Xv$LE_EFF_VMS!=0,]
col_to_trash <- c('VE_FLT',"LE_GEAR",'SI_SP','SI_HE', 'SI_DATE', 'SI_TIME', 'SI_HARB', 'flag')
y0Xv         <- y0Xv[, !colnames(y0Xv) %in% col_to_trash]
do_map (x=NULL, x2=y0Xv, year="2010", take_all=TRUE)

}
