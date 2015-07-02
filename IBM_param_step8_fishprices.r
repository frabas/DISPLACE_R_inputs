 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!OBTAIN A PRICE PER STOCK PER CAT PER QUARTER!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!ON HARBOUR NODES!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##


 ## HARBOUR SPE
 ## IBM parametrisation
 ## Francois Bastardie (DTU-Aqua)
 ## outputs: mainly .dat files to be used for the IBM simulations

# GENERAL SETTINGS
  general <- list()
  general$main.path             <- file.path("C:", "Users", "fbas", "Documents", "GitHub", "DISPLACE_input_raw")
  general$main.path.code        <- file.path("C:", "Users", "fbas", "Documents", "GitHub", "DISPLACE_R_inputs")
  general$main_path_input       <- file.path("C:", "Users", "fbas", "Documents", "GitHub", "DISPLACE_input")

  general$igraph                <- 11
  general$case_study            <- "baltic_only"
  general$case_study_countries  <- c("DEN", "SWE", "DEU")    # for the Baltic only
  general$a.year                <- "2012"
  general$a.country             <- "DEN"
  #general$a.country             <- "DEU"
  #general$a.country             <- "SWE"


  general$igraph                <- 56
  general$case_study            <- "myfish"
  general$case_study_countries  <- c("DEN")    # for the Baltic only
  general$a.year                <- "2012"
  general$a.country             <- "DEN"



# 1. load price data from josefine extraction.
# daily price per species per harbour per size from sales slips.

#1)	I would prefer the UK names for species including the 3 letters FAO code. Yes all species would be good since it is always easy to remove the irrelevant ones afterwards.

#2)	concerning the harbours, as many info as you have attached to them would be good i.e. names and possibly long/lat locations if available

#3)	I believe I need one price per harbour per species only so for the size categories issue I believe I will only use the ones with the larger amount landed for each species. Or maybe I may do an average of the species prices per harbour weighted by the amount landed in each category. You maybe know how to quickly do this latter case in SAS. Otherwise, just provide me the dataset per size category and I will do the rest!

#4)	the main constraint is that I need ultimately the data aggregated by day, so in case you do the weighted average (point 3) then this should be done by day, otherwise I´ll need all the sales slip records I suppose.


# M:\JSV\FBA_price_data.
# It is by size category.
# I have calculated a price by sales slip, and then calculated the mean price by sales harbor, date species and size sorting.
# I have summarized the landings (kg) and value (dkk) by the same categories, so it should be possible for you to do the weighting.
# I have included the UK species names and the FAO code, and the harbor names, position, eucode and nation.


  # read data---------------------------------
  #prices       <- read.table(file=file.path('C:','Users','fba','Dropbox','ibm_vessels_param_cutting_edge',
  prices       <- read.table(file=file.path(general$main.path,
                            paste("price_data2.csv",sep='')), header=TRUE, sep=",")
  prices$date2       <- strptime(  paste(prices$date) ,  "%e/%m/%Y" )
  # transform into euros
  if(general$a.year=="2010") prices$price <- prices$price * 0.13437 # convert DKK in euro in 2010
  if(general$a.year=="2012") prices$price <- prices$price * 0.1345 # convert DKK in euro in 2012

  # all info is good so keep price info also for myfish app
  prices_swe             <- read.table(file=file.path("C:","displace-project.org","repository", "ibm_vessels_param",
                            paste("SWE_DISPLACE_price_info.txt",sep='')), header=TRUE, sep="\t")
  prices_swe$date2       <-  strptime( prices_swe$date  ,  "%Y-%m-%d" )
  prices_swe$price       <- as.numeric(as.character(prices_swe$price_sek)) * 0.115 # convert SEK in euro in 2012
  #--------------------------------------------


  # stack DEN and SWE data---------------------  
  cols   <- c('harbor_name','harbor_lat','harbor_lon','date2','species_uk_code','size_sorting','price','kg') 
  prices <- rbind(
                  prices[, cols],
                  prices_swe[,cols]
                  )
  #--------------------------------------------




 # the time dimension
 prices$day         <-   format(strptime(  paste( prices$date2) , tz='GMT',  "%Y-%m-%d" ), "%d")
 prices$day2        <-   format(strptime(  paste( prices$date2) , tz='GMT',  "%Y-%m-%d" ), "%j")
 prices$month       <-   format(strptime(  paste( prices$date2) , tz='GMT',  "%Y-%m-%d" ), "%m")
 prices$year        <-   format(strptime(  paste( prices$date2) , tz='GMT',  "%Y-%m-%d" ), "%Y")
 prices$quarter     <-   quarters(prices$date2)


 # subset for the year
 prices <- prices[prices$year==general$a.year,]

 # get rid of NA
 prices <- prices[!is.na(prices$harbor_lon),]
 prices <- prices[!is.na(prices$harbor_lat),]

 # find out areas
 source(file=file.path(general$main.path.code,"IBM_param_utils_longlat_to_ICESareas.r"))
 coor <- data.frame(
                    x= as.numeric(as.character(prices[,'harbor_lon'])),
                    y= as.numeric(as.character(prices[,'harbor_lat']))
                    )
 coor$area   <- longlat_to_ICESareas(coor)
 coor[is.na(coor$area) | !(coor$area %in% c('22', '23', '24', '25', '26', '27', '28-1', '28-2', '29', '30', '31', '32',
                         'IIIan', 'IIIas') ), 'area'] <- 'nsea' # if out of range, assign all these nodes to North Sea stocks...
 coor[(coor$area %in% c('22', '23', '24')), 'area'] <- '2224'
 coor[(coor$area %in% c('25', '26', '27', '28-1', '28-2', '29','30', '31', '32')), 'area'] <- '2532'

 prices$stock <- paste( prices$species_uk_code, ".", coor$area, sep='')


 # keep only the relevant stocks
  # correct names for special cases (those across management areas)
  prices[prices$stock %in% c("COD.IIIan"), "stock"] <- 'COD.nsea'
  prices[prices$stock %in% c("COD.IIIas"), "stock"] <- 'COD.kat'
  prices[prices$stock %in% c("HAD.IIIan"), "stock"] <- 'HAD.nsea'
  prices[prices$stock %in% c("HER.IIIan", "HER.IIIas", "HER.2224"), "stock"] <- 'HER.3a22'
  prices[prices$stock %in% c("SPR.2224", "SPR.2532"), "stock"] <- 'SPR.2232'
  prices[prices$stock %in% c("PLE.2224", "PLE.2532"), "stock"] <- 'PLE.2232'
  prices[prices$stock %in% c("FLE.2224", "FLE.2532"), "stock"] <- 'FLE.2232'
  prices[prices$stock %in% c("TUR.2224", "TUR.2532"), "stock"] <- 'TUR.2232'
  prices[prices$stock %in% c("DAB.2224", "DAB.2532"), "stock"] <- 'DAB.2232'
  prices[grep('IIIan', prices$stock), "stock"] <- # for all other species, correct kask 
       gsub("IIIan", "kask", prices[grep('IIIan', prices$stock),'stock'])
  prices[grep('IIIas', prices$stock), "stock"] <- # for all other species, correct kask 
        gsub("IIIas", "kask", prices[grep('IIIas', prices$stock),'stock'])


 # subset for relevant populations
 pop_names                   <- read.table(file.path(general$main.path, "popsspe", paste("pop_names_",general$case_study,".txt", sep='')))
 prices                 <- prices[prices$stock %in% pop_names[,1],] # keep simulated stocks only
 prices$stock           <- factor(prices$stock)
 levels(prices$stock )  <- pop_names[,2][ match(levels(prices$stock), as.character(pop_names[,1]))] # map the name to integer
 prices$mapped_stk_code <- as.numeric(as.character(prices$stock))
 library(doBy)
 prices                 <- orderBy(~stock, data=prices) # library(doBy) # order from 0 to nbstock


 # clean up a little bit to avoid 0/0
 prices <- prices[prices$price>0 & !is.na(prices$price) & prices$kg>0,]


 # convert coordinates into UTM coordinates
 library(sp)
 library(rgdal)
 SP <- SpatialPoints(cbind(as.numeric(as.character(prices$harbor_lon)), as.numeric(as.character(prices$harbor_lat))),
                       proj4string=CRS("+proj=longlat +datum=WGS84"))
 prices <- cbind(prices,
                 spTransform(SP, CRS("+proj=utm  +ellps=intl +zone=32 +towgs84=-84,-107,-120,0,0,0,0,0")))    # convert to UTM


 # relate to the pt_graph (get a table from ping.harbours, SI_HARB, pt_graph) 
   if('DEN' %in% general$case_study_countries && 
        'DEU' %in% general$case_study_countries && 
          'SWE' %in% general$case_study_countries){   

     load(file.path(general$main.path, "merged_tables", general$case_study,
              paste("ping.harbours.","DEN",".",
               general$a.year,".igraph",general$igraph,".RData",sep='')) )   # get combined ping.harbours
   
     table_harb_and_idx_node <-
      ping.harbours[!duplicated(ping.harbours$SI_HARB,ping.harbours$pt_graph),
                                   c("SI_HARB", "pt_graph", "SI_LATI", "SI_LONG")]
  
    # relate to the pt_graph (get a table from ping.harbours, SI_HARB, pt_graph) and do a multimap pt_graph/port name
     load(file.path(general$main.path, "merged_tables", general$case_study,
              paste("ping.harbours.","DEU",".",
               general$a.year,".igraph",general$igraph,".RData",sep='')) )   # get combined ping.harbours
   
     table_harb_and_idx_node2 <-
      ping.harbours[!duplicated(ping.harbours$SI_HARB,ping.harbours$pt_graph),
                                   c("SI_HARB", "pt_graph", "SI_LATI", "SI_LONG")]
  
     # relate to the pt_graph (get a table from ping.harbours, SI_HARB, pt_graph) and do a multimap pt_graph/port name
     load(file.path(general$main.path, "merged_tables", general$case_study,
              paste("ping.harbours.","SWE",".",
               general$a.year,".igraph",general$igraph,".RData",sep='')) )   # get combined ping.harbours
   
     table_harb_and_idx_node3 <-
      ping.harbours[!duplicated(ping.harbours$SI_HARB,ping.harbours$pt_graph),
                                   c("SI_HARB", "pt_graph", "SI_LATI", "SI_LONG")]
                                   
                                   
     table_harb_and_idx_node$SI_LATI  <- as.numeric(as.character(table_harb_and_idx_node$SI_LATI))
     table_harb_and_idx_node2$SI_LATI  <- as.numeric(as.character(table_harb_and_idx_node2$SI_LATI))
     table_harb_and_idx_node3$SI_LATI  <- as.numeric(as.character(table_harb_and_idx_node3$SI_LATI))
     table_harb_and_idx_node$SI_LONG  <- as.numeric(as.character(table_harb_and_idx_node$SI_LONG))
     table_harb_and_idx_node2$SI_LONG  <- as.numeric(as.character(table_harb_and_idx_node2$SI_LONG))
     table_harb_and_idx_node3$SI_LONG  <- as.numeric(as.character(table_harb_and_idx_node3$SI_LONG))
    
    
     
    table_harb_and_idx_node  <- rbind(table_harb_and_idx_node, table_harb_and_idx_node2, table_harb_and_idx_node3)
    table_harb_and_idx_node  <- table_harb_and_idx_node[!duplicated(table_harb_and_idx_node$SI_HARB,table_harb_and_idx_node$pt_graph),]
   
   } else{
    load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.harbours.",general$a.country,".",general$a.year,".igraph",
                general$igraph,".RData",sep='')))

     table_harb_and_idx_node <-
      ping.harbours[!duplicated(ping.harbours$SI_HARB,ping.harbours$pt_graph),
                                   c("SI_HARB", "pt_graph", "SI_LATI", "SI_LONG")]

   }  

 # convert coordinates into UTM coordinates
 library(sp)
 library(rgdal)
 SP <- SpatialPoints(cbind(as.numeric(as.character(table_harb_and_idx_node$SI_LONG)), as.numeric(as.character(table_harb_and_idx_node$SI_LATI))),
                       proj4string=CRS("+proj=longlat +datum=WGS84"))
 table_harb_and_idx_node <- cbind(table_harb_and_idx_node,
                 spTransform(SP, CRS("+proj=utm  +ellps=intl +zone=32 +towgs84=-84,-107,-120,0,0,0,0,0")))    # convert to UTM

 colnames(table_harb_and_idx_node)[colnames(table_harb_and_idx_node) == "coords.x1" ] <- "UTM_lon"
 colnames(table_harb_and_idx_node)[colnames(table_harb_and_idx_node) == "coords.x2" ] <- "UTM_lat"

 # brute force to link nodes to harbours (with euclidian distance)
 idx <- rep(0, nrow(prices))
 for(i in 1:nrow(prices)){
    idx[i] <- which.min ( sqrt( ((prices[i, "coords.x1"] -   table_harb_and_idx_node  [, "UTM_lon"])^2) +  (((prices[i, "coords.x2"] -   table_harb_and_idx_node  [, "UTM_lat"]))^2)) )
    print(i)
 }
 prices$UTM_lon <- table_harb_and_idx_node$UTM_lon[idx]
 prices$UTM_lat <- table_harb_and_idx_node$UTM_lat[idx]
   
 
     
 # then merge with the key, because:
 # 1. several harbours could have been linked to the same pt_graph (which is expected)
 # 2. furthermore need to retrieve the names of the harbours that are used to build the graph in the DISPLACE parameterisation (from step1 and 2)
 # then we need hereafter to reassign a consistent harbour name i.e. SI_HARB to the price records (because possibly coming from several ports)
 prices <- merge(
                      table_harb_and_idx_node,
                      prices,
                      by.x=c( "UTM_lon", "UTM_lat"),
                      by.y=c( "UTM_lon", "UTM_lat")
                       )

 
# prices$harbor_name_from_data <- prices$harbor_name
# prices$harbor_name           <-  table_harb_and_idx_node[match(prices$pt_graph, table_harb_and_idx_node$pt_graph), 'SI_HARB']
 
 
 


# here we are: prices per stock per size per day per graph harbour.
library(doBy)
prices <-orderBy(~stock+pt_graph+month+day, data=prices)





#-------------------------------------------------------------------------------
# export for a multimap in c++ (average price per quarter) pop/cat--------------
# 
av_prices_per_harbour_per_quarter            <- aggregate( prices$price, 
                                                         list( prices$stock,  prices$size_sorting, prices$pt_graph,  prices$quarter), 
                                                         mean, na.rm=TRUE)
colnames(av_prices_per_harbour_per_quarter ) <-  c('stock', 'size_sorting', 'pt_graph', 'quarter', 'price')


# apply all.combi
all.combi                                    <- expand.grid(
                                                            pt_graph= unique(table_harb_and_idx_node$pt_graph), 
                                                            stock=unique(av_prices_per_harbour_per_quarter$stock), 
                                                            size_sorting=unique(av_prices_per_harbour_per_quarter$size_sorting), 
                                                            quarter=c("Q1","Q2","Q3","Q4")
                                                            )
av_prices_per_harbour_per_quarter            <- merge(all.combi, av_prices_per_harbour_per_quarter, all=TRUE)
 #=> NA if no historic landings for this stock this cat in this harb...

av_prices_per_quarter            <-  aggregate( prices$price, 
                                                list( prices$stock,  prices$size_sorting,  prices$quarter), mean, na.rm=TRUE)
colnames(av_prices_per_quarter ) <-  c('stock', 'size_sorting',  'quarter', 'price')


# an annoying aspect is that we do not have the same cat for the stocks:
lapply(split(av_prices_per_quarter, f=av_prices_per_quarter$stock), function(x) unique(x[,'size_sorting']))


# fill in the NA gaps with average per stock per cat
dd <- merge(av_prices_per_harbour_per_quarter, av_prices_per_quarter, 
                  by.x=c("stock", "quarter", "size_sorting"),
                  by.y=c("stock", "quarter", "size_sorting"))
dd[is.na(dd$price.x),"price.x"] <- dd[is.na(dd$price.x),"price.y"] 
av_prices_per_harbour_per_quarter <- dd[,  c('stock', 'size_sorting', 'pt_graph', 'quarter', 'price.x')]
 #=> no NAs any more...good!

av_prices_per_harbour_per_quarter <- orderBy(~stock+pt_graph+size_sorting+quarter, data=av_prices_per_harbour_per_quarter)

  
# generate one file per port per quarter ordering the species in the right order
# To export a .dat for a multimap of fish price :metier/pop per harbour
   for (a.port in unique(av_prices_per_harbour_per_quarter$pt_graph)){ 
    a.port <- as.numeric(as.character(a.port)) - 1 ##!!! OFFSET FOR C++ !!!##
    for (a.quarter in c("Q1","Q2","Q3","Q4")){

    prices_a_quarter <-
          av_prices_per_harbour_per_quarter[av_prices_per_harbour_per_quarter$pt_graph==(a.port+1) & av_prices_per_harbour_per_quarter$quarter==a.quarter,
           c('stock', 'price.x')]
   
    #round
    prices_a_quarter$price.x <- round(prices_a_quarter$price.x, 3)
    
    
        write.table(prices_a_quarter[, c("stock", "price.x")],
           file=file.path(general$main.path, "harboursspe",
             paste(a.port,"_quarter", gsub("Q","",a.quarter),"_each_species_per_cat.dat",sep='')),
               col.names=TRUE,  row.names=FALSE, sep= ' ', quote=FALSE)
             

   }}


   #---------------------------
   # get a name for harbours...(CAUTION: USE SI_HARB, NOT harbour_name because SI_HARB correpond to the graph)
    # debug: remove possible blank space....
   table_harb <- prices[,c('pt_graph','SI_HARB')]
   table_harb$SI_HARB <-  gsub(" ","", table_harb$SI_HARB) 
   table_harb <- table_harb[!duplicated(data.frame(table_harb$pt_graph, table_harb$SI_HARB)),]
   
   ## GET A MULTIMAP PT_GRAPH/HARBOUR NAMES 
   table_harb$pt_graph <-  as.numeric(as.character(table_harb$pt_graph)) - 1 ##!!! OFFSET FOR C++ !!!##
   write.table(table_harb,
           file=file.path(general$main.path, "harboursspe",
             paste("names_harbours.dat",sep='')),
               col.names=FALSE,  row.names=FALSE, sep= ' ', quote=FALSE)


##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##
##-----------------------------------------------------------------------------##


# a table defining the marketable categories for each fish length
# obtained from the EU REGULATION 2406/96
# and the mean weight-at-szgroup
# basically filled by HAND to make it short!

#the file is called : comcat_per_szgroup_done_by_hand.dat















#---------------------------------------------------------
#---------------------------------------------------------
#---------------------------------------------------------
#---------PLOTS-------------------------------------------
#---------------------------------------------------------
#---------------------------------------------------------
#---------------------------------------------------------
# ggmap plot
library(ggmap)
denmark <- qmap(location = 'denmark', fullpage = TRUE)

n <- 10
fake_data <- factor(gl(3, n, label = c('a','b','c')))[sample(3*n,.50*3*n)]
f_df <- data.frame(x = factor(1), y = fake_data)
pie  <- qplot(x, y, data = f_df, geom = 'bar', fill = y) +
  coord_polar(theta = 'y') + theme_nothing()


vplayout <- function(x, y)  viewport(layout.pos.row = x, layout.pos.col = y)
grid.newpage()
pushViewport(viewport(layout = grid.layout(19,19)))
print(denmark, vp = vplayout(1:19, 1:19))
print(pie, vp = vplayout(10, 10))



#---------------------------------------------------------
#---------------------------------------------------------
 # using ggmap for plotting time series centered on the ports.
  coord.ports <- prices[!duplicated(data.frame(prices$SI_LONG,prices$SI_LATI)), c("SI_LONG","SI_LATI")]
  require(ggmap)
  require(mapproj)
  map.center <- geocode("odense")
  SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), maprange = TRUE, source="google", zoom=6)

  coord.ports$lon <- as.numeric(as.character(coord.ports$SI_LONG ))
  coord.ports$lat <- as.numeric(as.character(coord.ports$SI_LATI ))

  # here a trick: transform coordinates in in local map coordinates
  prices$x <-  (as.numeric(prices$day)/5)+as.numeric(as.character(prices$SI_LONG))
  prices$y <-  (prices$price/30)+as.numeric(as.character(prices$SI_LAT))

  prices_this_sp <- prices[prices$stock=="10",]

  SHmap + geom_line(
  aes(x=x, y=y), data=prices_this_sp)
  
  
  savePlot(filename=file.path(outPath,
       paste("ggmap_ports.jpeg",sep='')), type="jpeg")


  #-------------------------------------------------------------
  draw.xy <- function (x, y, xx, yy, xlim = NULL, ylim = NULL, width = 1,
    height = 0.5, bg = NULL, border = 1, type = "p", col = 1, ...)
 {
    n <- length(x)
    if (length(y) != n | length(xx) != n | length(yy) != n)
        stop("Arguments 'x', 'y', 'xx' and 'yy' should all be numeric vectors of the same length")
    if (length(col) > 1 & length(col) != n)
        warning("Argument 'col' should be single colour or a vector of colours with the same length as 'x'")
    if (is.null(xlim))
        xlim <- range(xx, na.rm = T)
    if (is.null(ylim))
        ylim <- range(yy, na.rm = T)
    marg <- 0.8
    xxs <- x + marg * width * (xx - xlim[1])/(xlim[2] - xlim[1]) -
        0.5 * width * marg
    yys <- y + marg * height * (yy - ylim[1])/(ylim[2] - ylim[1]) -
        0.5 * height * marg
    col <- rep(col, length.out = n)
    df <- data.frame(x, y, xxs, yys, col)
    xy <- unique(paste(x, y))
    lst <- lapply(xy, function(xy) subset(df, paste(x, y) ==
        xy))
    nxy <- length(lst)
    for (i in 1:nxy) {
        lsti <- lst[[i]]
        xi <- lsti$x[1]
        yi <- lsti$y[1]
        xxi <- lsti$xxs
        yyi <- lsti$yys
        yy0 <- yi - 0.5 * height * marg
        coli <- as.character(lsti$col)
        rect(xi - 0.5 * width, yi - 0.5 * height, xi + 0.5 *
            width, yi + 0.5 * height, col = bg, border = border)

        if (type == "h")
            segments(xxi, yyi, xxi, yy0, lend = 2, coli, ...)
        else lines(xxi, yyi, type, col = coli, ...)
    }
}

  # load shape file
  library(maptools)
  sh1 <- readShapePoly(file.path(general$main.path,"shp","francois_EU"))
  windows(12,5)
  par(mar=c(2,2,1,1))
  plot(sh1,  xlim=c(5, 15 ), ylim=c(55,56), col=grey(0.9))

  an <- function (x) as.numeric(as.character(x))
  
  prices_sub <- prices[prices$stock %in% c(3,10),]
  
  draw.xy(x=an(prices_sub$harbor_lon),
          y=an(prices_sub$harbor_lat),
          xx=an(prices_sub$day2),
          yy=an(prices_sub$price),
          col= an(prices_sub$stock), type = "l", width = 2, height = 1, bg="transparent")



 
  #------------------------------------------------
   library(RgoogleMaps)
  lat = c(40.702147,40.718217,40.711614);
  lon = c(-74.012318,-74.015794,-73.998284);
  center = c(mean(lat), mean(lon));
  zoom <- min(MaxZoom(range(lat), range(lon)));
  #this overhead is taken care of implicitly by GetMap.bbox();
  MyMap <- GetMap(center=center, zoom=zoom,markers = '&markers=color:blue|label:S|40.702147,-74.015794&markers=color:green|label:G|40.711614,-74.012318&markers=color:red|color:red|label:C|40.718217,-73.998284', destfile = "MyTile1.png");

   tmp <- PlotOnStaticMap(MyMap, lat = c(40.702147,40.711614,40.718217), lon = c(-74.015794,-74.012318,-73.998284), destfile = "MyTile1.png", cex=1.5,pch=20,col=c('red', 'blue', 'green'), add=FALSE);
   #and add lines:
   PlotOnStaticMap(MyMap, lat = c(40.702147,40.711614,40.718217), lon = c(-74.015794,-74.012318,-73.998284), lwd=1.5,col=c('red', 'blue', 'green'), FUN = lines, add=TRUE)



