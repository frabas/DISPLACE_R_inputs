## IBM
## find landings per month per species for each igraph node
## to be used for the depletion of the pops each month that would result from
## the not explicitly described other activities
## in the simulation, e.g. other country like  the Swedish fleet...
## but should also be used for vessels not equipped with VMS i.e. smaller vessels...



 general <-
        list(   main.path.ibm= file.path("C:","Users", "fba", "Dropbox", "ibm_vessels_param"),
                   region.of.interest= "BE_and_BW_and_Kattegat",
                    #a.year="2010")
                    a.year="2012")

  general$case_study <- "baltic_only"
  if(general$case_study=="canadian_paper")    general$igraph <- 4
  if(general$case_study=="baltic_only_2010")  general$igraph <- 6
  if(general$case_study=="baltic_only")       general$igraph <- 11
 
  general$main.path      <- file.path("C:","displace-project.org","repository", "ibm_vessels_param")
  general$main.path.code <- file.path("C:","displace-project.org","repository", "ibm_vessels_param_R")

 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!UTILS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

  library(maps)
 
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

 ICESrectangle <- function (dF) 
{
    rectChar1n2 <- as.integer(2 * (dF[, "SI_LATI"] - 35.5))
    rectChar3 <- ifelse(dF[, "SI_LONG"] <= -40, "A", ifelse(dF[, 
        "SI_LONG"] <= -30, "B", ifelse(dF[, "SI_LONG"] <= -20, 
        "C", ifelse(dF[, "SI_LONG"] <= -10, "D", ifelse(dF[, 
            "SI_LONG"] <= 0, "E", ifelse(dF[, "SI_LONG"] <= 10, 
            "F", ifelse(dF[, "SI_LONG"] <= 20, "G", ifelse(dF[, 
                "SI_LONG"] <= 30, "H", "I"))))))))
    rectChar4 <- as.integer(dF[, "SI_LONG"]%%10)
    rectID <- paste(rectChar1n2, rectChar3, rectChar4, sep = "")
    return(rectID)
}


  ICESrectangle2LonLat <- function (statsq, midpoint = F) {
    part1 <- substr(statsq, 1, 2)
    part2 <- substr(statsq, 3, 4)
    labels <- 0:90
    latlabels <- ifelse(labels < 10, paste("0", labels, sep = ""),
        as.character(labels))
    latvalues <- seq(35.5, 80.5, 0.5) + 0.25
    lonlabels <- paste(rep(LETTERS[2:8], rep(10, 7)), rep(0:9,
        7), sep = "")
    lonvalues <- (-40:29) + 0.5
    indx <- match(part1, latlabels)
    lat <- latvalues[indx]
    indx <- match(part2, lonlabels)
    lon <- lonvalues[indx]
    if (any(is.na(lat)) | any(is.na(lon)))
        warning("Some stat squares have not been recognised.")
    if (midpoint == F) {
        lat <- lat - 0.25
        lon <- lon - 0.5
    }
    return(data.frame(SI_LATI = lat, SI_LONG = lon))
   }


    c.listquote <- function (...)
   {
    args <- as.list(match.call()[-1])
    lstquote <- list(as.symbol("list"))
    for (i in args) {
        if (class(i) == "name" || (class(i) == "call" && i[[1]] !=
            "list")) {
            i <- eval(substitute(i), sys.frame(sys.parent()))
        }
        if (class(i) == "call" && i[[1]] == "list") {
            lstquote <- c(lstquote, as.list(i)[-1])
        }
        else if (class(i) == "character") {
            for (chr in i) {
                lstquote <- c(lstquote, list(parse(text = chr)[[1]]))
            }
        }
        else stop(paste("[", deparse(substitute(i)), "] Unknown class [",
            class(i), "] or is not a list()", sep = ""))
    }
    return(as.call(lstquote))
   }


 ##---------------------------------------
 ## READ IGRAPH CODE SQUARE---------------
 ##---------------------------------------
  code_square_per_node <- read.table(file=file.path(general$main.path,  "igraph", 
                              paste("code_square_for_graph",general$igraph,"_points.dat", sep='')), header=TRUE) # built in ibm_param_vessels.r

  code_square_per_node$pt_graph <- 0:(nrow(code_square_per_node)-1)




  ##---------------------------------------
  ## IMPORT LANDINGS IN KILO DATA----------
  ##---------------------------------------
   cat(paste("import landings data from STECF (annual landings in tons per rectangle)\n",sep=''))

   # http://stecf.jrc.ec.europa.eu/data-reports
   # All countries from 2012 STECF effort regime working group---------     
   # annual landings only...NOT BY QUARTER (OR AT LEAST SEMESTER)!!
   landings_all <- read.table(file=file.path(general$main.path, "STECF",
                           'stecf_landings_per_rectangle_per_country_2012.csv'), sep=",", header=TRUE)

   # keep only the essentials...
   landings_all <-  landings_all[,c('COUNTRY','REG.GEAR.COD', 'VESSEL_LENGTH', 'RECTANGLE', 'SPECIES', 'X2012') ]
                            ##=> in TONS!
  
   #...and aggregate
   nm         <- names(landings_all)
   idx.col    <- grep('X2012', nm) # index columns with species weight
   library(data.table)
   DT  <- data.table(landings_all) # library data.table for fast grouping replacing aggregate()
   # AGGREGATE WEIGHT PER SPECIES
   eq1  <- c.listquote( paste ("sum(",nm[idx.col],",na.rm=TRUE)",sep="") )
   DT$X2010 <-as.double(DT$X2012)
   DT$REG.GEAR.COD<-as.factor(DT$REG.GEAR.COD)
   DT$VESSEL_LENGTH<-as.factor(DT$VESSEL_LENGTH)
   DT$RECTANGLE<-as.factor(DT$RECTANGLE)
   DT$SPECIES  <-as.factor(DT$SPECIES)
   DT$COUNTRY  <-as.factor(DT$COUNTRY)
   land.agg    <- DT[,eval(eq1), by=list(COUNTRY, REG.GEAR.COD, VESSEL_LENGTH, RECTANGLE, SPECIES)]
   land.agg    <- data.frame(land.agg)
   colnames(land.agg) <- c("country","metier", "vsize", "rectangle", "species", "landings")
  
   # correct some levels....baltic_only_2010
   #levels(land.agg$vsize)
   #"NONE"    "O10M"    "o10t12m" "O10T12M" "o10t15m" "O10T15M" "o12t18m" "O12T18M" "o15m"    "O15M"    "o18t24m" "O18T24M" "o24t40m" "O24T40M" "o40m"   
   #"O40M"    "o8t10m"  "O8T10M"  "u10m"    "U10M"    "u8m"     "U8M" 
   #levels(land.agg$vsize) <- c("u12m", "u12m", "o10t12m","o10t12m","o10t15m","o10t15m","o12t18m","o12t18m","o15m", "o15m","o18t24m","o18t24m","o24t40m","o24t40m","o40m",   
   #                                   "o40m","o8t10m","o8t10m" ,"u10m","u10m","u8m","u8m") 
   
   # correct some levels....
   levels(land.agg$vsize)
   # "none"    "NONE"    "o10m"    "O10M"    "o10t12m" "O10T12M" "o10t15m" "O10T15M" "o12t18m" "O12T18M" "o15m"    "O15M"    "o18t24m" "O18T24M" "o24t40m" "O24T40M" "o40m"    "O40M"    "o8t10m"  "O8T10M"  "u10m"    "U10M"    "u8m"    
   # "U8M"
   levels(land.agg$vsize) <- c("u12m", "u12m", "o10t12m", "o10t12m", "o10t12m", "o10t12m", "o10t15m", "o10t15m", "o12t18m", 
                                "o12t18m", "o15m", "o15m", "o18t24m", "o18t24m", "o24t40m", "o24t40m", "o40m",    "o40m", "o8t10m",  "o8t10m",  "u10m",    "u10m",    "u8m", "u8m")
   
  
  
  
   # remove the lines with 0 in weight because due to some landings but during other years we did not keep here.
   land.agg <- land.agg[land.agg$landings !=0,]
  
   landings <- land.agg 
  
  ##---------------------------------------------------------------------
  ## AGGREGATE LANDINGS PER SQUARE---------------------------------------
  ## AND MERGE WITH CODE_SQUARE GIVEN POP SPECIFIC DISTRIBUTION----------
  ##---------------------------------------------------------------------
  
  
   # find out areas
   landings[,c('SI_LATI','SI_LONG')] <- ICESrectangle2LonLat(landings$rectangle, midpoint = TRUE)
   source(file=file.path(general$main.path.code,"IBM_param_utils_longlat_to_ICESareas.r"))
   landings$x <- as.numeric(as.character((landings$SI_LONG) ))
   landings$y <-  as.numeric(as.character((landings$SI_LATI) )) 
   landings$code_area   <- longlat_to_ICESareas(landings)
   landings[is.na(landings$code_area) | !(landings$code_area %in% c('22', '23', '24', '25', '26', '27', '28-1', '28-2', '29', '30', '31', '32',
                         'IIIan', 'IIIas', 'IVa', 'IVb', 'IVc') ), 'code_area'] <- 'out' # if out of range,remove...
   landings[(landings$code_area %in% c('IVa', 'IVb', 'IVc')), 'code_area'] <- 'nsea'
   landings[(landings$code_area %in% c('22', '23', '24')), 'code_area'] <- '2224'
   landings[(landings$code_area %in% c('25', '26', '27', '28-1', '28-2', '29','30', '31', '32')), 'code_area'] <- '2532'
   
   # temporary species names
   landings$species   <- paste(landings$species,".", landings$code_area, sep='')
 
  # correct names for special cases (those across management areas)
   landings[ landings$species %in% c("COD.IIIan"), "species"] <- 'COD.nsea'
   landings[ landings$species %in% c("COD.IIIas"), "species"] <- 'COD.kat'
   landings[ landings$species %in% c("HAD.IIIan"), "species"] <- 'HAD.nsea'
   landings[ landings$species %in% c("HER.IIIan", "HER.IIIas", "HER.2224"), "species"] <- 'HER.3a22'
   landings[ landings$species %in% c("SPR.2224", "SPR.2532"), "species"] <- 'SPR.2232'
   landings[ landings$species %in% c("PLE.2224", "PLE.2532"), "species"] <- 'PLE.2232'
   landings[ landings$species %in% c("FLE.2224", "FLE.2532"), "species"] <- 'FLE.2232'
   landings[ landings$species %in% c("TUR.2224", "TUR.2532"), "species"] <- 'TUR.2232'
   landings[ landings$species %in% c("DAB.2224", "DAB.2532"), "species"] <- 'DAB.2232'
   landings[grep('IIIa',  landings$species), "species"] <- # for all other species, correct kask 
     paste( substr(landings[grep('IIIa',  landings$species),'species'], 1,3), ".kask", sep='')

 
 
  # CAUTION !! CAUTION !! CAUTION !! Remove the landings from SIMULATED vessels 
  # unfortunately cannot be  done exactly rigorously because 
  # mismatch between VMS-equipped are not (< and >15m) and DCF segments (<10, 10-12,12-18, etc.)
  
  # first, take a look...
  #unique(landings[landings$country %in% c('DEU', 'DNK'),"vsize"])
   unique(landings[landings$country %in% c( 'DNK', 'DEU', 'SWE') ,"vsize"])

  # then remove simulated vessels:
  #idx_to_remove <- which(landings$country %in% c('DEU', 'DNK')  & 
  idx_to_remove <- which(landings$country %in% c( 'DNK', 'DEU', 'SWE')  & 
                      landings$vsize %in% c('o18t24m','o24t40m','o40m', 'o15m', 'o12t18m'))
  idx_to_keep   <- (1:nrow(landings))[-idx_to_remove]
  
 
 
 
  # here compute the respective part of landings which is
  # from simulated vessels vs. other landings (those recorded into stecf that may be different than the ICES ones...pfff)
  landings_not_simulated_vessels  <- landings[idx_to_keep, ]
  dd1                              <- tapply(landings_not_simulated_vessels$landings, list(landings_not_simulated_vessels$species), sum, na.rm=TRUE)
  landings_simulated_vessels       <- landings[idx_to_remove, ]
  dd2                              <- tapply(landings_simulated_vessels$landings, list(landings_simulated_vessels$species), sum, na.rm=TRUE)
  sp                               <- names(dd2) [names(dd2) %in% names(dd1)]
  compare                          <- cbind.data.frame(not_simu=dd1[names(dd1) %in% sp], simu=dd2[names(dd2) %in% sp]  )
  an                               <- function(x) as.numeric(as.character(x))
  compare$percent_in_simu          <- round(an(compare$simu) /(an(compare$simu)+an(compare$not_simu) )*100 )
  pop_names                        <- read.table(file.path(general$main.path,  "popsspe",paste("pop_names_",general$case_study,".txt", sep="")))
  compare[as.character(pop_names[,1]),]
  dd        <- cbind.data.frame(as.character(pop_names[,2]), compare[as.character(pop_names[,1]), 'percent_in_simu' ])
  names(dd) <- c("stock","percent_in_simu")
  dd        <- replace(dd, is.na(dd), 100)
  write.table(dd,    # ordered by species pop_names... 
               file= file.path(general$main.path, "popsspe", 
                  paste('percent_landings_from_simulated_vessels.dat', sep='')), 
                 row.names=FALSE, col.names=TRUE, quote=FALSE)
   
  
  
  
  
  
  # ...and remove
  landings      <- landings[idx_to_keep, ]





  # aggregate
  nm         <- names(landings)
  idx.col  <- grep('landings', nm) # index columns with species weight
  library(data.table)
  DT  <- data.table(landings) # library data.table for fast grouping replacing aggregate()
  # AGGREGATE WEIGHT PER SPECIES
  eq1  <- c.listquote( paste ("sum(",nm[idx.col],",na.rm=TRUE)",sep="") )
  DT$landings<-as.double(DT$landings)
  DT$rectangle<-as.factor(DT$rectangle)
  DT$species<-as.factor(DT$species)
  land.agg <- DT[,eval(eq1),by=list(rectangle, species)]
  land.agg <- data.frame( land.agg)
  colnames(land.agg) <- c("rectangle", "species", nm[idx.col])


  
   # keep pops of interest only
    pop_names                        <- read.table(file.path(general$main.path,  "popsspe",paste("pop_names_",general$case_study,".txt", sep="")))
   land.agg       <- land.agg[land.agg$species %in% pop_names[,1],] # keep simulated stocks only 
   land.agg$species <- factor(land.agg$species)
   levels(land.agg$species)  <- pop_names[,2][ match(levels(land.agg$species), as.character(pop_names[,1]))] # map the name to integer
   land.agg$species <- as.numeric(as.character(land.agg$species))
   library(doBy)
   land.agg       <- orderBy(~species, data=land.agg) # library(doBy) # order from 0 to nbstock
 
  
   # STECF data are annual while we need semester data at least 
   # (because different spatail avai key in function of semester)
   #=> assume a division by 2
   land.agg$landings <- land.agg$landings/2  
   land.agg$semester <- 1
   land.agg2 <- land.agg
   land.agg2$semester <- 2
   land.agg <- rbind.data.frame(land.agg, land.agg2)
   
   
   # STECF data are in TONS 
   # while paprameterisation for c++ need kilos...
   land.agg$landings <- land.agg$landings *1000 
  
  
  
  
    # merge (pop by pop)
    # knowing pop-spe and semester-spe list of nodes
    for(pop in  1:nrow(pop_names)){
     for(semester in 1:2){
      cat(paste("------------", "\n"))
      cat(paste(pop_names[pop,1], "\n"))
      cat(paste("semester ", semester, "\n"))
        # find out the list of nodes from avai (giving the distribtion of the pop over nodes)
        from_avai_this_semester <- 
                   read.table(file.path(general$main.path,  "popsspe",   "static_avai",
                      paste(pop_names[pop,2], 'spe_avai_szgroup_nodes_semester', semester, ".dat", sep='')), header=TRUE)
        lst_nodes_from_avai_this_semester <- unique(from_avai_this_semester[,1])
        code_square_pop_semester <- 
              code_square_per_node[code_square_per_node$pt_graph %in% unique(lst_nodes_from_avai_this_semester),]
        
        
        # a check 
        map(xlim=c(-5,15),ylim=c(50,60))
        points(code_square_pop_semester[,1:2],col=2)
        
        # find out the number of graph nodes per square (presence for this species this semester) 
        # to later make the dispaching of landings between points
        nb_pt_graph_per_square <- tapply(code_square_pop_semester$pt_graph, code_square_pop_semester$code_square, length)
        code_square_pop_semester$code_square <- factor(code_square_pop_semester$code_square)
        code_square_pop_semester$nb_pt_graph_per_square <- nb_pt_graph_per_square[as.character(code_square_pop_semester$code_square)]
    
        # subset this pop this semester
        landings_this_pop <- land.agg[ land.agg$species==pop_names[pop,2] & land.agg$semester==semester, ]
        
        
        if(nrow(landings_this_pop)!=0){
         
              
           # do the merging
           merged_this_species_this_quarter <- 
              merge(code_square_pop_semester, landings_this_pop, by.x='code_square', by.y='rectangle')

           # divide the landings evenly between nodes
           merged_this_species_this_quarter$landings <- 
               merged_this_species_this_quarter$landings / merged_this_species_this_quarter$nb_pt_graph_per_square
        
           # check if merging did not lost some landings (e.g. if potential landings in square that are not in avai?!..)
           cat(paste("success rate for this pop this semester",
             round(sum(merged_this_species_this_quarter$landings)    /  sum(landings_this_pop$landings) *100,2),
              "% \n"))
        
           # convert landings by semester into landings by month assuming equal distribution 
           merged_this_species_this_quarter$landings <- merged_this_species_this_quarter$landings /6
        
           if(nrow(merged_this_species_this_quarter)==0){
              cat(paste(" no match in the data for this sp this semester  ","\n"))
     
               # ...then, put 0s
               merged_this_species_this_quarter <- cbind(code_square_pop_semester$pt_graph, 0)
               colnames(merged_this_species_this_quarter)  <- c('pt_graph', 'landings')
              }
        
        } else {
        cat(paste(" no other landings in the data for this sp this semester  ","\n"))
     
        # ...then, put 0s
        merged_this_species_this_quarter <- cbind(code_square_pop_semester$pt_graph, 0)
        colnames(merged_this_species_this_quarter)  <- c('pt_graph', 'landings')
        }
  
 #browser()       
            # save for a multimap in c++  pt_graph / landings kg per month (to be used every months)
            # to fill in the pop attribute
        write.table(round(merged_this_species_this_quarter[, c('pt_graph', 'landings')]),
               file= file.path(general$main.path, "popsspe", 
                  paste(pop_names[pop,2], 'spe_stecf_oth_land_per_month_per_node_semester', semester, ".dat", sep='')), 
                 row.names=FALSE, col.names=TRUE, quote=FALSE)
   
         
  }}
  
  
  