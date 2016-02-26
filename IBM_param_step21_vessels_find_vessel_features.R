 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!LOAD & COMPUTE FOR OBTAINING!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## vesselsspe_features_quarter[xx].dat          !!!!!!!!!!!!!!!##
 ## with VE_REF| speed| fuel. cons. rate|         !!!!!!!!!!!!!!##
 ## vessel length| KW | carry cap.| tank cap.| nbpingspertrip| !##
 ## gamma par1| gamma par2  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##


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
 general$case_study_countries  <- c("DEN")    
 general$a.year                <- "2012"
 general$a.country             <- "DEN"


 # load the combined graph with the "merged" table for DNK
 load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.fgrounds.","DEN",".",general$a.year,".igraph",general$igraph,".RData",sep='')))
 ping.fgrounds.den  <- ping.fgrounds
 load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.harbours.","DEN",".",general$a.year,".igraph",general$igraph,".RData",sep='')))
 ping.harbours.den  <- ping.harbours


 if("DEN" %in% general$case_study_countries &&
      "DEU" %in% general$case_study_countries &&
       "SWE" %in% general$case_study_countries ){

 # load the combined graph with the "merged" table for DEU
 load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.fgrounds.","DEU",".",general$a.year,".igraph",general$igraph,".RData",sep='')))
 ping.fgrounds.deu  <- ping.fgrounds[, !colnames(ping.fgrounds) %in% c("QUARTER")]
 load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.harbours.","DEU",".",general$a.year,".igraph",general$igraph,".RData",sep='')))
 ping.harbours.deu  <- ping.harbours
 ping.harbours.deu <- ping.harbours[, !colnames(ping.harbours) %in% c("QUARTER")]


  # load the combined graph with the "merged" table for SWE
 load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.fgrounds.","SWE",".",general$a.year,".igraph",general$igraph,".RData",sep='')))
 ping.fgrounds.swe  <- ping.fgrounds[, !colnames(ping.fgrounds) %in% c("QUARTER")]
 load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.harbours.","SWE",".",general$a.year,".igraph",general$igraph,".RData",sep='')))
 ping.harbours.swe  <- ping.harbours
 ping.harbours.swe <- ping.harbours[, !colnames(ping.harbours) %in% c("QUARTER")]

 # small adjustement for SWE data
 ping.fgrounds.swe$VE_FLT   <- NA
 ping.fgrounds.swe$SI_SP    <- NA
 ping.fgrounds.swe$SI_HE    <- NA
 ping.fgrounds.swe$KW_HOURS <- NA
 ping.harbours.swe$VE_FLT   <- NA
 ping.harbours.swe$SI_SP    <- NA
 ping.harbours.swe$SI_HE    <- NA
 ping.harbours.swe$KW_HOURS <- NA
 ping.harbours.swe$KW_HOURS <- NA


 # combined DNK and GER (removing useless info on kg landing)
  ping.fgrounds <- rbind(
                        ping.fgrounds.den[,-grep("KG", colnames(ping.fgrounds.den))],
                         ping.fgrounds.deu[,-grep("KG", colnames(ping.fgrounds.deu))],
                          ping.fgrounds.swe[, colnames(ping.fgrounds.den[,-grep("KG", colnames(ping.fgrounds.den))])]
                         )
  # keep only DNK and DEU, and SWE vessels
  ping.fgrounds <- ping.fgrounds[c(grep("DNK", ping.fgrounds$VE_REF), grep("DEU", ping.fgrounds$VE_REF), grep("SWE", ping.fgrounds$VE_REF)),]

  # combined DNK, DEU, and SWE (removing useless info on kg landing)
  ping.harbours <- rbind(
                        ping.harbours.den[,-grep("KG", colnames(ping.harbours.den))],
                         ping.harbours.deu[,-grep("KG", colnames(ping.harbours.deu))],
                          ping.harbours.swe[,colnames(ping.harbours.den[,-grep("KG", colnames(ping.harbours.den))])]
                         )
  # keep only DNK and DEU, and SWE vessels
  ping.harbours <- ping.harbours[c(grep("DNK", ping.harbours$VE_REF), grep("DEU", ping.harbours$VE_REF), grep("SWE", ping.harbours$VE_REF)),]


  # save the combined datasets
  save(ping.fgrounds,  file=file.path(general$main.path, "merged_tables", general$case_study,
             paste("ping.fgrounds.","DEN.DEU.SWE",".",general$a.year,".igraph",general$igraph,".RData", sep='')))
      cat(paste("save the combined ping.fgrounds', this year...OK\n\n",sep=""))
  save(ping.harbours,  file=file.path(general$main.path, "merged_tables", general$case_study,
             paste("ping.harbours.","DEN.DEU.SWE",".",general$a.year,".igraph",general$igraph,".RData", sep='')))
     cat(paste("save the combined 'ping.harbours', this year...OK\n\n",sep=""))

 } else{ # end combine countries
 ping.fgrounds <- ping.fgrounds.den
 ping.harbours <- ping.harbours.den
 }



  features <- NULL

  load(file.path(general$main.path,"merged_tables", general$case_study,
           paste("all_merged_weight_",general$a.country,"_",general$a.year,".RData",sep='')))
  x <- all.merged ; rm(all.merged); gc(reset=TRUE)

  # ensure the FT_REF to be converted in numeric later on
  x$FT_REF          <- factor(x$FT_REF)
  levels(x$FT_REF ) <- 1:length(x$FT_REF)

  
  
  
  # debug to get consistency between c++ list of vesselids and c++ [DNK000]_possible_metiers files
  # because possible vessels have been removed earlier e.g. those targetting OYE or MUS exclusively...
  load(file.path(general$main.path, "popsspe", paste("betas",general$case_study,"_INTEGER_",general$case_study,".RData", sep='')) )
  nrow(x)
  ping.fgrounds                <- ping.fgrounds[ping.fgrounds$VE_REF %in% unique(all.betas.vid$VE_REF),]
  ping.fgrounds$VE_REF         <- factor(ping.fgrounds$VE_REF)
  vessels.still.there          <- unique(as.character(ping.fgrounds$VE_REF))
  x                            <- x[x$VE_REF %in% vessels.still.there,]
  nrow(x)



  # add quarters, and then semesters
  x$quarter <- quarters(as.POSIXct(x$SI_DATE, tz="GMT"))

 
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

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


 
 
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

 ## IN-BETWEEN TRIP DURATION - GAMMA PARAMETERS-----------------
  # get the interval time between trips
  x$tmp                               <- c(0,diff( as.numeric(as.character(x$FT_REF))))
  x$tmp2                              <- c(diff( as.numeric(as.character(x$FT_REF))),0)
  x                                   <- x[x$tmp!=0 |  x$tmp2!=0,]
  ctime                               <- strptime(  paste(x$SI_DATE, x$SI_TIME) ,  "%e/%m/%Y %H:%M" )
  x                                   <- cbind.data.frame(x, date.in.R=ctime)
  x$effort.mins                       <- abs(c(0, as.numeric(x[-nrow(x),"date.in.R"] -
                                               x[-1,"date.in.R"], units="mins")))
  x$time                              <- x$effort.mins /60
  x$a.diff                            <- c(0,diff( as.numeric(as.character(x$FT_REF))))
  x1                                  <- x[x$a.diff!=0 &  !is.na(x$a.diff),]
  time.interval.with.prev.trip.hours  <- x1[, c("VE_REF", "FT_REF", "time", "quarter") ]
  x2                                  <- x[x$a.diff==0 &  !is.na(x$a.diff),]
  trip.duration.hours                 <- x2[, c("VE_REF", "FT_REF", "time", "quarter") ]

  # fit a gamma law for bwtrip
  # make the average for trip duration (will be used to detect vessel with daily trips)
   for(vid in unique(time.interval.with.prev.trip.hours$VE_REF)){
       print(vid)
       timeint.this.v             <- time.interval.with.prev.trip.hours[time.interval.with.prev.trip.hours$VE_REF==vid,]
       trip.duration.hours.this.v <- trip.duration.hours[trip.duration.hours$VE_REF==vid,]
       for (a.quarter in c("Q1","Q2","Q3","Q4")){
            # 1- between trip interval in hours
            samples          <- timeint.this.v[timeint.this.v$quarter==a.quarter,'time']
            samples          <- samples[!is.na(samples)]
            #print(samples)
            samples          <- samples[samples<3000 & samples>0]  # interval >125 days impossible.
            gamma.nll        <- function(par,data) -sum(dgamma(data,shape=par[1],scale=par[2],log=T))
            opt              <- optim(c(1,1),gamma.nll,data=samples,method='BFGS')
            #print(rgamma(100, shape=opt$par[1], scale = opt$par[2]) )
            # collect...
            if (round(opt$par[2],4)==1) opt$par[2] <- 100 # CAUTION: handcrafted correction to avoid too numerous trips if bad optim

            # 2- trip duration in hours
            dd               <- trip.duration.hours.this.v[ trip.duration.hours.this.v$quarter==a.quarter,'time']
            dd               <- dd[dd!=0]
            av.trip.duration <- mean(dd, na.rm=TRUE)
            if(is.na(av.trip.duration)) av.trip.duration <- 1000


            features <- rbind.data.frame(features,
                 data.frame(VE_REF=vid, quarter=a.quarter, shape=round(opt$par[1],4), scale = round(opt$par[2],4),
                              av.trip.duration=round(av.trip.duration,0)))
       }
    }


  ## VESSEL (MAX) SPEED IN KNOTS-----------------
  load(file.path(general$main.path,"merged_tables", general$case_study,
           paste("all_merged_weight_",general$a.country,"_",general$a.year,".RData",sep='')))
  x <- all.merged ; rm(all.merged); gc(reset=TRUE)

  # add quarters, and then semesters
  x$quarter <- quarters(as.POSIXct(x$SI_DATE, tz="GMT"))

  # great circle distance
  `distance` <-
     function(lon,lat,lonRef,latRef){
                    x1 <- lon
                    y1 <- lat
                    x2 <- lonRef
                    y2 <- latRef

                    pd <- pi/180

                    a1<- sin(((y2-y1)*pd)/2)
                    a2<- cos(y1*pd)
                    a3<- cos(y2*pd)
                    a4<- sin(((x2-x1)*pd)/2)
                    a <- a1*a1+a2*a3*a4*a4

                                      c <- 2*atan2(sqrt(a),sqrt(1-a));
                                      R <- 6371;
                                      dx1 <- R*c
     return(dx1)
     }

     an <- function(x) as.numeric(as.character(x))
     last <- nrow(x)
     dists <- distance(an(x$SI_LATI[-last]), an(x$SI_LONG[-last]), an(x$SI_LATI[-1]), an(x$SI_LONG[-1]))
     x$dist <- c(0, dists[-last])
     x$knots <- (x$dist/1.852) / (an(x$LE_EFF_VMS)/60)
     max.speed.per.vid <- tapply(x$knots, x$VE_REF,
                  function(x) {x <- x[!is.infinite(x) & !is.na(x) & x<20] ; quantile(x, probs=c(0.99))})
                  #=> remove outliers included...
     # collect...
     features       <- cbind.data.frame(features, speed=0)
     features$speed <- round(max.speed.per.vid [as.character(features$VE_REF)],2) # map


    # VESSEL LENGTH-------------------
    if(general$a.country=="DEN"){
      load(file.path("C:","merging", "EflaloAndTacsat",
           paste("eflalo3_",general$a.year,".RData",sep='')))
      eflalo  <- eflalo[grep("DNK", as.character(eflalo$VE_REF)),]
      }
    if(general$a.country=="DEU") {
       load(file.path("C:","merging", "EflaloAndTacsat",  "GermanEflaloTacsat",
           paste("ger_eflalo",general$a.year,".RData",sep='')))
       eflalo$VE_LEN <- as.numeric(as.character(eflalo$VE_LEN)) /100 # convert in meters...
       }
    if(general$a.country=="SWE") {
       load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.fgrounds.",general$a.country,".",general$a.year,".igraph",general$igraph,".RData",sep='')))
       eflalo <- ping.fgrounds ; eflalo$LE_EFF <- eflalo$LE_EFF_VMS
      }
    x <- eflalo ; rm(eflalo); gc(reset=TRUE)

    x              <- subset(x,FT_REF != 0)
    
    features              <- cbind.data.frame(features, length=0)
    vessel.length.per.vid <- x[!duplicated(x$VE_REF),c("VE_REF","VE_LEN")]
    features$length       <- round(vessel.length.per.vid [match( features$VE_REF,vessel.length.per.vid$VE_REF), "VE_LEN"], 0) # map


    # VESSEL KW-------------------
   if(general$a.country=="DEN"){
      load(file.path("C:","merging", "EflaloAndTacsat",
           paste("eflalo3_",general$a.year,".RData",sep='')))
      eflalo  <- eflalo[grep("DNK", as.character(eflalo$VE_REF)),]
      }
    if(general$a.country=="DEU") {
       load(file.path("C:","merging", "EflaloAndTacsat",  "GermanEflaloTacsat",
           paste("ger_eflalo",general$a.year,".RData",sep='')))
       eflalo$VE_LEN <- as.numeric(as.character(eflalo$VE_LEN)) /100 # convert in meters...
       }
    if(general$a.country=="SWE") {
       load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.fgrounds.",general$a.country,".",general$a.year,".igraph",general$igraph,".RData",sep='')))
       eflalo <- ping.fgrounds ; eflalo$LE_EFF <- eflalo$LE_EFF_VMS
       an <- function(x) as.numeric(as.character(x))
      }
    x <- eflalo ; rm(eflalo); gc(reset=TRUE)

    x              <- subset(x,FT_REF != 0)

    features           <- cbind.data.frame(features, length=0)
    vessel.kw.per.vid  <- x[!duplicated(x$VE_REF),c("VE_REF","VE_KW")]
    features$KW        <- round(as.numeric(as.character(vessel.kw.per.vid [match( features$VE_REF,vessel.kw.per.vid$VE_REF), "VE_KW"])), 0) # map




    # FUEL CONSUMPTION RATE IN LITERS PER HOUR & TANK CAPACITY IN LITERS-------------------
    if(general$a.country=="DEN"){
      load(file.path("C:","merging", "EflaloAndTacsat",
           paste("eflalo3_",general$a.year,".RData",sep='')))
      eflalo  <- eflalo[grep("DNK", as.character(eflalo$VE_REF)),]
      }
   if(general$a.country=="DEU") {
       load(file.path("C:","merging", "EflaloAndTacsat",  "GermanEflaloTacsat",
           paste("ger_eflalo",general$a.year,".RData",sep='')))
       eflalo$VE_LEN <- as.numeric(as.character(eflalo$VE_LEN)) /100 # convert in meters...
       }
    if(general$a.country=="SWE") {
     load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.fgrounds.",general$a.country,".",general$a.year,".igraph",general$igraph,".RData",sep='')))
       eflalo <- ping.fgrounds ; eflalo$LE_EFF <- eflalo$LE_EFF_VMS /60
       }
    x <- eflalo ; rm(eflalo); gc(reset=TRUE)


     # compute effort
   x$SI_DATIM     <- as.POSIXct(paste(x$FT_DDAT,  x$FT_DTIME,   sep=" "), tz="GMT", format="%d/%m/%Y  %H:%M")
   x              <- subset(x,FT_REF != 0)
   x$ID           <- paste(x$VE_REF, x$FT_REF,sep="_")
   library(doBy)
   x              <- orderBy(~VE_REF+SI_DATIM+FT_REF, data=x)
   x$SI_DATIM2    <- as.POSIXct(paste(x$FT_LDAT,  x$FT_LTIME,    sep=" "), tz="GMT", format="%d/%m/%Y  %H:%M")
   an                  <- function(x) as.numeric(as.character(x))
   if(!"LE_EFF" %in% colnames(x)) x$LE_EFF       <- an(difftime(x$SI_DATIM2, x$SI_DATIM, units="hours"))

   table.fuelcons.per.engine <-
     read.table(file= file.path(general$main.path, "IBM_datainput_engine_consumption.txt"), header=TRUE,sep="")
   linear.model              <- lm(calc_cons_L_per_hr_max_rpm~ kW2, data=table.fuelcons.per.engine)  # conso = a*Kw +b
    #=> so, it is assumed here that knowing the kW of the vessel is enought
    # to guess its fuel consumption at maximal speed



   x$fuel_cons_rate <- predict(linear.model, newdata=data.frame(kW2=an(x$VE_KW))) # Liter per hour
   fuel.cons.rate.per.vid <-
       tapply(x$fuel_cons_rate, x$VE_REF, function(x) {x <- x[!is.na(x)] ; x[1]})
    # collect...
    features              <- cbind.data.frame(features, fuelconsrate=0)
    features$fuelconsrate <- round(fuel.cons.rate.per.vid [as.character(features$VE_REF)],3) # map



    # compute fuel cons (vessel specific)
    # (assuming equal consumption between fishing and steaming phases)
    an <- function(x) as.numeric(as.character(x))
    x$fuelcons       <- as.numeric(as.character(x$fuel_cons_rate)) * as.numeric(as.character(x$LE_EFF)) # liter per hour

    # caution, need to divide the fuel cons between records when several logbook events!!!!
    dd                        <- table(x$ID)
    x$nb_records              <- factor(x$ID) # init
    levels(x$nb_records)      <- dd[levels(x$nb_records)]   # more than 1 if change of day or ICES rect.
    x$fuelcons                <- as.numeric(as.character(x$fuelcons)) / as.numeric(as.character(x$nb_records))
  


    # compute the tank cap
    library(data.table)
    DT                    <- data.table(x) # library data.table for fast grouping replacing aggregate()
    eq1                   <- c.listquote( paste ("sum(","fuelcons",", na.rm=TRUE)",sep="") )
    DT$VE_REF             <- as.factor(DT$VE_REF)
    x.agg                 <- DT[,eval(eq1),by=list(VE_REF,FT_REF)]
    x.agg                 <- data.frame( x.agg)
    colnames(x.agg)       <- c("VE_REF", "FT_REF", "totcons")
    tank.capacity.per.vid <-
        tapply(x.agg$totcons, x.agg$VE_REF, function(x) {x<- x[x<100000]; quantile(x, probs=0.9)})# {x<- x[x<100000]; quantile(x, probs=0.99)})
        #=> remove outliers included...
    tank.capacity.per.vid[is.na(tank.capacity.per.vid)] <- 
        mean(tank.capacity.per.vid, na.rm=TRUE) # a quantile chosen to deal with outliers....

    # as the fuel consumed is likely to be dependent on the particular fishing behaviour of the vessel then go one step further and
    # model the tank capacity eg according to vessel length
    dd    <- cbind(cap=tank.capacity.per.vid[vessel.length.per.vid$VE_REF], vessel.length.per.vid)
    plot(dd$VE_LEN, dd$cap)
    nls1  <- nls(cap~ b*VE_LEN^a, data=dd, start=list(a=5, b=100))
    points(dd$VE_LEN, predict(nls1), col=4)
    #=> this is OK...

    dd            <- dd[complete.cases(dd) & dd$cap!=0,]
    dd$log_cap    <- log(dd$cap)
    dd$log_length <- log(dd$VE_LEN)
    plot( dd$log_length, dd$log_cap)
    #=> an exponential model seems good and actually better than the nls fit for larger vessels...but not for smaller ones!

    pred <- predict(lm(log_cap~log_length, data=dd), dd)
    abline(lm(log_cap~log_length, data=dd))
    plot(dd$VE_LEN, dd$cap, xlim=c(0,50), ylim=c(0,100000))
    points( dd$VE_LEN, exp(pred), pch="*")

    # collect...
    features <- cbind.data.frame(features, tank_capacity_max=0)
    features$tank_capacity_max <- round(tank.capacity.per.vid [as.character(features$VE_REF)], 0) # map from eflalo for selected vessels

    # apply the exp or the nls model
    features$log_length              <- log(features$length)
    features$tank_capacity_model_log <- round( exp( predict(lm(log_cap~log_length, data=dd), features) ) ) # if the exp model
    features$VE_LEN                  <- features$length
    features$tank_capacity_model_nls <- round( predict(nls1, features) )  # if the nls model



    # CARRYING CAPACITY IN KG---------------------------------------------------------------------
    if(general$a.country=="DEN"){
      load(file.path("C:","merging", "EflaloAndTacsat",
           paste("eflalo3_",general$a.year,".RData",sep='')))
      eflalo  <- eflalo[grep("DNK", as.character(eflalo$VE_REF)),]
      }
    if(general$a.country=="DEU") {
       load(file.path("C:","merging", "EflaloAndTacsat",  "GermanEflaloTacsat",
           paste("ger_eflalo",general$a.year,".RData",sep='')))
       eflalo$VE_LEN <- as.numeric(as.character(eflalo$VE_LEN)) /100 # convert in meters...
       }
    if(general$a.country=="SWE") {
      load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.fgrounds.",general$a.country,".",general$a.year,".igraph",general$igraph,".RData",sep='')))
       eflalo <- ping.fgrounds ; eflalo$LE_EFF <- eflalo$LE_EFF_VMS /60
      }
    x <- eflalo ; rm(eflalo); gc(reset=TRUE)


    library(data.table)
    DT  <- data.table(x) # library data.table for fast grouping replacing aggregate()
    nm <- colnames(x)
    idx.col <- grep("KG", nm)
    eq1  <- c.listquote( paste ("sum(",nm[idx.col],",na.rm=TRUE)",sep="") )
    DT$VE_REF<-as.factor(DT$VE_REF)
    x.agg <- DT[,eval(eq1),by=list(VE_REF, FT_REF)]
    x.agg <- data.frame( x.agg)
    x.agg$tot_landings <- apply(x.agg[, -c(1:2)], 1, sum, na.rm=TRUE)   # kg

    carrying.capacity.per.vid <-
        tapply(x.agg$tot_landings, x.agg$VE_REF, function(x) {; quantile(x, probs=0.9)})   # {; quantile(x, probs=0.99)})
         #=> remove outliers included...also assume that this is independent of the fuel tank capacitiy
         # this is also implicitely related to underlying quotas...
         # but it is the best we can do so far to estimate this capacity!
    # collect...
    features                   <- cbind.data.frame(features, carrying_capacity=0)
    features$carrying_capacity <- round(carrying.capacity.per.vid[as.character(features$VE_REF)], 0) # map


    # as the carrying capacity of fish  is likely to be dependent on the particular fishing behaviour of the vessel then go one step further and
    # model the  capacity eg according to vessel length
    dd <- cbind(cap=carrying.capacity.per.vid[vessel.length.per.vid$VE_REF], vessel.length.per.vid)
    plot(dd$VE_LEN, dd$cap)
    nls2 <- nls(cap~ b*VE_LEN^a, data=dd, start=list(a=1, b=1000))
    points(dd$VE_LEN, predict(nls2), col=4)
    #=> this is OK...

    # collect...
    features <- cbind.data.frame(features, carrying_capacity_max=0)
    features$carrying_capacity_max <- round(carrying.capacity.per.vid [as.character(features$VE_REF)], 0) # map from eflalo for selected vessels

    # the nls model
    features$VE_LEN              <- features$length
    features$carrying_capacity_model_nls <- round ( predict(nls2, features) )  # if the nls model






    # NB PINGS PER TRIP-------------------
    # load the combined graph with the "merged" table i.e. the ping.fgrounds object
    load(file.path(general$main.path, "merged_tables", general$case_study,
               paste("ping.fgrounds.",general$a.country,".",general$a.year,".igraph",general$igraph,".RData",sep='')))
    x <- ping.fgrounds
    x$SI_STATE <- as.numeric(as.character(x$SI_STATE))

    library(data.table)
    DT              <- data.table(x) # library data.table for fast grouping replacing aggregate()
    eq1             <- c.listquote( paste ("length(unique(","pt_graph","))",sep="") )
    DT$VE_REF       <- as.factor(DT$VE_REF)
    x.agg           <- DT[,eval(eq1),by=list(VE_REF,FT_REF)]
    x.agg           <- data.frame( x.agg)
    colnames(x.agg) <- c("VE_REF", "FT_REF", "nb_visits_of_different_fgrounds")
    nb.pings.per.trip.per.vid <-
        tapply(x.agg$nb_visits_of_different_fgrounds, x.agg$VE_REF, function(x) {; max(x, na.rm=TRUE)})
         #=>sometimes, no fpings for some vessels i.e. steaming only...then set to 3
    nb.pings.per.trip.per.vid <-  replace(nb.pings.per.trip.per.vid, is.na(nb.pings.per.trip.per.vid), 3)
    # collect...
    features <- cbind.data.frame(features, nb_pings_per_trip=0)
    features$nb_pings_per_trip <- round(nb.pings.per.trip.per.vid[as.character(features$VE_REF)], 0) # map



    # FUEL CONSUMPTION MULTIPLIER-------------------
    # not vessel specific (for the time being)
    features$mult_fuelcons_when_steaming  <-  1
    features$mult_fuelcons_when_fishing   <-  1.1
    features$mult_fuelcons_when_returning <-  1.1
    features$mult_fuelcons_when_inactive  <-  0.2





    ## check for NA on the German side
    head(features)




   # order columns and save per quarter----------------------
   for (a.quarter in c("Q1","Q2","Q3","Q4")){

    features_a_quarter <-
          features[features$quarter==a.quarter,
           c('VE_REF', 'speed', 'fuelconsrate', 'length', 'KW', 'carrying_capacity_model_nls',
                'tank_capacity_model_nls', 'nb_pings_per_trip', 'shape', 'scale', 'av.trip.duration',
                 'mult_fuelcons_when_steaming', 'mult_fuelcons_when_fishing','mult_fuelcons_when_returning','mult_fuelcons_when_inactive')]

    # keep only country spe vessels
    #features_a_quarter <-     features_a_quarter[substr(   features_a_quarter$VE_REF,1,3) %in% general$a.country,]

    FRANCOIS <- TRUE
    if(FRANCOIS) { do.append <- FALSE}
    if(!FRANCOIS)    { do.append <- TRUE}
      write.table(features_a_quarter,
           file=file.path(general$main.path, "vesselsspe",
             paste("vesselsspe_features_quarter", gsub("Q","",a.quarter),".dat",sep='')),
               col.names=FALSE,  row.names=FALSE, sep= '|', quote=FALSE, append=do.append)

  }

