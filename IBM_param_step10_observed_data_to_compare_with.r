

 # GENERAL SETTINGS
  general <- list()
  general$main.path      <- file.path("C:","displace-project.org","repository", "ibm_vessels_param")
  general$main.path.code <- file.path("C:","displace-project.org","repository", "ibm_vessels_param_R")
 
  #general$igraph <- 4 # for the Canadian paper
  #general$case_study <- "canadian_paper"
  #general$case_study_countries <- c("DNK", "GER") # for the Canadian paper
  #general$a.year <- "2010"
  
  #general$igraph <- 6
  #general$case_study <- "baltic_only"
  #general$case_study_countries <- "DNK" # for the Baltic only
  #general$a.year <- "2010"
  #general$a.country <- "DNK"      

  general$igraph <- 11
  general$case_study <- "baltic_only"
  general$case_study_countries <- c("DEN", "DEU", "SWE") # for the Baltic only
  general$a.year <- "2012"
 

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
 ##!!!!!!!!!!!!!!OBSERVED DATA!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## (i.e. WHICH HAVE BEEN USED FOR THE PARAMETERISZTION)!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

   general$a.country <- "DEN"
   #general$a.country <- "DEU"
   #general$a.country <- "SWE"

   load(file.path(general$main.path,"merged_tables", general$case_study,
           paste("all_merged_weight_",general$a.country,"_",general$a.year,".RData",sep='')))
   
   a <- sum(as.numeric(as.character(all.merged[all.merged$SI_STATE==1, 'LE_EFF_VMS']))/60)
   b <- sum(as.numeric(as.character(all.merged[all.merged$SI_STATE==2, 'LE_EFF_VMS']))/60)
   a+b
  
   ### subset for the relevant vid  (the ones that have been subsetted in ibm_vessels_step4.r) ###
   load(file=file.path(general$main.path, "merged_tables", general$case_study,
             paste("vid_this_case_study_",general$a.country,".",general$a.year,".igraph",general$igraph,".RData", sep='')))  # get vid_this_case_study
   all.merged        <- all.merged[all.merged$VE_REF %in% vid_this_case_study,]
   all.merged$VE_REF <- factor(all.merged$VE_REF)
  
   x <- all.merged
 
 
  ## check
  x[x$VE_REF=="DNK000001730","LE_KG_NEP" ]
  x[x$VE_REF=="DNK000005491" & x$LE_KG_SPR!=0 & !is.na(x$LE_KG_SPR), ]
  x[x$VE_REF=="DNK000019933" & x$LE_KG_SPR!=0 & !is.na(x$LE_KG_SPR), ]
  x[x$VE_REF=="DNK000006040" & x$LE_KG_COD!=0 & !is.na(x$LE_KG_COD), ]
 

  # find out the area
  #x$SI_LATI <- as.numeric(as.character(x$SI_LATI))
  #x$SI_LONG <- as.numeric(as.character(x$SI_LONG))
  #x$area   <- ICESarea2(x, string=TRUE)
  #x[is.na(x$area), 'area'] <- 'nsea' # if out of range, assign all these nodes to North Sea stocks...

   # find out areas
   source(file=file.path(general$main.path.code, "IBM_param_utils_longlat_to_ICESareas.r"))
   x$x           <- as.numeric(as.character((x$SI_LONG) ))
   x$y           <- as.numeric(as.character((x$SI_LATI) ))
   x$code_area   <- longlat_to_ICESareas(x)

   
   x[is.na(x$code_area) | !(x$code_area %in% c('22', '23', '24', '25', '26', '27', '28-1', '28-2', '29', '30', '31', '32',
                         'IIIan', 'IIIas', 'IVa', 'IVb', 'IVc') ), 'code_area'] <- 'nsea' # rmenber  that out of range has been assigned to North Sea in step4 plan B...
   x[(x$code_area %in% c('IVa', 'IVb', 'IVc')), 'code_area'] <- 'nsea'
   x[(x$code_area %in% c('22', '23', '24')), 'code_area'] <- '2224'
   x[(x$code_area %in% c('25', '26', '27', '28-1', '28-2', '29','30', '31', '32')), 'code_area'] <- '2532'

   
   #check
   x[x$VE_REF=="DNK000006040" & x$LE_KG_COD!=0 & !is.na(x$LE_KG_COD), ]


   # add quarters, and then semesters
   x$quarter          <- quarters(as.POSIXct(x$SI_DATE, tz="GMT"))
   x$semester         <- factor(x$quarter)
   levels(x$semester) <- c(1,1,2,2, NA)
   x$month            <-  format(strptime(  x$SI_DATE , tz='GMT',  "%e/%m/%Y"  ), "%m")
   x$year.month       <- paste(general$a.year, x$month)

   # add trip count + time betwen trip in hours
   x$nbtrip <- as.numeric( c(0,diff(x$FT_REF)) != 0 ) # count trip id only one time before aggregating...
   x$bwtrip <- 0 # init
   time_in_R <- strptime(  paste(x$SI_DATE, x$SI_TIME, sep=' ') ,  "%e/%m/%Y %H:%M" )
   x <- cbind.data.frame( x, time_in_R=time_in_R)
   x$bwtrip[x$nbtrip==1] <- difftime(x[x$nbtrip==1, 'time_in_R'], x[which(x$nbtrip==1)-1, 'time_in_R'], units="hours") # time betwen trip in hours
   x[c(0,diff(as.numeric(x$VE_REF)))!=0, "bwtrip"] <- 0 # correct when change of vid
   x$bwtrip[x$bwtrip<0] <- 0 # correct for first trip
   x$bwtrip <- replace(x$bwtrip,x$bwtrip==0, NA)
   # add trip duration (tricky but it works ;-)
   x$trip_duration <- 0 # init
   x$trip_duration[which(x$nbtrip==1)-1] <- difftime( x[which(x$nbtrip==1)-1, 'time_in_R'],  
           x[c(1,which(x$nbtrip==1)[-length(which(x$nbtrip==1))]), 'time_in_R'], units="hours")# time betwen trip in hours
   x$trip_duration <- replace(x$trip_duration, x$trip_duration==0, NA)
 
   # then, aggregate...
   nm           <- names(x)
   idx.col.w    <- grep('KG', nm) # index columns with species weight
   #idx.col.v    <- grep('EURO', nm) # index columns with species value
   #idx.col      <- c(idx.col.w, idx.col.v)
   idx.col.e    <- grep('LE_EFF_VMS', nm)
   idx.col.t    <- grep('nbtrip', nm)
   idx.col.b    <- grep('bwtrip', nm)
   x$LE_EFF_VMS <- as.numeric(as.character(x$LE_EFF_VMS))
   idx.col      <- c(idx.col.w, idx.col.e, idx.col.t, idx.col.b)

   library(data.table)
   DT  <- data.table(x) # library data.table for fast grouping replacing aggregate()
   # AGGREGATE WEIGHT (OR VALUE) PER SPECIES  -------------> SUM
   eq1  <- c.listquote( paste ("sum(",nm[idx.col],",na.rm=TRUE)",sep="") )
   x.agg <- DT[,eval(eq1),by=list(VE_REF, year.month, code_area)]
   x.agg <- data.frame( x.agg)
   colnames(x.agg) <- c("VE_REF", "year.month", "code_area", nm[idx.col])

 
   nm         <- names(x)
   idx.col.d  <- grep('trip_duration', nm)
   idx.col.b  <- grep('bwtrip', nm)
   idx.col    <- c(idx.col.d, idx.col.b)
   library(data.table)
   DT  <- data.table(x) # library data.table for fast grouping replacing aggregate()
   # AGGREGATE ---------------------->MEAN
   eq1  <- c.listquote( paste ("mean(",nm[idx.col],",na.rm=TRUE)",sep="") )
   x.agg2 <- DT[,eval(eq1),by=list(VE_REF, year.month, code_area)]
   x.agg2 <- data.frame( x.agg2)
   colnames(x.agg2) <- c("VE_REF", "year.month", "code_area", "av_trip_duration", "av_bwtrip")

   #collate
   x.agg <- cbind(x.agg, x.agg2[,-c(1:3)])
 
 
 
 
 ## plot total effort (fishing + steaming)   ----!!!!FOR EXPORT!!!!
 obs.effort.2012 <- tapply(x.agg$LE_EFF_VMS/60, x.agg$year.month, sum, na.rm=TRUE)
 matplot( obs.effort.2012, ylim=c(1,max(obs.effort.2012)), type="b", main="observed total effort (in vms)",
            xlab="Month", ylab="Effort [hours]", lwd=2, lty=1)

 ## plot total nb trips   ----!!!!FOR EXPORT!!!!
 obs.nbtrips.2012 <- tapply(x.agg$nbtrip, x.agg$year.month, sum, na.rm=TRUE)
 matplot( obs.nbtrips.2012, ylim=c(1,max(obs.nbtrips.2012)), type="b", main="observed total nbtrip (in vms)",
            xlab="Month", ylab="nb trips", lwd=2, lty=1)

 ## plot average  trip duration   ----!!!!FOR EXPORT!!!!
 obs.av.trip.duration.2012 <- tapply(x.agg$av_trip_duration, x.agg$year.month, mean, na.rm=TRUE)
 obs.av.trip.duration.2012 <- replace(obs.av.trip.duration.2012, is.na(obs.av.trip.duration.2012 ), 0) # per hour
 matplot( obs.av.trip.duration.2012, ylim=c(1,max(obs.av.trip.duration.2012)), type="b", main="av trip duration (in vms)",
            xlab="Month", ylab="av trip duration", lwd=2, lty=1)

 ## plot total hours between trip    ----!!!!FOR EXPORT!!!!
 obs.bwtrip.2012 <- tapply(x.agg$bwtrip, x.agg$year.month, sum, na.rm=TRUE)
 matplot( obs.bwtrip.2012, ylim=c(1,max(obs.bwtrip.2012)), type="b", main="observed total bwtrip (in vms)",
            xlab="Month", ylab="bwtrip [hours]", lwd=2, lty=1)

 ## plot mean hours between trip    ----!!!!FOR EXPORT!!!!
 obs.av.bwtrip.2012 <- tapply(x.agg$av_bwtrip, x.agg$year.month, mean, na.rm=TRUE)
 obs.av.bwtrip.2012 <- replace(obs.av.bwtrip.2012, is.na(obs.av.bwtrip.2012 ), 0) # per hour
 matplot( obs.av.bwtrip.2012, ylim=c(1,max(obs.av.bwtrip.2012)), type="b", main="observed mean bwtrip (in vms)",
            xlab="Month", ylab="av_bwtrip [hours]", lwd=2, lty=1)

 ## effort per vid ----!!!!FOR EXPORT!!!!
 obs.effort.2012.per.vid <- tapply(x.agg$LE_EFF_VMS/60, list(x.agg$VE_REF,x.agg$year.month), sum, na.rm=TRUE)
 obs.effort.2012.per.vid <- replace(obs.effort.2012.per.vid, is.na(obs.effort.2012.per.vid ), 0) # per hour

 ## nb trip per vid ----!!!!FOR EXPORT!!!!
 obs.nbtrips.2012.per.vid <- tapply(x.agg$nbtrip, list(x.agg$VE_REF, x.agg$year.month), sum, na.rm=TRUE)
 obs.nbtrips.2012.per.vid <- replace(obs.nbtrips.2012.per.vid, is.na(obs.nbtrips.2012.per.vid ), 0) # per hour

 ## plot average  trip duration   ----!!!!FOR EXPORT!!!!
 obs.av.trip.duration.2012.per.vid <- tapply(x.agg$av_trip_duration, list(x.agg$VE_REF, x.agg$year.month), mean, na.rm=TRUE)
 obs.av.trip.duration.2012.per.vid <- replace(obs.av.trip.duration.2012.per.vid, is.na(obs.av.trip.duration.2012.per.vid ), 0) # per hour


  ## between trip (in hours) per vid ----!!!!FOR EXPORT!!!!
 obs.bwtrip.2012.per.vid <- tapply(x.agg$bwtrip, list(x.agg$VE_REF,x.agg$year.month), sum, na.rm=TRUE)
 obs.bwtrip.2012.per.vid <- replace(obs.bwtrip.2012.per.vid, is.na(obs.bwtrip.2012.per.vid ) | obs.bwtrip.2012.per.vid <0, 0) # per hour
 
 ## average nb hours between trip (in hours) per vid ----!!!!FOR EXPORT!!!!
 obs.av.bwtrip.2012.per.vid <- tapply(x.agg$av_bwtrip, list(x.agg$VE_REF, x.agg$year.month), sum, na.rm=TRUE)
 obs.av.bwtrip.2012.per.vid <- replace(obs.av.bwtrip.2012.per.vid, is.na(obs.av.bwtrip.2012.per.vid ) | obs.av.bwtrip.2012.per.vid <0, 0) # per hour

 ##
 head(x.agg[x.agg$VE_REF=="DNK000001730",],50)


 # RESHAPE
 nm         <- names(x.agg)
 idx.col.w  <- grep('KG', nm) # index columns with species weight
 # remove no longer used col
 x.agg <- x.agg[, !colnames(x.agg) %in% c("LE_EFF_VMS", "nbtrip", "bwtrip", "av_trip_duration", "av_bwtrip")]
 # reshape
 x.agg$id    <- interaction(x.agg$VE_REF, x.agg$year.month, x.agg$code_area)
 x.agg.long  <- reshape(x.agg, direction="long", ids="id",
                  times=nm[idx.col.w], timevar="species", v.names="LE_KG_", varying=4:(ncol(x.agg)-1))  # be patient....
 x.agg.long  <- x.agg.long[,c("VE_REF",  "year.month", "code_area", "species", "LE_KG_")]
 rownames(x.agg.long)   <- NULL
 colnames(x.agg.long)   <- c("VE_REF",  "year.month", "code_area", "species", "KG")


 ####------

  # temporary species names
   x.agg.long$species   <- paste(gsub("LE_KG_", "",x.agg.long$species),".", x.agg.long$code_area, sep='')

  # correct names for special cases (those across management areas)
  # caution: this creates duplicates: need to aggregate afterward
   x.agg.long[ x.agg.long$species %in% c("COD.IIIan"), "species"] <- 'COD.nsea'
   x.agg.long[ x.agg.long$species %in% c("COD.IIIas"), "species"] <- 'COD.kat'
   x.agg.long[ x.agg.long$species %in% c("HAD.IIIan"), "species"] <- 'HAD.nsea'
   x.agg.long[ x.agg.long$species %in% c("HER.IIIan", "HER.IIIas", "HER.2224"), "species"] <- 'HER.3a22'
   x.agg.long[ x.agg.long$species %in% c("SPR.2224", "SPR.2532"), "species"] <- 'SPR.2232'
   x.agg.long[ x.agg.long$species %in% c("PLE.2224", "PLE.2532"), "species"] <- 'PLE.2232'
   x.agg.long[ x.agg.long$species %in% c("FLE.2224", "FLE.2532"), "species"] <- 'FLE.2232'
   x.agg.long[ x.agg.long$species %in% c("TUR.2224", "TUR.2532"), "species"] <- 'TUR.2232'
   x.agg.long[ x.agg.long$species %in% c("DAB.2224", "DAB.2532"), "species"] <- 'DAB.2232'
   x.agg.long[grep('IIIa',  x.agg.long$species), "species"] <- # for all other species, correct kask
     paste( substr(x.agg.long[grep('IIIa',  x.agg.long$species),'species'], 1,3), ".kask", sep='')



 # change names of stock to INTEGER...
 pop_names    <- read.table(file.path(general$main.path, "popsspe", paste("pop_names_", general$case_study, ".txt", sep="")))
 x.agg.long       <- x.agg.long[x.agg.long$species %in% pop_names[,1],] # keep simulated stocks only
 x.agg.long$mapped_stk_code <- factor(x.agg.long$species)
 levels(x.agg.long$mapped_stk_code )  <- pop_names[,2][ match(levels(x.agg.long$mapped_stk_code), as.character(pop_names[,1]))] # map the name to integer
 x.agg.long <- x.agg.long[,c("VE_REF",  "year.month", "mapped_stk_code", "KG")]
 x.agg.long <- x.agg.long[!is.na(x.agg.long$mapped_stk_code),] # remove NA stocks

 # check
 head(x.agg.long[x.agg.long$VE_REF=="DNK000001730" & x.agg.long$mapped_stk_code=="0",],50)
 head(x.agg.long[x.agg.long$VE_REF=="DNK000005491" & x.agg.long$mapped_stk_code=="5",],50)
 head(x.agg.long[x.agg.long$VE_REF=="DNK000019933" & x.agg.long$mapped_stk_code=="7",],50)

 # AGGREGATE AGAIN TO REMOVE DUPLICATES (created by the correction of names for special cases (those across management areas))
 library(data.table)
 DT  <- data.table(x.agg.long) # library data.table for fast grouping replacing aggregate()
 eq1  <- c.listquote( paste ("sum(","KG",",na.rm=TRUE)",sep="") )
 x.agg.long2 <- DT[,eval(eq1),by=list(VE_REF, year.month, mapped_stk_code)]
 x.agg.long2 <- data.frame( x.agg.long2)
 colnames(x.agg.long2) <- c("VE_REF", "year.month",  "mapped_stk_code", "KG")

 # check
 head(x.agg.long2[x.agg.long2$VE_REF=="DNK000019933" & x.agg.long2$mapped_stk_code=="7",],50)

 ## reshape back ----!!!!FOR EXPORT!!!!
 x.agg.wide.vid <- reshape(x.agg.long2, timevar="mapped_stk_code", idvar=c("VE_REF","year.month"), direction="wide")
 x.agg.wide.vid <- replace(x.agg.wide.vid, is.na(x.agg.wide.vid ), 0) # per hour
 
  # possibly, add the missing stocks for this country (remenber the list of stocks has been defined from DEN) 
  if(general$a.country!="DEN"){
  x.agg.wide.vid<-replace(x.agg.wide.vid,(is.na(x.agg.wide.vid)),0)
  for(i in 0:(nrow(pop_names)-1)){   
  if(paste('KG.',i , sep='') %in% colnames(x.agg.wide.vid)){
  }else{ x.agg.wide.vid<-cbind(x.agg.wide.vid,0)
  colnames(x.agg.wide.vid)<-replace(colnames(x.agg.wide.vid),colnames(x.agg.wide.vid)==0,paste('KG.',i , sep='')) 
  }}
  }

 x.agg.wide.vid <- x.agg.wide.vid[, c('VE_REF','year.month', paste('KG.', 0: (nrow(pop_names)-1), sep=''))] ## reorder the columns!


 library(data.table)
 DT  <- data.table(x.agg.long) # library data.table for fast grouping replacing aggregate()
 # AGGREGATE WEIGHT (OR VALUE) PER SPECIES
 eq1  <- c.listquote( paste ("sum(","KG",",na.rm=TRUE)",sep="") )
 x.agg.long.tot <- DT[,eval(eq1),by=list(year.month, mapped_stk_code)]
 x.agg.long.tot <- data.frame( x.agg.long.tot)
 colnames(x.agg.long.tot) <- c("year.month", "mapped_stk_code", "pop")


 ## reshape back ----!!!!FOR EXPORT!!!!
 x.agg.wide <- reshape(x.agg.long.tot, timevar="mapped_stk_code", idvar="year.month", direction="wide")
 
   # possibly, add the missing stocks for this country (remenber the list of stocks has been defined from DEN) 
  if(general$a.country!="DEN"){
  x.agg.wide<-replace(x.agg.wide,(is.na(x.agg.wide)),0)
  for(i in 0:(nrow(pop_names)-1)){   
  if(paste('pop.',i , sep='') %in% colnames(x.agg.wide)){
  }else{ x.agg.wide<-cbind(x.agg.wide,0)
  colnames(x.agg.wide)<-replace(colnames(x.agg.wide),colnames(x.agg.wide)==0,paste('pop.',i , sep='')) 
  }}
  }

 x.agg.wide <- x.agg.wide[, c('year.month', paste('pop.', 0: (nrow(pop_names)-1), sep=''))] ## reorder the columns!


  
  
 ## plot observed landings per species
 matplot( x.agg.wide[,paste('pop.', 0: (nrow(pop_names)-1) , sep='')]/1e3, ylim=c(1,1e4), type="b", main="Obs. landings per species",
           pch= as.character(1:(nrow(pop_names)) ), xlab="Month", ylab="Observed landings [tons]", lwd=2, lty=1)

 ## total landings per species in 2012 for the simulated vessels,
 ## will be used for calibration by comparing obs with simulated ones

 obj <- apply(x.agg.wide[,paste('pop.', 0: (nrow(pop_names)-1) , sep='')]/1e3   , 2, sum, na.rm=TRUE)
  barplot(obj[order(obj, decreasing=TRUE)],
                   main="Total landings (tons) in 2012", cex.names=0.7, las=2 )
 savePlot(filename = file.path(general$main.path,
             paste("obs_landings_tons_per_pop_in_2012_for_the_simulated_",general$a.country,"_",general$case_study,"_vessels.jpeg",sep="")),
                         type = "jpeg", device = dev.cur())

 ## EXPORT!!!!!!!!!!!!!!!!!!!!!
 save (x.agg.wide, obs.effort.2012, obs.nbtrips.2012, obs.bwtrip.2012, obs.av.bwtrip.2012, obs.av.trip.duration.2012, 
        obs.effort.2012.per.vid, obs.nbtrips.2012.per.vid,
          obs.bwtrip.2012.per.vid, obs.av.bwtrip.2012.per.vid, obs.av.trip.duration.2012.per.vid, 
          x.agg.wide.vid,  
                file=file.path(general$main.path, paste("obs2012_",general$a.country,"_",general$case_study,".RData",sep='')))

## DNK
#round(apply(x.agg.wide[,paste('pop.', 0: (nrow(pop_names)-1) , sep='')]/1e3, 2, sum, na.rm=TRUE))
# pop.0  pop.1  pop.2  pop.3  pop.4  pop.5  pop.6  pop.7  pop.8  pop.9 pop.10 pop.11 pop.12 pop.13 pop.14 pop.15 pop.16 pop.17 pop.18 pop.19 pop.20 pop.21 pop.22 pop.23 pop.24 pop.25 pop.26 pop.27 pop.28 pop.29 pop.30
#    68    757  22073   7960    760   7199   1954   6310   1237     24   6210  10401    560     20    113    452   6674   6419     80    358     66    631   2524   1025    998     37     18     24    174     42  11002 
# DEU
# pop.0  pop.1  pop.2  pop.3  pop.4  pop.5  pop.6  pop.7  pop.8  pop.9 pop.10 pop.11 pop.12 pop.13 pop.14 pop.15 pop.16 pop.17 pop.18 pop.19 pop.20 pop.21 pop.22 pop.23 pop.24 pop.25 pop.26 pop.27 pop.28 pop.29 pop.30
#     7      8     25   5580      4      0      0   1005    114      0   2422   2227     66      1    263   1487      0      0      0      0      0    831    330      6    215      0      0      0    986      0    225 
# SWE 
# pop.0  pop.1  pop.2  pop.3  pop.4  pop.5  pop.6  pop.7  pop.8  pop.9 pop.10 pop.11 pop.12 pop.13 pop.14 pop.15 pop.16 pop.17 pop.18 pop.19 pop.20 pop.21 pop.22 pop.23 pop.24 pop.25 pop.26 pop.27 pop.28 pop.29 pop.30
#     0    102   1769   4104   6179    752      0  10317    231      8   1182   8500     72      0     45    707   2130    220      0      0      0     54      2      8     50      0      0      2      0      0      0 




  if (FALSE){
    ###################DEPRECATED#############################
    ###################DEPRECATED#############################

   # COMPARE WITH ICES DATA:
   ## CAUTION: NEED TO REDO AN EXTRACTION WITH THE RIGHT LIST OF SPECIES...
   ICES_stat <- read.table (file.path(general$main.path,
       "IBM_datainput_ICES_catch_statistics_III_IV_2009-actually_landings_only.csv"), sep=",")
   colnames(ICES_stat) <- c("Country","Species", "Division", "2009")
     ICES_stat <- ICES_stat[-1,]


   ICES_stat$Division <- factor(ICES_stat$Division)
   levels(ICES_stat$Division) <-  c('kask','2224','2224','2224',
                                     '2224','2224','2224','2532',
                                     '2532','2532','2532','2532',
                                     '2532','2532','2532','2532',
                                     '2532', 'nsea','nsea','nsea',
                                     'nsea','nsea','nsea','nsea',
                                     'nsea')      #pble with e.g. sprat 2232!...

   ICES_stat$Species <- factor(ICES_stat$Species)
   levels(ICES_stat$Species) <- c('COD','HER','MAC','MUS','DAB','SOL','CSH','FLE','PLE','SPR','HAD','PRA','NEP','NOP','POL','POK','SAN', 'SOL', 'WHG')

   ICES_stat$pop <- paste(ICES_stat$Species,".", ICES_stat$Division, sep='')

   ## change stock names for INTEGER...
   pop_names    <- read.table(file.path(general$main.path, "popsspe","pop_names.txt"))
   ICES_stat       <- ICES_stat[ICES_stat$pop %in% pop_names[,1],] # keep simulated stocks only
   ICES_stat$mapped_stk_code <- factor(ICES_stat$pop)
   levels(ICES_stat$mapped_stk_code )  <- pop_names[,2][ match(levels(ICES_stat$mapped_stk_code), as.character(pop_names[,1]))] # map the name to integer


    ## get landings in weight for Denmark, all vessels
    an <- function(x) as.numeric(as.character(x))
    ICES_stat_dnk <- ICES_stat[ICES_stat$Country=="Denmark",]
    dnk <-   tapply(an(ICES_stat_dnk[,'2009']), ICES_stat_dnk$mapped_stk_code, sum, na.rm=TRUE)
    names(dnk) <- paste("pop.", names(dnk), sep='')
    ## get landings in weight for Germany, all vessels
    ICES_stat_ger <- ICES_stat[ICES_stat$Country=="Germany",]
    ger <-   tapply(an(ICES_stat_ger[,'2009']), ICES_stat_ger$mapped_stk_code, sum, na.rm=TRUE)
    names(ger) <- paste("pop.", names(ger), sep='')
    ## sum up for other countries (i.e. exclud. dnk and ger)
    ICES_stat_oth <- ICES_stat[!ICES_stat$Country %in% c("Germany", "Denmark"),]
    oth <-   tapply(an(ICES_stat_oth[,'2009']), ICES_stat_oth$mapped_stk_code, sum, na.rm=TRUE)
    names(oth) <- paste("pop.", names(oth), sep='')

    ## is missing species? e.g. CSH
    all_combi_dnk <- rep(0,  nrow(pop_names) )
    names(all_combi_dnk) <- paste("pop.",  0: (nrow(pop_names)-1), sep='')
    all_combi_dnk[names(dnk)] <- dnk
    all_combi_ger <- rep(0, nrow(pop_names) )
    names(all_combi_ger) <- paste("pop.",  0: (nrow(pop_names)-1) , sep='')
    all_combi_oth[names(ger)] <- ger
    all_combi_oth <- rep(0, nrow(pop_names) )
    names(all_combi_oth) <- paste("pop.",  0: (nrow(pop_names)-1) , sep='')
    all_combi_oth[names(oth)] <- oth

    ## and then fill in the table in the stock_parameters.doc
    mat <- cbind.data.frame(round(apply(x.agg.wide[,paste('pop.',  0: (nrow(pop_names)-1) , sep='')]/1e3, 2, sum, na.rm=TRUE)),
        all_combi_dnk,
          all_combi_ger,
           all_combi_oth)
    colnames(mat) <- c("simulated_dnk_vessels", "all_dnk", "all_ger", "oth_countries")

  } # end FALSE

