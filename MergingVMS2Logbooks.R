
FRANCOIS     <- FALSE
CHRISTIAN    <- TRUE


# set your own path here:
if(FRANCOIS){
   main_path_data         <- file.path("C:","merging", "EflaloAndTacsat")   # where are eflalo and tacsat data
   main_path_ibm_param    <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis")  # where is myEUports.RData
   main_path_ibm_param_R  <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_R_inputs", "old")  # where is vmstools_mergeEflalo2Pings.r etc.

   }
if(CHRISTIAN){
   main_path_data          <- file.path("C:")    # TO DO: inform the right path
   main_path_ibm_param     <- file.path("C:")    # TO DO: inform the right path
   main_path_ibm_param_R   <- file.path("C:")    # TO DO: inform the right path
  }
  
 
  
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  ### LOAD TACSAT & EFLALO DATA SETS ###
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  #y <- "2005"
  #y <- "2006"
  #y <- "2007"
  #y <- "2008"
  #y <- "2009"  
  #y <- "2010"
  #y <- "2012"
  #y <- "2013"
  #y <- "2014"
  y <- "2015"
  
  if(FRANCOIS){ 
                # ease the loading by creating RData objects.
                #tacsat <- read.table(file.path("C:","merging","EflaloAndTacsat", paste("tacsat2_",y,".csv",sep="")), header=TRUE, sep=",")
                #save(tacsat, file=file.path("C:","merging","EflaloAndTacsat", paste("tacsat",y,".RData",sep="")))
                #eflalo <- read.table(file.path("C:","merging","EflaloAndTacsat", paste("eflalo3_",y,".csv",sep="")), header=TRUE, sep=",")
                #save(eflalo, file=file.path("C:","merging","EflaloAndTacsat", paste("eflalo_",y,".RData",sep="")))
                
                load(file.path(main_path_data, paste("tacsat",y,".RData",sep='')) )
                load(file.path(main_path_data, paste("eflalo_",y,".RData",sep='')))
  }
  if(CHRISTIAN){ 
        eflalo <-read.table(file=file.path(main_path_data, paste("ger_eflalo",y,".csv", sep="")), sep=",", header=TRUE)
        tacsat <-read.table(file=file.path(main_path_data, paste("TACSAT2_GER_", y, ".csv", sep="")), sep=",", header=TRUE)  
  }



 
  
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  ### LOAD A PORT DATA.FRAME         ###
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
   
   load(file.path(main_path_ibm_param, "GRAPH", "myEUports.RData"))  # get EU_ports
 
  
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  ### DETECT POSITIONS IN HARBOUR    ###
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
   
 
  library(doBy) # need to order tacsat first (e.g. for Christian)
  tacsat$SI_DATE <- as.POSIXct(tacsat$SI_DATE, tz='GMT',   "%d/%m/%Y") #=> caution: becomes  "%Y-%m-%d"  !!
  tacsat$SI_TIME <-  format(strptime(  paste(tacsat$SI_TIME), tz='GMT',  "%H:%M" ), "%H:%M")    
  tacsat <- orderBy(~VE_REF+SI_DATE+SI_TIME, data=tacsat)
  
  anf <- function(x) as.numeric(as.character(x))
  source(file.path( main_path_ibm_param_R, 'vmstools_pointInHarbour.r'))
  
  tacsat$SI_HARB <- NA
  tacsat$SI_HARB <- pointInHarbour(lon=anf(tacsat$SI_LONG), lat=anf(tacsat$SI_LATI), harbours=EU_ports, rowSize=30, returnNames=TRUE)
  inHarb <- tacsat$SI_HARB
  inHarb <- replace(inHarb, !is.na(inHarb), 1)
  inHarb <- replace(inHarb, is.na(inHarb), 0)
  inHarb <- as.numeric(inHarb)

  # assign a trip identifier
  tacsat$SI_FT <- 1 # init
  idx <- which(inHarb==0)
  tacsat[idx,"SI_FT"] <- cumsum(inHarb) [idx] # add a SI_FT index

  # keep 'out of harbour' points only
  # (but keep the departure point and the arrival point lying in the harbour)
  startTrip <- c(diff(tacsat[,"SI_FT"]), 0)
  endTrip   <- c(0, diff(tacsat[,"SI_FT"]))
  tacsat[which(startTrip>0),"SI_FT"]  <-  tacsat[which(startTrip>0)+1,"SI_FT"] # tricky here
  tacsat[which(endTrip<0),"SI_FT"]    <-  tacsat[which(endTrip<0)-1,"SI_FT"] # tricky here
  tacsat <- tacsat[which(inHarb==0 |  startTrip>0 |  endTrip<0),]
    
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  ### ASSIGN A FISHING STATE         ###
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  tacsat$SI_STATE <- 2 # init (1: fishing; 2: steaming)
  tacsat$SI_STATE [(tacsat$SI_SP>4 & tacsat$SI_SP<8)] <-1
    ## => fill in SI_STATE with a fake rule just to add some guesses
 
 
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  ### FEW ADJUSTEMENTS...            ###
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  eflalo$LE_MET_level6 <- eflalo$LE_MET
  eflalo <- eflalo[eflalo$LE_MET!="No_logbook6",]
  eflalo <- eflalo[eflalo$LE_MET!="No_logbook6_>10",]
  if(FRANCOIS) eflalo$VE_FLT<-"fleet1"



  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  ### VESSEL BY VESSEL MERGING       ###
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  
  
  
  if(FRANCOIS) namefolder <- file.path("C:","output", paste("merged_DEN_", y, sep=''))
  if(CHRISTIAN)    namefolder <- file.path("C:","output", paste("merged_DEU_", y, sep=''))
  
  vessels <- unique(eflalo$VE_REF)
  if(FRANCOIS) vessels <- vessels[ grep("DNK", vessels) ]
  if(CHRISTIAN)    vessels <- vessels[ grep("DEU", vessels) ]
  
  
  an <- function(x) as.numeric(as.character(x))
  af <- function(x) as.factor(as.character(x))
  ac <- function(x) as.character(x)
  source(file.path( main_path_ibm_param_R, 'vmstools_mergeEflalo2Pings.r'))
  source(file.path( main_path_ibm_param_R, 'vmstools_segmentTacsatSpeed.r'))
  source(file.path( main_path_ibm_param_R, 'vmstools_c.listquote.r'))
  source(file.path( main_path_ibm_param_R, 'vmstools_countPings.r'))
  library(data.table)
  
  mergeEflalo2PingsForDisplace (eflalo=eflalo, tacsat=tacsat,  vessels=vessels,
          general=list(output.path=namefolder,  visual.check = TRUE,
                visual.check=TRUE, detectFishing=TRUE, speed="segment", what.speed="calculated", conserve.all=TRUE))

  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  ### LOOK AT ONE GIVEN VESSEL       ###
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  if(FRANCOIS){
    load(file.path(namefolder, "merged_DNK000003612_2010.RData"))
    head(merged[,c("VE_REF", "FT_REF", "VE_FLT", "VE_KW", "LE_MET_level6", "LE_GEAR",
                 "SI_LATI", "SI_LONG", "SI_SP", "SI_HE", "SI_HARB","SI_STATE","SI_RECT","LE_EFF_VMS","flag", 
                 "SI_DATE","SI_TIME",
                 "LE_KG_COD","LE_KG_DAB")], 40)
   }

  
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  ### BUILD ONE BIG DATASET          ###
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  
  # ...selecting relevant species:
  spp <- c('COD', 'CSH', 'DAB', 'ELE', 'FLE', 'HAD', 'HER', 'HKE', 'HOM',
         'LEM', 'MAC', 'MON', 'MUS', 'NEP', 'NOP', 'OYF', 'PLE', 'POK', 'PRA', 'SAN',
           'SOL', 'SPR', 'TUR', 'WHB', 'WIT', 'WHG',
            'OTH')

                          
  vnames <- as.character(unique(tacsat$VE_REF))
   

  source(file.path( main_path_ibm_param_R, 'vmstools_bindAllMergedTables.r'))
  tmp <- bindAllMergedTables (vessels=vnames, a.year=y, species.to.keep=spp, 
             folder = file.path(namefolder), all.in.one.table=FALSE)
   
   
   ##=> all_merged_value_2015.RData and all_merged_weight_2015.RData are now created in the output folder...
  
  
  # rename if you like
  if(FRANCOIS) {
  load(file.path(namefolder, "all_merged_weight_2015.RData"))
  coupled_VMS_logbooks <- all.merged
  save(coupled_VMS_logbooks, file=file.path(namefolder, "coupled_VMS_logbooks_DEN_2015.RData"))
 
  load(file.path(main_path_data, paste("eflalo_","2015",".RData",sep='')))
  logbooks <- eflalo
  save(logbooks, file=file.path(namefolder, "logbooks_DEN_2015.RData"))
  } 
   
  if(CHRISTIAN) {
  load(file.path(namefolder, "all_merged_weight_2015.RData"))
  coupled_VMS_logbooks <- all.merged
  save(coupled_VMS_logbooks, file=file.path(namefolder, "coupled_VMS_logbooks_DEU_2015.RData"))
 
  load(file.path(main_path_data, paste("eflalo_","2015",".RData",sep='')))
  logbooks <- eflalo
  save(logbooks, file=file.path(namefolder, "logbooks_DEU_2015.RData"))
  } 
   
   
   
 #---------------------------------------------#
 #------plot used harbours---------------------#
 #---------------------------------------------# 

 load(file.path(namefolder, "all_merged_weight_2015.RData"))
 used.ports.when.merging.proc <- as.character(unique(all.merged$SI_HARB))
 used.ports.when.merging.proc <- used.ports.when.merging.proc [!used.ports.when.merging.proc=="NA"]
 all(used.ports.when.merging.proc %in% EU_ports$Description ) # should be TRUE
 used <- EU_ports[EU_ports$Description%in%used.ports.when.merging.proc,]
 plot(used$lon, used$lat, col=4)
 library(mapdata)
 map("worldHires", add=TRUE) 


 # save
 if(CHRISTIAN) { used_ger <- used$Description; save(used_ger, file=file.path(main_path_ibm_param, "used_ports_ger_2015.RData")) }
 if(FRANCOIS) { used_den <- used$Description; save(used_den, file=file.path(main_path_ibm_param, "used_ports_den_2015.RData")) }
                 