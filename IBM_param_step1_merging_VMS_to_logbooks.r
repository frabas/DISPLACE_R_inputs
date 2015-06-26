
FRANCOIS <- TRUE
HEINO    <- FALSE

                                           
#library(vmstools)  # 54
#library(help=vmstools)  # getting help...
#memory.limit(4000)
# => not in use anymore, use external R source instead
# to be independent of potential broken code from vmstools updates


# set your own path here:
if(FRANCOIS){
   main_path_data         <- file.path("C:","merging", "EflaloAndTacsat")
   main_path_ibm_param    <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_raw")  
   main_path_ibm_param_R  <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_R_inputs") 

   }
if(HEINO){
   main_path_data          <-  file.path("C:","merging", "EflaloAndTacsat","GermanEflaloTacsat")
   main_path_ibm_param     <- file.path("C:","displace-project.org","repository", "ibm_vessels_param")   
   main_path_ibm_param_R   <- file.path("C:","displace-project.org","repository", "ibm_vessels_param_R")   
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
  y <- "2012"
  #y <- "2013"
  y <- "2014"
  
  if(FRANCOIS){ 
                # ease the loading by creating RData objects.
                #tacsat <- read.table(file.path("C:","merging","EflaloAndTacsat", paste("tacsat2_",y,".csv",sep="")), header=TRUE, sep=",")
                #save(tacsat, file=file.path("C:","merging","EflaloAndTacsat", paste("tacsat",y,".RData",sep="")))
                #eflalo <- read.table(file.path("C:","merging","EflaloAndTacsat", paste("eflalo3_",y,".csv",sep="")), header=TRUE, sep=",")
                #save(eflalo, file=file.path("C:","merging","EflaloAndTacsat", paste("eflalo_",y,".RData",sep="")))
                
                load(file.path(main_path_data, paste("tacsat",y,".RData",sep='')) )
                load(file.path(main_path_data, paste("eflalo_",y,".RData",sep='')))
  }
  if(HEINO){ 
        eflalo <-read.table(file=file.path(main_path_data, paste("ger_eflalo",y,".csv", sep="")), sep=",", header=TRUE)
        tacsat <-read.table(file=file.path(main_path_data, paste("TACSAT2_GER_", y, ".csv", sep="")), sep=",", header=TRUE)  
  }



  # from http://ec.europa.eu/fisheries/cfp/control/codes/index_en.htm 
  EUports <- read.table(file=file.path(main_path_ibm_param,"IBM_datainput_EU_ports_location.csv"), header=TRUE,sep=",") 
 
  
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  ### CREATE A PORT DATA.FRAME       ###
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
   
  plot(EUports$Longitude, EUports$Latitude, xlim=c(-10,20), ylim=c(50,70))
  library(mapdata)
  map("worldHires", add=TRUE)
                    
  
  ## useful to detect missing harbours BY HAND....
  #dd <- NULL
  #dd <- rbind.data.frame(dd, data.frame(locator(1)))

 
  ## add missing harbour found from suspicious lat/long
  missing.ports.dnk <- EUports[1:56,] ; missing.ports.dnk[]<- NA  #init
 
  # long/lat
  coord.missing.dnk <- data.frame(NA, ncol=3, nrow=nrow(missing.ports.dnk))
  coord.missing.dnk[1,1] <- 10.3087 ; coord.missing.dnk[1,2] <- 56.9913    ; coord.missing.dnk[1,3] <- "DNK"
  coord.missing.dnk[2,1] <-  10.6720 ; coord.missing.dnk[2,2] <- 54.7507      ; coord.missing.dnk[2,3] <-  "DNK"
  coord.missing.dnk[3,1] <-  9.7120 ; coord.missing.dnk[3,2] <- 55.2627      ; coord.missing.dnk[3,3] <- "DNK"
  coord.missing.dnk[4,1] <- 6.2040 ; coord.missing.dnk[4,2] <- 53.4080      ; coord.missing.dnk[4,3] <-  "NLD"
  coord.missing.dnk[5,1] <-  11.9293 ; coord.missing.dnk[5,2] <- 54.5727         ; coord.missing.dnk[5,3] <- "DNK"
  coord.missing.dnk[6,1] <-   12.0420 ; coord.missing.dnk[6,2] <- 54.8920      ; coord.missing.dnk[6,3] <-  "DNK"
  coord.missing.dnk[7,1] <- 11.198 ; coord.missing.dnk[7,2] <-  55.2113     ; coord.missing.dnk[7,3] <- "DNK"
  coord.missing.dnk[8,1] <-  -1.1440 ; coord.missing.dnk[8,2] <- 60.1707      ; coord.missing.dnk[8,3] <-  "DNK"
  coord.missing.dnk[9,1] <-10.7127  ; coord.missing.dnk[9,2] <- 56.5333      ; coord.missing.dnk[9,3] <- "DNK"
  coord.missing.dnk[10,1] <-  10.9253 ; coord.missing.dnk[10,2] <-57.2967   ; coord.missing.dnk[10,3] <- "DNK"
  coord.missing.dnk[11,1] <- 10.9253 ; coord.missing.dnk[11,2] <-57.2960    ; coord.missing.dnk[11,3] <- "DNK"
  coord.missing.dnk[12,1] <- 10.6713 ; coord.missing.dnk[12,2] <- 54.752   ; coord.missing.dnk[12,3] <-  "DNK"
  coord.missing.dnk[13,1] <-  10.2407 ; coord.missing.dnk[13,2] <-55.0940    ; coord.missing.dnk[13,3] <-  "DNK"
  coord.missing.dnk[14,1] <- 10.6713 ; coord.missing.dnk[14,2] <- 54.7507    ; coord.missing.dnk[14,3] <-  "DNK"
  coord.missing.dnk[15,1] <-  10.6713 ; coord.missing.dnk[15,2] <- 54.7513    ; coord.missing.dnk[15,3] <-  "DNK"
  coord.missing.dnk[16,1] <- 9.9880  ; coord.missing.dnk[16,2] <- 54.6853    ; coord.missing.dnk[16,3] <-   "DNK"
  coord.missing.dnk[17,1] <- -4.3267 ; coord.missing.dnk[17,2] <- 48.0993      ; coord.missing.dnk[17,3] <- "GBR"
  coord.missing.dnk[18,1] <-  9.965 ; coord.missing.dnk[18,2] <-57.594        ; coord.missing.dnk[18,3] <-  "DNK"
  coord.missing.dnk[19,1] <-8.304  ; coord.missing.dnk[19,2] <- 56.55      ; coord.missing.dnk[19,3] <-    "DNK"
  coord.missing.dnk[20,1] <- 9.63  ; coord.missing.dnk[20,2] <- 55.513      ; coord.missing.dnk[20,3] <- "DNK"
  coord.missing.dnk[21,1] <- 10.052 ; coord.missing.dnk[21,2] <-55.822          ; coord.missing.dnk[21,3] <- "DNK"
  coord.missing.dnk[22,1] <- 9.783 ; coord.missing.dnk[22,2] <- 54.913      ; coord.missing.dnk[22,3] <- "DNK"
  coord.missing.dnk[23,1] <- 15.137 ; coord.missing.dnk[23,2] <-  55.058         ; coord.missing.dnk[23,3] <-  "DNK"
  coord.missing.dnk[24,1] <- -0.066 ; coord.missing.dnk[24,2] <-53.581      ; coord.missing.dnk[24,3] <-  "DNK"
  coord.missing.dnk[25,1] <-  8.565 ; coord.missing.dnk[25,2] <- 55.088         ; coord.missing.dnk[25,3] <- "DNK"
  coord.missing.dnk[26,1] <-  10.588 ; coord.missing.dnk[26,2] <-57.718        ; coord.missing.dnk[26,3] <- "DNK"
  coord.missing.dnk[27,1] <-10.2585  ; coord.missing.dnk[27,2] <-54.942           ; coord.missing.dnk[27,3] <-"DNK"
  coord.missing.dnk[28,1] <-  12.467 ; coord.missing.dnk[28,2] <- 54.953        ; coord.missing.dnk[28,3] <- "DNK"
  coord.missing.dnk[29,1] <-  8.565 ; coord.missing.dnk[29,2] <- 55.088           ; coord.missing.dnk[29,3] <- "DNK"
  coord.missing.dnk[30,1] <-  12.375 ; coord.missing.dnk[30,2] <-55.253          ; coord.missing.dnk[30,3] <- "DNK"
  coord.missing.dnk[31,1] <- 8.223  ; coord.missing.dnk[31,2] <- 56.703        ; coord.missing.dnk[31,3] <-"DNK"
  coord.missing.dnk[32,1] <-  8.219 ; coord.missing.dnk[32,2] <- 56.698      ; coord.missing.dnk[32,3] <-  "DNK"
  coord.missing.dnk[33,1] <-   8.222; coord.missing.dnk[33,2] <- 56.697       ; coord.missing.dnk[33,3] <-  "DNK"
  coord.missing.dnk[34,1] <- 10.502 ; coord.missing.dnk[34,2] <- 57.494        ; coord.missing.dnk[34,3] <- "DNK"
  coord.missing.dnk[35,1] <- 14.692 ; coord.missing.dnk[35,2] <- 55.094       ; coord.missing.dnk[35,3] <-   "DNK"
  coord.missing.dnk[36,1] <- 8.5485 ; coord.missing.dnk[36,2] <- 56.583       ; coord.missing.dnk[36,3] <-  "DNK"
  coord.missing.dnk[37,1] <- 9.8885 ; coord.missing.dnk[37,2] <-55.271       ; coord.missing.dnk[37,3] <-  "DNK"
  coord.missing.dnk[38,1] <- 11.709 ; coord.missing.dnk[38,2] <-55.72            ; coord.missing.dnk[38,3] <- "DNK"
  coord.missing.dnk[39,1] <- 11.35 ; coord.missing.dnk[39,2] <-55.88          ; coord.missing.dnk[39,3] <-  "DNK"
  coord.missing.dnk[40,1] <- 14.835 ; coord.missing.dnk[40,2] <-55.249           ; coord.missing.dnk[40,3] <- "DNK"
  coord.missing.dnk[41,1] <-10.6645 ; coord.missing.dnk[41,2] <-55.449           ; coord.missing.dnk[41,3] <- "DNK"
  coord.missing.dnk[42,1] <-11.368046 ; coord.missing.dnk[42,2] <-55.971685           ; coord.missing.dnk[42,3] <- "DNK"
  coord.missing.dnk[43,1] <-9.034 ; coord.missing.dnk[43,2] <-57.156          ; coord.missing.dnk[43,3] <- "DNK"
  coord.missing.dnk[44,1] <- 9.128 ; coord.missing.dnk[44,2] <- 57.259          ; coord.missing.dnk[44,3] <- "DNK"
  coord.missing.dnk[45,1] <- 12.60204 ; coord.missing.dnk[45,2] <- 55.72024          ; coord.missing.dnk[45,3] <- "DNK"
  coord.missing.dnk[46,1] <- 10.82736 ; coord.missing.dnk[46,2] <- 54.93368          ; coord.missing.dnk[46,3] <- "DNK"
  coord.missing.dnk[47,1] <- 10.83198 ; coord.missing.dnk[47,2] <- 54.93308          ; coord.missing.dnk[47,3] <- "DNK"
  coord.missing.dnk[48,1] <- 10.66025 ; coord.missing.dnk[48,2] <-56.19643          ; coord.missing.dnk[48,3] <- "DNK"
  coord.missing.dnk[49,1] <- 11.13019 ; coord.missing.dnk[49,2] <- 55.88393         ; coord.missing.dnk[49,3] <- "DNK"
  coord.missing.dnk[50,1] <- 9.869905 ; coord.missing.dnk[50,2] <- 55.85714         ; coord.missing.dnk[50,3] <- "DNK"
  coord.missing.dnk[51,1] <- 10.542769 ; coord.missing.dnk[51,2] <- 57.44643        ; coord.missing.dnk[51,3] <- "DNK"
  coord.missing.dnk[52,1] <- 11.504004 ; coord.missing.dnk[52,2] <- 56.72321          ; coord.missing.dnk[52,3] <- "DNK"
  coord.missing.dnk[53,1] <- 11.076788 ; coord.missing.dnk[53,2] <- 55.67857          ; coord.missing.dnk[53,3] <- "DNK"
  coord.missing.dnk[54,1] <- 10.617532 ; coord.missing.dnk[54,2] <- 55.60714          ; coord.missing.dnk[54,3] <- "DNK"
  coord.missing.dnk[55,1] <- 10.70298 ; coord.missing.dnk[55,2] <- 54.93750          ; coord.missing.dnk[55,3] <- "DNK"
  coord.missing.dnk[56,1] <- 11.15155 ; coord.missing.dnk[56,2] <- 55.16964         ; coord.missing.dnk[56,3] <- "DNK"



  for (i in 1 : nrow(coord.missing.dnk)){
      missing.ports.dnk[i,"Longitude"] <-coord.missing.dnk[i,1];  missing.ports.dnk[i,"Latitude"] <-coord.missing.dnk[i,2] ; 
      missing.ports.dnk [i,"ISO.3.Country.Code"] <- coord.missing.dnk[i,3]  ;  missing.ports.dnk [i,"Description"] <- paste("NA",i,sep='')
      points(missing.ports.dnk$Longitude[i], missing.ports.dnk$Latitude[i], xlim=c(-1,20), ylim=c(50,65), pch=16, cex=0.5, col=2)
      ## browser()
  }



  
  # look at missing harbour for Germany and retrieve the coordinates then.
  #missing_ports_from_Germany <- unique(eflalo$FT_DHAR)[ ! (unique(eflalo$FT_DHAR)  %in%   unique(EUports$Description))]
  #eflalo[eflalo$FT_DHAR %in% missing_ports_from_Germany,]

   missing.ports.ger <- EUports[1:56,] ; missing.ports.ger[]<- NA  #init
   coord.missing.ger <- data.frame(NA, ncol=4, nrow=nrow(missing.ports.ger))   
  
   # retreived coordinates from google map
   coord.missing.ger[1,1] <-8.212223 ; coord.missing.ger[1,2] <-56.703752        ; coord.missing.ger[1,3] <- "DNK"    ;  coord.missing.ger[1,4] <-"Thyborøn"
   coord.missing.ger[2,1] <-6.213913 ; coord.missing.ger[2,2] <-53.423446        ; coord.missing.ger[2,3] <- "NLD"    ;  coord.missing.ger[2,4] <-"Louwersoog"
   coord.missing.ger[3,1] <-13.730793 ; coord.missing.ger[3,2] <-54.136696        ; coord.missing.ger[3,3] <- "GER"    ;  coord.missing.ger[3,4] <-"Freest"
   coord.missing.ger[4,1] <-14.691353 ; coord.missing.ger[4,2] <-55.096837        ; coord.missing.ger[4,3] <- "DNK"    ;  coord.missing.ger[4,4] <-"Rønne"
   coord.missing.ger[5,1] <-55.0621 ; coord.missing.ger[5,2] <-15.136313        ; coord.missing.ger[5,3] <- "DNK"    ;  coord.missing.ger[5,4] <-"Nexoe"
   coord.missing.ger[6,1] <- -8.398418 ; coord.missing.ger[6,2] <-43.363129        ; coord.missing.ger[6,3] <- "SPA"    ;  coord.missing.ger[6,4] <-"La Coruna"
   coord.missing.ger[7,1] <- -8.443558 ; coord.missing.ger[7,2] <-54.634232        ; coord.missing.ger[7,3] <- "IRE"    ;  coord.missing.ger[7,4] <-"Killebegs"
   coord.missing.ger[8,1] <-9.964743 ; coord.missing.ger[8,2] <-57.594471        ; coord.missing.ger[8,3] <- "DNK"    ;  coord.missing.ger[8,4] <-"Hirtshals"
   coord.missing.ger[9,1] <-9.990506 ; coord.missing.ger[9,2] <-54.684897        ; coord.missing.ger[9,3] <- "GER"    ;  coord.missing.ger[9,4] <-"Maasholm"
   coord.missing.ger[10,1] <- -3.547239 ; coord.missing.ger[10,2] <-58.611463        ; coord.missing.ger[10,3] <- "SCO"    ;  coord.missing.ger[10,4] <-"Scrabster"
   coord.missing.ger[11,1] <- 13.811002 ; coord.missing.ger[11,2] <-54.107874        ; coord.missing.ger[11,3] <- "GER"    ;  coord.missing.ger[11,4] <-"Karlshagen"
   coord.missing.ger[12,1] <- 8.544052 ; coord.missing.ger[12,2] <-53.7897        ; coord.missing.ger[12,3] <- "GER"    ;  coord.missing.ger[12,4] <-"Spieka Neufeld"
   coord.missing.ger[13,1] <- 13.031133 ; coord.missing.ger[13,2] <-54.434047        ; coord.missing.ger[13,3] <- "GER"    ;  coord.missing.ger[13,4] <-"Barhoeft"
   coord.missing.ger[14,1] <- 8.568127 ; coord.missing.ger[14,2] <-55.087235       ; coord.missing.ger[14,3] <- "GER"    ;  coord.missing.ger[14,4] <-"Havneby"
   coord.missing.ger[15,1] <- -21.971118 ; coord.missing.ger[15,2] <-64.067433       ; coord.missing.ger[15,3] <- "ISL"    ;  coord.missing.ger[15,4] <-"Hafnarfjoerdur"
   coord.missing.ger[16,1] <- -21.938374 ; coord.missing.ger[16,2] <-64.152544       ; coord.missing.ger[16,3] <- "ISL"    ;  coord.missing.ger[16,4] <-"Reykjavik"
   coord.missing.ger[17,1] <- -17.041193 ; coord.missing.ger[17,2] <-20.911798       ; coord.missing.ger[17,3] <- "MAUR"    ;  coord.missing.ger[17,4] <-"Nouadhibou"
   coord.missing.ger[18,1] <- -5.159115 ; coord.missing.ger[18,2] <-57.894759       ; coord.missing.ger[18,3] <- "ENG"    ;  coord.missing.ger[18,4] <-"Ullapool"
   coord.missing.ger[19,1] <- 8.16498 ; coord.missing.ger[19,2] <-53.536278       ; coord.missing.ger[19,3] <- "GER"    ;  coord.missing.ger[19,4] <-"Accumersiel"
   coord.missing.ger[20,1] <- 8.396106 ; coord.missing.ger[20,2] <-54.629301      ; coord.missing.ger[20,3] <- "GER"    ;  coord.missing.ger[20,4] <-"Amrum"
   coord.missing.ger[21,1] <- 8.515819 ; coord.missing.ger[21,2] <-53.738635      ; coord.missing.ger[21,3] <- "GER"    ;  coord.missing.ger[21,4] <-"Dorum"
   coord.missing.ger[22,1] <- 8.754362 ; coord.missing.ger[22,2] <-54.682193      ; coord.missing.ger[22,3] <- "GER"    ;  coord.missing.ger[22,4] <-"Schluettsiel"
   coord.missing.ger[23,1] <- 8.846884 ; coord.missing.ger[23,2] <-54.263319      ; coord.missing.ger[23,3] <- "GER"    ;  coord.missing.ger[23,4] <-"Eiderlocksperrwerk"
   coord.missing.ger[24,1] <- 8.837721 ; coord.missing.ger[24,2] <-54.030486      ; coord.missing.ger[24,3] <- "GER"    ;  coord.missing.ger[24,4] <-"Friedrichskoog"
   coord.missing.ger[25,1] <- 8.491437 ; coord.missing.ger[25,2] <-53.646876      ; coord.missing.ger[25,3] <- "GER"    ;  coord.missing.ger[25,4] <-"Wremen"
   coord.missing.ger[26,1] <- 8.35612 ; coord.missing.ger[26,2] <-53.59842      ; coord.missing.ger[26,3] <- "GER"    ;  coord.missing.ger[26,4] <-"Fedderwardersiel"
   coord.missing.ger[27,1] <- 13.584956 ; coord.missing.ger[27,2] <-54.482356      ; coord.missing.ger[27,3] <- "GER"    ;  coord.missing.ger[27,4] <-"Neu Mukran"
   coord.missing.ger[28,1] <- 8.799677 ; coord.missing.ger[28,2] <-54.284068      ; coord.missing.ger[28,3] <- "GER"    ;  coord.missing.ger[28,4] <-"Toenning"
   coord.missing.ger[29,1] <- 6.748431 ; coord.missing.ger[29,2] <-53.560577      ; coord.missing.ger[29,3] <- "GER"    ;  coord.missing.ger[29,4] <-"Borkum"
   coord.missing.ger[30,1] <- 7.163104 ; coord.missing.ger[30,2] <-53.701953      ; coord.missing.ger[30,3] <- "GER"    ;  coord.missing.ger[30,4] <-"Norderney"
   coord.missing.ger[31,1] <- 23.687691 ; coord.missing.ger[31,2] <-70.664513      ; coord.missing.ger[31,3] <- "GER"    ;  coord.missing.ger[31,4] <-"Hammerfest"
   coord.missing.ger[32,1] <- 8.381602 ; coord.missing.ger[32,2] <-54.631857      ; coord.missing.ger[32,3] <- "NOR"    ;  coord.missing.ger[32,4] <-"Wittduen"
   coord.missing.ger[33,1] <- 11.92936 ; coord.missing.ger[33,2] <-54.572093      ; coord.missing.ger[33,3] <- "GER"    ;  coord.missing.ger[33,4] <-"Gedser"
   coord.missing.ger[34,1] <- 12.466262 ; coord.missing.ger[34,2] <-54.953057      ; coord.missing.ger[34,3] <- "DNK"    ;  coord.missing.ger[34,4] <-"Klintholm"
   coord.missing.ger[35,1] <- 9.030704 ; coord.missing.ger[35,2] <-53.874392      ; coord.missing.ger[35,3] <- "GER"    ;  coord.missing.ger[35,4] <-"Niendorf" #??
   coord.missing.ger[36,1] <- 7.889382 ; coord.missing.ger[36,2] <-54.178424      ; coord.missing.ger[36,3] <- "GER"    ;  coord.missing.ger[36,4] <-"Helgoland" 
   coord.missing.ger[37,1] <- 10.588598 ; coord.missing.ger[37,2] <-57.71745      ; coord.missing.ger[37,3] <- "DNK"    ;  coord.missing.ger[37,4] <-"Skagen" 
   coord.missing.ger[38,1] <- -1.773226 ; coord.missing.ger[38,2] <-57.50319      ; coord.missing.ger[38,3] <- "ENG"    ;  coord.missing.ger[38,4] <-"Peterhead" 
   coord.missing.ger[39,1] <- 10.713342 ; coord.missing.ger[39,2] <-56.53343      ; coord.missing.ger[39,3] <- "DNK"    ;  coord.missing.ger[39,4] <-"Bonnerup" 
   coord.missing.ger[40,1] <- 2.924947 ; coord.missing.ger[40,2] <-51.234253      ; coord.missing.ger[40,3] <- "BEL"    ;  coord.missing.ger[40,4] <-"Ostende" 
   coord.missing.ger[41,1] <- 11.792897 ; coord.missing.ger[41,2] <-57.600094      ; coord.missing.ger[41,3] <- "SWE"    ;  coord.missing.ger[41,4] <-"Donsoe" 
   coord.missing.ger[42,1] <- 13.109025 ; coord.missing.ger[42,2] <-54.568355      ; coord.missing.ger[42,3] <- "GER"    ;  coord.missing.ger[42,4] <-"Vitte" 
   coord.missing.ger[43,1] <- 13.288454 ; coord.missing.ger[43,2] <-54.234572      ; coord.missing.ger[43,3] <- "GER"    ;  coord.missing.ger[43,4] <-"Stahlbrode" 
   coord.missing.ger[44,1] <- 13.031924 ; coord.missing.ger[44,2] <-54.434059      ; coord.missing.ger[44,3] <- "GER"    ;  coord.missing.ger[44,4] <-"Barhoeft" 
   coord.missing.ger[45,1] <- 13.111157 ; coord.missing.ger[45,2] <-54.58443      ; coord.missing.ger[45,3] <- "GER"    ;  coord.missing.ger[45,4] <-"Kloster" 
   coord.missing.ger[46,1] <- 12.386734 ; coord.missing.ger[46,2] <-54.353305      ; coord.missing.ger[46,3] <- "GER"    ;  coord.missing.ger[46,4] <-"Neuendorf_H" 
   coord.missing.ger[47,1] <- 13.708616 ; coord.missing.ger[47,2] <-54.280585      ; coord.missing.ger[47,3] <- "GER"    ;  coord.missing.ger[47,4] <-"Thiessow" 
   coord.missing.ger[48,1] <- 10.753165 ; coord.missing.ger[48,2] <-53.90698      ; coord.missing.ger[48,3] <- "GER"    ;  coord.missing.ger[48,4] <-"Gothmund" 
   coord.missing.ger[49,1] <- 8.807568 ; coord.missing.ger[49,2] <-54.497536      ; coord.missing.ger[49,3] <- "GER"    ;  coord.missing.ger[49,4] <-"Alterkoogchaussee" 
   coord.missing.ger[50,1] <- 8.295486 ; coord.missing.ger[50,2] <-54.758609      ; coord.missing.ger[50,3] <- "GER"    ;  coord.missing.ger[50,4] <-"Hörnum" 
   coord.missing.ger[51,1] <- 8.702157 ; coord.missing.ger[51,2] <-54.500814      ; coord.missing.ger[51,3] <- "GER"    ;  coord.missing.ger[51,4] <-"Ostersiel" 
   coord.missing.ger[52,1] <- 7.36594 ; coord.missing.ger[52,2] <-53.723021      ; coord.missing.ger[52,3] <- "GER"    ;  coord.missing.ger[52,4] <-"Baltrum" 
   coord.missing.ger[53,1] <- 8.857183 ; coord.missing.ger[53,2] <-54.11859      ; coord.missing.ger[53,3] <- "GER"    ;  coord.missing.ger[53,4] <-"Busum" 
   coord.missing.ger[54,1] <- 8.631 ; coord.missing.ger[54,2] <-54.259      ; coord.missing.ger[54,3] <- "GER"    ;  coord.missing.ger[54,4] <-"Bohler" 
   coord.missing.ger[55,1] <- 10.805125 ; coord.missing.ger[55,2] <-54.092926      ; coord.missing.ger[55,3] <- "GER"    ;  coord.missing.ger[55,4] <-"Neustadt" 
   coord.missing.ger[56,1] <- 11.194196 ; coord.missing.ger[56,2] <-54.415384      ; coord.missing.ger[56,3] <- "GER"    ;  coord.missing.ger[56,4] <-"Burgstaaken" 
                              

   for (i in 1 : nrow(coord.missing.ger)){
    missing.ports.ger[i,"Longitude"] <-coord.missing.ger[i,1];  missing.ports.ger[i,"Latitude"] <-coord.missing.ger[i,2] ; 
    missing.ports.ger [i,"ISO.3.Country.Code"] <- coord.missing.ger[i,3]  ;  missing.ports.ger [i,"Description"] <- coord.missing.ger[i,4]
    points(missing.ports.ger$Longitude[i], missing.ports.ger$Latitude[i], xlim=c(-1,20), ylim=c(50,65), pch="+", cex=0.5, col=3)
    }  
 
  
 
  EU_ports <- rbind(EUports, missing.ports.ger, missing.ports.dnk) ## HERE WE ARE
  
    
  colnames(EU_ports) [colnames(EU_ports) %in% "Latitude"]  <- "lat"
  colnames(EU_ports) [colnames(EU_ports) %in% "Longitude"] <- "lon"
  EU_ports$range <- 3 # in km
  
 
  
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  ### DETECT POSITIONS IN HARBOUR    ###
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
   
 
  library(doBy) # need to order tacsat first (e.g. for Heino)
  tacsat$SI_DATE <- as.POSIXct(tacsat$SI_DATE, tz='GMT',   "%d/%m/%Y") #=> caution: becomes  "%Y-%m-%d"  !!
  tacsat$SI_TIME <-  format(strptime(  paste(tacsat$SI_TIME) , tz='GMT',  "%H:%M" ), "%H:%M")    
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
  if(HEINO)    namefolder <- file.path("C:","output", paste("merged_DEU_", y, sep=''))
  
  vessels <- unique(eflalo$VE_REF)
  if(FRANCOIS) vessels <- vessels[ grep("DNK", vessels) ]
  if(HEINO)    vessels <- vessels[ grep("DEU", vessels) ]
  
  
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
   
   
   ##=> all_merged_value_2012.RData and all_merged_weight_2012.RData are now created in the output folder...
   
   
 #---------------------------------------------#
 #------plot used harbours---------------------#
 #---------------------------------------------# 
if(FALSE){ 

load(file.path(namefolder, "all_merged_weight_2012.RData"))
used.ports.when.merging.proc <- as.character(unique(all.merged$SI_HARB))
used.ports.when.merging.proc <- used.ports.when.merging.proc [!used.ports.when.merging.proc=="NA"]
all(used.ports.when.merging.proc %in% EU_ports$Description ) # should be TRUE
used <- EU_ports[EU_ports$Description%in%used.ports.when.merging.proc,]
plot(used$lon, used$lat, col=4)
library(mapdata)
map("worldHires", add=TRUE) 



# Swedish data are not merged following the above procedure...anyway we need to inform the visited ports by Swedish vessels. 
# add on top of previous ones all the visited ports by swedish vessels (sent by Patrik J.)
SWEports           <- read.table(file=file.path(main_path_ibm_param,"SWE_harbour_o12.txt"), header=TRUE,sep="\t") 
SWEports           <- cbind(SWEports, Coordinates="XXXX", range=3)
colnames(SWEports) <- c('lon','lat', 'ISO.3.Country.Code', 'Description', 'LOCODE.Code', 'Coordinates', 'range' )  # rename
SWEports           <- SWEports[, c('ISO.3.Country.Code', 'LOCODE.Code', 'Description',  'Coordinates', 'lat','lon', 'range')]  # reorder columns


# save
if(HEINO) { used_ger <- used$Description; save(used_ger, file=file.path(main_path_ibm_param, "used_ports_ger_2012.RData")) }
if(FRANCOIS) { used_den <- used$Description; save(used_den, file=file.path(main_path_ibm_param, "used_ports_den_2012.RData")) }
if(PATRIK) { used_swe <- SWEports$Description; save(used_swe, file=file.path(main_path_ibm_param, "used_ports_swe_2012.RData")) }  # we keep them all for Sweden

 
} # end FALSE 
  

  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  ### SAVE PORTS                     ###
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###

  multi_countries <- TRUE
  if(multi_countries){
     load (file=file.path(main_path_ibm_param, "used_ports_swe_2012.RData"))
     load (file=file.path(main_path_ibm_param, "used_ports_ger_2012.RData"))
     load (file=file.path(main_path_ibm_param, "used_ports_den_2012.RData"))
     ports.to.keep <- EU_ports[ EU_ports$Description %in% unique(c(as.character(used_swe), as.character(used_ger), as.character(used_den))), ]  # search names among the EU list
     ports.to.keep <- rbind(ports.to.keep, SWEports[! (SWEports$Description  %in% ports.to.keep$Description), ])  # for swe, add all the other ports, i.e. those not already added
     ports.to.keep <- ports.to.keep [ !duplicated( ports.to.keep $Description),]  # keep only one record for one name... 
     # ...but potentially still redundant ports if countries give different names to the same port!
     # nevertheless it is important to keep all redundant ports on board as the names link back to the country specific logbooks
     save(ports.to.keep , file=file.path(main_path_ibm_param, "ports_to_keep_2012.RData")) 
  } else{
    load (file=file.path(main_path_ibm_param, "used_ports_den_2012.RData"))
    ports.to.keep <- EU_ports[ EU_ports$Description %in% unique(c(as.character(used_den))), ]
    save(ports.to.keep , file=file.path(main_path_ibm_param, "ports_to_keep_2012.RData")) 
    }
    

  # check
  plot(ports.to.keep$lon, ports.to.keep$lat, xlim=c(-10,20), ylim=c(50,70), col=4)
  map("worldHires", add=TRUE) 
  points(missing.ports.dnk$Lon, missing.ports.dnk$Lat, col=2, pch=16, cex=0.5)
  points(missing.ports.ger$Lon, missing.ports.ger$Lat, col=3, pch="+")
  points(ports.to.keep[ports.to.keep$Coordinates=='XXXX',]$lon, ports.to.keep[ports.to.keep$Coordinates=='XXXX',]$lat, xlim=c(-10,20), ylim=c(50,70), col=5, pch=16, cex=0.5)
                 
  
 # deprecated but useful...
 if(FALSE){
 # convert coordinates into UTM coordinates
 library(sp)
 library(rgdal)
 ports.to.keep <- ports.to.keep[ !is.na(ports.to.keep$lon) &  !is.na(ports.to.keep$lat),]
 SP <- SpatialPoints(cbind(as.numeric(as.character(ports.to.keep$lon)), as.numeric(as.character(ports.to.keep$lat))),
                       proj4string=CRS("+proj=longlat +datum=WGS84"))
 ports.to.keep <- cbind(ports.to.keep, spTransform(SP, CRS("+proj=utm  +ellps=intl +zone=32 +towgs84=-84,-107,-120,0,0,0,0,0")))    # convert to UTM

 colnames( ports.to.keep)[colnames( ports.to.keep) == "coords.x1" ] <- "UTM_Easting"
 colnames( ports.to.keep)[colnames( ports.to.keep) == "coords.x2" ] <- "UTM_Northing"
  # brute force to detect overlapping points (with euclidian distance)
 conn <- matrix(ncol=3)
 colnames(conn) <- c('idx1', 'idx2', 'dist')
 
 for(i in 1:nrow( ports.to.keep)){
    if(!i %in% conn[,'idx1']){
      distances  <- sqrt( (( ports.to.keep[i, "UTM_Easting"] -    ports.to.keep  [-i, "UTM_Easting"])^2) +  ((( ports.to.keep[i, "UTM_Northing"] -    ports.to.keep  [-i, "UTM_Northing"]))^2)) 
      conn       <- rbind(conn, cbind(i, which.min (distances), distances[which.min (distances)])) 
     }
    print(i)
 }
 
 # potential overlapping points
 pts <- conn[conn[,3]>0 & conn[,3]<500,]
 cbind.data.frame(ports.to.keep[pts[,'idx1'], ],ports.to.keep[pts[,'idx2'], ]) 
 } # end FALSE



  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  ### SWEDEN                         ###
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  # special treatment for Sweden
  # because pre-processed data (by Patrik 14 Feb 14)
  
  all.merged <- read.table(file=file.path("C:","merging","EflaloAndTacsat", "SwedishEflaloTacsat", "DISPLACE_SWE_140218.txt"), sep="\t", header=TRUE)
  colnames(all.merged)[colnames(all.merged) %in% "VE_MET"]  <- "LE_MET_level6"
  colnames(all.merged)[colnames(all.merged) %in% "VE_GEAR"] <- "LE_GEAR"
  all.merged$flag <- 1 
  format_date <- "%Y-%m-%d %H:%M:%S" 
  all.merged$SI_DATIM <- as.POSIXct( all.merged$SI_DATIM, tz='GMT',   format_date)

  # compute effort in min
  all.merged$LE_EFF_VMS <- abs(c(0, as.numeric( all.merged[-nrow( all.merged),"SI_DATIM"] -
                                         all.merged[-1,"SI_DATIM"], units="mins")))
  start.trip <- c(1,diff( all.merged[,"FT_REF"]))
  all.merged[start.trip!=0, "LE_EFF_VMS"] <- 0  # just correct for the trip change points

  save(all.merged, file="C:\\displace-project.org\\repository\\ibm_vessels_param\\merged_tables\\baltic_only\\all_merged_weight_SWE_2012.RData")



  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  ### PLOTTING ON A GRID             ###
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###

 if(FRANCOIS){ a.year <- '2009'

  # 1:  fishing effort in hours
  load(file.path(namefolder, paste("all_merged_weight_",a.year,".RData",sep='')))
  all.merged$SI_LONG <- anf(all.merged$SI_LONG ) ; all.merged$SI_LATI <- anf(all.merged$SI_LATI )
  all.merged$LE_MET <- all.merged$LE_MET_level6

  # assign a trip identifier
  all.merged$tripnum <- 0 # init
  all.merged[all.merged$SI_HARB!="NA", "tripnum"] <- 1
  all.merged[, "tripnum"]  <- cumsum( all.merged[, "tripnum"]  )
  # keep only fishing positions (and the first point in the harbour)
  all.merged <- all.merged[all.merged$SI_STATE==1 | !all.merged$SI_HARB=="NA",]
  all.merged$SI_LONG <- anf(all.merged$SI_LONG)
  all.merged$SI_LATI <- anf(all.merged$SI_LATI)
  all.merged[,"LE_EFF_VMS"] <- as.numeric(as.character(all.merged[,"LE_EFF_VMS"] ))
  all.merged[, "LE_EFF_VMS"] <- replace(all.merged[,"LE_EFF_VMS"], is.na(all.merged[, "LE_EFF_VMS"]) | all.merged[, "LE_EFF_VMS"] < 0, 0)
  all.merged$LE_EFF_VMS <-   an(all.merged$LE_EFF_VMS)/ 60 # in hours
  dnk_area1 <- vmsGridCreate(all.merged, nameVarToSum="LE_EFF_VMS",  numCats=10,  plotPoints =FALSE, legendtitle="fishing (hours)",
          colLand="darkolivegreen4",  addICESgrid=TRUE,
              nameLon="SI_LONG", nameLat="SI_LATI", cellsizeX =0.05, cellsizeY =0.05, we = -3.5,ea = 3.5, so = 57, no = 61,
               breaks0=c(0,2,4,6,12,24,32, 100000), legendncol=2,
                outGridFile= paste("dnk",a.year,"_area1.asc",sep='') )
  rect(1.005,59.32,1.20,59.37, border=4, lwd=1.7)
   #59.32 N to 59.37N and 1.00 E to 1.20E
   # save
  #savePlot(filename = paste("dnk",a.year,"_area1",sep=''), type ="jpeg")

  # 2: A ZOOM
  dnk_area1_zoom <- vmsGridCreate(all.merged, nameVarToSum="LE_EFF_VMS",  numCats=10,  plotPoints =FALSE, legendtitle="fishing (hours)",
          colLand="darkolivegreen4",  addICESgrid=TRUE,
              nameLon="SI_LONG", nameLat="SI_LATI", cellsizeX =0.05, cellsizeY =0.05, we = -2,ea = 2, so = 58, no = 60.5,
               breaks0=c(0,2,4,6,12,24,32, 100000), legendncol=2,
                outGridFile= paste("dnk",a.year,"_area1_zoom.asc",sep='') )
  rect(1.005,59.32,1.20,59.37, border=4, lwd=1.7)
  #savePlot(filename =  paste("dnk",a.year,"_area1_zoom",sep=''), type ="jpeg")

  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  ### PLOTTING THE VMS TRACKS        ###
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###

  # 3: PLOT THE TRACKS
  load(file.path(namefolder, paste("all_merged_weight_",a.year,".RData",sep='')))
  all.merged$SI_LONG <- anf(all.merged$SI_LONG ) ; all.merged$SI_LATI <- anf(all.merged$SI_LATI )
   # assign a trip identifier
  all.merged$tripnum <- 0 # init
  all.merged[all.merged$SI_HARB!="NA", "tripnum"] <- 1
  all.merged[, "tripnum"]  <- cumsum( all.merged[, "tripnum"]  )

  windows(6,6)
  plot(0,0, type="n", xlim=c(-1.5,1.5),ylim=c(58,60.5), ylab="Degree North", xlab="Degree West", axes=FALSE)
  idx <- point.in.polygon(point.x = anf(all.merged$SI_LONG), point.y = anf(all.merged$SI_LATI),
        pol.x = c(-1.5, 1.5, 1.5, -1.5), pol.y = c(58, 58, 60.5, 60.5)) > 0
  focus <- all.merged[idx,]
  lst <- split(focus, focus$tripnum, drop=TRUE)
  for(i in 1:length(lst)){
    x <- lst[[i]]
    x$SI_STATE <- replace (x$SI_STATE, x$SI_STATE==2, -1) # disable
    if(nrow(x)>0) segments(x$SI_LONG, x$SI_LATI,
                                   c(x$SI_LONG [-1], x$SI_LONG [length(x$SI_LONG)]) ,
                                       c(x$SI_LATI[-1],x$SI_LATI[length(x$SI_LATI)]), col=an(x$SI_STATE))
  }
  rect(1.005,59.32,1.20,59.37, border=4, lwd=2)
  axis(1)
  axis(2, las=2)
  box()
  map("worldHires", add = TRUE, col = "darkolivegreen4", fill = TRUE,
        bg = "white",  xlim=c(-1.5,1.5),ylim=c(58,60), regions = c("uk",
            "ireland", "france", "germany", "netherlands", "norway",
            "belgium", "spain", "luxembourg", "denmark", "sweden",
            "iceland", "portugal", "italy", "sicily", "ussr",
            "sardinia", "albania", "monaco", "turkey", "austria",
            "switzerland", "czechoslovakia", "finland", "libya",
            "hungary", "yugoslavia", "poland", "greece", "romania",
            "bulgaria", "slovakia", "morocco", "tunisia", "algeria",
            "egypt"))
  #savePlot(filename =  paste("dnk",a.year,"_area1_zoom_with_tracks",sep=''), type ="jpeg")




  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  ### PLOTTING IN GOOGLE MAP         ###
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###

  
  Grid2KML <- function(output.mat=output.mat, what.quantity = 'effort', kmlfile='vms.kml',imagefile='vms.png')  {
     #Takes the output from vmsGridCreate ie. when plotMap is set to FALSE.
      #output.mat[["fishing"]] <- log(output.mat[["fishing"]])
  dd <- output.mat@grid
  d1 <- dd@cells.dim[1]
  d2 <- dd@cells.dim[2]
  fishing <- output.mat@data$fishing
  mat <- matrix((fishing),byrow=T,ncol=d1,nrow=d2)
  mat <- t(mat[d2:1,])
  bbox <- output.mat@bbox
  xxx <- seq(bbox[1,1],bbox[1,2],length=d1)
  yyy <- seq(bbox[2,1],bbox[2,2],length=d2)
  rr <- range(mat[mat!='-Inf'],na.rm=T)
  labs<-seq(rr[1],rr[2],length=9)
  image(xxx,yyy,mat,zlim=c(rr[1],rr[2]),xlab="",ylab="",col=rainbow(9))
  gd <- list(x=xxx,y=yyy,z=mat)
  gd.1 <- as.SpatialGridDataFrame.im(as.im(gd))
  proj4string(gd.1) <- CRS("+proj=longlat +datum=WGS84")
  vms.kml <- GE_SpatialGrid(gd.1)
  tf <- tempfile()
  png(file=paste(tf,imagefile,sep=''), width=vms.kml$width, height=vms.kml$height, bg="transparent",res=576)
  par(mar=c(0,0,0,0), xaxs="i", yaxs="i",cex=.25)
  image(as.image.SpatialGridDataFrame(gd.1[1]), col=heat.colors(9),xlim=vms.kml$xlim, ylim=vms.kml$ylim)
  dev.off()
  kmlOverlay(vms.kml, kmlfile=paste(tf,kmlfile,sep=''), imagefile=paste(tf,imagefile,sep=''), name=what.quantity)

  legend(x='topleft', legend=as.character(labs), pch = 22,pt.bg=heat.colors(length(labs)),
  title=what.quantity, ncol=2,bg="transparent",pt.cex=1.5 )
  }

  # call it...
  dnk_area1_zoom <- vmsGridCreate(all.merged, plotMap=FALSE, nameVarToSum="LE_EFF_VMS",  numCats=10,  plotPoints =FALSE, legendtitle="fishing (hours)",
          colLand="darkolivegreen4",  addICESgrid=TRUE,
              nameLon="SI_LONG", nameLat="SI_LATI", cellsizeX =0.05, cellsizeY =0.05, we = -2,ea = 2, so = 58, no = 60.5,
               breaks0=c(0,2,4,6,12,24,32, 100000), legendncol=2,
                outGridFile= paste("dnk",a.year,"_area1_zoom.asc",sep='') )

  Grid2KML (dnk_area1_zoom)  
  
  
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  ### PLOTTING THE ORIGIN OF LANDINGS###
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###
  sp     <- "LE_EURO_COD"
  what   <- "value"
  a.unit <- "(EURO)"
  cellsizeX = 0.05
  cellsizeY = 0.05 
  we <- 9.8; ea <- 12.7; no <- 55.2; so <- 54;
  breaks0 <- c(0, 100, 100 * (2^1), 100 * (2^2), 100 * (2^3), 100 * (2^4), 100 * 
            (2^5), 100 * (2^6), 100 * (2^7), 100 * (2^8), 100 * 
            (2^9), 1e+07) 
                                  

  load(file.path(namefolder, paste("all_merged_value_",a.year,".RData",sep='')))
   
  df1 <- all.merged[all.merged$SI_STATE == 1, c("SI_LATI", "SI_LONG", sp)]
  df1$SI_LATI <- anf(df1$SI_LATI)
  df1$SI_LONG <- anf(df1$SI_LONG)
  df1[, sp] <- replace(df1[, sp], is.na(df1[, sp]) | df1[, sp] < 0, 0)
  vmsGridCreate(df1, nameVarToSum = sp, numCats = 10, plotPoints = FALSE, 
        legendtitle = paste("landings", what, a.unit, sep = " "), 
        colLand = "darkolivegreen4", addICESgrid = TRUE, nameLon = "SI_LONG", 
        nameLat = "SI_LATI", cellsizeX = cellsizeX, cellsizeY = cellsizeY, 
        we = we, ea = ea, no = no, so = so, breaks0 = breaks0, 
        legendncol = 2)
    title(sp)
 }  #end if(FRANCOIS)                      