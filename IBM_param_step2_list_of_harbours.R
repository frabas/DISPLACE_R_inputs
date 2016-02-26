
FRANCOIS <- TRUE
XXX      <- FALSE

                                           
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
if(XXX){
   main_path_data          <-  file.path("C:","merging", "EflaloAndTacsat","GermanEflaloTacsat")
   main_path_ibm_param     <- file.path("C:","displace-project.org","repository", "ibm_vessels_param")   
   main_path_ibm_param_R   <- file.path("C:","displace-project.org","repository", "ibm_vessels_param_R")   
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
  ### SAVE                           ###
  ###--------------------------------###
  ###--------------------------------###
  ###--------------------------------###

  save(EU_ports, file=file.path(main_path_ibm_param, "myEUports.RData"))
  
  
                    