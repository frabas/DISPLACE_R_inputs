c.listquote <- function( ... ) {

   args <- as.list( match.call()[ -1 ] )
   lstquote <- list( as.symbol( "list" ) );
   for ( i in args ) {
      # Evaluate expression in parent eviron to see what it refers to
      if ( class( i ) == "name" || ( class( i ) == "call" && i[[1]] != "list" ) ) {
         i <- eval( substitute( i ), sys.frame( sys.parent() ) )
      }
      if ( class( i ) == "call" && i[[1]] == "list" ) {
         lstquote <- c( lstquote, as.list( i )[ -1 ] )
      }
      else if ( class( i ) == "character" )
      {
         for ( chr in i ) {
            lstquote <- c( lstquote, list( parse( text=chr )[[1]] ) )
         }
      }
      else
         stop( paste( "[", deparse( substitute( i ) ), "] Unknown class [", class( i ), "] or is not a list()", sep="" ) )
   }
   return( as.call( lstquote ) )
}


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#!!!!!!!!!GET AVAILABILITY!!!!!!!!!!!!!!#
#!!!!!!!!!SPATIAL ALLOCATION KEY!!!!!!!!#
#!!!!!!!!AT THE HAUL LEVEL!!!!!!!!!!!!!!#
#!!!!!!!!!!!!PER STOCK !!!!!!!!!!!!!!!!!#
#!!!(E.G COD IN NS, COD IN BW, etc.)!!!!#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
if(FALSE){
#igraph    <- "4"  # for the CJFAS paper
igraph    <- "11"  # for the Baltic onyl case
for (nmy in c("2008_2012", "2012")){
## CAUTION HERE: we need to run the script twice:
# 1: for the ibm runs: smoothed distribution over a period of 5 years
# 2: for the glm informing the catch equation (only one year to be sure to have the correponding avai to the origin of landings in this year)


bits.cpue <- read.table("C:\\displace-project.org\\repository\\ibm_vessels_param\\IBM_datainput_DATRAS_CPUE_all_sp_per_length_per_haul_BITS_2008_2012.csv", header=TRUE, sep=",")
ibts.cpue <- read.table("C:\\displace-project.org\\repository\\ibm_vessels_param\\IBM_datainput_DATRAS_CPUE_selected_sp_per_length_per_haul_NS-IBTS_2008_2012.csv", header=TRUE, sep=",")

#selected in datras for NS-IBTS: 
#                 Sandeel        Ammodytes marinus      SAN
#        Atlantic Herring          Clupea harengus      HER
#            Atlantic Cod             Gadus morhua      COD
#             Common  Dab          Limanda limanda      DAB
#                    Monk      Lophius piscatorius      MON
#                 Haddock Melanogrammus aeglefinus      HAD
#                 Whiting     Merlangius merlangus      WHG
#           European Hake    Merluccius merluccius      HKE
#    Lemon Sole             Microstomus kitt      LEM
#             Blue Mussel           Mytilus edulis      MUS
#          Norway Lobster      Nephrops norvegicus      NEP
#   European Flat Oyster            Ostrea edulis      OYF
#      European Flounder       Platichthys flesus      FLE
#        European Plaice    Pleuronectes platessa      PLE
#                 Saithe        Pollachius virens      POK
#                 Turbot            Psetta maxima      TUR
#      Atlantic Mackerel             Scomber scombrus      MAC
#            Common Sole              Solea solea      SOL
#                  Sprat        Sprattus sprattus      SPR
# Atlantic Horsemackerel      Trachurus trachurus      HOM
#        Norway Pout         Trisopterus esmarkii      NOP


if(nmy=="2012") years <- c(2012)
if(nmy=="2008_2012") years <- c(2008:2012)
 
# subset for the chosen period
ibts.cpue <- ibts.cpue[ibts.cpue$Year %in% years,]
bits.cpue <- bits.cpue[bits.cpue$Year %in% years,]
ibts.cpue$Year <- factor(ibts.cpue$Year)
bits.cpue$Year <- factor(bits.cpue$Year)

# get species fao code + add missing or mispelling species e.g. PRA and SAN
load("C:\\displace-project.org\\repository\\ibm_vessels_param\\IBM_datainput_speciesLatinNames.rda")
speciesLatinNames <- rbind.data.frame(speciesLatinNames, 
                              data.frame(species_eng="Pandalus", ff_species_latin="Pandalus", fao_code="PRA"))
speciesLatinNames <- rbind.data.frame(speciesLatinNames, 
                              data.frame(species_eng="Ammodytes marinus", ff_species_latin="Ammodytes marinus", fao_code="SAN"))

## CAUTION: CSH and PRA are all at 0s in the surveys!!
## so use the commercial cpue instead. 
## caution: tricky there because replace the records of the ibts survey with the commercial (Danish) geolocalized cpue...
# load the data from VMS/logbook coupling
load("C:\\output\\merged_DEN_2012\\all_merged_weight_2012.RData")
spp <- c("LE_KG_PRA", "LE_KG_CSH", "LE_KG_OYF", "LE_KG_MUS", "LE_KG_NEP")
ibts.cpue$Species <- as.character(ibts.cpue$Species)
bits.cpue$Species <- as.character(bits.cpue$Species)
for(sp in spp){
   if(sp=="LE_KG_PRA") latin <- "Pandalus"
   if(sp=="LE_KG_CSH") latin <- "Crangon crangon"
   if(sp=="LE_KG_MUS") latin <- "Mytilus edulis"
   if(sp=="LE_KG_OYF") latin <- "Ostrea edulis" 
   if(sp=="LE_KG_NEP") latin <- "Nephrops norvegicus"
   this.sp <- all.merged[!is.na(all.merged[,sp])  & all.merged[,sp] !=0 &
                               as.numeric(as.character(all.merged$SI_LONG))>-10, 
                         c("SI_LONG","SI_LATI","SI_DATE",sp,"LE_EFF_VMS")] 
                         
   this.sp$cpue                <- as.numeric(as.character(this.sp[,sp]))/(as.numeric(as.character(this.sp$LE_EFF_VMS))/60)
   this.sp$quarter             <- quarters(as.POSIXct(as.character(this.sp$SI_DATE)))
   this.sp                     <- this.sp[this.sp$quarter!="QNA",]
   this.sp$cpue                <- replace(this.sp$cpue, is.na(this.sp$cpue), 0)
   # create ibts.cpue like data format and fill in from commercial cpue
   cn                      <- colnames(ibts.cpue[ibts.cpue$Species==latin,])
   zz                      <- data.frame(matrix(0,ncol=length(cn), nrow=nrow(this.sp)))
   colnames(zz)            <- cn
   zz$ShootLat             <- this.sp$SI_LATI
   zz$ShootLon             <- this.sp$SI_LONG
   zz$Year                 <- "2012"
   zz$LngtClas             <- 5 # assume all indiv in the fist szgroup bin
   zz$CPUE_number_per_hour <- this.sp$cpue
   zz$Quarter              <- gsub("Q","", this.sp$quarter)
   zz$Survey               <- "commercial"
   zz$Species              <- latin
   zz$Ship                 <- ""
   zz$Gear                 <- ""
   # replace the data
   if(sp=="LE_KG_PRA" || sp=="LE_KG_CSH") {
     ibts.cpue               <- ibts.cpue[!ibts.cpue$Species==latin,]
     ibts.cpue               <- rbind.data.frame(ibts.cpue, zz)
     }
   if(sp=="LE_KG_MUS" || sp=="LE_KG_OYF") {
     bits.cpue               <- bits.cpue[!bits.cpue$Species==latin,]
     bits.cpue               <- rbind.data.frame(bits.cpue, zz)
     }
   if(sp=="LE_KG_NEP") {
   # do not remove the NEP in the (north sea) scientific survey but also add the info from commercial Danish activity
     bits.cpue               <- rbind.data.frame(bits.cpue, zz)     
     }

   }  #=> do not care about warnings here...
rm(all.merged); gc(reset=TRUE)

bits.cpue$Species_latin <- bits.cpue$Species
ibts.cpue$Species_latin <- ibts.cpue$Species
bits.cpue$Species <- speciesLatinNames$fao_code[match(bits.cpue$Species, speciesLatinNames$ff_species_latin)]
ibts.cpue$Species <- speciesLatinNames$fao_code[match(ibts.cpue$Species, speciesLatinNames$ff_species_latin)]

bits.cpue <- bits.cpue[!is.na(bits.cpue$Species),] 
ibts.cpue <- ibts.cpue[!is.na(ibts.cpue$Species),] 


## FILES FOR BUILDING A IGRAPH
load(file.path("C:","displace-project.org","repository","ibm_vessels_param","igraph",paste(igraph,"_graphibm.RData",sep=''))) # get coord
coord <- cbind(coord, 1:nrow(coord)) # keep track of the idx_node

# BUILD THE IGRAPH OBJECT IN R
# library(igraph)
# vertices    <- data.frame(name=as.character(unique(graph[,1])), x=coord[,1], y=coord[,2]) #(**)
# edges        <- data.frame(from=c(graph[,1]),
#                        to=c(graph[,2]),
#                        dist.km=graph[,3])
# g <- graph.data.frame(edges, directed=FALSE, vertices=vertices)
# print(g, e=TRUE, v=TRUE)

 # add a semester code
ibts.cpue$Semester <- factor(ibts.cpue$Quarter) # init
bits.cpue$Semester <- factor(bits.cpue$Quarter) # init
levels(bits.cpue$Semester) <- c(1,1,2,2) # BTS in Q1 and Q4
levels(ibts.cpue$Semester) <- c(1,1,2,2) # IBTS in Q1, Q2 and Q3


#------------------------  
#------------------------  
#------------------------  
# keep only relevant species
# BITS
species.to.keep <- c("COD", "PLE", "HER", "SPR", "WHG", "TUR", "FLE", "DAB", "POK", "SOL", "HOM", "MON", "HKE", "HAD", 
                          "MUS", "OYF", "NEP")
bits.cpue <- bits.cpue [bits.cpue$Species %in% species.to.keep,] 
bits.cpue$Species <- factor(bits.cpue$Species)

# NS-IBTS
species.to.keep <- levels(ibts.cpue$Species) # keep all, actually...
ibts.cpue <- ibts.cpue [ibts.cpue$Species %in% species.to.keep,] 
ibts.cpue$Species <- factor(ibts.cpue$Species)



#------------------------  
#------------------------  
#------------------------  
# design size group (every 10 cm)
#ibts.cpue$size_group <- floor(as.numeric(as.character(ibts.cpue$LngtClas)) / 100 )
#bits.cpue$size_group <- floor(as.numeric(as.character(bits.cpue$LngtClas)) / 100 )
# design size group (every 5 cm)
ibts.cpue$size_group <- floor(as.numeric(as.character(ibts.cpue$LngtClas)) / 50 )
bits.cpue$size_group <- floor(as.numeric(as.character(bits.cpue$LngtClas)) / 50 )

ibts.cpue$size_group[ibts.cpue$size_group>13] <-13 # i.e. a plusgroup when > 70 cm if every 5cm
bits.cpue$size_group[bits.cpue$size_group>13] <-13 # a plusgroup when > 70 cm  if every 5cm

#------------------------  
#------------------------  
#------------------------  
# fast aggregation using data.table
# to sum the nb of individuals over the new defined size_group
library(data.table)
# NS-IBTS
for (sp in c("CSH","PRA")){
    ibts.cpue[ibts.cpue$Species==sp,'ShootLat'] <- round(as.numeric(as.character(ibts.cpue[ibts.cpue$Species==sp,'ShootLat'])),1)
    ibts.cpue[ibts.cpue$Species==sp,'ShootLon'] <- round(as.numeric(as.character(ibts.cpue[ibts.cpue$Species==sp,'ShootLon'])),1)
    } #=> discretise a little bit the coordinates to improve the aggregation and speed up the reshape()!
ibts.cpue$ShootLat <- factor(ibts.cpue$ShootLat)
ibts.cpue$ShootLon <- factor(ibts.cpue$ShootLon)
ibts.cpue$size_group <- factor(ibts.cpue$size_group)
ibts.cpue$id <- factor(paste(ibts.cpue$Year,".",ibts.cpue$Quarter,".",
                          ibts.cpue$HaulNo,".",ibts.cpue$Species,".",ibts.cpue$ShootLon,".",ibts.cpue$ShootLat, sep=''))
DT  <- data.table(ibts.cpue) # library data.table for fast grouping replacing aggregate()
eq1  <- c.listquote(paste("sum(","CPUE_number_per_hour",",na.rm=TRUE)",sep=""))
agg.ibts.cpue <- DT[,eval(eq1),by=list(Survey,Year,Semester,Quarter,id, ShootLon,ShootLat, Species,size_group)]
agg.ibts.cpue <- data.frame(agg.ibts.cpue)
colnames(agg.ibts.cpue) <- c("Survey", "Year", "Semester", "Quarter", "id", "ShootLon", "ShootLat", "Species", "size_group", "nb_indiv")

# BITS
for (sp in c("OYF","MUS","NEP")){
    bits.cpue[bits.cpue$Species==sp,'ShootLat'] <- round(as.numeric(as.character(bits.cpue[bits.cpue$Species==sp,'ShootLat'])),1)
    bits.cpue[bits.cpue$Species==sp,'ShootLon'] <- round(as.numeric(as.character(bits.cpue[bits.cpue$Species==sp,'ShootLon'])),1)
    } #=> discretise a little bit the coordinates to improve the aggregation and speed up the reshape()!
bits.cpue$ShootLat <- factor(bits.cpue$ShootLat)
bits.cpue$ShootLon <- factor(bits.cpue$ShootLon)
bits.cpue$size_group <- factor(bits.cpue$size_group)
bits.cpue$id <- factor(paste(bits.cpue$Year,".",bits.cpue$Quarter,".",
                              bits.cpue$HaulNo,".",bits.cpue$Species,".",bits.cpue$ShootLon,".",bits.cpue$ShootLat, sep=''))
DT  <- data.table(bits.cpue) # library data.table for fast grouping replacing aggregate()
eq1  <- c.listquote(paste("sum(","CPUE_number_per_hour",",na.rm=TRUE)",sep=""))
agg.bits.cpue <- DT[,eval(eq1),by=list(Survey,Year,Semester,Quarter,id, ShootLon,ShootLat, Species,size_group)]
agg.bits.cpue <- data.frame(agg.bits.cpue)
colnames(agg.bits.cpue) <- c("Survey", "Year", "Semester", "Quarter", "id", "ShootLon", "ShootLat", "Species", "size_group", "nb_indiv")

#------------------------  
#------------------------  
#------------------------  
#reshape to the wide format
# tricky because reshape() is painful for large dataset
# so do it chunk by chunk accroding to species.
library(doBy)

# NS-IBTS
agg.ibts.cpue <- orderBy(~size_group, data=agg.ibts.cpue)
ibts.cpue.lst <- NULL # chunk by chunk
levels(agg.ibts.cpue$id) <- 1:length(levels(agg.ibts.cpue$id)) # make it easier...
for(sp in unique(ibts.cpue$Species)){
cat(paste(sp, "\n"))
ibts.cpue.lst[[sp]] <- reshape(agg.ibts.cpue[agg.ibts.cpue$Species==sp,], timevar="size_group",
         idvar="id", direction="wide", v.names="nb_indiv")

  # make dim compatible for do.call("rbind")
  nbcol.to.add <- 14 - length(grep("nb_", colnames(ibts.cpue.lst[[sp]])))   # knowing that we need 14 szgroup bins
  if(nbcol.to.add>0){
      tmp <- matrix(0,ncol=nbcol.to.add, nrow=nrow(ibts.cpue.lst[[sp]]))
      ibts.cpue.lst[[sp]] <- cbind(ibts.cpue.lst[[sp]], tmp)  
      colnames(ibts.cpue.lst[[sp]])  <- 
       c('Survey', 'Year', 'Semester', 'Quarter', 'id', 'ShootLon', 'ShootLat', 'Species',
           'nb_indiv.0', 'nb_indiv.1', 'nb_indiv.2', 'nb_indiv.3',
             'nb_indiv.4', 'nb_indiv.5', 'nb_indiv.6', 'nb_indiv.7',
              'nb_indiv.8', 'nb_indiv.9', 'nb_indiv.10', 'nb_indiv.11','nb_indiv.12', 'nb_indiv.13' )
   }
}
ibts.cpue <- do.call("rbind", ibts.cpue.lst)


# BITS
agg.bits.cpue <- orderBy(~size_group, data=agg.bits.cpue)
bits.cpue.lst <- NULL # chunk by chunk
levels(agg.bits.cpue$id) <- 1:length(levels(agg.bits.cpue$id)) # make it easier...
for(sp in unique(bits.cpue$Species)){
cat(paste(sp, "\n"))
bits.cpue.lst[[sp]] <- reshape(agg.bits.cpue[agg.bits.cpue$Species==sp,], timevar="size_group",
         idvar="id", direction="wide", v.names="nb_indiv")
  
  # make dim compatible for do.call("rbind")
  nbcol.to.add <- 14 - length(grep("nb_", colnames(bits.cpue.lst[[sp]])))   # knowing that we need 14 szgroup bins
  if(nbcol.to.add>0){
      tmp <- matrix(0,ncol=nbcol.to.add, nrow=nrow(bits.cpue.lst[[sp]]))
      bits.cpue.lst[[sp]] <- cbind(bits.cpue.lst[[sp]], tmp)  
      colnames(bits.cpue.lst[[sp]])  <- 
       c('Survey', 'Year', 'Semester', 'Quarter', 'id', 'ShootLon', 'ShootLat', 'Species',
           'nb_indiv.0', 'nb_indiv.1', 'nb_indiv.2', 'nb_indiv.3',
             'nb_indiv.4', 'nb_indiv.5', 'nb_indiv.6', 'nb_indiv.7',
              'nb_indiv.8', 'nb_indiv.9', 'nb_indiv.10', 'nb_indiv.11','nb_indiv.12', 'nb_indiv.13' )
   }

}                  
bits.cpue <- do.call("rbind", bits.cpue.lst)


ibts.cpue <- replace(ibts.cpue, is.na(ibts.cpue) | ibts.cpue=="Inf", 0)
bits.cpue <- replace(bits.cpue, is.na(bits.cpue) | bits.cpue=="Inf", 0)

# check
head(bits.cpue[bits.cpue$Species=="MUS",])

# save                      
save(bits.cpue, ibts.cpue, 
   file=file.path("C:","displace-project.org","repository","ibm_vessels_param","avai",paste("cpue_graph",igraph,"_",nmy,".RData",sep='')))

} # end for
} # end FALSE

#------------------------  
#------------------------  
#------------------------  
 
   # euclidean distance  (from crossdist in spatstat)
   if(FALSE){
   x1 <- coord[,1] # long graph
   y1 <- coord[,2] # lat graph
   x2 <- an(bits.cpue$ShootLon) # long bits
   y2 <- an(bits.cpue$ShootLat) # lat bits
   n1 <- length(x1)
   n2 <- length(x2)
   X1 <- matrix(rep(x1, n2), ncol = n2)
   Y1 <- matrix(rep(y1, n2), ncol = n2)
   X2 <- matrix(rep(x2, n1), ncol = n1)
   Y2 <- matrix(rep(y2, n1), ncol = n1)
   mat <- sqrt((X1 - t(X2))^2 + (Y1 - t(Y2))^2) 
   mat[1,] <- sqrt((X1[1,]-t(X2)[1,])^2 + (Y1[1,]-t(Y2)[1,])^2) 
   lst.graph.pts.with.idx.in.bits <-
       lapply(1:nrow(mat), function(i,mat) {which(mat[i,]<0.3)}, mat) # return the index in X less  than < 0.3 for each Y
   }
  
  
#------------------------  
#------------------------  
#------------------------  
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
                    return(dx1)}
  
  
  
#------------------------  
#------------------------  
#------------------------  
get_cpue_on_graph_nodes <-  function (obj=bits.cpue, igraph=4, coord=coord, sp=sp, S=S,
                          general=list(threshold=30, p=0.5, sp=c("COD"), S=1, method="maximum", years=nmy)){
  
  nb_size_group <- 13

 # subset species and semester
 obj <- obj[obj$Species==sp,]
 obj <- obj[obj$Semester==S,]
 cat(paste("sp ", sp, "\n"))
 cat(paste("S ", S, "\n"))
 
 if(nrow(obj)!=0){
 
 # remove NA
 obj <- replace(obj, is.na(obj), 0)
 
 # 
 an <- function(x) as.numeric(as.character(x))
 obj$ShootLon <- an(obj$ShootLon)
 obj$ShootLat <- an(obj$ShootLat)
 
 # a comment
 a.comment <- paste("graph", igraph, "_", sp,"_","S",S,"_",general$method,"_",general$years ,"_",general$survey, sep='')
 

  # compute the distance from each node of the graph to the nearest survey points 
  lst.graph.pts.with.idx.in.obj <- vector("list", length=nrow(coord))
  lst.graph.pts.with.idx.in.obj <- 
      lapply(1:length(lst.graph.pts.with.idx.in.obj), function(i, obj, coord){
        dd <- distance(an(obj$ShootLon), an(obj$ShootLat), coord[i,1],coord[i,2])
        names(dd) <- 1:length(obj$ShootLon)
        dd[dd < general$threshold]  ##!!!! THRESHOLD IN km  !!!!##
     }, obj, coord)  
  
 
 # check the links between one randomly chosen node and its neighbours by plotting
 if(FALSE){
   an <- function(x) as.numeric(as.character(x))
   plot(x=an(obj$ShootLon), y=an(obj$ShootLat),  col=grey(0.7), pch=16)
   segments(an(coord[,1])[760], an(coord[,2])[760],
            an(obj$ShootLon)[ an(names(lst.graph.pts.with.idx.in.obj[[760]])) ], 
               an(obj$ShootLat)[ an(names(lst.graph.pts.with.idx.in.obj[[760]])) ], col=3)
   points(x=coord[,"x"], y=coord[,"y"], col="black") # graph g
   points(x=coord[760,"x"], y=coord[760,"y"], col="red") # a pt of the graph g

  segments(an(coord[,1])[746], an(coord[,2])[746],
            an(obj$ShootLon)[ an(names(lst.graph.pts.with.idx.in.obj[[746]])) ], 
               an(obj$ShootLat)[ an(names(lst.graph.pts.with.idx.in.obj[[746]])) ], col=3)
   points(x=coord[746,"x"], y=coord[746,"y"], col="red") # a pt of the graph g

   }

 # inverse distance weighted average
  nm <- paste(rep(sp, each=nb_size_group),".nb_indiv.", 0:nb_size_group,sep='')
   tmp <- matrix(0, ncol=length(nm), nrow=nrow(coord))
   colnames(tmp) <- nm
   coord <- cbind(coord, tmp)
   for(szgroup in paste("nb_indiv.",0:nb_size_group,sep='')){
    plot(x=an(obj$ShootLon), y=an(obj$ShootLat),  col=grey(0.7), pch=16)
    cat(paste("szgroup", szgroup, "\n"))
     for(node in 1:nrow(coord)){
      #cat(paste("node", node, "\n"))
         idx.pts.obj  <- an(names(lst.graph.pts.with.idx.in.obj[[node]]))
         idx.pts.obj  <- idx.pts.obj  [  !is.na( an(obj[obj$Species==sp, szgroup][ idx.pts.obj ]) ) ] # remove distance with NA for catch (check why NA here)
         dist.pts.obj  <- lst.graph.pts.with.idx.in.obj[[node]] [as.character(idx.pts.obj)]
         if(length(dist.pts.obj)>0){
         coord[node, paste(sp,".",szgroup,sep='')] <-  
                   switch(general$method,
                      inverse= sum(an(obj[obj$Species==sp, szgroup][ idx.pts.obj ]) * ( (1/(dist.pts.obj^general$p)) / sum(1/(dist.pts.obj^general$p)) )), #weighted mean
                      maximum= max(an(obj[obj$Species==sp, szgroup][ idx.pts.obj ])) # keep only the neighbour with max
                      )
         } else{coord[node, paste(sp,".",szgroup,sep='')] <-0}             
         # check by plotting
         if(FALSE && szgroup=="nb_indiv.0")
         if(length(lst.graph.pts.with.idx.in.obj[[node]])!=0){
              segments(an(coord[,1])[node], an(coord[,2])[node],
                an(obj$ShootLon)[ an(names(lst.graph.pts.with.idx.in.obj[[node]])) ], 
                 an(obj$ShootLat)[ an(names(lst.graph.pts.with.idx.in.obj[[node]])) ], col=3)
              points(x=coord[node,"x"], y=coord[node,"y"], col="red") # a pt of the graph g
         savePlot(filename = file.path("C:","Users","fba","Dropbox","ibm_vessels_param","avai", paste("jpeg_igraph",igraph,"_",general$years,sep=''),
                            paste(a.comment,"link_nodes_with_surveys_",general$threshold,".jpeg",sep="")),type ="jpeg")

         }
      } # end node  


  # check 
    plot(x= obj[obj$Species==sp, "ShootLon"], y=obj[obj$Species==sp, "ShootLat"],  col=2, pch=1, 
                cex=obj[obj$Species==sp,szgroup] /max(coord[,paste(sp,".",szgroup,sep='')], obj[obj$Species==sp,szgroup])*4)
    #points(x= coord[,1], y=coord[,2],  col=1, pch=16, 
    #             cex=coord[,paste(sp,".",szgroup,sep='')] /max(coord[,paste(sp,".",szgroup,sep='')], obj[obj$Species==sp,szgroup])*4)
    points(x= coord[,1], y=coord[,2],  col=1, pch=16, 
                 cex=coord[,paste(sp,".",szgroup,sep='')]/ max(coord[,paste(sp,".",szgroup,sep='')])*4)
    points(x= obj[obj$Species==sp, "ShootLon"], y=obj[obj$Species==sp, "ShootLat"],  col=2, pch=1,  # overlay...
                cex=obj[obj$Species==sp,szgroup] /max(coord[,paste(sp,".",szgroup,sep='')], obj[obj$Species==sp,szgroup])*4)
    map(add=TRUE, xlim=c(-10,25),ylim=c(50,65))
    title(szgroup)
    savePlot(filename = file.path("C:","displace-project.org","repository","ibm_vessels_param","avai", paste("jpeg_igraph",igraph,"_", general$years,sep=''),
                            paste(a.comment,"_",szgroup,".jpeg",sep="")),type ="jpeg")
 
   } # end vzgruop

coord <-  replace(coord, is.infinite(coord), 0)          

} else{
print("no observation there....fill in with 0s")
   nm <- paste(rep(sp, each=nb_size_group),".nb_indiv.", 0:nb_size_group,sep='')
   tmp <- matrix(0, ncol=length(nm), nrow=nrow(coord))
   colnames(tmp) <- nm
   coord <- cbind(coord, tmp)
    for(szgroup in paste("nb_indiv.",0:nb_size_group,sep='')){
     for(node in 1:nrow(coord)){
         coord[node, paste(sp,".",szgroup,sep='')] <- 0
         } 
     }    
}

return(coord)
}    

#--------------------
#--------------------
#--------------------
#--------------------
set.avai <- function(lst.avai,sp,S,areas){
     obj <- get(paste("coord.",sp,".",S, sep=''), env=.GlobalEnv) # get in .GlobalEnv
     source(file=file.path("C:","Users","fbas", "Documents", "GitHub", "DISPLACE_R_inputs","IBM_param_utils_longlat_to_ICESareas.r"))
     ICESareas       <- longlat_to_ICESareas(obj)
     idx.areas          <- which(ICESareas %in% areas)
     obj.in.areas <- obj[idx.areas,]
     obj.in.areas <- replace(obj.in.areas, is.infinite(obj.in.areas), NA)
     obj.in.areas[,c(5:18)] <-
                sweep(obj.in.areas[,c(5:18)], 2,
                   apply(obj.in.areas[,c(5:18)], 2, sum, na.rm=TRUE), FUN="/")
     name.areas <- 'undefined'
     if(sp %in% c("SPR", "PLE", "FLE", "TUR", "DAB")){  # special case: west and east baltic merged
        if(all(c("IVa", "IVb", "IVc") %in% areas)) name.areas <- 'nsea'
        if(all(c("IIIan","IIIas") %in% areas)) name.areas <- 'kask'
        if(all(c("22", "23", "24") %in% areas)) name.areas <- '2232'
        if(all(c("25", "26", "27","28-1","28-2","29","30","31","32") %in% areas)) name.areas <- '2232'
     }else{
      if(sp %in% c("COD", "HAD")){ # special case: skagerat with nsea, kat alone
        if(all(c("IVa", "IVb", "IVc","IIIan") %in% areas)) name.areas <- 'nsea'
        if(all(c("IIIas") %in% areas)) name.areas <- 'kat'
        if(all(c("22", "23", "24") %in% areas)) name.areas <- '2224'
        if(all(c("25", "26", "27","28-1","28-2","29","30","31","32") %in% areas)) name.areas <- '2532'
       }else{
      if(sp %in% c("HER")){  # special case: 3a with western baltic
        if(all(c("IVa", "IVb", "IVc") %in% areas)) name.areas <- 'nsea'
        if(all(c("IIIan") %in% areas)) name.areas <- '3a22'
        if(all(c("22", "23", "24") %in% areas)) name.areas <- '3a22'
        if(all(c("25", "26", "27","28-1","28-2","29","30","31","32") %in% areas)) name.areas <- '2532'
       } else{ # default
        if(all(c("IVa", "IVb", "IVc") %in% areas)) name.areas <- 'nsea'
        if(all(c("IIIan","IIIas") %in% areas)) name.areas <- 'kask'
        if(all(c("22", "23", "24") %in% areas)) name.areas <- '2224'
        if(all(c("25", "26", "27","28-1","28-2","29","30","31","32") %in% areas)) name.areas <- '2532'
      }
      }
      }
     
     
     
     nm <- paste(sp,paste(name.areas ,collapse='.'),sep='.')
     lst.avai[[ nm ]][[S]] <- obj.in.areas
     # availability key: each col should sum to 1 i.e. for each age
return(lst.avai)
}

##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!MAIN!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
if(TRUE){

#map(xlim=c(0,17), ylim=c(50,60))
library(maps)
library(maptools)
library(mapdata)
map("worldHires", add=FALSE, col="green", fill=TRUE, bg="white",  xlim=c(-10,20) , ylim=c(49,63) , 
regions=c('uk','ireland','france','germany','netherlands', 'norway','belgium',
'spain','luxembourg','denmark', 'sweden','iceland', 'portugal','italy','sicily','ussr','sardinia','albania','monaco','turkey','austria',
'switzerland','czechoslovakia','finland','libya', 'hungary','yugoslavia','poland','greece','romania','bulgaria', 'slovakia','morocco',
'tunisia','algeria','egypt' ))
library(maptools)
ices_areas <- readShapeSpatial("C:\\displace-project.org\\repository\\ibm_vessels_param\\ices_areas\\ices_areas")
plot(ices_areas, col="grey",  add=TRUE)

load("C:\\displace-project.org\\repository\\ibm_vessels_param\\IBM_datainput_balticBathy.RData")
#contour(balticBathy$x,balticBathy$y,balticBathy$z,levels=c(60,120),add=T)
# replaced by: to filter out the noise
ma <- function(x,n){filter(x,rep(1/n,n), sides=1)}   # moving average filter
mm <- function(x,n){runmed(x, k=n)}                   # moving median filter
dd  <- mm(x=as.vector(balticBathy$z), n=21)
dd <- matrix(dd, nrow=dim(balticBathy$z)[1], ncol=dim(balticBathy$z)[2])
dd  <- mm(x=as.vector(t(balticBathy$z)), n=21)
dd <- matrix(dd, nrow=dim(t(balticBathy$z))[1], ncol=dim(t(balticBathy$z))[2])
contour(balticBathy$x,balticBathy$y, t(dd) ,levels=c(60,120),add=T)


load("C:\\displace-project.org\\repository\\ibm_vessels_param\\IBM_datainput_nseaBathy.RData")
#contour(nseaBathy$x,nseaBathy$y,nseaBathy$z,levels=c(60,120),add=T)
# replaced by: to filter out the noise
ma <- function(x,n){filter(x,rep(1/n,n), sides=1)}   # moving average filter
mm <- function(x,n){runmed(x, k=n)}                   # moving median filter
dd  <- mm(x=as.vector(nseaBathy$z), n=31)
dd <- matrix(dd, nrow=dim(nseaBathy$z)[1], ncol=dim(nseaBathy$z)[2])
dd  <- mm(x=as.vector(t(nseaBathy$z)), n=31)
dd <- matrix(dd, nrow=dim(t(nseaBathy$z))[1], ncol=dim(t(nseaBathy$z))[2])
contour(nseaBathy$x,nseaBathy$y, t(dd) ,levels=c(60,120),add=T)


method    <- "maximum" # for the canadian paper
threshold <- 50
igraph    <- "4"  # fro the paper

method    <- "inverse" # for the baltic only CS. because the mesh size if much finer the inverse would be better but unfortunately the survey points are too scarce.....
threshold <- 25
igraph    <- "11"  # for Baltic only case
p         <- 0.1
# => http://fr.wikipedia.org/wiki/Pond%C3%A9ration_inverse_%C3%A0_la_distance : p est un nombre positif réel, appelé le paramètre de puissance. Ici, le poids des points voisins diminue lorsque la distance augmente. Les plus grandes valeurs de p donnent une influence plus grande aux valeurs les plus proches du point interpolé. Pour 0 < p < 1 u(x) donne des pics lissés au-dessus du point interpolé xk, alors que pour p > 1 le pic devient plus pointu. 


#for (nmy in c("2005_2010", "2010")){
for (nmy in c("2008_2012", "2012")){
#for (nmy in c("2012")){


if(nmy=="2005_2010")  load(file.path("C:","displace-project.org","repository","ibm_vessels_param","avai",paste("cpue_graph",igraph,"_2005_2010.RData", sep='')))
if(nmy=="2010")       load(file.path("C:","displace-project.org","repository","ibm_vessels_param","avai",paste("cpue_graph",igraph,"_2010.RData", sep='')))
if(nmy=="2008_2012")  load(file.path("C:","displace-project.org","repository","ibm_vessels_param","avai",paste("cpue_graph",igraph,"_2008_2012.RData", sep='')))
if(nmy=="2012")       load(file.path("C:","displace-project.org","repository","ibm_vessels_param","avai",paste("cpue_graph",igraph,"_2012.RData", sep='')))


# input to DISPLACE_GUI
 obj       <- ibts.cpue
 obj$Stock <- as.character(obj$Species) # init
 obj$x     <- as.numeric(as.character(obj$ShootLon)) # init
 obj$y     <- as.numeric(as.character(obj$ShootLat)) # init
 source(file=file.path("C:","Users","fbas", "Documents", "GitHub", "DISPLACE_R_inputs","IBM_param_utils_longlat_to_ICESareas.r"))
 obj$ICESareas  <- longlat_to_ICESareas(obj)
 # convert Species in Stock name
 idx                <- obj$Species %in% c("SPR", "PLE", "FLE", "TUR", "DAB") & obj$ICESareas %in% c("IVa", "IVb", "IVc") 
 obj[idx, "Stock" ] <- paste(obj[idx,"Species"], 'nsea', sep="_")
 idx                <- obj$Species %in% c("SPR", "PLE", "FLE", "TUR", "DAB") & obj$ICESareas %in% c("IIIan","IIIas")
 obj[idx, "Stock" ] <- paste(obj[idx,"Species"], 'kask', sep="_")
 idx                <- obj$Species %in% c("SPR", "PLE", "FLE", "TUR", "DAB") & obj$ICESareas %in% c("IIIan","IIIas")
 obj[idx, "Stock" ] <- paste(obj[idx,"Species"], 'kask', sep="_")
 idx                <- obj$Species %in% c("SPR", "PLE", "FLE", "TUR", "DAB") & obj$ICESareas %in% c("22", "23", "24", "25", "26", "27","28-1","28-2","29","30","31","32")
 obj[idx, "Stock" ] <- paste(obj[idx,"Species"], '2232', sep="_")

 idx                <- obj$Species %in% c("COD", "HAD") & obj$ICESareas %in% c("IVa", "IVb", "IVc","IIIan")
 obj[idx, "Stock" ] <- paste(obj[idx,"Species"], 'nsea', sep="_")
 idx                <- obj$Species %in% c("COD", "HAD") & obj$ICESareas %in% c("IIIas")
 obj[idx, "Stock" ] <- paste(obj[idx,"Species"], 'kat', sep="_")
 idx                <- obj$Species %in% c("COD", "HAD") & obj$ICESareas %in% c("22", "23", "24")
 obj[idx, "Stock" ] <- paste(obj[idx,"Species"], '2224', sep="_")
 idx                <- obj$Species %in% c("COD", "HAD") & obj$ICESareas %in% c("25", "26", "27","28-1","28-2","29","30","31","32")
 obj[idx, "Stock" ] <- paste(obj[idx,"Species"], '2532', sep="_")

 idx                <- obj$Species %in% c("HER") & obj$ICESareas %in% c("IVa", "IVb", "IVc")
 obj[idx, "Stock" ] <- paste(obj[idx,"Species"], 'nsea', sep="_")
 idx                <- obj$Species %in% c("HER") & obj$ICESareas %in% c("IIIan")
 obj[idx, "Stock" ] <- paste(obj[idx,"Species"], '3a22', sep="_")
 idx                <- obj$Species %in% c("HER") & obj$ICESareas %in% c("22", "23", "24")
 obj[idx, "Stock" ] <- paste(obj[idx,"Species"], '3a22', sep="_")
 idx                <- obj$Species %in% c("HER") & obj$ICESareas %in% c("25", "26", "27","28-1","28-2","29","30","31","32")
 obj[idx, "Stock" ] <- paste(obj[idx,"Species"], '2532', sep="_")

 all_other_species  <- !obj$Species %in% c("SPR", "PLE", "FLE", "TUR", "DAB", "COD", "HAD", "HER")
 idx                <- all_other_species & obj$ICESareas %in% c("IVa", "IVb", "IVc")
 obj[idx, "Stock" ] <- paste(obj[idx,"Species"], 'nsea', sep="_")
 idx                <- all_other_species & obj$ICESareas %in% c("IIIan","IIIas")
 obj[idx, "Stock" ] <- paste(obj[idx,"Species"], 'kask', sep="_")
 idx                <- all_other_species & obj$ICESareas %in% c("22", "23", "24")
 obj[idx, "Stock" ] <- paste(obj[idx,"Species"], '2224', sep="_")
 idx                <- all_other_species & obj$ICESareas %in% c("25", "26", "27","28-1","28-2","29","30","31","32")
 obj[idx, "Stock" ] <- paste(obj[idx,"Species"], '2532', sep="_")

  
  write.table(obj, file=file.path("C:", "Users", "fbas", "Documents", "GitHub" ,"DISPLACE_input_raw", "avai",
           paste("survey_pop_distribution_on_points_ibts_2008_2012.txt",sep='')), sep=";", quote=FALSE, row.names=FALSE, col.names=TRUE)
     # but missing: the stock name. because species name is not giving the stock name then stock need to be given before processing into the GUI







print(nmy)
# load a graph
load(file.path("C:","displace-project.org","repository","ibm_vessels_param", "igraph", paste(igraph, "_graphibm.RData",sep=''))) # built from the R code
coord <- cbind(coord, 1:nrow(coord)) # keep track of the idx_node


an <<- function(x) as.numeric(as.character(x))
# calls
# => assign the returned objects in .GlobalEnv
spp <- unique(c(as.character(unique(ibts.cpue$Species)), as.character(unique(bits.cpue$Species))))
for (sp in as.character(spp)){ # per species
   for (S in 1:2){            # per semester
      flag1 <- 0; flag2 <-0
      if(sp %in% unique(bits.cpue$Species)){
        flag1 <-1
       assign(paste("bits.coord.",sp,".",S,sep=''),
            get_cpue_on_graph_nodes (obj=bits.cpue, igraph=igraph, coord=coord, sp=sp, S=S, ## BITS
                       general=list(main.path =file.path("C:","displace-project.org","repository","ibm_vessels_param"),
                       threshold=threshold,  p=p, method=method, years=nmy, survey="bits")),  envir =.GlobalEnv  
      )}                 
      if(sp %in% unique(ibts.cpue$Species)){
        flag2<-1
        assign(paste("ibts.coord.",sp,".",S,sep=''),
            get_cpue_on_graph_nodes (obj=ibts.cpue, igraph=igraph, coord=coord, sp=sp, S=S, ## IBTS
                       general=list(main.path =file.path("C:","displace-project.org","repository","ibm_vessels_param"),
                       threshold=threshold,  p=p, method=method, years=nmy, survey="ibts")),  envir =.GlobalEnv 
      )}
      # combined surveys => choose the max value  
      if((flag1+flag2)==2){
        assign(paste("coord.",sp,".",S,sep=''),
          pmax( get(paste("bits.coord.",sp,".",S,sep=''))  ,  get(paste("ibts.coord.",sp,".",S,sep=''))
      ),  envir =.GlobalEnv)} else{
        if(flag1==1){ assign(paste("coord.",sp,".",S,sep=''),get(paste("bits.coord.",sp,".",S,sep='')),  envir =.GlobalEnv  )          
        }else{
         if(flag2==1){ assign(paste("coord.",sp,".",S,sep=''), get(paste("ibts.coord.",sp,".",S,sep='')),  envir =.GlobalEnv  )          
       }}}
   } # end S
 } # end sp 

# check with plot
obj <- coord.COD.1
szgroup <-"nb_indiv.2"
plot(x= obj[,1], y=obj[,2],  col=1, pch=16, 
                 cex=obj[,paste("COD",".",szgroup,sep='')] /max(obj[,paste("COD",".",szgroup,sep='')],na.rm=TRUE)*4)
# => then, a spatial allocation key (i.e. the availability coeff) can be easily obtained from there... 
# CAUTION: per stock i.e. per region i.e. nscod, wcod, ecod...

# check with plot
obj <- coord.HER.1
szgroup <-"nb_indiv.2"
plot(x= obj[,1], y=obj[,2],  col=1, pch=16, 
                 cex=obj[,paste("HER",".",szgroup,sep='')] /max(obj[,paste("HER",".",szgroup,sep='')],na.rm=TRUE)*4)
# => then, a spatial allocation key (i.e. the availability coeff) can be easily obtained from there... 
# CAUTION: per stock i.e. per region i.e. nscod, wcod, ecod...

# check with plot
obj <- coord.SPR.1
szgroup <-"nb_indiv.2"
plot(x= obj[,1], y=obj[,2],  col=1, pch=16, 
                 cex=obj[,paste("SPR",".",szgroup,sep='')] /max(obj[,paste("SPR",".",szgroup,sep='')],na.rm=TRUE)*4)
# => then, a spatial allocation key (i.e. the availability coeff) can be easily obtained from there... 
# CAUTION: per stock i.e. per region i.e. nscod, wcod, ecod...

# check with plot
obj <- coord.SOL.1
szgroup <-"nb_indiv.2"
plot(x= obj[,1], y=obj[,2],  col=1, pch=16, 
                 cex=obj[,paste("SOL",".",szgroup,sep='')] /max(obj[,paste("SOL",".",szgroup,sep='')],na.rm=TRUE)*4)
library(maps)
map(add=TRUE)
# => then, a spatial allocation key (i.e. the availability coeff) can be easily obtained from there... 
# CAUTION: per stock i.e. per region i.e. nscod, wcod, ecod...


## potential pitfall: the spatial coverage of the surveys is not perfect (even if 5 years is taken)
## and then potential nodes with 0 avai in some place (blank areas) while not relevant... 

# calls
lst.avai <- list() # init
for (sp in spp){ # per species
  if(sp %in% c("SPR", "PLE", "FLE", "TUR", "DAB")){
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas= c("IVa", "IVb", "IVc") ) # NS
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas= c("IVa", "IVb", "IVc") ) # NS
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas= c("IIIan","IIIas") ) # IIIa
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas= c("IIIan","IIIas") ) # IIIa)
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas= c("22", "23", "24","25", "26", "27","28-1","28-2","29","30","31","32") ) # 2232
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas= c("22", "23", "24","25", "26", "27","28-1","28-2","29","30","31","32") ) # 2232 
      } else{
      if(sp %in% c("COD", "HAD")){
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas= c("IVa", "IVb", "IVc", "IIIan") ) # NS
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas= c("IVa", "IVb", "IVc", "IIIan") ) # NS
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas= c("IIIas") ) # NS
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas= c("IIIas") ) # NS
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas=  c("22", "23", "24") ) # BW
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas=  c("22", "23", "24") ) # BW
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas= c("25", "26", "27","28-1","28-2","29","30","31","32") ) # BE
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas= c("25", "26", "27","28-1","28-2","29","30","31","32") ) # BE
      } else{
       if(sp %in% c("HER")){
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas= c("IVa", "IVb", "IVc") ) # NS
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas= c("IVa", "IVb", "IVc") ) # NS
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas=  c("IIIan","IIIas","22", "23", "24") ) # BW
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas=  c("IIIan","IIIas","22", "23", "24") ) # BW
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas= c("25", "26", "27","28-1","28-2","29","30","31","32") ) # BE
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas= c("25", "26", "27","28-1","28-2","29","30","31","32") ) # BE
      }  else{
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas= c("IVa", "IVb", "IVc") ) # NS
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas= c("IVa", "IVb", "IVc") ) # NS
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas= c("IIIan","IIIas") ) # IIIa
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas= c("IIIan","IIIas") ) # IIIa)
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas=  c("22", "23", "24") ) # BW
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas=  c("22", "23", "24") ) # BW
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="1", areas= c("25", "26", "27","28-1","28-2","29","30","31","32") ) # BE
      lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S="2", areas= c("25", "26", "27","28-1","28-2","29","30","31","32") ) # BE
      }
      }
      }
} #=> NOTE THAT AVAI KEY IS RESTRICTED TO THE PROVIDED AREAS GIVEN THE SURVEY COVERAGE
  # EVEN IF VESSELS USE TO GO FARTHER THAN THAT...
  # SO VESSELS GOING FAR WILL LIKELY FISH ON AREAS WITHOUT ANY FISH BECAUSE AVAI WILL BE AT 0...
  # THIS IS WHY EXPLICIT POP BECOME IMPLICIT POP IF OUT OF RANGE (SEE THE C++ IBM CODE)
  
  #=> NOTE THAT POSSIBLE BIAS FROM VARIOUS NODE GRID RESOLUTION 
  # WHILE SOME STOCKS OVERALPPING SEVERAL REGIONS e.g. COD.NSEA including skagerrat 
  # SO THE RULE OF THUMB IS TO DEFINE THE SAME GRID RESOLUTION FOR THE AREA OF A GIVEN STUDIED STOCK....
  

 #=>  check: lapply(lst.avai,function(x) lapply(x, head))  and apply(lst.avai[[1]][[1]],2,sum,na.rm=TRUE) =>1,1,1,etc.

# check with plot
obj <- lst.avai$COD.nsea$"1"
szgroup <-"nb_indiv.4"
plot(x= obj[,1], y=obj[,2],  col=1, pch=16, 
                 cex=obj[,paste("COD",".",szgroup,sep='')] /max(obj[,paste("COD",".",szgroup,sep='')],na.rm=TRUE)*4)

# check with plot
obj <- lst.avai$COD.2224$"1"
szgroup <-"nb_indiv.4"
plot(x= obj[,1], y=obj[,2],  col=1, pch=16, 
                 cex=obj[,paste("COD",".",szgroup,sep='')] /max(obj[,paste("COD",".",szgroup,sep='')],na.rm=TRUE)*4)

# check with plot
obj <- lst.avai$COD.2532$"1"
szgroup <-"nb_indiv.4"
plot(x= obj[,1], y=obj[,2],  col=1, pch=16, 
                 cex=obj[,paste("COD",".",szgroup,sep='')] /max(obj[,paste("COD",".",szgroup,sep='')],na.rm=TRUE)*4)

# check with plot
obj <- lst.avai$SOL.2224$"1"
szgroup <-"nb_indiv.1"
plot(x= obj[,1], y=obj[,2],  col=1, pch=16, 
                 cex=obj[,paste("SOL",".",szgroup,sep='')] /max(obj[,paste("SOL",".",szgroup,sep='')],na.rm=TRUE)*4)

# check with plot
obj <- lst.avai$HKE.nsea$"1"
szgroup <-"nb_indiv.1"
plot(x= obj[,1], y=obj[,2],  col=1, pch=16, 
                 cex=obj[,paste("HKE",".",szgroup,sep='')] /max(obj[,paste("HKE",".",szgroup,sep='')],na.rm=TRUE)*4)


# add some additional species that are not informed from survey...
# then assuming evenly distributed availability...
for (sp in c("MZZ")){
for (S in as.character(1:2)){
  avai          <- 1/nrow(coord) # even
  mat           <- matrix(avai, ncol=14, nrow=nrow(coord)) # 10 
  colnames(mat) <- paste(sp,".","nb_indiv.",1:14,sep='')
  fake.avai     <- cbind(coord, mat)
  assign(paste("coord.",sp,".",S,sep=''), fake.avai)
  lst.avai <-  set.avai(lst.avai=lst.avai, sp=sp, S=S, areas= c("IVa", "IVb", "IVc") ) # NS
  # check apply(lst.avai[[1]][[1]],2,sum,na.rm=TRUE) =>1,1,1,etc.
}}


# save to R
save("lst.avai", file = file.path("C:","displace-project.org","repository","ibm_vessels_param", "avai",
                            paste("lst_avai_igraph",igraph,"_", nmy,"_",method,"_", threshold, ".RData",sep="")) )
## CAUTION the ibts and bits surveys are combinations of hauls from different scientifc vessels
## with potentially gear trawl with different selectivity ogives....

} # end for







if(FALSE){
# save2 to txt
for (i in 1: length(lst.avai)){ 
  nm <- names(lst.avai)
  for (j in 1: length(lst.avai[[i]])){ 
    avai <- lst.avai[[i]][[j]]
    nm2 <- names(lst.avai[[i]])
    options(scipen=999) # suppress the scientific notation 
    avai <- signif(avai,6)
    avai <- replace(avai, is.na(avai),0)
    write.table(avai, file = file.path("C:","ibm_vessels_param","avai",
                            paste(nm[i],".",nm2[j],"_",nmy,"_format1.dat",sep="")), 
                               row.names = FALSE, col.names = FALSE )
    avai2 <- cbind(rep(as.numeric(as.character(avai[,4])), each=14), c(t(avai[,-c(1:4)])))   # CAUTION 14 szgroup
    write.table(avai2, file = file.path("C:","ibm_vessels_param","avai",
                            paste(nm[i],".",nm2[j],"_",nmy,"_format2.dat",sep="")), 
                               row.names = FALSE, col.names = FALSE )
   # colnames(avai)
  # [1] "x"               "y"               "idx.port"        ""                "MZZ.nb_indiv.1"  "MZZ.nb_indiv.2"  "MZZ.nb_indiv.3"  "MZZ.nb_indiv.4" 
 #[9] "MZZ.nb_indiv.5"  "MZZ.nb_indiv.6"  "MZZ.nb_indiv.7"  "MZZ.nb_indiv.8"  "MZZ.nb_indiv.9"  "MZZ.nb_indiv.10" "MZZ.nb_indiv.11" "MZZ.nb_indiv.12"
#[17] "MZZ.nb_indiv.13" "MZZ.nb_indiv.14"

  }}                            

} # end FALSE

 
### TO DO: NEED PELAGIC SURVEYS (ACOUSTIC?) TO COVER THE SPRAT AND HERRING
### THEN JUST INFORM THIS PIECE OF CODE WITH DATA HAVING THE SAME FORMAT LIKE THE IBTS ONE...
 
 
} # end FALSE      