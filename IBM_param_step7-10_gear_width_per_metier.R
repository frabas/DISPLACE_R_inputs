 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!! metiersspe\metier_gear_widths_param_a.dat    !!!!!!##
 ##!!!!!!!!! metiersspe\metier_gear_widths_param_b.dat    !!!!!!##
 ##!!!!!!!!! metiersspe\metier_gear_widths_model_type.dat !!!!!!##   
 ##!!!!!!!!! metiersspe\metier_fspeed.dat                 !!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##


 ## METIER SPE
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
  # mkdir
  dir.create(path=file.path(general$main.path, "merged_tables", general$case_study),
                      showWarnings = TRUE, recursive = TRUE, mode = "0777")

  if(general$case_study=="canadian_paper"){
      year      <- "2010"
      years     <- "2005_2010"
      method    <- "maximum"
      threshold <- "50"
      }
  if(general$case_study=="baltic_only"){
      year      <- "2012"
      years     <- "2008_2012"
      method    <- "inverse"
      threshold <- "25"
      }

  if(general$case_study=="myfish"){
      year      <- "2012"
      years     <- "2008_2012"
      method    <- "inverse"
      threshold <- "25"
      }




   #-------------------------------
   #-------------------------------


   load(file.path(general$main.path,"merged_tables", general$case_study, paste("metier_names.", general$a.country,".", general$a.year,".igraph",general$igraph,".RData",sep='')))
   metier_names
   
   max_met <-  max(as.numeric(metier_names[,2]))

 
   #-------------------------------
   #-------------------------------

     # export to c++
     fspeed <- cbind.data.frame(0:max_met,
                                4) # assuming fishing at 4 knots for all
     
     write.table(fspeed, file=file.path(general$main.path, paste("metiersspe", sep=''),
                        paste("metier_fspeed.dat", sep='')), col.names=FALSE, row.names=FALSE, quote=FALSE)

    #-------------------------------
    #-------------------------------

  ## create metier specific gear widths files by hand
  ## from our BENTHIS analyses


   gear_param_per_metier <- data.frame(
a_metier=c('OT_CRU','OT_CRU','OT_DMF','OT_DMF','OT_MIX','OT_MIX','OT_MIX_ARA','OT_MIX_ARA','OT_MIX_DMF_BEN','OT_MIX_DMF_BEN','OT_MIX_DMF_PEL','OT_MIX_DMF_PEL','OT_MIX_DPS','OT_MIX_DPS','OT_MIX_NEP','OT_MIX_NEP','OT_MIX_TGS_CTC','OT_MIX_TGS_CTC','OT_MIX_TGS_OCC','OT_MIX_TGS_OCC','OT_SPF','OT_SPF','TBB_CRU','TBB_CRU','TBB_DMF','TBB_DMF','TBB_MOL','TBB_MOL','DRB_MOL','DRB_MOL','SDN_DEM','SDN_DEM','SSC_DEM','SSC_DEM'),
param=c('a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b','a','b'),
Estimate=c(5.10393560454806,0.468985756915913,9.6053549509854,0.433672763959314,10.6607888271164,0.292055014993337,37.5271604597435,0.149004797319136,3.21410379943408,77.981158829069,6.63707197355847,0.770594580782091,26.6738247840508,0.210221545999405,3.92727763464472,35.8253721834011,6.23686411376723,0.767375050454527,0.0192465419797634,119.140335982507,0.965238378524667,68.3889717127507,1.48117115311386,0.457788539321641,0.660086393453441,0.507845311175148,0.953001905566232,0.709356826689359,0.314245137194503,1.24544036138755,1948.83466676682,0.236271746198865,4461.27004311913,0.117589220782479),
Std..Error=c(1.81527145191998,0.0597519960969362,3.98228885098937,0.067572002767068,6.69386377505425,0.104413257104915,10.6717875588847,0.044963446750424,1.67854244656697,40.9297885227685,2.69086696344053,0.126123213329976,5.37466576335144,0.030829495804396,0.928442484509969,21.0228522096513,1.46159830273852,0.0732116002636393,0.000552819642352548,0.510207569180525,0.205245990518183,7.45180177818494,0.278399892100703,0.0346555048025894,0.172902115850281,0.0388684340513048,0.315715856194751,0.138412196798781,0.110027479611801,0.10614681568516,637.25152416296,0.0636712369543136,1665.50234108383,0.118756519107319),
t.value=c(2.81166521907769,7.84887179593252,2.41201864314765,6.41793562718951,1.59262112068153,2.79710664230959,3.51648308708138,3.31390958851994,1.91481830322951,1.90524216331315,2.46651806415295,6.10985527910701,4.96288066244663,6.81884476260001,4.22996329893018,1.70411568450042,4.26715336360309,10.4816046595234,34.8152281598731,233.513462322532,4.70283670871103,9.17750817164227,5.32030074414718,13.2096918492278,3.81768835047121,13.0657517744299,3.01854305657162,5.12495894939517,2.8560604887363,11.733186279291,3.05818753329251,3.71080816866175,2.67863330664306,0.990170658978435),
Pr...t..=c(0.00613312535554725,1.21619365805854e-11,0.021410083292817,2.48114253493853e-07,0.114790848188445,0.00631861326022122,0.000513087659147687,0.0010462790834138,0.0692370736030276,0.0705334706657513,0.0147045751318625,7.39218704723967e-09,1.2637878625965e-05,2.97113026239585e-08,0.000166717383514359,0.097483711710908,0.000314181622785133,5.0948672020349e-10,9.05842416252619e-12,5.10054218622276e-20,0.000204968683311441,5.36482029322678e-08,0.00313939649832079,4.44157761915604e-05,0.000458495488420268,5.11509704563588e-16,0.00678642704689924,5.16047183433098e-05,0.0075895814688592,6.18091407283774e-13,0.00391206507124518,0.000614325243514857,0.0438919330122769,0.367557330382699),
equ=c('DoS=a*(kW^b)','DoS=a*(kW^b)','DoS=a*(kW^b)','DoS=a*(kW^b)','DoS=a*(kW^b)','DoS=a*(kW^b)','DoS=a*(kW^b)','DoS=a*(kW^b)','DoS=(a*LOA)+b','DoS=(a*LOA)+b','DoS=a*(LOA^b)','DoS=a*(LOA^b)','DoS=a*(kW^b)','DoS=a*(kW^b)','DoS=(a*LOA)+b','DoS=(a*LOA)+b','DoS=a*(LOA^b)','DoS=a*(LOA^b)','DoS=(a*kW)+b','DoS=(a*kW)+b','DoS=(a*LOA)+b','DoS=(a*LOA)+b','beamw=a*(kW^b)','beamw=a*(kW^b)','beamw=a*(kW^b)','beamw=a*(kW^b)','beamw=a*(LOA^b)','beamw=a*(LOA^b)','dredgew=a*(LOA^b)','dredgew=a*(LOA^b)','seineropel=a*(kW^b)','seineropel=a*(kW^b)','seineropel=a*(LOA^b)','seineropel=a*(LOA^b)'),
nb_records=c(124,124,39,39,94,94,271,271,48,48,190,190,45,45,53,53,24,24,12,12,19,19,7,7,42,42,22,22,33,33,47,47,8,8)
)

# low bottom impact e.g. for gillnetters
gear_param_per_metier$a_metier <- as.character(gear_param_per_metier$a_metier) 
gear_param_per_metier          <- rbind.data.frame(gear_param_per_metier,
                                          c(a_metier="OTH", param="a", Estimate=0.001, "Std..Error"=NA,     "t.value"=NA,     "Pr...t.."=NA,                  equ="DoS=a*(kW^b)", nb_records=NA),
                                          c(a_metier="OTH", param="b", Estimate=0.5, "Std..Error"=NA,     "t.value"=NA,     "Pr...t.."=NA,                  equ="DoS=a*(kW^b)", nb_records=NA)
                                          ) 





if(general$case_study=="myfish"){

lookup_metier_names1 <- c(
 "DRB_MOL", 
 "FPO_CAT", 
 "FPO_DEF",
 "GNS_CAT",
 "GNS_CRU",
 "GNS_DEF",
 "GNS_SPF",
 "LHP_FIF",
 "LLS_FIF",
 "OTB_CRU",
 "OTB_DEF",
 "OTB_MCD",
 "OTB_SPF",
 "OTM_DEF",
 "OTM_SPF",
 "PS_SPF" ,
 "PTB_DEF",
 "PTB_MCD",
 "PTB_SPF",
 "PTM_DEF",
 "PTM_SPF",
 "SDN_DEF",
 "SSC_DEF",
 "TBB_CRU",
 "TBB_DEF"
 )

lookup_metier_names2 <- c(
 "DRB_MOL", 
 "OTH", 
 "OTH",
 "OTH",
 "OTH",
 "OTH",
 "OTH",
 "OTH",
 "OTH",
 "OT_CRU",
 "OT_DMF",
 "OT_MIX_NEP",
 "OT_SPF",
 "OT_DMF",
 "OT_SPF",
 "OTH" ,
 "OT_DMF",
 "OT_MIX_NEP",
 "OT_SPF",
 "OT_DMF",
 "OT_SPF",
 "SDN_DEM",
 "SSC_DEM",
 "TBB_CRU",
 "TBB_DMF"
 )

As   <- gear_param_per_metier [gear_param_per_metier$param=="a",]
Bs   <- gear_param_per_metier [gear_param_per_metier$param=="b",]
equs <- gear_param_per_metier [gear_param_per_metier$param=="a",]

res <- cbind.data.frame(lookup_metier_names1,
                 a=As[match(lookup_metier_names2, As$a_metier),"Estimate"],
                 b=Bs[match(lookup_metier_names2, Bs$a_metier),"Estimate"],
                 equ= equs[match(lookup_metier_names2, equs$a_metier),"equ"])


}



# export to c++     
     write.table(cbind(0:max_met, as.numeric(as.character(res$a))), file=file.path(general$main.path, paste("metiersspe", sep=''),
                        paste("metier_gear_widths_param_a.dat", sep='')), col.names=FALSE, row.names=FALSE, quote=FALSE)
     write.table(cbind(0:max_met, as.numeric(as.character(res$b))), file=file.path(general$main.path, paste("metiersspe", sep=''),
                        paste("metier_gear_widths_param_b.dat", sep='')), col.names=FALSE, row.names=FALSE, quote=FALSE)
     write.table(cbind(0:max_met, as.character(res$equ)), file=file.path(general$main.path, paste("metiersspe", sep=''),
                        paste("metier_gear_widths_model_type.dat", sep='')), col.names=FALSE, row.names=FALSE, quote=FALSE)

  




