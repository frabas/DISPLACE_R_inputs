# some args for the bunch of vessels to be created....
# Usage:
# RunVesselsConfigFiles.R application gis_path input_application_path igraph

args <- commandArgs(trailingOnly = TRUE)

general <- list()


if (length(args) < 2) {
  if(.Platform$OS.type == "windows") {
    general$application           <- "testexample" # ...or myfish
    general$main_path_gis         <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis", general$application)
    general$main.path.ibm         <- file.path("C:","Users","fbas","Documents","GitHub",paste("DISPLACE_input_", general$application, sep=''))
    general$igraph                <- 56
    do_plot <- TRUE
    }
} else {            
  general$application           <- args[1]
  general$main.path.param.gis   <- args[2]
  general$main.path.ibm         <- args[3]
  general$igraph                <- args[4]
  do_plot <- FALSE
 }

dir.create(file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep='')))
dir.create(file.path(general$main.path.ibm, paste("metiersspe_", general$application, sep='')))


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-----read input config file----------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

  if(length(grep("fisheries_economics_variables", list.files(file.path(general$main_path_gis, "FISHERIES"))))!=0)
   {
   # if a parameter file exists, use it:
     filename <- file.path(general$main_path_gis, "FISHERIES", "fisheries_economics_variables.csv")
     cnts     <- count.fields(filename, sep = ";") 
     vessel_specifications <- read.table(file=filename, sep=";", header=TRUE )
     vessel_specifications <- cbind.data.frame(vessel_specifications, id=1:nrow(vessel_specifications))
     #=> CAUTION: do not leave white rows at the end of this file! otherwise will create some extra wrong config_files
     cat(paste("Read vessels_specifications_per_harbour_metiers.csv \n"))
   } else{
   # if no parameter file then create it:
   
   
     #nameobj           <- paste("vessels_specifications_per_harbour_metiers.csv",sep='')  #....and possibly per vid!
     #tacsatp_this_ctry <- read.table(file.path(general$main_path_gis, "FISHERIES", nameobj), header=TRUE, sep=";")
     # caution, not all vids are here.....so use instead:
     nameobj           <- paste("vesselsspe_features_quarter1.dat",sep='')  #....and possibly per vid!
     tacsatp_this_ctry <- read.table(file.path(general$main.path.ibm, paste("vesselsspe_",general$application,sep=""), nameobj), header=FALSE, sep="|")

     # create and populate with default values
     vessel_specifications <- cbind.data.frame(VE_REF=unique(tacsatp_this_ctry$VE_REF),
                                                  "Nb_crew"=2,  # for this boat                                 
                                                  "Annual_other_income"=4790, # for this boat, e.g. in euro
                                                  "Landing_costs_percent"=5.0, # for this boat, taxes in percent                    
                                                  "Crewshare_and_unpaid_labour_costs_percent"=27.9,   # for this boat, percent share
                                                  "Other_variable_costs_per_unit_effort"=587,  # for this boat, euro per hour    
                                                  "Annual_insurance_costs_per_crew"=1500,   # for this boat, per crew member
                                                  "Standard_labour_hour_opportunity_costs"=26,   # for this boat e.g. in euro per hour
                                                  "Standard_annual_full_time_employment_hours"=2000,    # FTE, for this boat
                                                  "Other_annual_fixed_costs"=14000,   # for this boat, e.g. in euro 
                                                  "Vessel_value"=300000,  # for this boat, e.g. in euro
                                                  "Annual_depreciation_rate"=4,  # for this boat, in percent
                                                  "Opportunity_interest_rate"=4,   # for this boat, in percent
                                                  "Annual_discount_rate"=4,     # for this boat, in percent
                                               
                                                  # some extra variables (PUT AFTER!)
                                                  "vsize" = tacsatp_this_ctry[, 5]
                                 
                                               )  
   
    # just an illustration for ad hoc corrections
   vessel_specifications[vessel_specifications$vsize < 15, "Other_variable_costs_per_unit_effort"] <- 
                            vessel_specifications[vessel_specifications$vsize < 15, "Other_variable_costs_per_unit_effort"]  *0.1
   vessel_specifications[vessel_specifications$vsize < 15, "Other_annual_fixed_costs"] <- 
                            vessel_specifications[vessel_specifications$vsize < 15, "Other_annual_fixed_costs"]  *0.2
   vessel_specifications[vessel_specifications$vsize < 15, "Vessel_value"] <- 
                            vessel_specifications[vessel_specifications$vsize < 15, "Vessel_value"]  *0.5
  
   }
    


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-----export file as a DISPLACE input file--------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


     # save .dat files
       write.table(vessel_specifications,
           file=file.path(general$main.path.ibm, paste("vesselsspe_", general$application, sep=''),
             paste("vesselsspe_economic_features.dat",sep='')),
               col.names=FALSE,  row.names=FALSE, quote=FALSE, append=FALSE, sep = "|")
  
       cat(paste("vesselsspe_economic_features.dat....OK", "\n"))





