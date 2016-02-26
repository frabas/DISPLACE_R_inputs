  
# GENERAL SETTINGS
  general <- list()
  general$main.path      <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_raw")
  general$main.path.code <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_R_inputs")

     




     ## check for NAs in .dat files in vesselspe
     lst_files <- list.files (file.path(general$main.path, "vesselsspe"))
     for (fi in lst_files){
        if(length(grep("features",fi))!=0) a.sep="|" else a.sep=" "
        dd <- read.table(file=file.path(general$main.path,"vesselsspe", fi), header=FALSE, sep=a.sep)
        #print(head(dd,2))
        if( any(is.na(dd)) ) cat(paste("NAs found in ",fi,"......\n"))

     }




     
     # if deadly bug when cpue files are loaded then check:
     lastly, check the number of species in  vesselsspe_percent_tacs_per_pop_semester1
     and     [vid]_cpue_per_stk_on_nodes_quarter[]   and DNK000013712_gshape_cpue_per_stk_on_nodes_quarter1
     because pop_name definition is circular....that can lead to mix up the pop!
     #=> we should have multimap with the right number of pop !!!
     # if not then reparameterize step for cpue files....
     




     ## TO DO: check for absence of ports for given vessel given quarter while some fgrounds are found....why?
       ## (see magic numbers in c++ to fix it)

    
