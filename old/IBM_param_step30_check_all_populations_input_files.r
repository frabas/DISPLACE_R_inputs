  
  # GENERAL SETTINGS
  general <- list()
  general$main.path      <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_raw")
  general$main.path.code <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_R_inputs")

     



   ## check for NAs in .dat files in popsspe
     lst_files <- list.files (file.path(general$main.path, "popsspe"),recursive = TRUE)
     for (fi in lst_files){
        if(length(grep("spe_SSB_R_parameters",fi))==0){
        dd <- read.table(file=file.path(general$main.path,"popsspe", fi), header=FALSE)
        #print(head(dd,2))
        if( any(is.na(dd)) ) cat(paste("NAs found in ",fi,"......\n"))
        }
     }





     
      ####::::::::::TO DO!!
      # TO DO: N-at-sizgroup => // DEADLY BUG: MAKE SURE THAT NO BLANK IS LEFT IN THE VERY END OF THE .DAT FILE...
      # maybe use write.table with eol="\r\n" will produce Windows' line endings on a Unix-alike OS,


 

     ## TO DO: check presence of all pop in the avai files
    ## if not present then forced to use as implicit pop.....
