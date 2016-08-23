 # GENERAL SETTINGS
  general <- list()
  general$main.path      <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_raw")
  general$main.path.code <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_R_inputs")








     
     ## check for NAs in .dat files in metiersspe
     lst_files <- list.files (file.path(general$main.path, "metiersspe"), include.dirs = FALSE)
     for (fi in lst_files){
        dd <- read.table(file=file.path(general$main.path,"metiersspe", fi), header=FALSE)
        #print(head(dd,2))
        if( any(is.na(dd)) ) cat(paste("NAs found in ",fi,"......\n"))

     }



     
     # check the file given the metier names
     dd <- read.table(file=file.path(general$main.path,"metiersspe",
       "metier_selectivity_ogives.dat"), header=TRUE, sep=" ")
     forgotten_metiers <- (0:max(unique(dd[,1])) )[!  0:max(unique(dd[,1])) %in% unique(dd[,1]) ]
     if(length( forgotten_metiers )!=0) { cat(paste("some forgotten metiers found!!......\n")) }
     #=> likely to lead to the wrong size of the c++ 'metiers' object, then lead to deadly c++ bugs



     ## TO DO : change in c++: use the met_names_txt file instead of gussing nbmetier from selectivity ogive...
     
