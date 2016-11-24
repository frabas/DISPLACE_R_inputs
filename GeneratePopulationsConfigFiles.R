 # GENERAL SETTINGS

   args <- commandArgs(trailingOnly = TRUE)

   general <- list()

   if (length(args) < 2) {
     if(.Platform$OS.type == "windows") {
       general$application           <- "testexample" # ...or myfish
       general$main_path_gis         <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis", general$application)
       general$main.path.ibm         <- file.path("C:","Users","fbas","Documents","GitHub",
                                                     paste("DISPLACE_input_", general$application, sep=''))
       general$igraph                <- 56  # caution: should be consistent with existing objects already built upon a given graph
   
     }
  } else {
       general$application           <- args[1]
       general$main_path_gis         <- args[2]
       general$main.path.ibm         <- args[3]
       general$igraph                <- args[4]  # caution: should be consistent with existing vessels already built upon a given graph
  }
  
  cat(paste("START \n"))


 dir.create(path=file.path(general$main_path_gis, "POPULATIONS", "pops_config_files"))
 cat(paste("Creating a /POPULATIONS/pops_config_files folder....done \n"))


 path      <- file.path(general$main_path_gis, "POPULATIONS", "SpatialLayers")
 namefiles <- list.files(file.path( path))
 cat(paste("Entering /POPULATIONS/SpatialLayers folder....done \n"))


 # parse
 dd <- data.frame()
 for (a_file in namefiles){
    infos <- unlist(strsplit(a_file, split="_"))
    popid <- as.numeric(gsub("contour", "", infos[1])) 
    size  <- infos[2]
    if(length(grep(".dbf", infos[3]))!=0) sizegroups <- as.character(gsub(".dbf", "", infos[3])) 
    if(length(grep(".shx", infos[3]))!=0) sizegroups <- as.character(gsub(".shx", "", infos[3])) 
    if(length(grep(".shp", infos[3]))!=0) sizegroups <- as.character(gsub(".shp", "", infos[3])) 
    dd <- rbind.data.frame(dd, cbind(popid, size, sizegroups))
   }
   colnames(dd) <- c("popid", "size", "sizegroups")
   dd <- dd[!duplicated(data.frame(dd$popid, dd$size, dd$sizegroups)),]

 cat(paste("Parsing GIS layer file name....done \n"))
   
  
 for (popid in as.numeric(as.character(unique(dd$popid)))){
   
    cat(paste("popid", popid, "\n"))
   
    # create a config file
    szgroups                                      <-  gsub("-", " ", paste(dd[dd$popid==popid & dd$size=="small", "sizegroups"],
                                                      dd[dd$popid==popid & dd$size=="medium", "sizegroups"],
                                                      dd[dd$popid==popid & dd$size=="large", "sizegroups"], sep="_"))   # 14 size groups
    selected_szgroups                             <-  c(2,5,7,9)
    name_gis_file_for_total_abundance_per_polygon <- c(
                          paste("SpatialLayers/contour",popid,"_small_",  dd[dd$popid==popid & dd$size=="small", "sizegroups"], sep=''), 
                          paste("SpatialLayers/contour",popid,"_medium_", dd[dd$popid==popid & dd$size=="medium", "sizegroups"], sep=''), 
                          paste("SpatialLayers/contour",popid,"_large_",  dd[dd$popid==popid & dd$size=="large", "sizegroups"], sep='')
                                                       )           # in 3 size categories
    name_gis_layer_field                          <- "GRIDCODE"    # e.g. giving occurences in polygon
    is_gis_layer_field_relative_numbers           <- FALSE         # if relative categories (e.g. high to low) then xfold_gis_layer_field will be used to convert in absolute
    xfold_gis_layer_field                         <- c(1, 1, 1, 1, 1)  # [not used if is_gis_layer_field_relative_numbers is FALSE]
  
  
   do_append <- FALSE

   a_comment <- popid
   namefile  <- file.path(general$main_path_gis, "POPULATIONS", "pops_config_files", paste(a_comment, "pops_creator_args_",  general$application, ".dat", sep=''))
 
   write("# config file for the Object editor: adding some population(s)", file=namefile)
   write("# (the shortestPaths library will have to be re-created for the graph)", file=namefile, ncolumns=1, append=TRUE)
   write("# --------------", file=namefile, ncolumns=1, append=TRUE)

   write("# input folder for config file", file=namefile, ncolumns=1, append=TRUE)
   write(general$main_path_gis, file=namefile, ncolumns=1, append=TRUE)

   write("# output folder for parameterisation file", file=namefile, ncolumns=1, append=TRUE)
   write(general$main.path.ibm, file=namefile, ncolumns=1, append=TRUE)

   write("# input folder for DISPLACE", file=namefile, ncolumns=1, append=TRUE)
   write(general$main.path.ibm, file=namefile, ncolumns=1, append=TRUE)

   write("# name of the application", file=namefile, ncolumns=1, append=TRUE)
   write(general$application, file=namefile, ncolumns=1, append=TRUE)

   write("# name of the graph for this application", file=namefile, ncolumns=1, append=TRUE)
   write(general$igraph, file=namefile, ncolumns=1, append=TRUE)

   write("# append to existing pop files", file=namefile, ncolumns=1, append=TRUE)
   write(do_append, file=namefile, ncolumns=1, append=TRUE)

   write("# name gis file for total abundance per polygon", file=namefile, ncolumns=length(name_gis_file_for_total_abundance_per_polygon), append=TRUE)
   write(name_gis_file_for_total_abundance_per_polygon, file=namefile, ncolumns=length(name_gis_file_for_total_abundance_per_polygon), append=TRUE)

   write("# name_gis_layer_field",file=namefile, ncolumns=1, append=TRUE)
   write(name_gis_layer_field, file=namefile, ncolumns=1, append=TRUE)

   write("# is_gis_layer_field_relative_numbers",file=namefile, ncolumns=1, append=TRUE)
   write(is_gis_layer_field_relative_numbers, file=namefile, ncolumns=1, append=TRUE)

   write("# xfold_gis_layer_field",file=namefile, ncolumns=1, append=TRUE)
   write(xfold_gis_layer_field, file=namefile, ncolumns=length(xfold_gis_layer_field), append=TRUE)

   write("# popid", file=namefile, ncolumns=1, append=TRUE)
   write(popid, file=namefile, ncolumns=length(popid), append=TRUE)

   write("# all size groups", file=namefile, ncolumns=1, append=TRUE)
   write(szgroups, file=namefile, ncolumns=length(szgroups), append=TRUE)

   write("# selected size groups", file=namefile, ncolumns=1, append=TRUE)
   write(selected_szgroups, file=namefile, ncolumns=length(selected_szgroups), append=TRUE)

   }


cat(paste("....done \n"))


