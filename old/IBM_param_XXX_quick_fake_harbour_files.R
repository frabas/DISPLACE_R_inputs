

### build quick and dirty fake harbour files

dd   <- read.table("C:\\Users\\fbas\\Documents\\GitHub\\DISPLACE_input\\graphsspe\\coord1.dat")
harb <- read.table("C:\\Users\\fbas\\Documents\\GitHub\\DISPLACE_input\\harboursspe_adriatic\\harbours.dat", sep=";")

dep <-  ((nrow(dd)/3)+1) -nrow(harb)
idx <-  dep : (nrow(dd)/3)   # all idx nodes for ports

idx <- idx - 1 ##!!## CAUTION OFFSET BY 1 in C++ ##!!##

 # obtain a c++ multimap with stock / price for cat
for (i in idx){
 prices_per_species_per_cat <- c(6.15, 8.94, 10.62,   # hake
                                 7.54, 16.52, 20.00,  # sole
                                 2.81, 3.79, 5.44,    # mullet
                                 4.26, 7.24, 10.70    # Mantis
                                 ) # stock name 0:Hake, 1:Sole, 2: Mullet, 3: Mantis  # ANCONA prices  euro per kilo.
              # => ......default
 
 if(i==20674) prices_per_species_per_cat <- c(6.19, 8.12, 11.12,   # hake
                                 6.25, 10.54, 16.14,  # sole
                                 2.81, 5.98, 5.44,    # mullet
                                 5.28, 7.24, 11.86    # Mantis
                                 ) # stock name 0:Hake, 1:Sole, 2: Mullet, 3: Mantis  # CESENATICO prices  euro per kilo.

 if(i==20709) prices_per_species_per_cat <- c(6.15, 8.94, 7.81,   # hake
                                 4.86, 7.88, 13.19,  # sole
                                 2.84, 3.71, 5.75,    # mullet
                                 3.56, 5.93, 10.54    # Mantis
                                 ) # stock name 0:Hake, 1:Sole, 2: Mullet, 3: Mantis  # SAN BENEDETTO DEL TRONTO prices  euro per kilo.
 
 if(i==20676) prices_per_species_per_cat <- c(6.15, 8.94, 10.62,   # hake
                                 7.56, 12.89, 18.82,  # sole
                                 3.00, 3.09, 5.08,    # mullet
                                 4.97, 7.24, 10.38    # Mantis
                                 ) # stock name 0:Hake, 1:Sole, 2: Mullet, 3: Mantis  # CIVITANOVA MARCHE prices  euro per kilo.
 
 if(i==20680) prices_per_species_per_cat <- c(6.15, 7.56, 10.62,   # hake
                                 7.54, 10.23, 20.00,  # sole
                                 2.81, 6.21, 5.44,    # mullet
                                 4.26, 7.85, 10.70    # Mantis
                                 ) # stock name 0:Hake, 1:Sole, 2: Mullet, 3: Mantis  # GORO prices  euro per kilo.
 
 if(i==20675) prices_per_species_per_cat <- c(6.15, 4.97, 6.08,   # hake
                                 4.62, 9.40, 12.35,  # sole
                                 3.34, 5.13, 6.21,    # mullet
                                 4.26, 8.35, 8.68    # Mantis
                                 ) # stock name 0:Hake, 1:Sole, 2: Mullet, 3: Mantis  # CHIOGGIA prices  euro per kilo.
 
 
 write.table(cbind(stock= rep(0:3, each=3), price_per_cat= prices_per_species_per_cat), 
                file= file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input","harboursspe_adriatic", paste(i,"_quarter1_each_species_per_cat.dat",sep="")), row.names=FALSE, quote=FALSE)
 write.table(cbind(stock= rep(0:3, each=3), price_per_cat= prices_per_species_per_cat), 
                file= file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input","harboursspe_adriatic", paste(i,"_quarter2_each_species_per_cat.dat",sep="")), row.names=FALSE, quote=FALSE)
 write.table(cbind(stock= rep(0:3, each=3), price_per_cat= prices_per_species_per_cat), 
                file= file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input","harboursspe_adriatic", paste(i,"_quarter3_each_species_per_cat.dat",sep="")), row.names=FALSE, quote=FALSE)
 write.table(cbind(stock= rep(0:3, each=3), price_per_cat= prices_per_species_per_cat), 
                file= file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input","harboursspe_adriatic", paste(i,"_quarter4_each_species_per_cat.dat",sep="")), row.names=FALSE, quote=FALSE)
}

 write.table(cbind(idx,  sapply(rownames(harb), function (x) paste(unlist(strsplit(x, " ")), collapse="_"))    ),
    file= file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input","harboursspe_adriatic",  "names_harbours.dat"), row.names=FALSE, col.names=FALSE, quote=FALSE)

