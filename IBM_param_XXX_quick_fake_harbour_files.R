

### build quick and dirty fake harbour files

dd   <- read.table("C:\\Users\\fbas\\Documents\\GitHub\\DISPLACE_input\\graphsspe\\coord0.dat")
harb <- read.table("C:\\Users\\fbas\\Documents\\GitHub\\DISPLACE_input\\harboursspe_adriatic\\harbours.dat", sep=";")

dep <-  ((nrow(dd)/3)+1) -nrow(harb)
idx <-  dep : (nrow(dd)/3)   # all idx nodes for ports

  # price per kilo is 2 euros per cat here assuming 14 stocks and 6 com cat.
for (i in idx){
 write.table(cbind(rep(0:13, each=6), 2.00), file= file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input","harboursspe_adriatic", paste(i,"_quarter1_each_species_per_cat.dat",sep="")), row.names=FALSE)
 write.table(cbind(rep(0:13, each=6), 2.00), file= file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input","harboursspe_adriatic",  paste(i,"_quarter2_each_species_per_cat.dat",sep="")), row.names=FALSE)
 write.table(cbind(rep(0:13, each=6), 2.00), file= file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input","harboursspe_adriatic",  paste(i,"_quarter3_each_species_per_cat.dat",sep="")), row.names=FALSE)
 write.table(cbind(rep(0:13, each=6), 2.00), file= file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input","harboursspe_adriatic",  paste(i,"_quarter4_each_species_per_cat.dat",sep="")), row.names=FALSE)
}

 write.table(cbind(idx,  sapply(rownames(harb), function (x) paste(unlist(strsplit(x, " ")), collapse="_"))    ),
    file= file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input","harboursspe_adriatic",  "names_harbours.dat"), row.names=FALSE, col.names=FALSE, quote=FALSE)

