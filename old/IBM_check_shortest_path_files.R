
dd <- list.files("C:\\Users\\fbas\\Documents\\GitHub\\DISPLACE_input\\shortPaths_balticonly_a_graph11")
pts2 <- as.numeric(gsub(".dat", "", unlist(lapply(strsplit(dd, "_") , function(x) x[2])) ))

dd <- list.files("C:\\Users\\fbas\\Documents\\GitHub\\DISPLACE_input\\shortPaths_balticonly_a_graph11.old")
pts3 <- as.numeric(gsub(".dat", "", unlist(lapply(strsplit(dd, "_") , function(x) x[2])) ))

pts2[!pts2 %in% pts3]
pts3[!pts3 %in% pts2]

mat2 <- read.table("C:\\Users\\fbas\\Documents\\GitHub\\DISPLACE_input\\vesselsspe_balticonly\\vesselsspe_harbours_quarter1.dat", header=TRUE)
pts <- unique(mat2$pt_graph )
pts[! pts %in% pts2]

mat2 <- read.table("C:\\Users\\fbas\\Documents\\GitHub\\DISPLACE_input\\vesselsspe_balticonly\\vesselsspe_harbours_quarter2.dat", header=TRUE)
pts <- unique(mat2$pt_graph )
pts[! pts %in% pts2]

mat2 <- read.table("C:\\Users\\fbas\\Documents\\GitHub\\DISPLACE_input\\vesselsspe_balticonly\\vesselsspe_harbours_quarter3.dat", header=TRUE)
pts <- unique(mat2$pt_graph )
pts[! pts %in% pts2]

mat2 <- read.table("C:\\Users\\fbas\\Documents\\GitHub\\DISPLACE_input\\vesselsspe_balticonly\\vesselsspe_harbours_quarter4.dat", header=TRUE)
pts <- unique(mat2$pt_graph )
pts[! pts %in% pts2]

mat2 <- read.table("C:\\Users\\fbas\\Documents\\GitHub\\DISPLACE_input\\vesselsspe_balticonly\\vesselsspe_fgrounds_quarter1.dat", header=TRUE)
pts <- unique(mat2$pt_graph )
pts[! pts %in% pts2]

mat2 <- read.table("C:\\Users\\fbas\\Documents\\GitHub\\DISPLACE_input\\vesselsspe_balticonly\\vesselsspe_fgrounds_quarter2.dat", header=TRUE)
pts <- unique(mat2$pt_graph )
pts[! pts %in% pts2]

mat2 <- read.table("C:\\Users\\fbas\\Documents\\GitHub\\DISPLACE_input\\vesselsspe_balticonly\\vesselsspe_fgrounds_quarter3.dat", header=TRUE)
pts <- unique(mat2$pt_graph )
pts[! pts %in% pts2]

mat2 <- read.table("C:\\Users\\fbas\\Documents\\GitHub\\DISPLACE_input\\vesselsspe_balticonly\\vesselsspe_fgrounds_quarter4.dat", header=TRUE)
pts <- unique(mat2$pt_graph )
pts[! pts %in% pts2]

