
## find the tstep corresponding to a change of year-quarter
## for a several years ahead
start_y <- 2009
end_y   <- 2015
start_y <- 2012
end_y   <- 2017
t.seq <- seq(as.POSIXct(paste(start_y,"-01-01 00:00:00",sep='')),
        as.POSIXct(paste(end_y,"-12-31 00:00:00",sep='')), by="hours")

#year-months
idx <- NULL
idx <- which( t.seq %in% as.POSIXct(paste(rep(start_y:end_y, each=12),"-",
                                c('01','02','03','04','05','06','07','08','09','10','11','12'),
                                 "-01 00:00:00",sep='')))
                                        
idx <- c(idx,   -1) # sentinel

write.table(idx[-1], file= file.path("C:","Users","fba","Dropbox","ibm", "Cpp", "ibm_vessels", "simusspe", paste("tstep_months_",start_y,"_",end_y,".dat", sep="")),
                      col.names=FALSE, row.names=FALSE)


#year-quarters
idx <- NULL
for(y in c(start_y:end_y)){
   idx <- c(idx, which( t.seq==as.POSIXct(paste(y,"-01-01 00:00:00",sep=''))),
                  which( t.seq==as.POSIXct(paste(y,"-04-01 00:00:00",sep=''))),
                   which( t.seq==as.POSIXct(paste(y,"-07-01 00:00:00",sep=''))),
                    which( t.seq==as.POSIXct(paste(y,"-10-01 00:00:00",sep='')))
                    
                    )
}
idx <- c(idx,   -1) # sentinel

write.table(idx[-1], file= file.path("C:","Users","fba","Dropbox","ibm", "Cpp", "ibm_vessels", "simusspe", paste("tstep_quarters_",start_y,"_",end_y,".dat", sep="")),
                      col.names=FALSE, row.names=FALSE)

#semesters
idx <- NULL
for(y in c(start_y:end_y)){
   idx <- c(idx, which( t.seq==as.POSIXct(paste(y,"-01-01 00:00:00",sep=''))),
                   which( t.seq==as.POSIXct(paste(y,"-07-01 00:00:00",sep='')))
                    )
}
idx <- c(idx,   -1) # sentinel


write.table(idx[-1], file=file.path("C:","Users","fba","Dropbox","ibm", "Cpp", "ibm_vessels", "simusspe", paste("tstep_semesters_",start_y,"_",end_y,".dat", sep="")),
                      col.names=FALSE, row.names=FALSE)

#years
idx <- NULL
for(y in c(start_y:end_y)){
   idx <- c(idx, which( t.seq==as.POSIXct(paste(y,"-01-01 00:00:00",sep='')))
                    )
}
idx <- c(idx,   -1) # sentinel

write.table(idx[-1], file=file.path("C:","Users","fba","Dropbox","ibm", "Cpp", "ibm_vessels", "simusspe", paste("tstep_years_",start_y,"_",end_y,".dat", sep="")),
                      col.names=FALSE, row.names=FALSE)
