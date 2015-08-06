
## find the tstep corresponding to a change of year-quarter
## for a several years ahead

general <- list()
general$main.path.param   <- file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_raw")

start_y <- 2009
end_y   <- 2015
start_y <- 2012
end_y   <- 2017
t.seq <- seq(as.POSIXct(paste(start_y,"-01-01 00:00:00",sep='')),
        as.POSIXct(paste(end_y,"-12-31 00:00:00",sep='')), by="hours")


#year-days
idx <- NULL
idx <- which( t.seq %in% as.POSIXct(paste(rep(start_y:end_y, each=365),"-",
                                 c(rep('01',each=31), rep('02',each=28), rep('03',each=31), rep('04',each=30), 
                                  rep('05',each=31), rep('06',each=30), rep('07',each=31), rep('08',each=31), rep('09',each=30), rep('10',each=31), rep('11',each=30), rep('12',each=31)),
                                 "-", sprintf("%02d", c(1:31, 1:28,1:31, 1:30, 1:31, 1:30, 1:31, 1:31, 1:30, 1:31, 1:30, 1:31)),
                                 " 00:00:00",sep='')))
idx <- c(idx,   -1) # sentinel

write.table(idx[-1], file= file.path(general$main.path.param, "simusspe", paste("tstep_days_",start_y,"_",end_y,".dat", sep="")),
                      col.names=FALSE, row.names=FALSE)


#year-months
idx <- NULL
idx <- which( t.seq %in% as.POSIXct(paste(rep(start_y:end_y, each=12),"-",
                                c('01','02','03','04','05','06','07','08','09','10','11','12'),
                                 "-01 00:00:00",sep='')))
                                        
idx <- c(idx,   -1) # sentinel

write.table(idx[-1], file= file.path(general$main.path.param, "simusspe", paste("tstep_months_",start_y,"_",end_y,".dat", sep="")),
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

write.table(idx[-1], file= file.path(general$main.path.param, "simusspe", paste("tstep_quarters_",start_y,"_",end_y,".dat", sep="")),
                      col.names=FALSE, row.names=FALSE)

#semesters
idx <- NULL
for(y in c(start_y:end_y)){
   idx <- c(idx, which( t.seq==as.POSIXct(paste(y,"-01-01 00:00:00",sep=''))),
                   which( t.seq==as.POSIXct(paste(y,"-07-01 00:00:00",sep='')))
                    )
}
idx <- c(idx,   -1) # sentinel


write.table(idx[-1], file=file.path(general$main.path.param, "simusspe", paste("tstep_semesters_",start_y,"_",end_y,".dat", sep="")),
                      col.names=FALSE, row.names=FALSE)

#years
idx <- NULL
for(y in c(start_y:end_y)){
   idx <- c(idx, which( t.seq==as.POSIXct(paste(y,"-01-01 00:00:00",sep='')))
                    )
}
idx <- c(idx,   -1) # sentinel

write.table(idx[-1], file=file.path(general$main.path.param, "simusspe", paste("tstep_years_",start_y,"_",end_y,".dat", sep="")),
                      col.names=FALSE, row.names=FALSE)
