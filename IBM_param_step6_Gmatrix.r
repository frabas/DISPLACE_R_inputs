

FRANCOIS <- TRUE
TANJA    <- FALSE
if(TANJA)    main.path <- file.path("C:","Dropbox","Vessel","ibm_vessels_param")
if(FRANCOIS) main.path <- file.path("C:","Users", "fbas", "Documents", "GitHub", "DISPLACE_input_raw")


#case_study <- "canadian_paper"  # 0 to 37 pops
case_study  <- "baltic_only"     # 0 to 30
if(case_study=="canadian_paper") a_size_group_bin_in_cm <- 10
if(case_study=="baltic_only")    a_size_group_bin_in_cm <- 5


if(case_study=="canadian_paper"){
 # pop number per age group 2010
 number <- read.csv(file=file.path(main.path,
                 "IBM_datainput_abundance_2010.csv"),
                    sep=",",header=TRUE) 
                        
 # pop number per age group 2011 (take the following year after the start year, for validation)
 number_yplus1 <- read.csv(file=file.path(main.path,
                 "IBM_datainput_abundance_2011.csv"),
                    sep=",",header=TRUE)     
 a.year <- 2010
 }
 
 
 
if(case_study=="baltic_only"){
 # pop number per age group 2012 
 number <- read.csv(file=file.path(main.path,
                 "IBM_datainput_abundance_2012.csv"),
                    sep=",",header=TRUE)     
 
 # pop number per age group 2013 (take the following year after the start year, for validation)
 #number_yplus1 <- read.csv(file=file.path(main.path,
 #                "IBM_datainput_abundance_2013.csv"),
 #                   sep=",",header=TRUE)  ## NOT YET AVAILABLE  !!  
 number_yplus1 <-  number # data not available yet...then temporary!!
 a.year <- 2012
 }
 
 



# pop parameters
 pa <- read.csv(file=file.path(main.path,
                  paste("IBM_datainput_stockdata_",case_study,".csv", sep='')), 
                    sep=',', header=TRUE)

                                                            


################################
################################
################################
################################


#von bertalanfy growth
vbg <- function (Linf, K, to=0, timesteps) {
        Linf * (1 - exp(-K * (timesteps - to)))
}
a_pop_pa <- pa[pa$pop.to.keeps =="SPR.2232", ]
plot( 1:21, vbg (a_pop_pa[,'Linfs'],a_pop_pa[,'Ks'], 0, 1:21), type="b")
lines( 1:21, vbg (a_pop_pa[,'Linfs'],a_pop_pa[,'Ks']*0.8, 0, 1:21), type="b", col=2)  
# alter the brody growth curvature parameter k which determines how fast the fish approaches its Linf
lines( 1:21, vbg (a_pop_pa[,'Linfs'],a_pop_pa[,'Ks']*0.5, 0, 1:21), type="b", col=3)

# SSB-R
ssbr <- function (alpha, beta, ssb) {
       alpha*ssb*exp(-beta*ssb)
}
a_pop_pa <- pa[pa$pop.to.keeps =="SPR.2232", ]
plot( seq(0, 1000000000, by=1e6), ssbr (alpha=a_pop_pa[,'a_SSB'], beta=a_pop_pa[,'b_SSB'], ssb=seq(0, 1000000000, by=1e6)), type="l")
lines(seq(0, 1000000000, by=1e6), ssbr (a_pop_pa[,'a_SSB']*0.5,a_pop_pa[,'b_SSB']*0.5, seq(0, 1000000000, by=1e6)), type="l", col=2)
lines(seq(0, 1000000000, by=1e6), ssbr (a_pop_pa[,'a_SSB']*1.2,a_pop_pa[,'b_SSB']*1.2, seq(0, 1000000000, by=1e6)), type="l", col=3)
assess <- read.table(file='C:/Users/fba/Dropbox/ibm_vessels_param/summary_table_from_WGBFAS11_SPR_BE.txt',header = TRUE,  sep=",")
points( assess$TOTSPBIO*1e6, assess$RECRUITS*1e6, pch="+", cex=2)


# build a matrix a scenarios
multiplier_for_biolsce_all_pops  <- expand.grid(biolsce_maturity=1, biolsce_M=1, biolsce_weight=1, biolsce_init_pops=1, biolsce_init_pops_2011=1, 
                                         biolsce_fecundity=1, biolsce_Ks=c(1, 0.8), biolsce_recru=c(1, 0.5), 
                                          pop=c('SPR.2232', 'HER.3a22', 'COD.2224'))


multiplier_for_biolsce_all_pops <- cbind(sce=1: (nrow(multiplier_for_biolsce_all_pops)/length(unique(multiplier_for_biolsce_all_pops$pop))), multiplier_for_biolsce_all_pops)

write.table(multiplier_for_biolsce_all_pops, quote=FALSE,
                 file=file.path(main.path,"popsspe",paste("multiplier_for_biolsce.dat",sep='')), append=FALSE,
                   row.names=FALSE, col.names=TRUE)


################################
################################
################################
################################

if(case_study =="canadian_paper") sces <- 1  
if(case_study =="baltic_only")    sces <- 1:4

##### FOR-LOOP OVER BIOLOGICAL SCENARIOS ############
# ....because some parts of the parameterization are scenario specific!
for (sce in sces){




 timesteps   <- 21       #time steps 10 years with 2 semesters each 
 NbPeriods   <- 2        # 2 semesters within the year
 pop         <- 10000    #number of simulated individuals


# init the output file with headers (a multimap for c++ with / pop idx / values over the szgroup) 
 write("stock  init_maturity_per_szgroup", file=file.path(main.path,"popsspe",paste("init_maturity_per_szgroup_biolsce",sce,".dat",sep='')), append=FALSE) 
 write("stock  init_M_per_szgroup", file=file.path(main.path,"popsspe",paste("init_M_per_szgroup_biolsce",sce,".dat",sep='')), append=FALSE) 
 write("stock  init_weight_per_szgroup", file=file.path(main.path,"popsspe",paste("init_weight_per_szgroup_biolsce",sce,".dat",sep='')), append=FALSE) 
 write("stock  init_pops_per_szgroup", file=file.path(main.path,"popsspe",paste("init_pops_per_szgroup_biolsce",sce,".dat",sep='')), append=FALSE) 
 write("stock  init_pops_per_szgroup", file=file.path(main.path,"popsspe",paste("init_pops_per_szgroup_2011_biolsce",sce,".dat",sep='')), append=FALSE) 
 write("stock  init_fecundity_per_szgroup", file=file.path(main.path,"popsspe",paste("init_fecundity_per_szgroup_biolsce",sce,".dat",sep='')), append=FALSE) 
 write("stock  init_proprecru_per_szgroup", file=file.path(main.path,"popsspe",paste("init_proprecru_per_szgroup_biolsce",sce,".dat",sep='')), append=FALSE) 


 
##### FOR-LOOP OVER POP ############
for(x in 1:length(pa$Ks)){
  if(!is.na(pa$index_pops[x])){
 
  # species-specific parameters
  K               <-pa$Ks[x]                #K Bertalanffy
  Linf            <-pa$Linfs[x]             #Linf Bertalanfy 
  l50             <-pa$l50s[x]              #L50
  d               <-pa$ds[x]                #d*L^e fecundity
  e               <-pa$es[x]
  stock           <-pa$pop.to.keeps[x]
  aa              <-pa$aa[x]               
  bb              <-pa$bb[x]              
  a_SSB           <-pa$a_SSB[x]           
  b_SSB           <-pa$b_SSB[x]           
  r_age           <-pa$r_age[x]

  # get the sce matrix specific to this pop.
  multiplier_for_biolsce <- multiplier_for_biolsce_all_pops[multiplier_for_biolsce_all_pops$pop==as.character(stock),]  
  if(nrow(multiplier_for_biolsce)!=0){ # ie in case the pop is found in the sce matrix....
    # species-specific parameters AND sce
    K               <-K     *multiplier_for_biolsce[sce, "biolsce_Ks"]   
    Linf            <-Linf    
    l50             <-l50
    d               <-d     *multiplier_for_biolsce[sce, "biolsce_fecundity"]       
    e               <-e
    aa              <-aa    *multiplier_for_biolsce[sce, "biolsce_weight"]
    bb              <-bb    *multiplier_for_biolsce[sce, "biolsce_weight"]
    a_SSB           <-a_SSB *multiplier_for_biolsce[sce, "biolsce_recru"]
    b_SSB           <-b_SSB *multiplier_for_biolsce[sce, "biolsce_recru"]
    r_age           <-r_age
  }
   


  #simulate individual growth trajectories
  indlength<-mat.or.vec(pop,timesteps)  #define growth matrix
  meanI    <-mat.or.vec(pop,timesteps)      #mean Increment
  inc      <-mat.or.vec(pop,timesteps)        #increment
  varI     <-mat.or.vec(pop,timesteps)       #variance of increment

  #assign initial size of recruits
  for(i in 1:pop){
    indlength[i,1]<-abs(rnorm(1,0.5,0.5))            
  }

  # create growth trajectories
  if(!is.na(pa$Linfs[x])){
  
  for(ii in 1:pop){                           
    for(jj in 2:timesteps){
      varL<-0.01*Linf
      Linfe<-rnorm(1,mean=Linf,sd=sqrt(varL))     #stochasticity in Linf
      if(Linfe<=indlength[ii,(jj-1)]) {
        inc[ii,jj]<-0 
        } else {
        varK<-0.01*K
        Kr<-abs(rnorm(1,mean=K,sd=sqrt(varK)) )          #stochasticity in K

        meanI[ii,jj]<-(Linfe-indlength[ii,(jj-1)])*(1-exp(-Kr* 1/NbPeriods))     
        varI[ii,jj]<-0.01*meanI[ii,jj]  

        inc[ii,jj]<- abs(rnorm(1,mean=meanI[ii,jj],sd=sqrt(varI[ii,jj]))) 
        }       #stochasticity in growth increments 
      indlength[ii,jj]<-indlength[ii,(jj-1)]+inc[ii,jj]

      }
  }

  times<-t(matrix(1,timesteps,pop)*(1:timesteps))
  plot(times,indlength)                            #plot growth curves

  l<-c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,1000) *a_size_group_bin_in_cm  # vector of 14 length groups of eg 10 cm bin or 5 cm
    
  #size groups from growth trajectories
  S<-matrix(0,pop,timesteps)
  for(i in 1:timesteps){
    S[,i]<- cut(indlength[,i], breaks=l, labels=FALSE)
  }

 
  ## ALK Age-Length Keys
  #1- build age distribution matrix A                
  #2- build szgroup distribution matrix C                
  A<- matrix(0,length(l),11) # 11 age classes
  B<- matrix(0,length(l),21) # 21 tsteps
  C<- matrix(0,length(l),11) # 11 age classes
  for(sz in 1:length(l)){
      B[sz, ] <- apply(S, 2, function(x, sz) {length(x[x==sz])}, sz)   # nb in size groups per age and semesters , age group over ten years
  }
  C[,1:2] <- B[, 1:2]  # keep intact semesters 1 and 2 for the first year  #age 0 from first semester   #age 1 from second semester
  count<-2
  for(ij in c(3,5,7,9,11,13,15,17,19)){
      count<-count+1
      C[,count] <- B[, ij] + B[,ij+1]  # add semester 1 and semester 2
  }
  A <- sweep(C, 2, apply(C, 2, sum), FUN="/") # then scale to 1 PER AGE => distribution of age over szgroups
  C <- sweep(C, 1, apply(C, 1, sum), FUN="/") # then scale to 1 PER SZGROUP  => distribution of szgroup over ages
  C <- replace(C, is.na(C), 0)
  
  A  <-round(A,7)
  As <-t(A)
  C  <-round(C,7)
  Cs <-t(C)
  write(As[1:11,1:14],file=file.path(main.path,"popsspe",
           paste(pa$index_pops[x],"spe_percent_age_per_szgroup_biolsce",sce,".dat",sep='')),ncolumns=11, sep=" ")   #age 0-10
  write(Cs[1:11,1:14],file=file.path(main.path,"popsspe", 
           paste(pa$index_pops[x], "spe_percent_szgroup_per_age_biolsce",sce,".dat",sep='')),ncolumns=11, sep=" ")   #age 0-10
 }


    #init proportion of recruits per size group
    # i.e. extract one column (the column of the age of recruit) from the percent_age_per_szgroup matrix....
    if(!is.na(r_age)){
    proprecru       <- As[r_age +1,]  # caution: add + 1 for offset i.e. if age 0 => column 1
    } else{
    proprecru       <- rep(0, 14)
    }
    init_proprecru  <- rbind(pa$index_pops[x], proprecru) 
    write(init_proprecru[,1:14], file=file.path(main.path,"popsspe",paste("init_proprecru_per_szgroup_biolsce",sce,".dat",sep='')),sep=" ",ncolumns=2, append=TRUE) 
   


  # init number per size group   comes in Thousands!
  if(stock %in% levels(number$stock)){
    number1        <- number       [number$stock %in% stock,2:12]     #age 0-10    
    number1_yplus1 <- number_yplus1[number_yplus1$stock %in% stock,2:12]     #age 0-10    
                                    
     
    number1[is.na(number1)]               <- 0
    number1_yplus1[is.na(number1_yplus1)] <- 0
    pops        <-matrix(0,15,11)
    pops_yplus1 <-matrix(0,15,11)
    for(i in 1:10){                                            
      pops[,i]        <- A[,i]*as.numeric(as.character(number1[1,i]))
      pops_yplus1[,i] <- A[,i]*as.numeric(as.character(number1_yplus1[1,i]))
    }

    init_pops        <- rbind(pa$index_pops[x],rowSums(pops))
    init_pops_yplus1 <- rbind(pa$index_pops[x],rowSums(pops_yplus1))
    #init_pops<-round(init_pops)  not necessary because ind. in thousands
    write(init_pops[,1:14], file=file.path(main.path,"popsspe",paste("init_pops_per_szgroup_biolsce",sce,".dat",sep='')), sep=" ",ncolumns=2, append=TRUE) 
    write(init_pops_yplus1[,1:14], file=file.path(main.path,"popsspe",paste("init_pops_per_szgroup_",a.year+1,"_biolsce",sce,".dat",sep='')), sep=" ",ncolumns=2, append=TRUE) 
  } else{
  # fill in with fake numbers for implicit pops i.e.
  #  the pops for which we do not have info on N because not assessed by ICES
  # in this case the pop is not truly simulated in the IBM simulation but catches can still be done
  # using historic vessel and species-specific cpues...see Bastardie et al 2010
   options(scipen=99)
  write.table(cbind(rep(pa$index_pops[x],14), rep(100000,14))[1:14,],
                 file=file.path(main.path,"popsspe",paste("init_pops_per_szgroup_biolsce",sce,".dat",sep='')), append=TRUE,
                   row.names=FALSE, col.names=FALSE)
  write.table(cbind(rep(pa$index_pops[x],14), rep(100000,14))[1:14,],
                 file=file.path(main.path,"popsspe",paste("init_pops_per_szgroup_",a.year+1,"_biolsce",sce,".dat",sep='')), append=TRUE,
                   row.names=FALSE, col.names=FALSE)

  }

  #init weight per size group
  if(!is.na(aa)){
    weight<-aa*(l+5)^bb/1000       #length-weight in cm-g from fishbase, here divided by 1000 ->> cm-kg
   } else{
    weight<- rep(0, 14)
   }
    init_weight<-rbind(pa$index_pops[x],weight) 
    write(init_weight[,1:14], file=file.path(main.path,"popsspe",paste("init_weight_per_szgroup_biolsce",sce,".dat",sep='')),sep=" ",ncolumns=2, append=TRUE) 
 

  #build size transition matrix G
  incr<-inc[,-c(1)]                  #remove first column, growth from previous to present size                          
  increment<-c(incr)                  #vectorize
  leng<-indlength[,-c(21)]            #remove last column , to combine length with growth increment to next size group                       
  len<-c(leng)                        #vectorize
         
  values<-mat.or.vec(length(len),4)                                           
  values[,1]<-as.numeric(as.character(len))
  values[,4]<-as.numeric(as.character(increment))   
    
  val<-cut(values[,1],breaks=l) #put into size bins                  
  values[,2]<-val 
  levels(val)
  levels(val)<-l               #change labels of size bins
  values[,3]<-as.numeric(as.character(val))   #create vector of lower bounds in 10cm intervals


  n<-length(l)-1
  G<-matrix(0,(n),(n))

  for(b in 1:n){
    for(a in 1:n){
      if(b<=a){
        value<-subset(values,values[,2]==b)
        if(var(value[,4])==0|length(value[,1])<2){
          G[a,b]<-0
        }else{             
        mea<-mean(value[,4]) 
        vari<-var(value[,4])
        fun<-function(x) dnorm(x,mean=(l[b]+5+mea),sd=sqrt(vari))
        G[a,b]<-integrate(fun,l[a],l[a+1])$value
        }
      }
    }
  }

  G<-round(G,3)
  if(all(G==0)) G[1,1] <-1 # e.g. blue mussels
  write.table(G,  file=file.path(main.path,"popsspe", 
                    paste(pa$index_pops[x],"spe_size_transition_matrix_biolsce",sce,".dat",sep='')),
                      sep=" ",col.names=FALSE, row.names=FALSE)




  #build size distribution vector L
  surv<-round(exp(-(0.12*27*(l+0.5)^(-1))),4)      #length dependent mortality vector using the lower bound length (+1 to ignore 0) to get survival
  mort<-round((1-surv),4)
  ## EXPORT
  mort <- cbind (pa$index_pops[x], mort)
  write.table(mort[1:14,], file=file.path(main.path,"popsspe",paste("init_M_per_szgroup_biolsce",sce,".dat",sep='')), append=TRUE, sep=" ", col.names=FALSE, row.names=FALSE) 


  #need a first row to decribe recruitment for stable size distribution from SSB
  fec<- d*(l+5)^(e)          #   fecundity 
  fec[is.na(fec)]<-0
  fec<-round(fec,2)
  mat<-1/(1+exp(-0.2*(l+5-l50)))  #maturity ogive
  mat[is.na(mat)]<-0
  mat<-round(mat,4)
  mat[1]<-0

  mat <- cbind (pa$index_pops[x], mat)
  write.table(mat[1:14,], file=file.path(main.path,"popsspe",paste("init_maturity_per_szgroup_biolsce",sce,".dat",sep='')), append=TRUE, sep=" ", col.names=FALSE, row.names=FALSE) 




  
  if(!is.na(e)){
    fec<-cbind(pa$index_pops[x],fec)
    write.table(fec[1:14,], file=file.path(main.path,"popsspe",paste("init_fecundity_per_szgroup_biolsce",sce,".dat",sep='')), append=TRUE, sep=" ", col.names=FALSE, row.names=FALSE) 
   } else{
    # fill in with fake numbers for implicit pops i.e.
    #  the pops for which we do not have info on N because not assessed by ICES
    # in this case the pop is not truly simulated in the IBM simulation but catches can still be done
    # using historic vessel and species-specific cpues...see Bastardie et al 2010
     options(scipen=99)
    write.table(cbind(rep(pa$index_pops[x],14), rep(100000,14))[1:14,],
                   file=file.path(main.path, "popsspe", paste("init_fecundity_per_szgroup_biolsce",sce,".dat",sep='')), append=TRUE,
                   row.names=FALSE, col.names=FALSE)

  }


  } # end if
  
  


  
  a_SSB <- replace(a_SSB, is.na(a_SSB), 0)   # put 0 instead of NA because 'double' required by c++
  b_SSB <- replace(b_SSB, is.na(b_SSB), 0)    # put 0 instead of NA because 'double' required by c++
  
  # SSB-R
  write.table(c(a_SSB, b_SSB), 
                file=file.path(main.path,"popsspe",paste(pa$index_pops[x],"spe_SSB_R_parameters_biolsce",sce,".dat",sep='')),
                  append=FALSE, sep=" ", col.names=FALSE, row.names=FALSE) 

  # initial TAC
  write.table(pa[x,13], 
                file=file.path(main.path,"popsspe",paste(pa$index_pops[x],"spe_initial_tac.dat",sep='')),
                  append=FALSE, sep=" ", col.names=FALSE, row.names=FALSE) 

 # fbar ages and LTMP F target and Fpercent e.g. f multiplier +/-10%  and TAC range e.g. +/-15%
  write.table(pa[x,14:18], 
                file=file.path(main.path,"popsspe",paste(pa$index_pops[x],"spe_fbar_amin_amax_ftarget_Fpercent_TACpercent.dat",sep='')),
                  append=FALSE, sep=" ", col.names=FALSE, row.names=FALSE) 

  
} # end for
  




} # end loop over sce








if(FALSE){
###----------------------------------------------------------------
# check ssb

N    <- read.table(file=file.path(main.path,"popsspe",paste("init_pops_per_szgroup.dat",sep='')), sep="", dec=".", header=TRUE)
MAT  <- read.table(file=file.path(main.path,"popsspe",paste("init_maturity_per_szgroup.dat",sep='')), sep="", dec=".", header=TRUE)
W    <-  read.table(file=file.path(main.path,"popsspe",paste("init_weight_per_szgroup.dat",sep='')), sep="", dec=".", header=TRUE)
mat <- cbind(N, MAT, W)
mat [,"ssb"] <- mat[,'init_maturity_per_szgroup']*mat[,'init_weight_per_szgroup']*mat[,'init_pops_per_szgroup']*1000/2     #only females!! divide by 2

mat<-mat[mat$stock%in% c(2,3,4,5,7,10,11,12,14,15,20,21,26,29,30),c(1,7)]
res<-aggregate(mat$ssb, by= list(stock=mat$stock), FUN=sum)
res$x<-res$x /1e6  #=> SSB per stock in thousands tons


 pa <- read.csv(file=file.path(main.path,
                  paste("IBM_datainput_stockdata.csv", sep='')), 
                    sep=',', header=TRUE)

res$stocknames<- pa[pa$index_pops%in%c(2,3,4,5,7,10,11,12,14,15,20,21,26,29,30),1]
res

###----------------------------------------------------------------
 
#ICES have a summary database at http://www.ices.dk/datacentre/StdGraphDB/FishStockDB.mdb containing a summary 
#of all stocks assessments done by ICES.
#These data are in the form of an access database.
library(RODBC);library(googleVis)
if(FRANCOIS) channel <- odbcConnectAccess("C:\\Users\\fba\\Dropbox\\ibm_vessels_param\\FishStockDB.mdb")
if(TANJA) channel <- odbcConnectAccess("C:\\Users\\Tanja\\Documents\\My Dropbox\\Vessel\\ibm_vessels_param\\FishStockDB.mdb")
fishdta <- sqlFetch(channel,"Fishdata")[,1:9]
fishhrv <- gvisMotionChart(fishdta, idvar="FishStock", timevar="Year")
plot(fishhrv) ## pretty nice!
# we can use this database to get total landings per stock (but no discards info unfortunatly)
### but only ASSESSED stocks...so we need to use e.g. ICES_catch_statistics_1950_2009 instead 
## (http://www.ices.dk/fish/CATChSTATISTICS.asp) to get all landed species
## but still without discards!  Is this data also exist somewhere including discards????

}  # end FALSE