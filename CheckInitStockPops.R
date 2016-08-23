


N <- read.table("C:/Users/fbas/Documents/GitHub/DISPLACE_input_adriatic/popsspe_adriatic/init_pops_per_szgroup_biolsce1.dat", header=TRUE)
sum(N[N[,1]==0, 2])

maturity <- read.table("C:/Users/fbas/Documents/GitHub/DISPLACE_input_adriatic/popsspe_adriatic/init_maturity_per_szgroup_biolsce1.dat", header=TRUE)

wt <- read.table("C:/Users/fbas/Documents/GitHub/DISPLACE_input_adriatic/popsspe_adriatic/init_weight_per_szgroup_biolsce1.dat", header=TRUE)

SSB <- N[N[,1]==0, 2]*1000 * maturity[maturity[,1]==0, 2] * wt[wt[,1]==0, 2] *3
sum(SSB) /1000  # in tons
 <- read.table("C:/Users/fbas/Documents/GitHub/DISPLACE_input_adriatic/popsspe_adriatic/init_pops_per_szgroup_biolsce1.dat", header=TRUE)
sum(N[N[,1]==0, 2])

maturity <- read.table("C:/Users/fbas/Documents/GitHub/DISPLACE_input_adriatic/popsspe_adriatic/init_maturity_per_szgroup_biolsce1.dat", header=TRUE)

wt <- read.table("C:/Users/fbas/Documents/GitHub/DISPLACE_input_adriatic/popsspe_adriatic/init_weight_per_szgroup_biolsce1.dat", header=TRUE)

SSB <- N[N[,1]==0, 2]*1000 * maturity[maturity[,1]==0, 2] * wt[wt[,1]==0, 2] *100
sum(SSB) /1000  # in tons
SSB <- N[N[,1]==1, 2]*1000 * maturity[maturity[,1]==1, 2] * wt[wt[,1]==1, 2] *5
sum(SSB) /1000  # in tons
SSB <- N[N[,1]==2, 2]*1000 * maturity[maturity[,1]==2, 2] * wt[wt[,1]==2, 2] *150
sum(SSB) /1000  # in tons
SSB <- N[N[,1]==3, 2]*1000 * maturity[maturity[,1]==3, 2] * wt[wt[,1]==3, 2] *50
sum(SSB) /1000  # in tons

 (aa*(60))^bb)/1000


l50 <- 23
 SSB <- N[N[,1]==0, 2]*1000 *  1/(1+exp(-0.2*(l+(a_size_group_bin_in_cm/2)-l50)))   * wt[wt[,1]==0, 2]
sum(SSB) /1000  # in tons

l50 <- 5
 SSB <- N[N[,1]==0, 2]*1000 *  1/(1+exp(-0.2*(l+(a_size_group_bin_in_cm/2)-l50)))   * wt[wt[,1]==0, 2]
sum(SSB) /1000  # in tons

 dd <- wt[wt[,1]==0, 2]
dd[length(dd)] <- 2
l50 <- 23
 SSB <- N[N[,1]==0, 2]*1000 *   maturity[maturity[,1]==3, 2]   *dd
sum(SSB) /1000  # in tons


SSB <- N[N[,1]==0, 2]*1000 * maturity[maturity[,1]==0, 2] * wt[wt[,1]==0, 2]
sum(SSB) /1000  # in tons
SSB <- N2[N2[,1]==0, 2]*1000 * maturity[maturity[,1]==0, 2] * wt[wt[,1]==0, 2]
sum(SSB) /1000  # in tons
SSB <- N2[N2[,1]==1, 2]*1000 * maturity[maturity[,1]==1, 2] * wt[wt[,1]==1, 2]
sum(SSB) /1000  # in tons
SSB <- N2[N2[,1]==2, 2]*1000 * maturity[maturity[,1]==2, 2] * wt[wt[,1]==2, 2]
sum(SSB) /1000  # in tons
SSB <- N2[N2[,1]==3, 2]*1000 * maturity[maturity[,1]==3, 2] * wt[wt[,1]==3, 2]
sum(SSB) /1000  # in tons



