library(EMC2)

## Exp. 3
dat <- get(load("data/Exp3_clean.RData"))


emc_b_a <- get(load("out/RDM_b_a_Exp3.RData"))
emc_b_i <- get(load("out/RDM_b_i_Exp3.RData"))
emc_P <- get(load("out/Priming_a_Exp3.RData"))
emc_Pi <- get(load("out/Priming_i_Exp3.RData"))
emc_N <- get(load("out/Noise_Exp3.RData"))
emc_BSs_a <- get(load("out/RDM_BSs_a_Exp3.RData"))
emc_BSs_i <- get(load("out/RDM_BSs_i_Exp3.RData"))
emc_bVv_a <- get(load("out/RDM_bVv_a_Exp3.RData"))
emc_bVv_i <- get(load("out/RDM_bVv_i_Exp3.RData"))

check(emc_Pi)

compare(list(
  b_a = emc_b_a, b_i = emc_b_i, P = emc_P, Pi = emc_Pi, N = emc_N, 
  BSs_a = emc_BSs_a, BSs_i = emc_BSs_i,  
  bVv_a = emc_bVv_a, bVv_i = emc_bVv_i
  ), BayesFactor = F)

# pps
pP <- predict(emc_P, n_cores=9)
pPi <- predict(emc_Pi, n_cores=9)
pN <- predict(emc_N, n_cores=9)
pBSs_a <- predict(emc_BSs_a, n_cores=9)
pBSs_i <- predict(emc_BSs_i, n_cores=9)
pbVv_a <- predict(emc_bVv_a, n_cores=9)
pbVv_i <- predict(emc_bVv_i, n_cores=9)

cfun <- \(d) d$S==d$R
plot_cdf(dat, pP, factors=c("S","CI1","CI2"), layout=c(2,4))
plot_cdf(dat, pPi, factors=c("S","CI1","CI2"), layout=c(2,4))
plot_cdf(dat, pN, factors=c("S","CI1","CI2"), layout=c(2,4))
plot_cdf(dat, pBSs_a, factors=c("S","CI1","CI2"), layout=c(2,4))
plot_cdf(dat, pBSs_i, factors=c("S","CI1","CI2"), layout=c(2,4))
plot_cdf(dat, pbVv_a, factors=c("S","CI1","CI2"), layout=c(2,4))
plot_cdf(dat, pbVv_i, factors=c("S","CI1","CI2"), layout=c(2,4))

plot_delta(dat, pBSs_i, delta_factor = "CI1")
plot_delta(dat, pBSs_i, delta_factor = "CI2")
plot_delta(dat, pbVv_i, delta_factor = "CI1")
plot_delta(dat, pbVv_i, delta_factor = "CI2")

plot_caf(dat, pbVv_a, caf_factor = "CI1")
plot_caf(dat, pbVv_a, caf_factor = "CI2")
plot_caf(dat, pbVv_i, caf_factor = "CI1")
plot_caf(dat, pbVv_i, caf_factor = "CI2")

plot_density(dat, pBSs_a, factors=c("S","CI1","CI2"), layout=c(2,4))
plot_density(dat, pBSs_i, factors=c("S","CI1","CI2"), layout=c(2,4))
plot_density(dat, pbVv_a, factors=c("S","CI1","CI2"), layout=c(2,4))
plot_density(dat, pbVv_i, factors=c("S","CI1","CI2"), layout=c(2,4))


credint(emc_P, map = T)
credint(emc_BSs_a, map = T)
credint(emc_bVv_a, map = T)

rtfun <- \(d) {
  m <- as.vector(tapply(d$rt,d[,c("CI1","CI2")],mean))
  m[-4]-m[4]
}

cfun <- \(d) {
  m <- as.vector(tapply(d$S==d$R,d[,c("CI1","CI2")],mean))
  m[4]-m[-4]
}


plot_stat(dat,pBSs_a,stat_fun=rtfun)
plot_stat(dat,pBSs_i,stat_fun=rtfun)
plot_stat(dat,pbVv_a,stat_fun=cfun)
plot_stat(dat,pbVv_i,stat_fun=cfun)


tapply(dat$rt,dat[,c("CI1","CI2")],mean)

tapply(pP$rt,pP[,c("CI1","CI2")],mean)
tapply(pBSs_a$rt,pBSs_a[,c("CI1","CI2")],mean)
tapply(pBSs_i$rt,pBSs_i[,c("CI1","CI2")],mean)
tapply(pbVv_a$rt,pbVv_a[,c("CI1","CI2")],mean)
tapply(pbVv_i$rt,pbVv_i[,c("CI1","CI2")],mean)


tapply(dat$S==dat$R,dat[,c("CI1","CI2")],mean)

tapply(pP$S==pP$R,pP[,c("CI1","CI2")],mean)
tapply(pBSs_a$S==pBSs_a$R,pBSs_a[,c("CI1","CI2")],mean)
tapply(pBSs_i$S==pBSs_i$R,pBSs_a[,c("CI1","CI2")],mean)
tapply(pbVv_i$S==pbVv_i$R,pbVv_a[,c("CI1","CI2")],mean)
