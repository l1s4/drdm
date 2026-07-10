library(EMC2)
load("data/Exp3_clean.RData")
#load("dLBA/Exp3_clean.RData")
dat <- combi_3
dat_suffix = "_Exp3"

# for testing:
#dat <- dat[dat$subjects %in% c(1, 2), ]

#head(dat)
# CI (cc, ci, ic, ii): congruency
# CI1 (c, i): congruency first task 
# CI2 (c, i): congruency second task 
# S: stimulus (i.e. target)
# R: response
# IS1: irrelevant stimulus first task
# IS2: irrelevant stimulus second task


# B: caution, avg. of thresholds across accumulators
# b: response bias towards response favored by irrel. dim., diff. between 
#    thresholds 
# v: diff. in mean rates between accumulators that match vs. mismatch irrel. 
#    dimension
# s: diff. in rate variability between accumulators that match vs. mismatch 
#    irrel. dimension
# V: avg. of mean rates across accumulators
# S: avg. of rate variability across accumulators

formulas = list(
  # models with #R have not been run
  
#  # 1
#  RDM_b_a = list(v~lM, B~lR+iM1+iM2, s~lM, t0~1), 
#  RDM_b_i = list(v~lM, B~lR*iM1+lR*iM2, s~lM, t0~1),
#  RDM_B_a = list(v~lM, B~lR+CI1+CI2, s~lM, t0~1),    #(R)
#  RDM_B_i = list(v~lM, B~lR*CI1+lR*CI2, s~lM, t0~1), #(R)
#  RDM_v_a = list(v~lM+iM1+iM2, B~lR, s~lM, t0~1),    #(R)
#  RDM_v_i = list(v~lM*iM1+lM*iM2, B~lR, s~lM, t0~1), #(R)
#  RDM_V_a = list(v~lM+CI1+CI2, B~lR, s~lM, t0~1),    #(R)
#  RDM_V_i = list(v~lM*CI1+lM*CI2, B~lR, s~lM, t0~1), #(R)
#  RDM_s_a = list(v~lM, B~lR, s~lM+iM1+iM2, t0~1),    #(R)
#  RDM_s_i = list(v~lM, B~lR, s~lM*iM1+lM*iM2, t0~1), #(R)
#  RDM_S_a = list(v~lM, B~lR, s~lM+CI1+CI2, t0~1),    #(R)
#  RDM_S_i = list(v~lM, B~lR, s~lM*CI1+lM*CI2, t0~1), #(R)
  
  
  # 2 
  
  # Priming Weigard / = drop v from Molloy2026
  Priming_a = list(v~CI1*lM+CI2*lM, B~lR+iM1+iM2, s~lM, t0~1),    # = RDM_bV_i1
  Priming_i = list(v~CI1*lM+CI2*lM, B~lR*iM1+lR*iM2, s~lM, t0~1), # = RDM_bV_ii
  RDM_bV_a = list(v~lM+CI1+CI2, B~lR+iM1+iM2, s~lM, t0~1), #R*
  RDM_bV_i2 = list(v~lM+CI1+CI2, B~lR*iM1+lR*iM2, s~lM, t0~1), #R*

  # drop V from Molloy2026
  RDM_bv_a = list(v~lM+iM1+iM2, B~lR+iM1+iM2, s~lM, t0~1), #R*
  RDM_bv_i1 = list(v~lM*iM1+lM*iM2, B~lR+iM1+iM2, s~lM, t0~1), #R*
  RDM_bv_i2 = list(v~lM+iM1+iM2, B~lR*iM1+lR*iM2, s~lM, t0~1), #R*
  RDM_bv_ii = list(v~lM*iM1+lM*iM2, B~lR*iM1+lR*iM2, s~lM, t0~1), #R*
 
  # drop S from Conflict Cancellation:  
  RDM_Bs_a = list(v~lM, B~lR+CI1+CI2, s~lM+iM1+iM2, t0~1), #R*
  RDM_Bs_i2 = list(v~lM, B~lR*CI1+lR*CI2, s~lM+iM1+iM2, t0~1), #R*
  Noise = list(v~lM, B~CI1+CI2+lR, s~iM1*lM+iM2*lM, t0~1), # = RDM_Bs_i3
  RDM_Bs_ii = list(v~lM, B~lR*CI1+lR*CI2, s~lM*iM1+lM*iM2, t0~1), #R*
  
  # drop s from Conflict Cancellation:
  RDM_BS_a = list(v~lM, B~lR+CI1+CI2, s~lM+CI1+CI2, t0~1), #R*
  RDM_BS_i2 = list(v~lM, B~lR*CI1+lR*CI2, s~lM+CI1+CI2, t0~1), #R*
  RDM_BS_i3 = list(v~lM, B~lR+CI1+CI2, s~lM*CI1+lM*CI2, t0~1), #R*
  RDM_BS_ii = list(v~lM, B~lR*CI1+lR*CI2, s~lM*CI1+lM*CI2, t0~1), #R*
 
#  # remaining 2-mechanism models 
#  RDM_Bb_a = list(v~lM, B~lR+iM1+iM2+CI1+CI2, s~lM, t0~1), #(R)
#  RDM_bs_a = list(v~lM, B~lR+iM1+iM2, s~lM+iM1+iM2, t0~1), #(R)
#  RDM_bS_a = list(v~lM, B~lR+iM1+iM2, s~lM+CI1+CI2, t0~1), #(R)
#  RDM_Bv_a = list(v~lM+iM1+iM2, B~lR+CI1+CI2, s~lM, t0~1), #(R)
#  RDM_BV_a = list(v~lM+CI1+CI2, B~lR+CI1+CI2, s~lM, t0~1), #(R)
#  RDM_vS_a = list(v~lM+iM1+iM2, B~lR, s~lM+CI1+CI2, t0~1), #(R)
#  RDM_Vs_a = list(v~lM+CI1+CI2, B~lR, s~lM+iM1+iM2, t0~1), #(R)
#  RDM_VS_a = list(v~lM+CI1+CI2, B~lR, s~lM+CI1+CI2, t0~1), #(R)
#  RDM_vs_a = list(v~lM+iM1+iM2, B~lR, s~lM+iM1+iM2, t0~1), #(R)
#  # (drop b from Molloy2026): 
#  RDM_Vv_a = list(v~lM+CI1+CI2+iM1+iM2, B~lR, s~lM, t0~1), #(R)
#  # (drop B from Conflict Cancellation):
#  RDM_Ss_a = list(v~lM, B~lR, s~lM+CI1+CI2+iM1+iM2, t0~1), #(R)
  

  # 3
  
  # Molloy2026
  RDM_bVv_a = list(v~lM+CI1+CI2+iM1+iM2, B~lR+iM1+iM2, s~lM, t0~1),
  RDM_bVv_i = list(v~lM+CI1+CI2+iM1+iM2, B~lR*iM1+lR*iM2, s~lM, t0~1), 
  
  # Conflict cancellation
  RDM_BSs_a = list(v~lM, B~lR+CI1+CI2, s~lM+CI1+CI2+iM1+iM2, t0~1), 
  RDM_BSs_i = list(v~lM, B~lR*CI1+lR*CI2, s~lM+CI1+CI2+iM1+iM2, t0~1), 
  
  # Molloy2026 and B instead of b
  RDM_BVv_i = list(v~lM+CI1+CI2+iM1+iM2, B~lR*CI1+lR*CI2, s~lM, t0~1), #R
  
  # Molloy2026 and s instead of v
  RDM_bVs_i1 = list(v~lM*CI1+lM*CI2, B~lR+iM1+iM2, s~lM+iM1+iM2, t0~1), #R*
  RDM_bVs_i2 = list(v~lM+CI1+CI2, B~lR*iM1+lR*iM2, s~lM+iM1+iM2, t0~1), #R*
  RDM_bVs_i3 = list(v~lM+CI1+CI2, B~lR+iM1+iM2, s~lM*iM1+lM*iM2, t0~1), #R*
  
  # Molloy2026 and Ss instead of Vv / Conflict cancellation and b instead of B
  RDM_bSs_i = list(v~lM, B~lR*iM1+lR*iM2, s~lM+CI1+CI2+iM1+iM2, t0~1), 
  
  # Conflict cancellation and V instead of B
  RDM_VSs_a = list(v~lM+CI1+CI2, B~lR, s~lM+CI1+CI2+iM1+iM2, t0~1), 
  RDM_VSs_i = list(v~lM*CI1+lM*CI2, B~lR, s~lM+CI1+CI2+iM1+iM2, t0~1), 
  
  # remaining 3 mechanism models: 
  # Bbv, BbV, BbS, Bvs, BvS, BVs, BVS, bVS, Vvs, VvS, vSs, VSs, Bbs, bSs
 
  
  # 4: 
  # Molloy2026 add S
  RDM_bVvS_a = list(v~lM+CI1+CI2+iM1+iM2, B~lR+iM1+iM2, s~lM+CI1+CI2, t0~1), #(R)
  RDM_bVvS_i = list(v~lM+CI1+CI2+iM1+iM2, B~lR*iM1+lR*iM2, s~lM+CI1+CI2, t0~1), #R
  RDM_bVvS_ii = list(v~lM+CI1+CI2+iM1+iM2, B~lR*iM1+lR*iM2, s~lM*CI1+lM*CI2, t0~1), #R
  
  # Molloy2026 add s
  RDM_bVvs_a = list(v~lM+CI1+CI2+iM1+iM2, B~lR+iM1+iM2, s~lM+iM1+iM2, t0~1), #(R)
  RDM_bVvs_i = list(v~lM+CI1+CI2+iM1+iM2, B~lR*iM1+lR*iM2, s~lM+iM1+iM2, t0~1), #R
  RDM_bVvs_ii = list(v~lM+CI1+CI2+iM1+iM2, B~lR*iM1+lR*iM2, s~lM*iM1+lM*iM2, t0~1), #R
  
  # Conflict Cancellation, add V
  RDM_BVSs_i2 = list(v~lM+CI1+CI2, B~lR*CI1+lR*CI2, s~lM+CI1+CI2+iM1+iM2, t0~1), #R
  RDM_BVSs_ii = list(v~lM*CI1+lM*CI2, B~lR*CI1+lR*CI2, s~lM+CI1+CI2+iM1+iM2, t0~1), #R
  
  # Conflict Cancellation, add v
  RDM_BvSs_i2 = list(v~lM+iM1+iM2, B~lR*CI1+lR*CI2, s~lM+CI1+CI2+iM1+iM2, t0~1), #R
  RDM_BvSs_ii = list(v~lM*iM1+lM*iM2, B~lR*CI1+lR*CI2, s~lM+CI1+CI2+iM1+iM2, t0~1) #R
  
  # remaining: BbVv, BbSs, VvSs, Bbvs, BbvS, BbVs, BbVS, BVvs, BVvS, BvSs, 
  # BVSs, bVvs, bVvS, bvSs, bVSs 
  
  
  #likely too many mechanisms: BbVvS, BbVvs, BbVSs, BbvSs, BVvSs, bVvSs; BbVvSs
)



acc_ci1 <- function(data) factor(data$lR == data$IS1)
acc_ci2 <- function(data) factor(data$lR == data$IS2)

ADmat <- cbind(d = c(-1/2, 1/2))

fit_model <- function(i, cores_per_chain=2) {
  fname = paste0("out/", names(formulas[i]), dat_suffix, ".RData")
  
  designRDM <- design(model = RDM, data = dat, 
                      matchfun = function(d) d$S == d$lR, 
                      functions = list(iM1 = acc_ci1, iM2 = acc_ci2),
                      formula = formulas[[i]], 
                      constants = c(s = log(1)),
                      contrasts = list(lM = ADmat, lR = contr.helmert)
                      )
  emc <- make_emc(dat, designRDM)
  
  fit(emc, fileName = fname, cores_per_chain=cores_per_chain)
}
#fit_model(1, cores_per_chain = 8)
library(parallel)
mclapply(1:length(formulas), fit_model, cores_per_chain = 6, mc.cores = 3)