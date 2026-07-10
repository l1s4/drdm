library(dplyr)

# Experiment 1a: Combi blocks ##################################################
# combi: stimuli were central letters of color words
files <- list.files(path = "data/Exp1a_data/combi", full.names = TRUE)
ldf <- lapply(files, read.table)
dat_combi <- bind_rows(ldf)
colnames(dat_combi) <- c("subj", "block", "catch", "stimulus",
                         "stimuluscolor", "flankercolor", "flanker", "stroop",
                         "xr", "r", "corr", "rt", "re")

length(unique(dat_combi$subj))    # verify subj, N = 24
table(dat_combi$block)            # 4 blocks, 4608 trials each; 4608 / 24 = 192

# check xr, r and corr
table(dat_combi$catch) / nrow(dat_combi)    # 25% catch trials; 1 is catch
sum(dat_combi$corr == 1) / nrow(dat_combi)  # corr: response, correct = 1
all((dat_combi$xr == dat_combi$r) == (dat_combi$corr == 1))
table(dat_combi$xr)             # expected response (1-4)
table(dat_combi$r)              # response (0-4); 0 is timeout
all((dat_combi$r == 0) == (dat_combi$rt == 5000))   # 0 is timeout
table(dat_combi$stimulus)       # color word (0-3)
table(dat_combi$stimuluscolor)  # color of middle letter (0-3) [i.e. target]
table(dat_combi$flankercolor)   # color of flanking letters (0-3)


# verify stimulus, stroop and flanker congruencies
temp <- dat_combi[dat_combi$catch != 1, ]     # exclude catch

# stimuluscolor is color of central letter and determines expected response
all(temp$stimuluscolor+1 == temp$xr)

# stroop is stroop-congruency; 1 is congruent
all((temp$stimulus == temp$stimuluscolor) == (temp$stroop == 1))

#TODO: this does not make sense
# flanker seems to be flanker-congruency; 1 is congruent
all((temp$stimulus == temp$flankercolor) == (temp$flanker == 1))
# but not in 1185 trials (all of those have stroop = 0 and flanker = 0)
# => ???
temp[!(temp$stimulus == temp$flankercolor) == (temp$flanker == 1), ]# |> nrow()
#TODO: moving on for now, assuming flanker = 1 is congruent and 
# flankercolor is flankercolor

table((temp$stimuluscolor == temp$flankercolor), temp$flanker)




# create congruency columns
dat_combi$CI1 <- ifelse(dat_combi$flanker == 1, "congruent", "incongruent")
dat_combi$CI2 <- ifelse(dat_combi$stroop == 1, "congruent", "incongruent")
dat_combi$CI <- ifelse(
  dat_combi$flanker == 0 & dat_combi$stroop == 0, "II",
  ifelse(dat_combi$flanker == 0 & dat_combi$stroop == 1, "IC",
         ifelse(dat_combi$flanker == 1 & dat_combi$stroop == 0, "CI", "CC"))
)

# exclusions: catch trials (n=4608) and timeout responses (n=20)
combi_1a <- dat_combi[(dat_combi$catch != 1) & (dat_combi$r != 0), ]    

combi_1a$rt <- combi_1a$rt / 1000   # convert to seconds

# exclude RT faster and slower than 3SD from the mean for each condition
mean_rt_cond <- aggregate(rt ~ CI, FUN=mean, combi_1a)
sds_cond <- aggregate(rt ~ CI, FUN=sd, combi_1a)
cond_mean <- ave(combi_1a$rt, combi_1a$CI, FUN=mean)
cond_sd <- ave(combi_1a$rt, combi_1a$CI, FUN=sd)
combi_1a <- combi_1a[abs(combi_1a$rt - cond_mean) < 3 * cond_sd, ] # excl. n=270
# but then RTs = 0 are still included:
min(combi_1a$rt)
mean_rt_cond$rt - sds_cond$rt * 3
mean_rt_cond$rt + sds_cond$rt * 3
# exclude fast and slow RTs
combi_1a <- combi_1a[combi_1a$rt > 0.3 & combi_1a$rt < 3, ] # excl. n=3



# rename for EMC2
names(combi_1a)[names(combi_1a) == "subj"] <- "subjects"
names(combi_1a)[names(combi_1a) == "r"] <- "R"
combi_1a$S <- combi_1a$stimuluscolor + 1          # +1 to match S and R

combi_1a$flanking <- combi_1a$flankercolor + 1    # +1 to match S
combi_1a$colorword <- combi_1a$stimulus + 1       # +1 to match S

combi_1a$IS1 <- combi_1a$flanking     # first irrelevant dimension
combi_1a$IS2 <- combi_1a$colorword    # first irrelevant dimension

factor_cols <- c("S", "R", "CI", "subjects", "flanking", "colorword", "IS1", 
                 "IS2", "CI1", "CI2")
combi_1a[factor_cols] <- lapply(combi_1a[factor_cols], factor)


# drop unused cols and save
dropcols <- c("re", "catch", "block", "xr", "stimuluscolor", "flankercolor",
              "flanker", "stroop", "stimulus", "flanking", "colorword")
combi_1a <- combi_1a[! names(combi_1a) %in% dropcols]
save(combi_1a, file = "data/Exp1a_clean.RData")



# Experiment 2: Combi blocks ###################################################
files <- list.files(path = "data/Exp2_data/combi", full.names = TRUE)
ldf <- lapply(files, read.table)
dat_combi <- bind_rows(ldf)
colnames(dat_combi) <- c("subj", "block", "catch", "stimulus",
                         "stimuluscolor", "stimulusposition", "simon", "stroop",
                         "xr", "r", "corr", "rt", "re")

length(unique(dat_combi$subj))    # verify subj, N = 23
table(dat_combi$block)            # 4 blocks, 4416 trials each; 4416 / 23 = 192

# check xr, r and corr
table(dat_combi$catch) / nrow(dat_combi)    # 25% catch trials; 1 is catch
sum(dat_combi$corr == 1) / nrow(dat_combi)  # corr: response, correct = 1
all((dat_combi$xr == dat_combi$r) == (dat_combi$corr == 1))
table(dat_combi$xr)                 # expected response (1-4)
table(dat_combi$r)                  # response (0-4); 0 is timeout
all((dat_combi$r == 0) == (dat_combi$rt == 5000))   # 0 is timeout
table(dat_combi$stimulus)           # color word (0-3)
table(dat_combi$stimuluscolor)      # color of middle letter (0-3) [i.e. target]
table(dat_combi$stimulusposition)   # stimulusposition on screen (0-3)

# verify stimulus, stroop and flanker congruencies
temp <- dat_combi[dat_combi$catch != 1, ]     # exclude catch

# stimuluscolor is color of central letter and determines expected response
all(temp$stimuluscolor+1 == temp$xr)

# stroop is stroop-congruency; 1 is congruent
all((temp$stimulus == temp$stimuluscolor) == (temp$stroop == 1))
# simon seems to be simon-congruency; 1 is congruent
all((temp$stimulus == temp$stimulusposition) == (temp$simon == 1))


# create congruency column
dat_combi$CI1 <- ifelse(dat_combi$simon == 1, "congruent", "incongruent")
dat_combi$CI2 <- ifelse(dat_combi$stroop == 1, "congruent", "incongruent")
dat_combi$CI <- ifelse(
  dat_combi$simon == 0 & dat_combi$stroop == 0, "II",
  ifelse(dat_combi$simon == 0 & dat_combi$stroop == 1, "IC",
         ifelse(dat_combi$simon == 1 & dat_combi$stroop == 0, "CI", "CC"))
)

# exclusions: catch trials (n=4416) and timeout responses (n=65)
combi_2 <- dat_combi[(dat_combi$catch != 1) & (dat_combi$r != 0), ]    

combi_2$rt <- combi_2$rt / 1000   # convert to seconds

# exclude RT faster and slower than 3SD from the mean for each condition
mean_rt_cond <- aggregate(rt ~ CI, FUN=mean, combi_2)
sds_cond <- aggregate(rt ~ CI, FUN=sd, combi_2)
cond_mean <- ave(combi_2$rt, combi_2$CI, FUN=mean)
cond_sd <- ave(combi_2$rt, combi_2$CI, FUN=sd)
combi_2 <- combi_2[abs(combi_2$rt - cond_mean) < 3 * cond_sd, ]
# but then RTs = 0 are still included:
min(combi_2$rt)
mean_rt_cond$rt - sds_cond$rt * 3
mean_rt_cond$rt + sds_cond$rt * 3
# exclude fast and slow RTs
combi_2 <- combi_2[combi_2$rt > 0.3 & combi_2$rt < 3, ]   # excl n=8


# rename for EMC2
names(combi_2)[names(combi_2) == "subj"] <- "subjects"
names(combi_2)[names(combi_2) == "r"] <- "R"
combi_2$S <- combi_2$stimuluscolor + 1              # +1 to match S and R

combi_2$position <- combi_2$stimulusposition + 1    # +1 to match S
combi_2$colorword <- combi_2$stimulus + 1           # +1 to match S
combi_2$IS1 <- combi_2$position    # first irrelevant stimulus dimension
combi_2$IS2 <- combi_2$colorword   # second irrelevant stimulus dimension

factor_cols <- c("S", "R", "CI", "subjects", "position", "colorword", "IS1", 
                 "IS2", "CI1", "CI2")
combi_2[factor_cols] <- lapply(combi_2[factor_cols], factor)


# drop unused cols and save
dropcols <- c("re", "catch", "block", "xr", "stimuluscolor", "stimulusposition",
              "stroop", "simon", "stimulus", "position", "colorword")
combi_2 <- combi_2[! names(combi_2) %in% dropcols]
save(combi_2, file = "data/Exp2_clean.RData")



# Experiment 3: Combi blocks ###################################################
files <- list.files(path = "data/Exp3_data/combi", full.names = TRUE)
ldf <- lapply(files, read.table)
dat_combi <- bind_rows(ldf)
colnames(dat_combi) <- c("subj", "block", "stimulus",
                         "flankercolor", "stimulusposition", "simon", "flanker",
                         "xr", "r", "corr", "rt", "re")

length(unique(dat_combi$subj))    # verify subj, N = 24
table(dat_combi$block)            # 4 blocks, 4608 trials each; 4608 / 24 = 192

# check xr, r and corr
sum(dat_combi$corr == 1) / nrow(dat_combi)  # corr: response, correct = 1
all((dat_combi$xr == dat_combi$r) == (dat_combi$corr == 1))
table(dat_combi$xr)                 # expected response (1-4)
table(dat_combi$r)                  # response (1-4); 0 is timeout
all((dat_combi$r == 0) == (dat_combi$rt == 5000))   # 0 is timeout
table(dat_combi$stimulus)           # color of central x (0-3) [i.e. target]
table(dat_combi$flankercolor)       # color of flanking x (0-3)
table(dat_combi$stimulusposition)   # stimulusposition on screen (0-3)

# stimulus is color of central letter and determines expected response
all(dat_combi$stimulus+1 == dat_combi$xr)

# flanker is flanker-congruency; 1 is congruent
all((dat_combi$stimulus == dat_combi$flankercolor) == (dat_combi$flanker == 1))
# simon is simon-congruency; 1 is congruent
all((dat_combi$stimulus == dat_combi$stimulusposition) == (dat_combi$simon == 1))

# create congruency column
dat_combi$CI1 <- ifelse(dat_combi$simon == 1, "congruent", "incongruent")
dat_combi$CI2 <- ifelse(dat_combi$flanker == 1, "congruent", "incongruent")
dat_combi$CI <- ifelse(
  dat_combi$simon == 0 & dat_combi$flanker == 0, "II",
  ifelse(dat_combi$simon == 0 & dat_combi$flanker == 1, "IC",
         ifelse(dat_combi$simon == 1 & dat_combi$flanker == 0, "CI", "CC"))
)


# exclusions
combi_3 <- dat_combi[dat_combi$r != 0, ]    # exclude timeout responses (n=5)

combi_3$rt <- combi_3$rt / 1000   # convert to seconds

# exclude RT faster and slower than 3SD from the mean for each condition
mean_rt_cond <- aggregate(rt ~ CI, FUN=mean, combi_3)
sds_cond <- aggregate(rt ~ CI, FUN=sd, combi_3)
cond_mean <- ave(combi_3$rt, combi_3$CI, FUN=mean)
cond_sd <- ave(combi_3$rt, combi_3$CI, FUN=sd)
combi_3 <- combi_3[abs(combi_3$rt - cond_mean) < 3 * cond_sd, ] # excl. n=356
# but then RTs = 0 are still included:
min(combi_3$rt)
mean_rt_cond$rt - sds_cond$rt * 3
mean_rt_cond$rt + sds_cond$rt * 3
# exclude fast and slow RTs
combi_3 <- combi_3[combi_3$rt > 0.3 & combi_3$rt < 3, ]   # no resps too fast


# rename for EMC2
names(combi_3)[names(combi_3) == "subj"] <- "subjects"
names(combi_3)[names(combi_3) == "r"] <- "R"
combi_3$S <- combi_3$stimulus + 1              # +1 to match S and R

combi_3$position <- combi_3$stimulusposition + 1    # +1 to match S
combi_3$flanking <- combi_3$flankercolor + 1        # +1 to match S

combi_3$IS1 <- combi_3$position     # first irrelevant stimulus dimension
combi_3$IS2 <- combi_3$flanking     # second irrelevant stimulus dimension


factor_cols <- c("S", "R", "CI", "subjects", "position", "flanking", "IS1", 
                 "IS2", "CI1", "CI2")
combi_3[factor_cols] <- lapply(combi_3[factor_cols], factor)


# drop unused cols and save
dropcols <- c("re", "catch", "block", "xr", "flankercolor", "stimulusposition",
              "flanker", "simon", "stimulus", "position", "flanking")
combi_3 <- combi_3[! names(combi_3) %in% dropcols]
save(combi_3, file = "data/Exp3_clean.RData")

