### analysis-predictionNA-traj.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: mar  4 2022 (09:16) 
## Version: 
## Last-Updated: apr 21 2022 (16:03) 
##           By: Brice Ozenne
##     Update #: 58
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

## * Parameters
n.resampling <- 100 ## 10000
fold.size <- 0.1
fold.repetition <- 50

iter_sim <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
n.iter_sim <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_COUNT"))
if(is.na(iter_sim)){iter_sim <- 1}
if(is.na(n.iter_sim)){n.iter_sim <- 10}

vec.resampling <- (1+n.resampling*(iter_sim-1)):(n.resampling*iter_sim)
cat("iteration ",iter_sim," over ",n.iter_sim,"\n", sep = "")
cat("vec.repetition:\n")
print(vec.resampling)
cat("\n")

## * Path
if(system("whoami",intern=TRUE)=="hpl802"){
    ## nothing: on the server
    ## cd ucph/hdir/SundKonsolidering_BioStatHome/Cluster/BrainDrug-WP3/
    ## setwd("h:/SundKonsolidering_BioStatHome/Cluster/BrainDrug-WP3/")
    ## source("code-data-analysis/analysis-predictionNA-traj.R")
}else if(system("whoami",intern=TRUE)=="unicph\\hpl802"){
    setwd("c:/Users/hpl802/Documents/Github/article-predictionNP1BD3/")
}else{ ## 
    setwd("Vibeke put your path here")
}
path.code <- "./code-data-analysis"
path.output <- "./output/"
if(dir.exists(path.output)==FALSE){
    dir.create(path.output)
}
if(dir.exists(file.path(path.output,"analysis-predictionNA-traj"))==FALSE){
    dir.create(file.path(path.output,"analysis-predictionNA-traj"))
}

path.results <- "./results/"
if(dir.exists(path.results)==FALSE){
    dir.create(path.results)
}
if(dir.exists(file.path(path.results,"analysis-predictionNA-traj"))==FALSE){
    dir.create(file.path(path.results,"analysis-predictionNA-traj"))
}

## * Packages and function
library(data.table)
setDTthreads(1)
library(pROC)
library(ranger)
library(splines)
library(BuyseTest)
library(mice)
## devtools::install_github("NightingaleHealth/ggforestplot")

## * Load data
source(file.path(path.code,"0-data-management.R"))
df.traj <- readRDS(file = file.path(path.results,"traj.Rdata"))

name.predictor <- c("sex","age","MR_OFCthick","HAMD17","hsCRP","lvpet","cognitive_cluster","EEG_vigilance","CATS_scoretotal","CAR_AUCi","neuroticism")
nameT.predictor <- c("MR_OFCthick","HAMD17","hsCRP","lvpet","cognitive_cluster","EEG_vigilance","CATS_scoretotal","CAR_AUCi","neuroticism")
nameR.predictor <- c("female","age","MR_OFCthick","HAMD17","low_hsCRP","lvpet","cognitive_cluster2","cognitive_cluster3","EEG_vigilance")
nameRT.predictor <- c("MR_OFCthick","HAMD17","low_hsCRP","lvpet","cognitive_cluster2","cognitive_cluster3","EEG_vigilance")

## * Missing values
dfWR.NP1_traj <- merge(dfWR.NP1,
                       df.traj, by = "CIMBI_ID")
dfWR.NP1_traj$Y_traj <- (dfWR.NP1_traj$class == 2)
## butils::DIM(dfWR.NP1_traj)
## [1] 87 36

## ** trajectory
set.seed(10)

## *** fit models
e.glm0_imptraj <- glm(Y_traj ~ female + age, family = binomial(link = "logit"), data = dfWR.NP1_traj)
## e.glm_imptraj <- miss.glm(Y_traj ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance + CATS_scoretotal + CAR_AUCi + neuroticism, data = dfWR.NP1_traj,
##                  control = list(print_iter = FALSE, tol_em = 1e-5, ll_obs_cal = FALSE))
e.glm_imptraj <- glm(Y_traj ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance + CATS_scoretotal + CAR_AUCi + neuroticism,
                   family = binomial(link = "logit"), data = dfWR.NP1_traj)
e.ranger_imptraj <- ranger(formula = Y_traj ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance + CATS_scoretotal + CAR_AUCi + neuroticism,
                         data = na.omit(dfWR.NP1_traj), probability = TRUE)

## e.glm0_imptraj <- glm(Y_traj ~ female + age, family = binomial(link = "logit"), data = dfWR.NP1_traj)
## e.glm_imptraj <- glm(Y_traj ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance,
##                    family = binomial(link = "logit"), data = dfWR.NP1_traj)
## e.ranger_imptraj <- ranger(formula = Y_traj ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance,
##                          data = na.omit(dfWR.NP1_traj), probability = TRUE)

## *** assess performance
if(iter_sim==1){
    ePerf.imptraj.IF <- performance(list(glm0_cctraj = e.glm0_imptraj, glm_imptraj = e.glm_imptraj, rf_imptraj = e.ranger_imptraj), data = dfWR.NP1_traj,
                                    fold.repetition = fold.repetition, fold.balance = TRUE, fold.size = fold.size,
                                    individual.fit = TRUE, impute = "mean",
                                    conf.level = 0.95, seed = 10)
    saveRDS(ePerf.imptraj.IF, file = file.path(path.results,"perf-imp-traj-IF.rds"))
    ePerf.imptraj.IF
}

ePerf.imptraj.perm <- performanceResample(list(glm0_imptraj = e.glm0_imptraj, glm_imptraj = e.glm_imptraj, rf_imptraj = e.ranger_imptraj), data = dfWR.NP1_traj,
                                          fold.repetition = fold.repetition, fold.balance = TRUE, fold.size = fold.size,
                                          individual.fit = TRUE, impute = "mean",
                                          type.resampling = "permutation", n.resampling = vec.resampling, seed = 10,
                                          filename = file.path(path.results,"analysis-predictionNA-traj",paste0("iter",iter_sim,"-tempo")))
saveRDS(ePerf.imptraj.perm, file = file.path(path.results,"analysis-predictionNA-traj",paste0("iter",iter_sim,"-final.rds")))
ePerf.imptraj.perm

## * sessionInfo
sessionInfo()


##----------------------------------------------------------------------
### analysis-predictionNA-w4812.R ends here
