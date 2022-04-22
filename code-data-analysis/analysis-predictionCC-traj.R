### analysis-predictionCC-traj.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: dec  1 2021 (13:12) 
## Version: 
## Last-Updated: apr 21 2022 (16:02) 
##           By: Brice Ozenne
##     Update #: 183
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
    ## source("code-data-analysis/analysis-predictionCC-traj.R")
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
if(dir.exists(file.path(path.output,"analysis-predictionCC-traj"))==FALSE){
    dir.create(file.path(path.output,"analysis-predictionCC-traj"))
}

path.results <- "./results/"
if(dir.exists(path.results)==FALSE){
    dir.create(path.results)
}
if(dir.exists(file.path(path.results,"analysis-predictionCC-traj"))==FALSE){
    dir.create(file.path(path.results,"analysis-predictionCC-traj"))
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

## * Complete data

## ** week 12

dfWR.NP1_cctraj <- merge(dfWR.NP1[rowSums(is.na(dfWR.NP1[,.SD,.SDcols = nameR.predictor]))==0,],
                         df.traj, by = "CIMBI_ID")
dfWR.NP1_cctraj$Y_traj <- (dfWR.NP1_cctraj$class == 2)

## *** fit models
ff_cctraj <- Y_traj ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance

e.glm0_cctraj <- glm(Y_traj ~ female + age, data = dfWR.NP1_cctraj, family = binomial(link = "logit"))
e.glm_cctraj <- glm(ff_cctraj, data = dfWR.NP1_cctraj, family = binomial(link = "logit"))
e.ranger_cctraj <- ranger(formula = ff_cctraj, data = dfWR.NP1_cctraj, probability = TRUE)

## *** assess performance
if(iter_sim==1){
    ePerf.cctraj.IF <- performance(list(glm0_cctraj = e.glm0_cctraj, glm_cctraj = e.glm_cctraj, rf_cctraj = e.ranger_cctraj), data = dfWR.NP1_cctraj,
                                   fold.repetition = fold.repetition, fold.balance = TRUE, fold.size = fold.size, conf.level = 0.95, seed = 10)
    saveRDS(ePerf.cctraj.IF, file = file.path(path.results,"perf-cc-traj-IF.rds"))
    ePerf.cctraj.IF
}
##      method metric      model estimate      se  lower  upper p.value p.value_comp
## 1  internal    auc glm0_cctraj   0.6602 0.06467 0.5172 0.7699 0.02982             
## 2  internal    auc  glm_cctraj   0.7552 0.05802 0.6188 0.8486 < 0.001       0.1396
## 3  internal    auc   rf_cctraj   1.0000 0.00000 1.0000 1.0000 < 0.001       <0.001
## 4  internal  brier glm0_cctraj   0.1939 0.02046 0.1577 0.2385                     
## 5  internal  brier  glm_cctraj   0.1699 0.02271 0.1307 0.2207               0.1276
## 6  internal  brier   rf_cctraj   0.0686 0.00823 0.0543 0.0868               <0.001
## 7        cv    auc glm0_cctraj   0.6109 0.06671 0.4673 0.7267 0.12371             
## 8        cv    auc  glm_cctraj   0.5859 0.06951 0.4378 0.7074 0.24208       0.7048
## 9        cv    auc   rf_cctraj   0.7026 0.05817 0.5718 0.8002 0.00402       0.0343
## 10       cv  brier glm0_cctraj   0.2040 0.02171 0.1656 0.2513                     
## 11       cv  brier  glm_cctraj   0.2273 0.02907 0.1769 0.2921               0.2231
## 12       cv  brier   rf_cctraj   0.1914 0.02184 0.1531 0.2394               0.0251

ePerf.cctraj.perm <- performanceResample(list(glm0_cctraj = e.glm0_cctraj, glm_cctraj = e.glm_cctraj, rf_cctraj = e.ranger_cctraj), data = dfWR.NP1_cctraj,
                                         fold.repetition = fold.repetition, fold.size = fold.size, fold.balance = TRUE,
                                         type.resampling = "permutation", n.resampling = vec.resampling, seed = 10,
                                         filename = file.path(path.results,"analysis-predictionCC-traj",paste0("iter",iter_sim,"-tempo")))
saveRDS(ePerf.cctraj.perm, file = file.path(path.results,"analysis-predictionCC-traj",paste0("iter",iter_sim,"-final.rds")))
ePerf.cctraj.perm                                       
##    metric      model   estimate   resample se.resample p.value p.value_comp
## 1:    auc glm0_cctraj 0.66022544 0.58200000 0.048943997   0.063           NA
## 2:    auc  glm_cctraj 0.75523349 0.70931643 0.050168478   0.181        0.701
## 3:    auc   rf_cctraj 1.00000000 0.99840821 0.002343665   0.475        0.811
## 4:  brier glm0_cctraj 0.19393107 0.20406055 0.005079584   0.949           NA
## 5:  brier  glm_cctraj 0.16985449 0.18354682 0.012414791   0.863        0.321
## 6:  brier   rf_cctraj 0.07130295 0.08731617 0.006412779   0.985        0.457

## * sessionInfo
sessionInfo()

## * Results
if(FALSE){
    ## ePerf.cctraj.IF <- readRDS(file = file.path(path.results,"perf-cc-traj-IF.rds"))
    ## ePerf.cctraj.perm <- readRDS(file = file.path(path.results,"perf-cc-traj-perm.rds"))
}

##----------------------------------------------------------------------
### analysis-predictionCC-traj.R ends here
