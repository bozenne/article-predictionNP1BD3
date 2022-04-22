### analysis-predictionNA-w12.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: mar  4 2022 (09:16) 
## Version: 
## Last-Updated: apr 21 2022 (16:03) 
##           By: Brice Ozenne
##     Update #: 56
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
    ## source("code-data-analysis/analysis-predictionNA-w12.R")
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
if(dir.exists(file.path(path.output,"analysis-predictionNA-w12"))==FALSE){
    dir.create(file.path(path.output,"analysis-predictionNA-w12"))
}

path.results <- "./results/"
if(dir.exists(path.results)==FALSE){
    dir.create(path.results)
}
if(dir.exists(file.path(path.results,"analysis-predictionNA-w12"))==FALSE){
    dir.create(file.path(path.results,"analysis-predictionNA-w12"))
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

name.predictor <- c("sex","age","MR_OFCthick","HAMD17","hsCRP","lvpet","cognitive_cluster","EEG_vigilance","CATS_scoretotal","CAR_AUCi","neuroticism")
nameT.predictor <- c("MR_OFCthick","HAMD17","hsCRP","lvpet","cognitive_cluster","EEG_vigilance","CATS_scoretotal","CAR_AUCi","neuroticism")
nameR.predictor <- c("female","age","MR_OFCthick","HAMD17","low_hsCRP","lvpet","cognitive_cluster2","cognitive_cluster3","EEG_vigilance")
nameRT.predictor <- c("MR_OFCthick","HAMD17","low_hsCRP","lvpet","cognitive_cluster2","cognitive_cluster3","EEG_vigilance")

## * Missing values

dfWR.NP1_w12 <- dfWR.NP1[rowSums(is.na(dfWR.NP1[,.SD,.SDcols = c("Y_w12")]))==0,]
## butils::DIM(dfWR.NP1_w12)
## [1] 87 36

## ** week 4
set.seed(10)

## *** fit models
e.glm0_impw12 <- glm(Y_w12 ~ female + age, family = binomial(link = "logit"), data = dfWR.NP1_w12)
## e.glm_impw12 <- miss.glm(Y_w12 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance + CATS_scoretotal + CAR_AUCi + neuroticism, data = dfWR.NP1_w12,
##                  control = list(print_iter = FALSE, tol_em = 1e-5, ll_obs_cal = FALSE))
e.glm_impw12 <- glm(Y_w12 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance + CATS_scoretotal + CAR_AUCi + neuroticism,
                   family = binomial(link = "logit"), data = dfWR.NP1_w12)
e.ranger_impw12 <- ranger(formula = Y_w12 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance + CATS_scoretotal + CAR_AUCi + neuroticism,
                         data = na.omit(dfWR.NP1_w12), probability = TRUE)

## e.glm0_impw12 <- glm(Y_w12 ~ female + age, family = binomial(link = "logit"), data = dfWR.NP1_w12)
## e.glm_impw12 <- glm(Y_w12 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance,
##                    family = binomial(link = "logit"), data = dfWR.NP1_w12)
## e.ranger_impw12 <- ranger(formula = Y_w12 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance,
##                          data = na.omit(dfWR.NP1_w12), probability = TRUE)

## *** assess performance
if(iter_sim==1){
    ePerf.impw12.IF <- performance(list(glm0_impw12 = e.glm0_impw12, glm_impw12 = e.glm_impw12, rf_impw12 = e.ranger_impw12), data = dfWR.NP1_w12,
                                   fold.repetition = fold.repetition, fold.balance = TRUE, fold.size = fold.size,
                                   individual.fit = TRUE, impute = "mean",
                                   conf.level = 0.95, seed = 10)
    saveRDS(ePerf.impw12.IF, file = file.path(path.results,"perf-imp-week12-IF.rds"))
    ePerf.impw12.IF
}

ePerf.impw12.perm <- performanceResample(list(glm0_impw12 = e.glm0_impw12, glm_impw12 = e.glm_impw12, rf_impw12 = e.ranger_impw12), data = dfWR.NP1_w12,
                                         fold.repetition = fold.repetition, fold.balance = TRUE, fold.size = fold.size,
                                         individual.fit = TRUE, impute = "mean",
                                         type.resampling = "permutation", n.resampling = vec.resampling, seed = 10,
                                         filename = file.path(path.results,"analysis-predictionNA-w12",paste0("iter",iter_sim,"-tempo")))
saveRDS(ePerf.impw12.perm, file = file.path(path.results,"analysis-predictionNA-w12",paste0("iter",iter_sim,"-final.rds")))
ePerf.impw12.perm

## * sessionInfo
sessionInfo()



##----------------------------------------------------------------------
### analysis-predictionNA-w12.R ends here
