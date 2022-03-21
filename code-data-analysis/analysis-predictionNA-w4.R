### analysis-predictionNA-w4.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: mar  4 2022 (09:16) 
## Version: 
## Last-Updated: mar 18 2022 (18:16) 
##           By: Brice Ozenne
##     Update #: 33
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

## * Parameters
n.resampling <- 2500 ## 1000
fold.number <- 25 ## 25

## * Path
if(system("whoami",intern=TRUE)=="hpl802"){
    ## nothing: on the server
    ## cd ucph/hdir/SundKonsolidering_BioStatHome/Cluster/BrainDrug-WP3/
    ## setwd("h:/SundKonsolidering_BioStatHome/Cluster/BrainDrug-WP3/")
    ## source("code-data-analysis/analysis-predictionNA-w4.R")
    
}else if(system("whoami",intern=TRUE)=="unicph\\hpl802"){
    setwd("c:/Users/hpl802/Documents/Github/article-predictionNP1BD3/")
}else{ ## 
    setwd("Vibeke put your path here")
}
path.code <- "./code-data-analysis"
path.results <- "./results"

## * Packages and function
library(data.table)
setDTthreads(1)
library(pROC)
library(ranger)
library(splines)
library(BuyseTest)
library(mice)
library(misaem)
## devtools::install_github("NightingaleHealth/ggforestplot")

## * Load data
source(file.path(path.code,"0-data-management.R"))

name.predictor <- c("sex","age","MR_OFCthick","HAMD17","hsCRP","lvpet","cognitive_cluster","EEG_vigilance","CATS_scoretotal","CAR_AUCi","neuroticism")
nameT.predictor <- c("MR_OFCthick","HAMD17","hsCRP","lvpet","cognitive_cluster","EEG_vigilance","CATS_scoretotal","CAR_AUCi","neuroticism")
nameR.predictor <- c("female","age","MR_OFCthick","HAMD17","low_hsCRP","lvpet","cognitive_cluster2","cognitive_cluster3","EEG_vigilance")
nameRT.predictor <- c("MR_OFCthick","HAMD17","low_hsCRP","lvpet","cognitive_cluster2","cognitive_cluster3","EEG_vigilance")

## * Missing values

dfWR.NP1_w4 <- dfWR.NP1[rowSums(is.na(dfWR.NP1[,.SD,.SDcols = c("Y_w4")]))==0,]
## butils::DIM(dfWR.NP1_w4)
## [1] 87 36

## ** week 4
set.seed(10)

## *** fit models
e.glm0_impw4 <- glm(Y_w4 ~ female + age, family = binomial(link = "logit"), data = dfWR.NP1_w4)
## e.glm_impw4 <- miss.glm(Y_w4 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance + CATS_scoretotal + CAR_AUCi + neuroticism, data = dfWR.NP1_w4,
##                  control = list(print_iter = FALSE, tol_em = 1e-5, ll_obs_cal = FALSE))
e.glm_impw4 <- glm(Y_w4 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance + CATS_scoretotal + CAR_AUCi + neuroticism,
                   family = binomial(link = "logit"), data = dfWR.NP1_w4)
e.ranger_impw4 <- ranger(formula = Y_w4 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance + CATS_scoretotal + CAR_AUCi + neuroticism,
                         data = na.omit(dfWR.NP1_w4), probability = TRUE)


## *** assess performance
ePerf.impw4 <- performanceResample(list(glm0_impw4 = e.glm0_impw4, glm_impw4 = e.glm_impw4, rf_impw4 = e.ranger_impw4), data = dfWR.NP1_w4,
                                   individual.fit = TRUE,
                                   fold.number = fold.number, fold.size = 0.1,
                                   type.resampling = "permutation", n.resampling = n.resampling, seed = 10,
                                   filename = file.path(path.results,"analysis-predictionNA","perf-imp-week4"))
ePerf.impw4
##    metric      model  estimate se lower upper p.value p.value_comp
## 1:    auc glm0_impw4 0.4911983 NA    NA    NA     0.8           NA
## 2:    auc  glm_impw4 0.4780392 NA    NA    NA     0.8           NA
## 3:    auc   rf_impw4 0.4910240 NA    NA    NA     0.6           NA
## 4:  brier glm0_impw4 0.2550562 NA    NA    NA     0.9           NA
## 5:  brier  glm_impw4 0.3448574 NA    NA    NA     0.9           NA
## 6:  brier   rf_impw4 0.2632697 NA    NA    NA     0.6           NA

## * Export
saveRDS(ePerf.impw4, file = file.path(path.results,"perf-imp-week4.rds"))



##----------------------------------------------------------------------
### analysis-predictionNA-w4.R ends here
