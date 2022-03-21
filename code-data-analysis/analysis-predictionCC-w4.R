### analysis-predictionCC-w4.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: dec  1 2021 (13:12) 
## Version: 
## Last-Updated: mar 18 2022 (10:52) 
##           By: Brice Ozenne
##     Update #: 155
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

## * Parameters
n.resampling <- 10000
fold.number <- 25

## * Path
if(system("whoami",intern=TRUE)=="hpl802"){
    ## nothing: on the server
}else if(system("whoami",intern=TRUE)=="unicph\\hpl802"){
    setwd("c:/Users/hpl802/Documents/Github/article-predictionNP1BD3/")
}else{ ## 
    setwd("Vibeke put your path here")
}
path.code <- "./code-data-analysis"
path.results <- "./results/"

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

## * Complete data

## ** week 4
dfWR.NP1_ccw4 <- dfWR.NP1[rowSums(is.na(dfWR.NP1[,.SD,.SDcols = c("Y_w4",nameR.predictor)]))==0,]

## *** fit models
ff_ccw4 <- Y_w4 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance

e.glm0_ccw4 <- glm(Y_w4 ~ female + age, data = dfWR.NP1_ccw4, family = binomial(link = "logit"))
e.glm_ccw4 <- glm(ff_ccw4, data = dfWR.NP1_ccw4, family = binomial(link = "logit"))
e.ranger_ccw4 <- ranger(ff_ccw4, data = dfWR.NP1_ccw4, probability = TRUE)

## *** assess performance
ePerf.ccw4 <- performanceResample(list(glm0_ccw4 = e.glm0_ccw4, glm_ccw4 = e.glm_ccw4, rf_ccw4 = e.ranger_ccw4), data = dfWR.NP1_ccw4,
                                  fold.number = fold.number, fold.size = 0.1,
                                  type.resampling = "permutation", n.resampling = n.resampling, seed = 10,
                                   filename = file.path(path.results,"analysis-predictionCC","perf-imp-week4")
ePerf.ccw4
##    metric     model  estimate se lower upper p.value p.value_comp
## 1:    auc glm0_ccw4 0.5034167 NA    NA    NA   0.195           NA
## 2:    auc  glm_ccw4 0.4795556 NA    NA    NA   0.485           NA
## 3:    auc   rf_ccw4 0.4933611 NA    NA    NA   0.444           NA
## 4:  brier glm0_ccw4 0.2456768 NA    NA    NA   0.248           NA
## 5:  brier  glm_ccw4 0.2783404 NA    NA    NA   0.463           NA
## 6:  brier   rf_ccw4 0.2600662 NA    NA    NA   0.481           NA


## * Export
saveRDS(ePerf.ccw4, file = file.path(path.results,"perf-cc-week4.rds"))

## * Results
if(FALSE){
    ## readRDS(file = file.path("results","analysis-prediction","perf-cc-week4.rds"))
    ## readRDS(file = file.path("results","analysis-prediction","perf-cc-week8.rds"))
    ## readRDS(file = file.path("results","analysis-prediction","perf-cc-week12.rds"))
}

##----------------------------------------------------------------------
### analysis-predictionCC-w4.R ends here
