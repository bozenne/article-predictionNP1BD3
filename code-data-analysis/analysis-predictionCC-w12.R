### analysis-predictionCC-w12.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: dec  1 2021 (13:12) 
## Version: 
## Last-Updated: mar  4 2022 (11:59) 
##           By: Brice Ozenne
##     Update #: 148
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
path.results <- "./results/analysis-prediction"

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

## ** week 12

dfWR.NP1_ccw12 <- dfWR.NP1[rowSums(is.na(dfWR.NP1[,.SD,.SDcols = c("Y_w12",nameR.predictor)]))==0,]

## *** fit models
ff_ccw12 <- Y_w12 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance

e.glm0_ccw12 <- glm(Y_w12 ~ female + age, data = dfWR.NP1_ccw12, family = binomial(link = "logit"))
e.glm_ccw12 <- glm(ff_ccw12, data = dfWR.NP1_ccw12, family = binomial(link = "logit"))
e.ranger_ccw12 <- ranger(ff_ccw12, data = dfWR.NP1_ccw12, probability = TRUE)

## *** assess performance
## performance(list(glm0_ccw12 = e.glm0_ccw12, glm_ccw12 = e.glm_ccw12, rf_ccw12 = ranger(ff_ccw12, data = dfWR.NP1_ccw12, probability = FALSE)),
##                           data = dfWR.NP1_ccw12)
set.seed(10)
ePerf.ccw12 <- performanceResample(list(glm0_ccw12 = e.glm0_ccw12, glm_ccw12 = e.glm_ccw12, rf_ccw12 = e.ranger_ccw12), data = dfWR.NP1_ccw12,
                                  fold.number = fold.number, fold.size = 0.1,
                                  type.resampling = "permutation", n.resampling = n.resampling, seed = 10)
ePerf.ccw12


##    metric      model  estimate se lower upper p.value p.value_comp
## 1:    auc glm0_ccw12 0.5813704 NA    NA    NA   0.070           NA
## 2:    auc  glm_ccw12 0.6136246 NA    NA    NA   0.094           NA
## 3:    auc   rf_ccw12 0.7704071 NA    NA    NA   0.002           NA
## 4:  brier glm0_ccw12 0.1945718 NA    NA    NA   0.087           NA
## 5:  brier  glm_ccw12 0.2115583 NA    NA    NA   0.080           NA
## 6:  brier   rf_ccw12 0.1634158 NA    NA    NA   0.004           NA

## * Export
saveRDS(ePerf.ccw12, file = file.path(path.results,"perf-cc-week12.rds"))

##----------------------------------------------------------------------
### analysis-predictionCC-w12.R ends here
