### analysis-predictionCC-w8.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: dec  1 2021 (13:12) 
## Version: 
## Last-Updated: mar 18 2022 (10:52) 
##           By: Brice Ozenne
##     Update #: 152
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

## ** week 8

dfWR.NP1_ccw8 <- dfWR.NP1[rowSums(is.na(dfWR.NP1[,.SD,.SDcols = c("Y_w8",nameR.predictor)]))==0,]

## *** fit models
ff_ccw8 <- Y_w8 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance

e.glm0_ccw8 <- glm(Y_w8 ~ female + age, data = dfWR.NP1_ccw8, family = binomial(link = "logit"))
e.glm_ccw8 <- glm(ff_ccw8, data = dfWR.NP1_ccw8, family = binomial(link = "logit"))
e.ranger_ccw8 <- ranger(ff_ccw8, data = dfWR.NP1_ccw8, probability = TRUE)

## *** assess performance
set.seed(10)
ePerf.ccw8 <- performanceResample(list(glm0_ccw8 = e.glm0_ccw8, glm_ccw8 = e.glm_ccw8, rf_ccw8 = e.ranger_ccw8), data = dfWR.NP1_ccw8,
                                  fold.number = fold.number, fold.size = 0.1,
                                  type.resampling = "permutation", n.resampling = n.resampling, seed = 10,
                                  filename = file.path(path.results,"analysis-predictionCC","perf-imp-week8")
ePerf.ccw8
##    metric     model  estimate se lower upper p.value p.value_comp
## 1:    auc glm0_ccw8 0.5199713 NA    NA    NA   0.161           NA
## 2:    auc  glm_ccw8 0.6720516 NA    NA    NA   0.010           NA
## 3:    auc   rf_ccw8 0.5512482 NA    NA    NA   0.209           NA
## 4:  brier glm0_ccw8 0.2581350 NA    NA    NA   0.275           NA
## 5:  brier  glm_ccw8 0.2407665 NA    NA    NA   0.008           NA
## 6:  brier   rf_ccw8 0.2538983 NA    NA    NA   0.153           NA

## * Export
saveRDS(ePerf.ccw8, file = file.path(path.results,"perf-cc-week8.rds"))

##----------------------------------------------------------------------
### analysis-predictionCC-w8.R ends here
