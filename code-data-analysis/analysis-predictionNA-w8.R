### analysis-predictionNA-w8.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: mar  4 2022 (09:16) 
## Version: 
## Last-Updated: mar  4 2022 (18:19) 
##           By: Brice Ozenne
##     Update #: 20
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

## * Parameters
n.resampling <- 10
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
path.results <- "./results"

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

dfWR.NP1_w8 <- dfWR.NP1[rowSums(is.na(dfWR.NP1[,.SD,.SDcols = c("Y_w8")]))==0,]
## butils::DIM(dfWR.NP1_w8)
## [1] 87 36

## ** week 4
set.seed(10)

## *** fit models
e.glm0_impw8 <- glm(Y_w8 ~ female + age, family = binomial(link = "logit"), data = dfWR.NP1_w8)
e.glm_impw8 <- glm(Y_w8 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance + CATS_scoretotal + CAR_AUCi + neuroticism,
                   family = binomial(link = "logit"), data = dfWR.NP1_w8)
e.ranger_impw8 <- ranger(formula = Y_w8 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance + CATS_scoretotal + CAR_AUCi + neuroticism,
                         data = na.omit(dfWR.NP1_w8), probability = TRUE)

## e.glm0_impw8 <- glm(Y_w8 ~ female + age, family = binomial(link = "logit"), data = dfWR.NP1_w8)
## e.glm_impw8 <- glm(Y_w8 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance,
##                    family = binomial(link = "logit"), data = dfWR.NP1_w8)
## e.ranger_impw8 <- ranger(formula = Y_w8 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance,
##                          data = na.omit(dfWR.NP1_w8), probability = TRUE)

## *** assess performance
ePerf.impw8 <- performanceResample(list(glm0_impw8 = e.glm0_impw8, glm_impw8 = e.glm_impw8, rf_impw8 = e.ranger_impw8), data = dfWR.NP1_w8,
                                   individual.fit = TRUE,
                                   fold.number = fold.number, fold.size = 0.1,
                                   type.resampling = "permutation", n.resampling = n.resampling, seed = 10)
ePerf.impw8

##    metric      model  estimate se lower upper p.value p.value_comp
## 1:    auc glm0_impw8 0.4935480 NA    NA    NA     1.0           NA
## 2:    auc  glm_impw8 0.5544182 NA    NA    NA     0.0           NA
## 3:    auc   rf_impw8 0.5106153 NA    NA    NA     0.3           NA
## 4:  brier glm0_impw8 0.2548968 NA    NA    NA     1.0           NA
## 5:  brier  glm_impw8 0.3557607 NA    NA    NA     0.9           NA
## 6:  brier   rf_impw8 0.2613369 NA    NA    NA     0.3           NA



## * Export
saveRDS(ePerf.impw8, file = file.path(path.results,"perf-imp-week8.rds"))



##----------------------------------------------------------------------
### analysis-predictionNA-w8.R ends here
