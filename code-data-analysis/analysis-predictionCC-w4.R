### analysis-predictionCC-w4.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: dec  1 2021 (13:12) 
## Version: 
## Last-Updated: apr 21 2022 (16:03) 
##           By: Brice Ozenne
##     Update #: 176
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
if(dir.exists(file.path(path.output,"analysis-predictionCC-w4"))==FALSE){
    dir.create(file.path(path.output,"analysis-predictionCC-w4"))
}

path.results <- "./results/"
if(dir.exists(path.results)==FALSE){
    dir.create(path.results)
}
if(dir.exists(file.path(path.results,"analysis-predictionCC-w4"))==FALSE){
    dir.create(file.path(path.results,"analysis-predictionCC-w4"))
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

## * Complete data

## ** week 4
dfWR.NP1_ccw4 <- dfWR.NP1[rowSums(is.na(dfWR.NP1[,.SD,.SDcols = c("Y_w4",nameR.predictor)]))==0,]

## *** fit models
ff_ccw4 <- Y_w4 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance

e.glm0_ccw4 <- glm(Y_w4 ~ female + age, data = dfWR.NP1_ccw4, family = binomial(link = "logit"))
e.glm_ccw4 <- glm(ff_ccw4, data = dfWR.NP1_ccw4, family = binomial(link = "logit"))
e.ranger_ccw4 <- ranger(formula = ff_ccw4, data = dfWR.NP1_ccw4, probability = TRUE)

## *** assess performance
if(iter_sim==1){
    ePerf.ccw4.IF <- performance(list(glm0_ccw4 = e.glm0_ccw4, glm_ccw4 = e.glm_ccw4, rf_ccw4 = e.ranger_ccw4), data = dfWR.NP1_ccw4,
                                 fold.repetition = fold.repetition, fold.balance = TRUE, fold.size = fold.size, conf.level = 0.95, seed = 10)
    saveRDS(ePerf.ccw4.IF, file = file.path(path.results,"perf-cc-week4-IF.rds"))
    ePerf.ccw4.IF
}
##      method metric     model estimate      se  lower upper p.value p.value_comp
## 1  internal    auc glm0_ccw4   0.5885 0.06575 0.4488 0.704  0.2032             
## 2  internal    auc  glm_ccw4   0.6748 0.06211 0.5367 0.780  0.0155      0.17990
## 3  internal    auc   rf_ccw4   0.9987 0.00182 0.9804 1.000  <0.001      < 0.001
## 4  internal  brier glm0_ccw4   0.2282 0.01555 0.1997 0.261                     
## 5  internal  brier  glm_ccw4   0.2174 0.01766 0.1854 0.255              0.30657
## 6  internal  brier   rf_ccw4   0.0974 0.00799 0.0829 0.114              < 0.001
## 7        cv    auc glm0_ccw4   0.5219 0.06659 0.3847 0.642  0.7447             
## 8        cv    auc  glm_ccw4   0.4469 0.06235 0.3227 0.564  0.3865      0.22605
## 9        cv    auc   rf_ccw4   0.5054 0.06632 0.3698 0.626  0.9354      0.25508
## 10       cv  brier glm0_ccw4   0.2431 0.01643 0.2129 0.278                     
## 11       cv  brier  glm_ccw4   0.2804 0.02185 0.2407 0.327              0.00606
## 12       cv  brier   rf_ccw4   0.2561 0.01890 0.2217 0.296              0.11593

## hist(ePerf.ccw4.IF$prediction$cv[,"glm0_ccw4",])
## hist(ePerf.ccw4.IF$prediction$cv[,"glm_ccw4",])

ePerf.ccw4.perm <- performanceResample(list(glm0_ccw4 = e.glm0_ccw4, glm_ccw4 = e.glm_ccw4, rf_ccw4 = e.ranger_ccw4), data = dfWR.NP1_ccw4,
                                       fold.repetition = fold.repetition, fold.size = fold.size, fold.balance = TRUE,
                                       type.resampling = "permutation", n.resampling = vec.resampling, seed = 10,
                                       filename = file.path(path.results,"analysis-predictionCC-w4",paste0("iter",iter_sim,"-tempo")))
saveRDS(ePerf.ccw4.perm, file = file.path(path.results,"analysis-predictionCC-w4",paste0("iter",iter_sim,"-final.rds")))
ePerf.ccw4.perm                                       
##       method metric     model   estimate  resample  se.resample p.value p.value_comp
##  1: internal    auc glm0_ccw4 0.58854510 0.5715602 0.0412351302     0.2           NA
##  2: internal    auc  glm_ccw4 0.67478604 0.6882159 0.0383556566     0.7          0.8
##  3: internal    auc   rf_ccw4 0.99868334 0.9994733 0.0007473946     0.9          0.3
##  4: internal  brier glm0_ccw4 0.22821715 0.2320790 0.0055392593     0.9           NA
##  5: internal  brier  glm_ccw4 0.21744280 0.2117331 0.0110660531     0.5          0.9
##  6: internal  brier   rf_ccw4 0.09737978 0.0985570 0.0058424204     0.7          0.2
##  7:       cv    auc glm0_ccw4 0.52191573 0.4335885 0.0734783168     0.2           NA
##  8:       cv    auc  glm_ccw4 0.44693878 0.4792818 0.0825943418     0.6          0.3
##  9:       cv    auc   rf_ccw4 0.50538512 0.4538028 0.0790312861     0.2          0.6
## 10:       cv  brier glm0_ccw4 0.24309664 0.2460664 0.0062241169     0.8           NA
## 11:       cv  brier  glm_ccw4 0.28036226 0.2733942 0.0147875149     0.5          0.2
## 12:       cv  brier   rf_ccw4 0.25614333 0.2651087 0.0146659225     0.7          0.1

## * sessionInfo
sessionInfo()

## * Results
if(FALSE){
    ## ePerf.ccw4.IF <- readRDS(file = file.path(path.results,"perf-cc-week4-IC.rds"))
    ## ePerf.ccw4.perm <- readRDS(file = file.path(path.results,"perf-cc-week4-perm.rds"))
}

##----------------------------------------------------------------------
### analysis-predictionCC-w4.R ends here
