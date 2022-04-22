### analysis-predictionCC-w8.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: dec  1 2021 (13:12) 
## Version: 
## Last-Updated: apr 21 2022 (16:03) 
##           By: Brice Ozenne
##     Update #: 172
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
if(dir.exists(file.path(path.output,"analysis-predictionCC-w8"))==FALSE){
    dir.create(file.path(path.output,"analysis-predictionCC-w8"))
}

path.results <- "./results/"
if(dir.exists(path.results)==FALSE){
    dir.create(path.results)
}
if(dir.exists(file.path(path.results,"analysis-predictionCC-w8"))==FALSE){
    dir.create(file.path(path.results,"analysis-predictionCC-w8"))
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

## ** week 8

dfWR.NP1_ccw8 <- dfWR.NP1[rowSums(is.na(dfWR.NP1[,.SD,.SDcols = c("Y_w8",nameR.predictor)]))==0,]

## *** fit models
ff_ccw8 <- Y_w8 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance

e.glm0_ccw8 <- glm(Y_w8 ~ female + age, data = dfWR.NP1_ccw8, family = binomial(link = "logit"))
e.glm_ccw8 <- glm(ff_ccw8, data = dfWR.NP1_ccw8, family = binomial(link = "logit"))
e.ranger_ccw8 <- ranger(formula = ff_ccw8, data = dfWR.NP1_ccw8, probability = TRUE)

## *** assess performance
if(iter_sim==1){
    ePerf.ccw8.IF <- performance(list(glm0_ccw8 = e.glm0_ccw8, glm_ccw8 = e.glm_ccw8, rf_ccw8 = e.ranger_ccw8), data = dfWR.NP1_ccw8,
                                 fold.repetition = fold.repetition, fold.balance = TRUE, fold.size = fold.size, conf.level = 0.95, seed = 10)
    saveRDS(ePerf.ccw8.IF, file = file.path(path.results,"perf-cc-week8-IF.rds"))
    ePerf.ccw8.IF
}
##      method metric     model estimate      se  lower upper p.value p.value_comp
## 1  internal    auc glm0_ccw8   0.6448 0.06260 0.5081 0.752  0.0388             
## 2  internal    auc  glm_ccw8   0.7748 0.05542 0.6427 0.863  <0.001       0.0689
## 3  internal    auc   rf_ccw8   1.0000 0.00000 1.0000 1.000  <0.001       <0.001
## 4  internal  brier glm0_ccw8   0.2379 0.01149 0.2164 0.262                     
## 5  internal  brier  glm_ccw8   0.1939 0.02114 0.1566 0.240               0.0181
## 6  internal  brier   rf_ccw8   0.0971 0.00705 0.0842 0.112               <0.001
## 7        cv    auc glm0_ccw8   0.5862 0.06359 0.4514 0.699  0.1995             
## 8        cv    auc  glm_ccw8   0.6448 0.06207 0.5094 0.752  0.0372       0.4442
## 9        cv    auc   rf_ccw8   0.5366 0.06333 0.4055 0.651  0.5708       0.0692
## 10       cv  brier glm0_ccw8   0.2517 0.01360 0.2264 0.280                     
## 11       cv  brier  glm_ccw8   0.2486 0.02587 0.2028 0.305               0.8916
## 12       cv  brier   rf_ccw8   0.2611 0.01699 0.2299 0.297               0.5719

ePerf.ccw8.perm <- performanceResample(list(glm0_ccw8 = e.glm0_ccw8, glm_ccw8 = e.glm_ccw8, rf_ccw8 = e.ranger_ccw8), data = dfWR.NP1_ccw8,
                                       fold.repetition = fold.repetition, fold.size = fold.size, fold.balance = TRUE,
                                       type.resampling = "permutation", n.resampling = vec.resampling, seed = 10,
                                       filename = file.path(path.results,"analysis-predictionCC-w8",paste0("iter",iter_sim,"-tempo")))
saveRDS(ePerf.ccw8.perm, file = file.path(path.results,"analysis-predictionCC-w8",paste0("iter",iter_sim,"-final.rds")))
ePerf.ccw8.perm                                       
##    metric     model   estimate   resample se.resample p.value p.value_comp
## 1:    auc glm0_ccw8 0.64478764 0.57331789 0.046970186   0.068           NA
## 2:    auc  glm_ccw8 0.77477477 0.69058044 0.047743665   0.031        0.402
## 3:    auc   rf_ccw8 0.99935650 0.99841313 0.002184528   0.548        0.965
## 4:  brier glm0_ccw8 0.23788782 0.24253945 0.006104175   0.812           NA
## 5:  brier  glm_ccw8 0.19392101 0.21949585 0.013511188   0.964        0.048
## 6:  brier   rf_ccw8 0.09780639 0.09908873 0.007115176   0.598        0.975

## * sessionInfo
sessionInfo()

## * Results
if(FALSE){
    ## ePerf.ccw8.IF <- readRDS(file = file.path(path.results,"perf-cc-week8-IF.rds"))
    ## ePerf.ccw8.perm <- readRDS(file = file.path(path.results,"perf-cc-week8-perm.rds"))
}


##----------------------------------------------------------------------
### analysis-predictionCC-w8.R ends here
