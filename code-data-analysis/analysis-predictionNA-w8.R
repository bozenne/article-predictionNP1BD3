### analysis-predictionNA-w8.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: mar  4 2022 (09:16) 
## Version: 
## Last-Updated: okt 27 2025 (18:16) 
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
    ## source("code-data-analysis/analysis-predictionNA-w8.R")
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
if(dir.exists(file.path(path.output,"analysis-predictionNA-w8"))==FALSE){
    dir.create(file.path(path.output,"analysis-predictionNA-w8"))
}

path.results <- "./results/"
if(dir.exists(path.results)==FALSE){
    dir.create(path.results)
}
if(dir.exists(file.path(path.results,"analysis-predictionNA-w8"))==FALSE){
    dir.create(file.path(path.results,"analysis-predictionNA-w8"))
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

dfWR.NP1_w8 <- dfWR.NP1[rowSums(is.na(dfWR.NP1[,.SD,.SDcols = c("Y_w8")]))==0,]
## butils::DIM(dfWR.NP1_w8)
## [1] 88 37

## ** week 8
set.seed(10)

## *** fit models
e.glm0_impw8 <- glm(Y_w8 ~ female + age, family = binomial(link = "logit"), data = dfWR.NP1_w8)
## e.glm_impw8 <- miss.glm(Y_w8 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance + CATS_scoretotal + CAR_AUCi + neuroticism, data = dfWR.NP1_w8,
##                  control = list(print_iter = FALSE, tol_em = 1e-5, ll_obs_cal = FALSE))
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
if(iter_sim==1){
    ePerf.impw8.IF <- performance(list(glm0_impw8 = e.glm0_impw8, glm_impw8 = e.glm_impw8, rf_impw8 = e.ranger_impw8), data = dfWR.NP1_w8,
                                  fold.repetition = fold.repetition, fold.balance = TRUE, fold.size = fold.size,
                                  individual.fit = TRUE, impute = "mean",
                                  conf.level = 0.95, seed = 10)
    saveRDS(ePerf.impw8.IF, file = file.path(path.results,"perf-imp-week8-IF.rds"))
    ePerf.impw8.IF
}
##      method metric     model estimate      se  lower upper p.value p.value_comp
## 1  internal    auc glm0_ccw8   0.6156 0.06058 0.4858 0.722  0.0785             
## 2  internal    auc glm_impw8   0.7786 0.05356 0.6513 0.864  <0.001      0.01970
## 3  internal    auc  rf_impw8   1.0000 0.00000 1.0000 1.000  <0.001      < 0.001
## 4  internal  brier glm0_ccw8   0.2408 0.00968 0.2225 0.261                     
## 5  internal  brier glm_impw8   0.1921 0.02044 0.1559 0.237              0.00808
## 6  internal  brier  rf_impw8   0.0915 0.00541 0.0815 0.103              < 0.001
## 7        cv    auc glm0_ccw8   0.5497 0.06068 0.4236 0.659  0.4255             
## 8        cv    auc glm_impw8   0.6213 0.06037 0.4916 0.727  0.0656      0.34590
## 9        cv    auc  rf_impw8   0.4750 0.06030 0.3535 0.587  0.6751      0.00605
## 10       cv  brier glm0_ccw8   0.2535 0.01141 0.2321 0.277                     
## 11       cv  brier glm_impw8   0.2575 0.02522 0.2125 0.312              0.85815
## 12       cv  brier  rf_impw8   0.2702 0.01431 0.2435 0.300              0.52571

ePerf.impw8.perm <- performanceResample(list(glm0_impw8 = e.glm0_impw8, glm_impw8 = e.glm_impw8, rf_impw8 = e.ranger_impw8), data = dfWR.NP1_w8,
                                        fold.repetition = fold.repetition, fold.balance = TRUE, fold.size = fold.size,
                                        individual.fit = TRUE, impute = "mean",
                                        type.resampling = "permutation", n.resampling = vec.resampling, seed = 10,
                                        filename = file.path(path.results,"analysis-predictionNA-w8",paste0("iter",iter_sim,"-tempo")))
saveRDS(ePerf.impw8.perm, file = file.path(path.results,"analysis-predictionNA-w8",paste0("iter",iter_sim,"-final.rds")))
ePerf.impw8.perm

## * sessionInfo
sessionInfo()

## * Bonus
if(FALSE){

    xxx <- misaem::miss.glm(Y_w8 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance + CATS_scoretotal + CAR_AUCi + neuroticism, data = dfWR.NP1_w8,
                            control = list(print_iter = FALSE, tol_em = 1e-5, ll_obs_cal = FALSE))


    ePerf.impw8.bis <- performance(list(glm0_impw8 = e.glm0_impw8, glm_impw8 = xxx, rf_impw8 = e.ranger_impw8), data = dfWR.NP1_w8,
                                   individual.fit = TRUE,
                                   fold.number = 1, fold.size = 0.1, seed = 10)

    ePerf.impw8.bis <- performance(list(glm0_impw8 = e.glm0_impw8, glm_impw8 = e.glm_impw8, rf_impw8 = e.ranger_impw8), data = dfWR.NP1_w8,
                                   individual.fit = TRUE,
                                   fold.number = 1, fold.size = 0.1, seed = 10)


    ePerf.impw8.bis <- performance(list(glm2_impw8 = xxx, rf_impw8 = e.ranger_impw8), data = dfWR.NP1_w8,
                                   individual.fit = TRUE,
                                   fold.number = 1, fold.size = 0.1, seed = 10)


    
    for(iSplit in 1:10){ ## iSplit <- 3
        test1 <- prediction[index[,"split"]==iSplit,"glm0_impw8"]
        GS1 <- predict(update(e.glm0_impw8, data = dfWR.NP1_w8[index[index[,"split"]!=iSplit,"observation"],]),
                       newdata = dfWR.NP1_w8[index[index[,"split"]==iSplit,"observation"],],
                       type = "response")
        print(range(test1-GS1))
        ## test - dfWR.NP1_w8[index[index[,"split"]==iSplit,"observation"], Y_w8]

    }

    dataSplit <- dfWR.NP1_w8[index[index[,"split"]==iSplit,"observation"],]
    GS2 <- NULL
    for(iData in 1:NROW(dataSplit)){ ## iData <- 1
        test.NA <- unlist(lapply(as.list(dataSplit[iData,.SD, .SDcols = all.vars(formula(e.glm_impw8))]), is.na))
        rm.var <- names(which(test.NA))

        fff <- formula(e.glm_impw8)
        if(length(rm.var)>0){
            fff <- update(fff, paste0(".~.-", paste0(rm.var, collapse = "-")))
        }


        
        xxx <- miss.glm(fff, data = dfWR.NP1_w8[index[index[,"split"]!=iSplit,"observation"],],
                        control = list(print_iter = FALSE, tol_em = 1e-5, ll_obs_cal = FALSE))
        lava::expit(as.double(model.matrix(fff, dataSplit[iData,]) %*% coef(xxx)))
        
        
        GS2 <- c(GS2,predict(update(e.glm_impw8, data = dfWR.NP1_w8[index[index[,"split"]!=iSplit,"observation"],],
                                  formula = fff),
                           newdata = dataSplit[iData,],
                           type = "response"))
    }
    test2 <- prediction[index[,"split"]==iSplit,"glm_impw8"]
    print(range(GS2 - test2))
    ## test2 - dfWR.NP1_w8[index[index[,"split"]==iSplit,"observation"], Y_w8]
    mean((test1 - dfWR.NP1_w8[index[index[,"split"]==iSplit,"observation"], Y_w8])^2)
    mean((test2 - dfWR.NP1_w8[index[index[,"split"]==iSplit,"observation"], Y_w8])^2)
}

##----------------------------------------------------------------------
### analysis-predictionNA-w8.R ends here
