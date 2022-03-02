### analysis-prediction.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: dec  1 2021 (13:12) 
## Version: 
## Last-Updated: mar  2 2022 (17:37) 
##           By: Brice Ozenne
##     Update #: 115
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

## * Path
if(system("whoami",intern=TRUE)=="unicph\\hpl802"){
    setwd("c:/Users/hpl802/Documents/Github/article-predictionNP1BD3/")
}else{ ## 
    setwd("Vibeke put you path here")
}
path.code <- "./code-data-analysis"
path.results <- "./results"

## * Packages and function
library(data.table)
setDTthreads(1)
library(mvtnorm)
library(forestControl)
library(selectiveInference)
library(pROC)
library(ranger)
library(splines)
library(BuyseTest)
library(mice)
library(ggplot2)
## devtools::install_github("NightingaleHealth/ggforestplot")
library(ggforestplot)

source(file.path(path.code,"FCT_forestplot.R"))
source(file.path(path.code,"FCT_glhtPool.R"))
name.predictor <- c("sex","age","MR_OFCthick","HAMD17","hsCRP","lvpet","cognitive_cluster","EEG_vigilance","CATS_scoretotal","CAR_AUCi","neuroticism")
nameR.predictor <- c("female","age","MR_OFCthick","HAMD17","low_hsCRP","lvpet","cognitive_cluster2","cognitive_cluster3","EEG_vigilance")

## * Load data
source(file.path(path.code,"0-data-management.R"))



## * Complete data
## ** week 4
## *** assess performance
set.seed(10)
ePerf.ccw4 <- performance(list(glm0_ccw4 = e.glm0_ccw4, glm_ccw4 = e.glm_ccw4, rf_ccw4 = e.ranger_ccw4),
                          data = dfWR.NP1_ccw4, fold.number = 100, fold.size = 0.1)
ePerf.ccw4
##      method metric     model   estimate          se      lower     upper      p.value p.value_comp
## 1  internal    auc glm0_ccw4 0.58680556 0.067302860 0.44366530 0.7049351 2.222716e-01           NA
## 2  internal    auc  glm_ccw4 0.69305556 0.062106054 0.55324473 0.7968469 9.170059e-03 1.111473e-01
## 3  internal    auc   rf_ccw4 0.99791667 0.002612833 0.97586903 0.9998220 3.750462e-06 6.110621e-07
## 4  internal  brier glm0_ccw4 0.22702258 0.017602184 0.19501642 0.2642816           NA           NA
## 5  internal  brier  glm_ccw4 0.21252407 0.019572090 0.17742629 0.2545648           NA 2.141445e-01
## 6  internal  brier   rf_ccw4 0.09768817 0.008326253 0.08265926 0.1154496           NA 1.110223e-15
## 7        cv    auc glm0_ccw4 0.50026389 0.065186250 0.36734343 0.6193880 9.967704e-01           NA
## 8        cv    auc  glm_ccw4 0.46946528 0.061354848 0.34609592 0.5833977 6.146552e-01 6.162546e-01
## 9        cv    auc   rf_ccw4 0.49236458 0.064388406 0.36155437 0.6105074 9.052989e-01 6.606699e-01
## 10       cv  brier glm0_ccw4 0.24690719 0.017305691 0.21521530 0.2832659           NA           NA
## 11       cv  brier  glm_ccw4 0.28051117 0.024045664 0.23712886 0.3318302           NA 3.669000e-02
## 12       cv  brier   rf_ccw4 0.26007552 0.019299611 0.22487116 0.3007913           NA 2.277458e-01

## *** DENSITY PLOT
dt.CVpred_ccw4 <- as.data.table(ePerf.ccw4, type = "prediction-cv", format = "long")
dt.CVpred_ccw4$model.lab <- factor(dt.CVpred_ccw4$model, levels = c("glm0_ccw4","glm_ccw4","rf_ccw4"),
                                   labels = c("logistic model \n no biomarkers", "logistic model \n biomarkers", "random forest \n biomarkers"))
## SANITY CHECK
## with(dt.CVpred_ccw4[model=="glm_ccw4"], auc(outcome, prediction, fold, observation))

ggHist_w4 <- ggplot(dt.CVpred_ccw4,aes(x=prediction, fill = as.factor(outcome)))
ggHist_w4 <- ggHist_w4 + geom_histogram(alpha = 0.5) + labs(x = "Predicted probability of recovery", y = "number of CV predictions", fill = "Observed \n recovery")
ggHist_w4 <- ggHist_w4 + facet_wrap(~model.lab)

mybreaks <- seq(0,1,length.out=20)
dt.hist_ccw4 <- dt.CVpred_ccw4[,.(ls.outcome = list(rep(.SD$outcome[1],length(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$mids))),
                                  ls.model = list(rep(.SD$model.lab[1],length(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$mids))),
                                  ls.pc = list(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$counts/sum(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$counts)),
                                  ls.breaks = list(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$mids)),
                               by = c("outcome","model.lab"), .SDcols = c("prediction","outcome","model.lab")]
dt.hist_ccw4[,c("outcome","model.lab") := NULL]
dt.hist_ccw4 <- as.data.table(lapply(dt.hist_ccw4,unlist))

ggHist2_w4 <- ggplot(dt.hist_ccw4, aes(x=ls.breaks,y=ls.pc,fill=as.character(ls.outcome)))
ggHist2_w4 <- ggHist2_w4 + geom_col(position = "dodge") + labs(x = "Predicted probability of recovery", y = "proportion of CV predictions", fill = "Observed \n recovery")
ggHist2_w4 <- ggHist2_w4 + facet_wrap(~ls.model)
ggHist2_w4

ggDens_w4 <- ggplot(dt.CVpred_ccw4,aes(x=prediction, fill = as.factor(outcome)))
ggDens_w4 <- ggDens_w4 + geom_density(alpha = 0.25, adjust = 0.75) + labs(x = "Predicted probability of recovery", y = "Density of CV predictions", fill = "Observed \n recovery")
ggDens_w4 <- ggDens_w4 + facet_wrap(~model.lab)

if(FALSE){
    ggsave(ggHist2_w4 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm")),
           filename = "./figures/hist-predCV-week4.pdf", width = 14)
    ggsave(ggHist2_w4 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm")),
           filename = "./figures/hist-predCV-week4.png", width = 14)
}

## *** CALIBRATION PLOT
ggCali_w4 <- ggplot(dt.CVpred_ccw4,aes(x=prediction, y = outcome))
ggCali_w4 <- ggCali_w4 + geom_smooth() + geom_rug() + geom_abline(slope = 1, intercept = 0, color = "red")
ggCali_w4 <- ggCali_w4 + facet_wrap(~model.lab)

## *** ROC CURVES
dt.CVroc_ccw4 <- as.data.table(ePerf.ccw4, type = "roc-cv")                                                      
dt.CVroc_ccw4$model.lab <- factor(dt.CVroc_ccw4$model, levels = c("glm0_ccw4","glm_ccw4","rf_ccw4"),
                                   labels = c("logistic model - no biomarkers", "logistic model - biomarkers", "random forest - biomarkers"))
## SANITY CHECK
## ggroc(roc(outcome ~ prediction, data = dt.CVpred_ccw4[fold == 3 & model == "glm_ccw4"]))
## ggplot(dt.CVroc_ccw4[fold==3 & model == "glm_ccw4"], aes(x=1-sp,y=se)) + geom_step() + geom_abline(slope=1,intercept=0,color = "black")

ggROC_w4 <- ggplot(dt.CVroc_ccw4,aes(x=1-sp,color=model.lab))
ggROC_w4 <- ggROC_w4 + geom_step(aes(y=se,group=interaction(fold,model.lab)),alpha=0.1, size = 1) + geom_abline(slope=1,intercept=0,color = "black")
ggROC_w4 <- ggROC_w4 + geom_smooth(aes(y=se,group=model.lab), se = FALSE, size = 3)
ggROC_w4 <- ggROC_w4 + labs(x="1-specificity", y="sensitivity")

if(FALSE){
    ggsave(ggROC_w4 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm")) + labs(color=""),
           filename = "./figures/roc-CV-week4.pdf", width = 14)
    ggsave(ggROC_w4 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm")) + labs(color=""),
           filename = "./figures/roc-CV-week4.png", width = 14)
}


## ** week 8
## *** assess performance
set.seed(10)
ePerf.ccw8 <- performance(list(glm0_ccw8 = e.glm0_ccw8, glm_ccw8 = e.glm_ccw8, rf_ccw8 = e.ranger_ccw8),
                          data = dfWR.NP1_ccw8, fold.number = 100, fold.size = 0.1)
ePerf.ccw8
##      method metric     model   estimate          se      lower     upper      p.value p.value_comp
## 1  internal    auc glm0_ccw8 0.62769010 0.065372873 0.48583237 0.7404929 0.0753593107           NA
## 2  internal    auc  glm_ccw8 0.81133429 0.052319172 0.68202876 0.8920563 0.0001019312 1.130793e-02
## 3  internal    auc   rf_ccw8 1.00000000 0.000000000 1.00000000 1.0000000 0.0000000000 2.801347e-04
## 4  internal  brier glm0_ccw8 0.23870239 0.009886525 0.22009079 0.2588878           NA           NA
## 5  internal  brier  glm_ccw8 0.17485453 0.021244163 0.13780314 0.2218680           NA 1.016948e-03
## 6  internal  brier   rf_ccw8 0.09426488 0.006777196 0.08187524 0.1085294           NA 2.532701e-05
## 7        cv    auc glm0_ccw8 0.52333572 0.062966746 0.39375920 0.6377029 0.7140942014           NA
## 8        cv    auc  glm_ccw8 0.67472740 0.060196096 0.54138807 0.7770317 0.0125103596 5.671532e-02
## 9        cv    auc   rf_ccw8 0.55047346 0.050755558 0.44573656 0.6433601 0.3335094385 2.656139e-02
## 10       cv  brier glm0_ccw8 0.25887419 0.014290417 0.23232748 0.2884542           NA           NA
## 11       cv  brier  glm_ccw8 0.23957752 0.029954201 0.18750851 0.3061055           NA 4.651872e-01
## 12       cv  brier   rf_ccw8 0.25586413 0.012963184 0.23167750 0.2825758           NA 5.361605e-01

## *** DENSITY PLOT
dt.CVpred_ccw8 <- as.data.table(ePerf.ccw8, type = "prediction-cv", format = "long")
dt.CVpred_ccw8$model.lab <- factor(dt.CVpred_ccw8$model, levels = c("glm0_ccw8","glm_ccw8","rf_ccw8"),
                                   labels = c("logistic model \n no biomarkers", "logistic model \n biomarkers", "random forest \n biomarkers"))
## SANITY CHECK
## with(dt.CVpred_ccw8[model=="glm_ccw8"], auc(outcome, prediction, fold, observation))

ggHist_w8 <- ggplot(dt.CVpred_ccw8,aes(x=prediction, fill = as.factor(outcome)))
ggHist_w8 <- ggHist_w8 + geom_histogram(alpha = 0.5) + labs(x = "Predicted probability of recovery", y = "number of CV predictions", fill = "Observed \n recovery")
ggHist_w8 <- ggHist_w8 + facet_wrap(~model.lab)

mybreaks <- seq(0,1,length.out=20)
dt.hist_ccw8 <- dt.CVpred_ccw8[,.(ls.outcome = list(rep(.SD$outcome[1],length(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$mids))),
                                  ls.model = list(rep(.SD$model.lab[1],length(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$mids))),
                                  ls.pc = list(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$counts/sum(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$counts)),
                                  ls.breaks = list(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$mids)),
                               by = c("outcome","model.lab"), .SDcols = c("prediction","outcome","model.lab")]
dt.hist_ccw8[,c("outcome","model.lab") := NULL]
dt.hist_ccw8 <- as.data.table(lapply(dt.hist_ccw8,unlist))

ggHist2_w8 <- ggplot(dt.hist_ccw8, aes(x=ls.breaks,y=ls.pc,fill=as.character(ls.outcome)))
ggHist2_w8 <- ggHist2_w8 + geom_col(position = "dodge") + labs(x = "Predicted probability of recovery", y = "proportion of CV predictions", fill = "Observed \n recovery")
ggHist2_w8 <- ggHist2_w8 + facet_wrap(~ls.model)
ggHist2_w8

ggDens_w8 <- ggplot(dt.CVpred_ccw8,aes(x=prediction, fill = as.factor(outcome)))
ggDens_w8 <- ggDens_w8 + geom_density(alpha = 0.25, adjust = 0.75) + labs(x = "Predicted probability of recovery", y = "Density of CV predictions", fill = "Observed \n recovery")
ggDens_w8 <- ggDens_w8 + facet_wrap(~model.lab)

if(FALSE){
    ggsave(ggHist2_w8 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm")),
           filename = "./figures/hist-predCV-week8.pdf")
    ggsave(ggHist2_w8 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm")),
           filename = "./figures/hist-predCV-week8.png")
}


## *** CALIBRATION PLOT
ggCali_w8 <- ggplot(dt.CVpred_ccw8,aes(x=prediction, y = outcome))
ggCali_w8 <- ggCali_w8 + geom_smooth() + geom_rug() + geom_abline(slope = 1, intercept = 0, color = "red")
ggCali_w8 <- ggCali_w8 + facet_wrap(~model.lab)

## *** ROC CURVES
dt.CVroc_ccw8 <- as.data.table(ePerf.ccw8, type = "roc-cv")                                                      
dt.CVroc_ccw8$model.lab <- factor(dt.CVroc_ccw8$model, levels = c("glm0_ccw8","glm_ccw8","rf_ccw8"),
                                   labels = c("logistic model - no biomarkers", "logistic model - biomarkers", "random forest - biomarkers"))
## SANITY CHECK
## ggroc(roc(outcome ~ prediction, data = dt.CVpred_ccw8[fold == 3 & model == "glm_ccw8"]))
## ggplot(dt.CVroc_ccw8[fold==3 & model == "glm_ccw8"], aes(x=1-sp,y=se)) + geom_step() + geom_abline(slope=1,intercept=0,color = "black")

ggROC_w8 <- ggplot(dt.CVroc_ccw8,aes(x=1-sp,color=model.lab))
ggROC_w8 <- ggROC_w8 + geom_step(aes(y=se,group=interaction(fold,model.lab)),alpha=0.1, size = 1) + geom_abline(slope=1,intercept=0,color = "black")
ggROC_w8 <- ggROC_w8 + geom_smooth(aes(y=se,group=model.lab), se = FALSE, size = 3)
ggROC_w8 <- ggROC_w8 + labs(x="1-specificity", y="sensitivity")

if(FALSE){
    ggsave(ggROC_w8 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm")) + labs(color=""),
           filename = "../figures/roc-CV-week8.pdf")
    ggsave(ggROC_w8 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm")) + labs(color=""),
           filename = "../figures/roc-CV-week8.png")
}

## *** permutation test
n.perm <- 100
ls.resperm <- vector(mode = "list", length = n.perm)

warper_w8 <- function(i, trace = FALSE, fold.number){
    iData <- copy(dfWR.NP1_ccw8)
    iData$Y_w8 <- sample(iData$Y_w8)
    iEPerf <- suppressWarnings(performance(list(glm0_ccw8 = e.glm0_ccw8, glm_ccw8 = e.glm_ccw8, rf_ccw8 = e.ranger_ccw8),
                                           data = iData, fold.number = fold.number, fold.size = 0.1, trace = trace))
    return(cbind(perm = i, as.data.table(iEPerf, type = "metric")))
}
## warper_w8(1, fold.number = 10)

cpus <- 7

cl <- snow::makeSOCKcluster(cpus)
doSNOW::registerDoSNOW(cl)

pb <- txtProgressBar(max = n.perm, style=3)
opts <- list(progress = function(n) setTxtProgressBar(pb, n))

parallel::clusterExport(cl, varlist = c("ff_ccw8","warper_w8"))

ls.perm_w8 <- foreach::`%dopar%`(
                           foreach::foreach(i=1:n.perm, .options.snow=opts, .packages = c("BuyseTest","data.table","ranger")), {
                               warper_w8(i, fold.number = 10)
                           })


dt.resperm_w8 <- as.data.table(do.call(rbind, ls.perm_w8))

dt.resperm_w8[metric == "auc",.(estimate = mean(estimate), sd = sd(estimate), type1error = mean(p.value<=0.05)), by = c("method","model")]
##      method     model  estimate         sd type1error
## 1: internal glm0_ccw8 0.5017719 0.06581663       0.04
## 2: internal  glm_ccw8 0.4985725 0.06846851       0.06
## 3: internal   rf_ccw8 0.5060115 0.06844682       0.06
## 4:       cv glm0_ccw8 0.4027030 0.09982635       0.46
## 5:       cv  glm_ccw8 0.4730674 0.10588802       0.28
## 6:       cv   rf_ccw8 0.4713885 0.09941016       0.29

dtPerfCV.ccw8 <- as.data.table(ePerf.ccw8)[method=="cv"]
dtPerfCV.ccw8[,perm := list(list(dt.resperm_w8[method == "cv" & metric == .SD$metric[1] & model == .SD$model[1],estimate])),by = c("metric","model"),.SDcols = c("metric","model")]

dtPerfCV.ccw8[metric == "auc", .(p.value = mean(estimate <= perm[[1]])), by = "model"]
##        model p.value
## 1: glm0_ccw8    0.11
## 2:  glm_ccw8    0.02
## 3:   rf_ccw8    0.23
dtPerfCV.ccw8[metric == "brier", .(p.value = mean(estimate >= perm[[1]])), by = "model"]
##        model p.value
## 1: glm0_ccw8    0.27
## 2:  glm_ccw8    0.01
## 3:   rf_ccw8    0.19

betahat <- as.data.table(ePerf.ccw8)[method == "cv" & metric == "brier" & model == "glm_ccw8",estimate]
betahat.perm <- dt.resperm[method == "cv" & metric == "brier" & model == "glm_ccw8",estimate]
mean(betahat>=betahat.perm)
## [1] 0.02


## saveRDS(list(estimate = ePerf.ccw8,
##             perm = dt.resperm),
##        file = file.path(path.results,"performance-w8.rds"))
## ePerf.ccw8 <- readRDS(file.path(path.results,"performance-w8.rds"))$estimate
## dt.resperm <- readRDS(file.path(path.results,"performance-w8.rds"))$perm


## as.data.table(performance(list(glm0_ccw8 = e.glm0_ccw8, glm_ccw8 = e.glm_ccw8),
##                           data = dfWR.NP1_ccw8, fold.number = 100, fold.size = 8, individual.fit = TRUE))

## ** week 12
## performance(list(glm0_ccw12 = e.glm0_ccw12, glm_ccw12 = e.glm_ccw12, rf_ccw12 = ranger(ff_ccw12, data = dfWR.NP1_ccw12, probability = FALSE)),
##                           data = dfWR.NP1_ccw12)
set.seed(10)
ePerf.ccw12 <- performance(list(glm0_ccw12 = e.glm0_ccw12, glm_ccw12 = e.glm_ccw12,
                                rf_ccw12 = ranger(ff_ccw12, data = dfWR.NP1_ccw12, probability = TRUE),
                                rf0_ccw12 = ranger(Y_w12~female + age + lvpet, data = dfWR.NP1_ccw12, probability = TRUE)),
                          data = dfWR.NP1_ccw12, fold.number = 100, fold.size = 0.1)
ePerf.ccw12
##      method metric      model   estimate          se      lower      upper      p.value p.value_comp
## 1  internal    auc glm0_ccw12 0.66037736 0.064890095 0.51683800 0.77038402 3.025560e-02           NA
## 2  internal    auc  glm_ccw12 0.79443893 0.056420373 0.65615610 0.88189965 3.531358e-04 4.727703e-02
## 3  internal    auc   rf_ccw12 1.00000000 0.000000000 1.00000000 1.00000000 0.000000e+00 2.312484e-04
## 4  internal    auc  rf0_ccw12 0.98609732 0.009872629 0.94472161 0.99655906 4.851309e-08 1.355621e-01
## 5  internal  brier glm0_ccw12 0.18160078 0.019088243 0.14779068 0.22314562           NA           NA
## 6  internal  brier  glm_ccw12 0.14937617 0.021474198 0.11269724 0.19799279           NA 4.420267e-02
## 7  internal  brier   rf_ccw12 0.05981177 0.008366624 0.04546931 0.07867829           NA 7.978087e-08
## 8  internal  brier  rf0_ccw12 0.10337873 0.013898692 0.07943135 0.13454591           NA 1.919462e-06
## 9        cv    auc glm0_ccw12 0.57585899 0.065358094 0.43786023 0.69155553 2.678011e-01           NA
## 10       cv    auc  glm_ccw12 0.60564052 0.069408960 0.45619775 0.72584786 1.566569e-01 6.443640e-01
## 11       cv    auc   rf_ccw12 0.76685204 0.053164055 0.64217487 0.85290100 2.378021e-04 1.098145e-02
## 12       cv    auc  rf0_ccw12 0.71861966 0.056896285 0.58949842 0.81335300 1.988739e-03 4.223849e-01
## 13       cv  brier glm0_ccw12 0.19537700 0.024267167 0.15316106 0.24922897           NA           NA
## 14       cv  brier  glm_ccw12 0.21379164 0.032326221 0.15895909 0.28753855           NA 3.798868e-01
## 15       cv  brier   rf_ccw12 0.16414574 0.021376252 0.12716862 0.21187478           NA 1.585040e-02
## 16       cv  brier  rf0_ccw12 0.17723569 0.023048452 0.13735911 0.22868880           NA 3.155619e-01

## *** DENSITY PLOT
dt.CVpred_ccw12 <- as.data.table(ePerf.ccw12, type = "prediction-cv", format = "long")
dt.CVpred_ccw12$model.lab <- factor(dt.CVpred_ccw12$model, levels = c("glm0_ccw12","glm_ccw12","rf_ccw12", "rf0_ccw12"),
                                    labels = c("logistic model - no biomarkers", "logistic model - biomarkers",
                                               "random forest - biomarkers","random forest - pet only"))
## SANITY CHECK
## with(dt.CVpred_ccw12[model=="glm_ccw12"], auc(outcome, prediction, fold, observation))

ggHist_w12 <- ggplot(dt.CVpred_ccw12,aes(x=prediction, fill = as.factor(outcome)))
ggHist_w12 <- ggHist_w12 + geom_histogram(alpha = 0.5) + labs(x = "Predicted probability of recovery", y = "number of CV predictions", fill = "Observed \n recovery")
ggHist_w12 <- ggHist_w12 + facet_wrap(~model.lab)


mybreaks <- seq(0,1,length.out=20)
dt.hist_ccw12 <- dt.CVpred_ccw12[,.(ls.outcome = list(rep(.SD$outcome[1],length(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$mids))),
                                  ls.model = list(rep(.SD$model.lab[1],length(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$mids))),
                                  ls.pc = list(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$counts/sum(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$counts)),
                                  ls.breaks = list(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$mids)),
                               by = c("outcome","model.lab"), .SDcols = c("prediction","outcome","model.lab")]
dt.hist_ccw12[,c("outcome","model.lab") := NULL]
dt.hist_ccw12 <- as.data.table(lapply(dt.hist_ccw12,unlist))

ggHist2_w12 <- ggplot(dt.hist_ccw12, aes(x=ls.breaks,y=ls.pc,fill=as.character(ls.outcome)))
ggHist2_w12 <- ggHist2_w12 + geom_col(position = "dodge") + labs(x = "Predicted probability of recovery", y = "proportion of CV predictions", fill = "Observed \n recovery")
ggHist2_w12 <- ggHist2_w12 + facet_wrap(~ls.model)
ggHist2_w12

ggDens_w12 <- ggplot(dt.CVpred_ccw12,aes(x=prediction, fill = as.factor(outcome)))
ggDens_w12 <- ggDens_w12 + geom_density(alpha = 0.25, adjust = 0.75) + labs(x = "Predicted probability of recovery", y = "Density of CV predictions", fill = "Observed \n recovery")
ggDens_w12 <- ggDens_w12 + facet_wrap(~model.lab)

if(FALSE){
    ggsave(ggHist2_w12 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm")),
           filename = "../figures/hist-predCV-week12.pdf")
    ggsave(ggHist2_w12 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm")),
           filename = "../figures/hist-predCV-week12.png")
}


## *** CALIBRATION PLOT
ggCali_w12 <- ggplot(dt.CVpred_ccw12,aes(x=prediction, y = outcome))
ggCali_w12 <- ggCali_w12 + geom_smooth() + geom_rug() + geom_abline(slope = 1, intercept = 0, color = "red")
ggCali_w12 <- ggCali_w12 + facet_wrap(~model.lab)

## *** ROC CURVES
dt.CVroc_ccw12 <- as.data.table(ePerf.ccw12, type = "roc-cv")                                                      
dt.CVroc_ccw12$model.lab <- factor(dt.CVroc_ccw12$model, levels = c("glm0_ccw12","glm_ccw12","rf_ccw12","rf0_ccw12"),
                                   labels = c("logistic model - no biomarkers", "logistic model - biomarkers", "random forest - biomarkers", "random forest - pet only"))
## SANITY CHECK
## ggroc(roc(outcome ~ prediction, data = dt.CVpred_ccw12[fold == 3 & model == "glm_ccw12"]))
## ggplot(dt.CVroc_ccw12[fold==3 & model == "glm_ccw12"], aes(x=1-sp,y=se)) + geom_step() + geom_abline(slope=1,intercept=0,color = "black")

ggROC_w12 <- ggplot(dt.CVroc_ccw12,aes(x=1-sp,color=model.lab))
ggROC_w12 <- ggROC_w12 + geom_step(aes(y=se,group=interaction(fold,model.lab)),alpha=0.1, size = 1) + geom_abline(slope=1,intercept=0,color = "black")
ggROC_w12 <- ggROC_w12 + geom_smooth(aes(y=se,group=model.lab), se = FALSE, size = 3)
ggROC_w12 <- ggROC_w12 + labs(x="1-specificity", y="sensitivity")

if(FALSE){
    ggsave(ggROC_w12 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm")) + labs(color=""),
           filename = "../figures/roc-CV-week12.pdf")
    ggsave(ggROC_w12 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm")) + labs(color=""),
           filename = "../figures/roc-CV-week12.png")
}


## *** permutation test
n.perm <- 100
ls.resperm <- vector(mode = "list", length = n.perm)

warper_w12 <- function(i, trace = FALSE, fold.number){
    iData <- copy(dfWR.NP1_ccw12)
    iData$Y_w12 <- sample(iData$Y_w12)
    iEPerf <- suppressWarnings(performance(list(glm0_ccw12 = e.glm0_ccw12, glm_ccw12 = e.glm_ccw12, rf_ccw12 = e.ranger_ccw12, rf0_ccw12 = e.ranger0_ccw12),
                                           data = iData, fold.number = fold.number, fold.size = 0.1, trace = trace))
    return(cbind(perm = i, as.data.table(iEPerf, type = "metric")))
}
## warper_w12(1, fold.number = 10)

cpus <- 7

cl <- snow::makeSOCKcluster(cpus)
doSNOW::registerDoSNOW(cl)

pb <- txtProgressBar(max = n.perm, style=3)
opts <- list(progress = function(n) setTxtProgressBar(pb, n))

parallel::clusterExport(cl, varlist = c("ff_ccw12","warper_w12"))

system.time(
    ls.perm_w12 <- foreach::`%dopar%`(
                                foreach::foreach(i=1:n.perm, .options.snow=opts, .packages = c("BuyseTest","data.table","ranger")), {
                                    warper_w12(i, fold.number = 10)
                                })
)
## bruger   system forløbet 
##     0.26     0.10   354.00 

dt.resperm_w12 <- as.data.table(do.call(rbind, ls.perm_w12))

dt.resperm_w12[metric == "auc",.(estimate = mean(estimate), sd = sd(estimate), type1error = mean(p.value<=0.05)), by = c("method","model")]
##      method      model  estimate         sd type1error
## 1: internal glm0_ccw12 0.5015889 0.07802765       0.05
## 2: internal  glm_ccw12 0.5047368 0.07940914       0.06
## 3: internal   rf_ccw12 0.5120357 0.08221397       0.09
## 4:       cv glm0_ccw12 0.4244886 0.10874097       0.32
## 5:       cv  glm_ccw12 0.4748888 0.10481419       0.28
## 6:       cv   rf_ccw12 0.4913347 0.10015871       0.19

dtPerfCV.ccw12 <- as.data.table(ePerf.ccw12)[method=="cv"]
dtPerfCV.ccw12[,perm := list(list(dt.resperm_w12[method == "cv" & metric == .SD$metric[1] & model == .SD$model[1],estimate])),by = c("metric","model"),.SDcols = c("metric","model")]

dtPerfCV.ccw12[metric == "auc", .(p.value = mean(estimate <= perm[[1]])), by = "model"]
##         model p.value
## 1: glm0_ccw12    0.10
## 2:  glm_ccw12    0.12
## 3:   rf_ccw12    0.00
dtPerfCV.ccw12[metric == "brier", .(p.value = mean(estimate >= perm[[1]])), by = "model"]
##         model p.value
## 1: glm0_ccw12    0.12
## 2:  glm_ccw12    0.10
## 3:   rf_ccw12    0.00

betahat <- as.data.table(ePerf.ccw12)[method == "cv" & metric == "brier" & model == "rf_ccw12",estimate]
betahat.perm <- dt.resperm_w12[method == "cv" & metric == "brier" & model == "rf_ccw12",estimate]
mean(betahat>=betahat.perm)
## [1] 0.02


## * Missing values

## ** week 8
set.seed(10)

e.glm0_w8 <- glm(Y_w8 ~ female + age, family = binomial(link = "logit"), data = dfWR.NP1_w8)
e.glm_w8 <- glm(Y_w8 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance + CATS_scoretotal + CAR_AUCi + neuroticism, family = binomial(link = "logit"), data = dfWR.NP1_w8)
e.ranger_w8 <- ranger(formula = Y_w8f ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance + CATS_scoretotal + CAR_AUCi + neuroticism,
                      data = na.omit(dfWR.NP1_w8))

## performance(e.ranger_w8, fold.number = 100, fold.size = 8)
##     method metric         model   estimate          se      lower     upper   p.value
## 1 internal    auc randomForest1 1.00000000 0.000000000 1.00000000 1.0000000 0.0000000
## 2 internal  brier randomForest1 0.03598857 0.002001488 0.03227197 0.0401332        NA
## 3       cv    auc randomForest1 0.51187202 0.049231003 0.41172790 0.6032751 0.8104758
## 4       cv  brier randomForest1 0.26984952 0.017368838 0.23786698 0.3061323        NA

set.seed(11)
ePerf.w8 <- performance(list(glm0_w8 = e.glm0_w8, glm_w8 = e.glm_w8),
                        data = dfWR.NP1_w8, fold.number = 5, fold.size = 0.1, individual.fit = TRUE)
ePerf.w8
##      method metric   model  estimate          se     lower     upper      p.value p.value_comp
## 1: internal    auc glm0_w8 0.5991949 0.062785456 0.4654191 0.7096529 1.391341e-01           NA
## 2: internal    auc  glm_w8 0.8878666 0.034755082 0.7971564 0.9395116 8.527960e-08 3.559246e-06
## 3: internal  brier glm0_w8 0.2409137 0.007848917 0.2260110 0.2567991           NA           NA
## 4: internal  brier  glm_w8 0.1432597 0.021336686 0.1069913 0.1918225           NA 2.368144e-07
## 5:       cv    auc glm0_w8 0.5817262 0.051931942 0.4731775 0.6755494 1.347925e-01           NA
## 6:       cv    auc  glm_w8 0.6655416 0.047086931 0.5641924 0.7485380 2.199599e-03 1.072280e-01
## 7:       cv  brier glm0_w8 0.2632526 0.017432257 0.2312103 0.2997355           NA           NA
## 8:       cv  brier  glm_w8 0.2838192 0.052223110 0.1978881 0.4070653           NA 6.376285e-01


dt.CVpred_w8 <- as.data.table(cbind(do.call(rbind,lapply(apply(attr(attr(ePerf.w8,"predictions")$cv,"index"),3,list),"[[",1)),
                                    do.call(rbind,lapply(apply(attr(ePerf.w8,"predictions")$cv,3,list),"[[",1))))
dt.CVpred_w8$Y <- dfWR.NP1_w8$Y_w8[dt.CVpred$observation]

## ggplot(dt.CVpred_w8,aes(x=glm0_w8, y = as.numeric(Y))) + geom_smooth() + geom_point() + geom_rug() + geom_abline(slope = 1, intercept = 0, color = "red")
## ggplot(dt.CVpred_w8,aes(x=glm_w8, y = as.numeric(Y))) + geom_smooth() + geom_point() + geom_rug() + geom_abline(slope = 1, intercept = 0, color = "red")

attr(ePerf.impw8,"auc")$glm0_w8[101,]
attr(ePerf.impw8,"auc")$glm_w8[92,]

auc(labels = dfWR.NP1_w8$Y_w8, predictions = dt.CVpred_w8$glm0_w8, fold = dt.CVpred_w8$fold, observation = dt.CVpred_w8$observation)

rm.fold <- unique(dt.CVpred_w8$fold[is.na(dt.CVpred_w8$glm_w8)])
keep.fold <- setdiff(1:100, rm.fold)
auc(labels = dfWR.NP1_w8$Y_w8,
    predictions = dt.CVpred_w8$glm_w8[dt.CVpred_w8$fold %in% keep.fold],
    fold = dt.CVpred_w8$fold[dt.CVpred_w8$fold %in% keep.fold],
    observation = dt.CVpred_w8$observation[dt.CVpred_w8$fold %in% keep.fold])

ls.ROC <- do.call(rbind,lapply(keep.fold, function(iFold){
    iRoc <- pROC::roc(dt.CVpred$Y[dt.CVpred$fold == iFold],dt.CVpred$glm_w8[dt.CVpred$fold == iFold], direction = "<")
    data.frame(fold = iFold, se = iRoc$sensitivities, sp = iRoc$specificities)
}))

ggplot(ls.ROC, aes(x = 1-sp,y=se)) + geom_step(aes(group = fold),alpha = 0.25) + geom_smooth() + geom_abline(slope = 1, intercept = 0, color = "red")


## ** week 12
set.seed(10)

e.glm0_w12 <- glm(Y_w12 ~ female + age, family = binomial(link = "logit"), data = dfWR.NP1_w12)
e.glm_w12 <- glm(Y_w12 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance + CATS_scoretotal + CAR_AUCi + neuroticism, family = binomial(link = "logit"), data = dfWR.NP1_w12)

set.seed(11)
ePerf.w12 <- as.data.table(performance(list(glm0_w12 = e.glm0_w12, glm_w12 = e.glm_w12), ## rf_w12 = e.ranger_w12),
                                         data = dfWR.NP1_w12, fold.number = 100, fold.size = 8, individual.fit = TRUE))
ePerf.w12


##----------------------------------------------------------------------
### analysis-prediction.R ends here
