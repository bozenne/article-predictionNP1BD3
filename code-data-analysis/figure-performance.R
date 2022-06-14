### figure-performance.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: mar  3 2022 (18:37) 
## Version: 
## Last-Updated: maj 10 2022 (16:56) 
##           By: Brice Ozenne
##     Update #: 32
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
library(BuyseTest)
library(ggplot2)
library(ggpubr)

## * Load data
## list.files(path.results)
ePerf.ccw4 <- readRDS(file.path(path.results,"perf-cc-week4-IF.rds"))
ePerf.ccw8 <- readRDS(file.path(path.results,"perf-cc-week8-IF.rds"))
ePerf.ccw12 <- readRDS(file.path(path.results,"perf-cc-week12-IF.rds"))
ePerf.cctraj <- readRDS(file.path(path.results,"perf-cc-traj-IF.rds"))
ePerf.impw4 <- readRDS(file.path(path.results,"perf-imp-week4-IF.rds"))
ePerf.impw8 <- readRDS(file.path(path.results,"perf-imp-week8-IF.rds"))
ePerf.impw12 <- readRDS(file.path(path.results,"perf-imp-week12-IF.rds"))
ePerf.imptraj <- readRDS(file.path(path.results,"perf-imp-traj-IF.rds"))

## * Complete data analysis

## ** week 4
dt.CVpred_ccw4 <- as.data.table(ePerf.ccw4, type = "prediction-cv", format = "long")
dt.CVpred_ccw4$model.lab <- factor(dt.CVpred_ccw4$model, levels = c("glm0_ccw4","glm_ccw4","rf_ccw4"),
                                   labels = c("logistic model \n no biomarkers", "logistic model \n biomarkers", "random forest \n biomarkers"))
dt.CVpred_ccw4$predcat <- cut(dt.CVpred_ccw4$prediction, breaks = seq(0,1, by = 0.1))
## SANITY CHECK
## with(dt.CVpred_ccw4[model=="glm_ccw4"], auc(outcome, prediction, repetition, observation))

## ggHist_w4 <- ggplot(dt.CVpred_ccw4,aes(x=prediction, fill = as.factor(outcome)))
## ggHist_w4 <- ggHist_w4 + geom_histogram(alpha = 0.5) + labs(x = "Predicted probability of recovery", y = "number of CV predictions", fill = "Observed \n recovery")
## ggHist_w4 <- ggHist_w4 + facet_wrap(~model.lab)

mybreaks <- seq(0,1,length.out=20)
dt.hist_ccw4 <- dt.CVpred_ccw4[,.(ls.outcome = list(rep(.SD$outcome[1],length(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$mids))),
                                  ls.model = list(rep(.SD$model.lab[1],length(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$mids))),
                                  ls.pc = list(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$counts/sum(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$counts)),
                                  ls.breaks = list(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$mids)),
                               by = c("outcome","model.lab"), .SDcols = c("prediction","outcome","model.lab")]
dt.hist_ccw4[,c("outcome","model.lab") := NULL]
dt.hist_ccw4 <- as.data.table(lapply(dt.hist_ccw4,unlist))

ggHist2_ccw4 <- ggplot(dt.hist_ccw4, aes(x=ls.breaks,y=ls.pc,fill=as.character(ls.outcome)))
ggHist2_ccw4 <- ggHist2_ccw4 + geom_col(position = "dodge") + labs(x = "Predicted probability of recovery", y = "proportion of CV predictions", fill = "Observed recovery at week 4")
ggHist2_ccw4 <- ggHist2_ccw4 + facet_wrap(~ls.model)
ggHist2_ccw4 <- ggHist2_ccw4 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm"),
                                     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggHist2_ccw4

## ggDens_w4 <- ggplot(dt.CVpred_ccw4,aes(x=prediction, fill = as.factor(outcome)))
## ggDens_w4 <- ggDens_w4 + geom_density(alpha = 0.25, adjust = 0.75) + labs(x = "Predicted probability of recovery", y = "Density of CV predictions", fill = "Observed \n recovery")
## ggDens_w4 <- ggDens_w4 + facet_wrap(~model.lab)

## *** CALIBRATION PLOT
## ggCali_w4 <- ggplot(dt.CVpred_ccw4,aes(x=prediction, y = outcome))
## ggCali_w4 <- ggCali_w4 + geom_smooth() + geom_rug() + geom_abline(slope = 1, intercept = 0, color = "red")
## ggCali_w4 <- ggCali_w4 + facet_wrap(~model.lab)

ggCali_ccw4 <- ggplot(dt.CVpred_ccw4[, .(empirical = mean(outcome)), by = c("predcat","model.lab")],
                    aes(x = predcat, y = empirical, color = model.lab, group = model.lab))
ggCali_ccw4 <-  ggCali_ccw4 + geom_point(size = 3, aes(shape = model.lab)) + geom_line(size = 1.5)
ggCali_ccw4 <-  ggCali_ccw4 + xlab("Predicted probability of recover") + ylab("Average observed probability of recover") + labs(color = "predictive model", shape = "predictive model")
ggCali_ccw4 <-  ggCali_ccw4 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm"),
                                legend.position="bottom")


## *** ROC
dt.CVroc_ccw4 <- as.data.table(ePerf.ccw4, type = "roc-cv")                                                      
dt.CVroc_ccw4$model.lab <- factor(dt.CVroc_ccw4$model, levels = c("glm0_ccw4","glm_ccw4","rf_ccw4"),
                                   labels = c("logistic model \n no biomarkers", "logistic model \n biomarkers", "random forest \n biomarkers"))
## SANITY CHECK
## ggroc(roc(outcome ~ prediction, data = dt.CVpred_ccw4[repetition == 3 & model == "glm_ccw4"]))
## ggplot(dt.CVroc_ccw4[repetition==3 & model == "glm_ccw4"], aes(x=1-sp,y=se)) + geom_step() + geom_abline(slope=1,intercept=0,color = "black")

ggROC_ccw4 <- ggplot(dt.CVroc_ccw4,aes(x=1-sp,color=model.lab))
ggROC_ccw4 <- ggROC_ccw4 + geom_step(aes(y=se,group=interaction(repetition,model.lab)),alpha=0.1, size = 1) + geom_abline(slope=1,intercept=0,color = "black")
ggROC_ccw4 <- ggROC_ccw4 + geom_smooth(aes(y=se,group=model.lab), se = FALSE, size = 3)
ggROC_ccw4 <- ggROC_ccw4 + labs(x="1-specificity", y="sensitivity") + facet_wrap(~model.lab, nrow = 1) + labs(color = "predictive model")
ggROC_ccw4 <- ggROC_ccw4 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm"),
                                legend.position="bottom")

## ** week 8

## *** DENSITY PLOT
dt.CVpred_ccw8 <- as.data.table(ePerf.ccw8, type = "prediction-cv", format = "long")
dt.CVpred_ccw8$model.lab <- factor(dt.CVpred_ccw8$model, levels = c("glm0_ccw8","glm_ccw8","rf_ccw8"),
                                   labels = c("logistic model \n no biomarkers", "logistic model \n biomarkers", "random forest \n biomarkers"))
dt.CVpred_ccw8$predcat <- cut(dt.CVpred_ccw8$prediction, breaks = seq(0,1, by = 0.1))
## SANITY CHECK
## with(dt.CVpred_ccw8[model=="glm_ccw8"], auc(outcome, prediction, repetition, observation))

## ggHist_w8 <- ggplot(dt.CVpred_ccw8,aes(x=prediction, fill = as.factor(outcome)))
## ggHist_w8 <- ggHist_w8 + geom_histogram(alpha = 0.5) + labs(x = "Predicted probability of recovery", y = "number of CV predictions", fill = "Observed \n recovery")
## ggHist_w8 <- ggHist_w8 + facet_wrap(~model.lab)

mybreaks <- seq(0,1,length.out=20)
dt.hist_ccw8 <- dt.CVpred_ccw8[,.(ls.outcome = list(rep(.SD$outcome[1],length(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$mids))),
                                  ls.model = list(rep(.SD$model.lab[1],length(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$mids))),
                                  ls.pc = list(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$counts/sum(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$counts)),
                                  ls.breaks = list(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$mids)),
                               by = c("outcome","model.lab"), .SDcols = c("prediction","outcome","model.lab")]
dt.hist_ccw8[,c("outcome","model.lab") := NULL]
dt.hist_ccw8 <- as.data.table(lapply(dt.hist_ccw8,unlist))

ggHist2_ccw8 <- ggplot(dt.hist_ccw8, aes(x=ls.breaks,y=ls.pc,fill=as.character(ls.outcome)))
ggHist2_ccw8 <- ggHist2_ccw8 + geom_col(position = "dodge") + labs(x = "Predicted probability of recovery", y = "proportion of CV predictions", fill = "Observed recovery at week 8")
ggHist2_ccw8 <- ggHist2_ccw8 + facet_wrap(~ls.model)
ggHist2_ccw8 <- ggHist2_ccw8 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm"),
                                     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggHist2_ccw8

## ggDens_w8 <- ggplot(dt.CVpred_ccw8,aes(x=prediction, fill = as.factor(outcome)))
## ggDens_w8 <- ggDens_w8 + geom_density(alpha = 0.25, adjust = 0.75) + labs(x = "Predicted probability of recovery", y = "Density of CV predictions", fill = "Observed \n recovery")
## ggDens_w8 <- ggDens_w8 + facet_wrap(~model.lab)

## *** CALIBRATION PLOT
## ggCali_w8 <- ggplot(dt.CVpred_ccw8,aes(x=prediction, y = outcome))
## ggCali_w8 <- ggCali_w8 + geom_smooth() + geom_rug() + geom_abline(slope = 1, intercept = 0, color = "red")
## ggCali_w8 <- ggCali_w8 + facet_wrap(~model.lab)

ggCali_ccw8 <- ggplot(dt.CVpred_ccw8[, .(empirical = mean(outcome)), by = c("predcat","model.lab")],
                    aes(x = predcat, y = empirical, color = model.lab, group = model.lab))
ggCali_ccw8 <-  ggCali_ccw8 + geom_point(size = 3, aes(shape = model.lab)) + geom_line(size = 1.5)
ggCali_ccw8 <-  ggCali_ccw8 + xlab("Predicted probability of recover") + ylab("Average observed probability of recover") + labs(color = "predictive model", shape = "predictive model")
ggCali_ccw8 <-  ggCali_ccw8 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm"),
                                legend.position="bottom")

## *** ROC
dt.CVroc_ccw8 <- as.data.table(ePerf.ccw8, type = "roc-cv")                                                      
dt.CVroc_ccw8$model.lab <- factor(dt.CVroc_ccw8$model, levels = c("glm0_ccw8","glm_ccw8","rf_ccw8"),
                                   labels = c("logistic model\nno biomarkers", "logistic model\nbiomarkers", "random forest\nbiomarkers"))
## SANITY CHECK
## ggroc(roc(outcome ~ prediction, data = dt.CVpred_ccw8[repetition == 3 & model == "glm_ccw8"]))
## ggplot(dt.CVroc_ccw8[repetition==3 & model == "glm_ccw8"], aes(x=1-sp,y=se)) + geom_step() + geom_abline(slope=1,intercept=0,color = "black")

ggROC_ccw8 <- ggplot(dt.CVroc_ccw8,aes(x=1-sp,color=model.lab))
ggROC_ccw8 <- ggROC_ccw8 + geom_step(aes(y=se,group=interaction(repetition,model.lab)),alpha=0.1, size = 1) + geom_abline(slope=1,intercept=0,color = "black")
ggROC_ccw8 <- ggROC_ccw8 + geom_smooth(aes(y=se,group=model.lab), se = FALSE, size = 3)
ggROC_ccw8 <- ggROC_ccw8 + labs(x="1-specificity", y="sensitivity") + facet_wrap(~model.lab, nrow = 1) + labs(color = "predictive model")
ggROC_ccw8 <- ggROC_ccw8 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm"),
                                legend.position="bottom")

## ** week 12

## *** DENSITY PLOT
dt.CVpred_ccw12 <- as.data.table(ePerf.ccw12, type = "prediction-cv", format = "long")
dt.CVpred_ccw12$model.lab <- factor(dt.CVpred_ccw12$model, levels = c("glm0_ccw12","glm_ccw12","rf_ccw12", "rf0_ccw12"),
                                    labels = c("logistic model\nno biomarkers", "logistic model\nbiomarkers",
                                               "random forest\nbiomarkers","random forest\npet only"))
dt.CVpred_ccw12$predcat <- cut(dt.CVpred_ccw12$prediction, breaks = seq(0,1, by = 0.1))
## SANITY CHECK
## with(dt.CVpred_ccw12[model=="glm_ccw12"], auc(outcome, prediction, repetition, observation))

## ggHist_w12 <- ggplot(dt.CVpred_ccw12,aes(x=prediction, fill = as.factor(outcome)))
## ggHist_w12 <- ggHist_w12 + geom_histogram(alpha = 0.5) + labs(x = "Predicted probability of recovery", y = "number of CV predictions", fill = "Observed \n recovery")
## ggHist_w12 <- ggHist_w12 + facet_wrap(~model.lab)

mybreaks <- seq(0,1,length.out=20)
dt.hist_ccw12 <- dt.CVpred_ccw12[,.(ls.outcome = list(rep(.SD$outcome[1],length(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$mids))),
                                  ls.model = list(rep(.SD$model.lab[1],length(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$mids))),
                                  ls.pc = list(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$counts/sum(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$counts)),
                                  ls.breaks = list(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$mids)),
                               by = c("outcome","model.lab"), .SDcols = c("prediction","outcome","model.lab")]
dt.hist_ccw12[,c("outcome","model.lab") := NULL]
dt.hist_ccw12 <- as.data.table(lapply(dt.hist_ccw12,unlist))

ggHist2_ccw12 <- ggplot(dt.hist_ccw12, aes(x=ls.breaks,y=ls.pc,fill=as.character(ls.outcome)))
ggHist2_ccw12 <- ggHist2_ccw12 + geom_col(position = "dodge") + labs(x = "Predicted probability of recovery", y = "proportion of CV predictions", fill = "Observed recovery week 12")
ggHist2_ccw12 <- ggHist2_ccw12 + facet_wrap(~ls.model)
ggHist2_ccw12 <- ggHist2_ccw12 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm"),
                                     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggHist2_ccw12

## ggDens_w12 <- ggplot(dt.CVpred_ccw12,aes(x=prediction, fill = as.factor(outcome)))
## ggDens_w12 <- ggDens_w12 + geom_density(alpha = 0.25, adjust = 0.75) + labs(x = "Predicted probability of recovery", y = "Density of CV predictions", fill = "Observed \n recovery")
## ggDens_w12 <- ggDens_w12 + facet_wrap(~model.lab)

## *** CALIBRATION PLOT
## ggCali_w12 <- ggplot(dt.CVpred_ccw12,aes(x=prediction, y = outcome))
## ggCali_w12 <- ggCali_w12 + geom_smooth() + geom_rug() + geom_abline(slope = 1, intercept = 0, color = "red")
## ggCali_w12 <- ggCali_w12 + facet_wrap(~model.lab)

ggCali_ccw12 <- ggplot(dt.CVpred_ccw12[, .(empirical = mean(outcome)), by = c("predcat","model.lab")],
                    aes(x = predcat, y = empirical, color = model.lab, group = model.lab))
ggCali_ccw12 <-  ggCali_ccw12 + geom_point(size = 3, aes(shape = model.lab)) + geom_line(size = 1.5)
ggCali_ccw12 <-  ggCali_ccw12 + xlab("Predicted probability of recover") + ylab("Average observed probability of recover") + labs(color = "predictive model", shape = "predictive model")
ggCali_ccw12 <-  ggCali_ccw12 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm"),
                                legend.position="bottom")

## *** ROC
dt.CVroc_ccw12 <- as.data.table(ePerf.ccw12, type = "roc-cv")                                                      
dt.CVroc_ccw12$model.lab <- factor(dt.CVroc_ccw12$model, levels = c("glm0_ccw12","glm_ccw12","rf_ccw12"),
                                   labels = c("logistic model\nno biomarkers", "logistic model\nbiomarkers", "random forest\nbiomarkers"))
## SANITY CHECK
## ggroc(roc(outcome ~ prediction, data = dt.CVpred_ccw12[repetition == 3 & model == "glm_ccw12"]))
## ggplot(dt.CVroc_ccw12[repetition==3 & model == "glm_ccw12"], aes(x=1-sp,y=se)) + geom_step() + geom_abline(slope=1,intercept=0,color = "black")

ggROC_ccw12 <- ggplot(dt.CVroc_ccw12,aes(x=1-sp,color=model.lab))
ggROC_ccw12 <- ggROC_ccw12 + geom_step(aes(y=se,group=interaction(repetition,model.lab)),alpha=0.1, size = 1) + geom_abline(slope=1,intercept=0,color = "black")
ggROC_ccw12 <- ggROC_ccw12 + geom_smooth(aes(y=se,group=model.lab), se = FALSE, size = 3)
ggROC_ccw12 <- ggROC_ccw12 + labs(x="1-specificity", y="sensitivity") + facet_wrap(~model.lab, nrow = 1) + labs(color = "predictive model")
ggROC_ccw12 <- ggROC_ccw12 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm"),
                                legend.position="bottom")


## * Missing data analysis

## ** week 4
## 
dt.CVpred_impw4 <- as.data.table(ePerf.impw4, type = "prediction-cv", format = "long")
dt.CVpred_impw4$model.lab <- factor(dt.CVpred_impw4$model, levels = c("glm0_impw4","glm_impw4","rf_impw4"),
                                   labels = c("logistic model \n no biomarkers", "logistic model \n biomarkers", "random forest \n biomarkers"))
dt.CVpred_impw4$predcat <- cut(dt.CVpred_impw4$prediction, breaks = seq(0,1, by = 0.1))

mybreaks <- seq(0,1,length.out=20)
dt.hist_impw4 <- dt.CVpred_impw4[,.(ls.outcome = list(rep(.SD$outcome[1],length(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$mids))),
                                    ls.model = list(rep(.SD$model.lab[1],length(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$mids))),
                                    ls.pc = list(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$counts/sum(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$counts)),
                                    ls.breaks = list(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$mids)),
                                 by = c("outcome","model.lab"), .SDcols = c("prediction","outcome","model.lab")]
dt.hist_impw4[,c("outcome","model.lab") := NULL]
dt.hist_impw4 <- as.data.table(lapply(dt.hist_impw4,unlist))

ggHist2_impw4 <- ggplot(dt.hist_impw4, aes(x=ls.breaks,y=ls.pc,fill=as.character(ls.outcome)))
ggHist2_impw4 <- ggHist2_impw4 + geom_col(position = "dodge") + labs(x = "Predicted probability of recovery", y = "proportion of CV predictions", fill = "Observed recovery at week 4")
ggHist2_impw4 <- ggHist2_impw4 + facet_wrap(~ls.model)
ggHist2_impw4 <- ggHist2_impw4 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm"),
                                     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggHist2_impw4

## *** CALIBRATION PLOT
ggCali_impw4 <- ggplot(dt.CVpred_impw4[, .(empirical = mean(outcome)), by = c("predcat","model.lab")],
                      aes(x = predcat, y = empirical, color = model.lab, group = model.lab))
ggCali_impw4 <-  ggCali_impw4 + geom_point(size = 3, aes(shape = model.lab)) + geom_line(size = 1.5)
ggCali_impw4 <-  ggCali_impw4 + xlab("Predicted probability of recover") + ylab("Average observed probability of recover") + labs(color = "predictive model", shape = "predictive model")
ggCali_impw4 <-  ggCali_impw4 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm"),
                                    legend.position="bottom")

## *** ROC
dt.CVroc_impw4 <- as.data.table(ePerf.impw4, type = "roc-cv")                                                      
dt.CVroc_impw4$model.lab <- factor(dt.CVroc_impw4$model, levels = c("glm0_impw4","glm_impw4","rf_impw4"),
                                   labels = c("logistic model\nno biomarkers", "logistic model\nbiomarkers", "random forest\nbiomarkers"))
## SANITY CHECK
## ggroc(roc(outcome ~ prediction, data = dt.CVpred_impw4[repetition == 3 & model == "glm_impw4"]))
## ggplot(dt.CVroc_impw4[repetition==3 & model == "glm_impw4"], aes(x=1-sp,y=se)) + geom_step() + geom_abline(slope=1,intercept=0,color = "black")

ggROC_impw4 <- ggplot(dt.CVroc_impw4,aes(x=1-sp,color=model.lab))
ggROC_impw4 <- ggROC_impw4 + geom_step(aes(y=se,group=interaction(repetition,model.lab)),alpha=0.1, size = 1) + geom_abline(slope=1,intercept=0,color = "black")
ggROC_impw4 <- ggROC_impw4 + geom_smooth(aes(y=se,group=model.lab), se = FALSE, size = 3)
ggROC_impw4 <- ggROC_impw4 + labs(x="1-specificity", y="sensitivity") + facet_wrap(~model.lab, nrow = 1) + labs(color = "predictive model")
ggROC_impw4 <- ggROC_impw4 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm"),
                                legend.position="bottom")
ggROC_impw4

## ** week 8

## *** DENSITY PLOT
dt.CVpred_impw8 <- as.data.table(ePerf.impw8, type = "prediction-cv", format = "long")
dt.CVpred_impw8$model.lab <- factor(dt.CVpred_impw8$model, levels = c("glm0_impw8","glm_impw8","rf_impw8"), 
                                   labels = c("logistic model \n no biomarkers", "logistic model \n biomarkers", "random forest \n biomarkers"))
dt.CVpred_impw8$predcat <- cut(dt.CVpred_impw8$prediction, breaks = seq(0,1, by = 0.1))

mybreaks <- seq(0,1,length.out=20)
dt.hist_impw8 <- dt.CVpred_impw8[,.(ls.outcome = list(rep(.SD$outcome[1],length(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$mids))),
                                  ls.model = list(rep(.SD$model.lab[1],length(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$mids))),
                                  ls.pc = list(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$counts/sum(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$counts)),
                                  ls.breaks = list(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$mids)),
                               by = c("outcome","model.lab"), .SDcols = c("prediction","outcome","model.lab")]
dt.hist_impw8[,c("outcome","model.lab") := NULL]
dt.hist_impw8 <- as.data.table(lapply(dt.hist_impw8,unlist))

ggHist2_impw8 <- ggplot(dt.hist_impw8, aes(x=ls.breaks,y=ls.pc,fill=as.character(ls.outcome)))
ggHist2_impw8 <- ggHist2_impw8 + geom_col(position = "dodge") + labs(x = "Predicted probability of recovery", y = "proportion of CV predictions", fill = "Observed recovery at week 8")
ggHist2_impw8 <- ggHist2_impw8 + facet_wrap(~ls.model)
ggHist2_impw8 <- ggHist2_impw8 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm"),
                                     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggHist2_impw8

## *** CALIBRATION PLOT

ggCali_impw8 <- ggplot(dt.CVpred_impw8[, .(empirical = mean(outcome)), by = c("predcat","model.lab")],
                    aes(x = predcat, y = empirical, color = model.lab, group = model.lab))
ggCali_impw8 <-  ggCali_impw8 + geom_point(size = 3, aes(shape = model.lab)) + geom_line(size = 1.5)
ggCali_impw8 <-  ggCali_impw8 + xlab("Predicted probability of recover") + ylab("Average observed probability of recover") + labs(color = "predictive model", shape = "predictive model")
ggCali_impw8 <-  ggCali_impw8 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm"),
                                legend.position="bottom")

## *** ROC
dt.CVroc_impw8 <- as.data.table(ePerf.impw8, type = "roc-cv")                                                      
dt.CVroc_impw8$model.lab <- factor(dt.CVroc_impw8$model, levels = c("glm0_impw8","glm_impw8","rf_impw8"),
                                   labels = c("logistic model\nno biomarkers", "logistic model\nbiomarkers", "random forest\nbiomarkers"))
## SANITY CHECK
## ggroc(roc(outcome ~ prediction, data = dt.CVpred_impw8[repetition == 3 & model == "glm_impw8"]))
## ggplot(dt.CVroc_impw8[repetition==3 & model == "glm_impw8"], aes(x=1-sp,y=se)) + geom_step() + geom_abline(slope=1,intercept=0,color = "black")

ggROC_impw8 <- ggplot(dt.CVroc_impw8,aes(x=1-sp,color=model.lab))
ggROC_impw8 <- ggROC_impw8 + geom_step(aes(y=se,group=interaction(repetition,model.lab)),alpha=0.1, size = 1) + geom_abline(slope=1,intercept=0,color = "black")
ggROC_impw8 <- ggROC_impw8 + geom_smooth(aes(y=se,group=model.lab), se = FALSE, size = 3)
ggROC_impw8 <- ggROC_impw8 + labs(x="1-specificity", y="sensitivity") + facet_wrap(~model.lab, nrow = 1) + labs(color = "predictive model")
ggROC_impw8 <- ggROC_impw8 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm"),
                                legend.position="bottom")
ggROC_impw8

## ** week 12

## *** DENSITY PLOT
dt.CVpred_impw12 <- as.data.table(ePerf.impw12, type = "prediction-cv", format = "long")
dt.CVpred_impw12$model.lab <- factor(dt.CVpred_impw12$model, levels = c("glm0_impw12","glm_impw12","rf_impw12"),
                                     labels = c("logistic model\nno biomarkers", "logistic model\nbiomarkers", "random forest\nbiomarkers"))
dt.CVpred_impw12$predcat <- cut(dt.CVpred_impw12$prediction, breaks = seq(0,1, by = 0.1))

mybreaks <- seq(0,1,length.out=20)
dt.hist_impw12 <- dt.CVpred_impw12[,.(ls.outcome = list(rep(.SD$outcome[1],length(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$mids))),
                                  ls.model = list(rep(.SD$model.lab[1],length(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$mids))),
                                  ls.pc = list(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$counts/sum(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$counts)),
                                  ls.breaks = list(hist(.SD$prediction, plot = FALSE, breaks = mybreaks)$mids)),
                               by = c("outcome","model.lab"), .SDcols = c("prediction","outcome","model.lab")]
dt.hist_impw12[,c("outcome","model.lab") := NULL]
dt.hist_impw12 <- as.data.table(lapply(dt.hist_impw12,unlist))

ggHist2_impw12 <- ggplot(dt.hist_impw12, aes(x=ls.breaks,y=ls.pc,fill=as.character(ls.outcome)))
ggHist2_impw12 <- ggHist2_impw12 + geom_col(position = "dodge") + labs(x = "Predicted probability of recovery", y = "proportion of CV predictions", fill = "Observed recovery at week 12")
ggHist2_impw12 <- ggHist2_impw12 + facet_wrap(~ls.model)
ggHist2_impw12 <- ggHist2_impw12 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm"),
                                     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggHist2_impw12

## *** CALIBRATION PLOT
ggCali_impw12 <- ggplot(dt.CVpred_impw12[, .(empirical = mean(outcome)), by = c("predcat","model.lab")],
                    aes(x = predcat, y = empirical, color = model.lab, group = model.lab))
ggCali_impw12 <-  ggCali_impw12 + geom_point(size = 3, aes(shape = model.lab)) + geom_line(size = 1.5)
ggCali_impw12 <-  ggCali_impw12 + xlab("Predicted probability of recover") + ylab("Average observed probability of recover") + labs(color = "predictive model", shape = "predictive model")
ggCali_impw12 <-  ggCali_impw12 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm"),
                                legend.position="bottom")

## *** ROC
dt.CVroc_impw12 <- as.data.table(ePerf.impw12, type = "roc-cv")                                                      
dt.CVroc_impw12$model.lab <- factor(dt.CVroc_impw12$model, levels = c("glm0_impw12","glm_impw12","rf_impw12"),
                                   labels = c("logistic model\nno biomarkers", "logistic model\nbiomarkers", "random forest\nbiomarkers"))
## SANITY CHECK
## ggroc(roc(outcome ~ prediction, data = dt.CVpred_impw12[repetition == 3 & model == "glm_impw12"]))
## ggplot(dt.CVroc_impw12[repetition==3 & model == "glm_impw12"], aes(x=1-sp,y=se)) + geom_step() + geom_abline(slope=1,intercept=0,color = "black")

ggROC_impw12 <- ggplot(dt.CVroc_impw12,aes(x=1-sp,color=model.lab))
ggROC_impw12 <- ggROC_impw12 + geom_step(aes(y=se,group=interaction(repetition,model.lab)),alpha=0.1, size = 1) + geom_abline(slope=1,intercept=0,color = "black")
ggROC_impw12 <- ggROC_impw12 + geom_smooth(aes(y=se,group=model.lab), se = FALSE, size = 3)
ggROC_impw12 <- ggROC_impw12 + labs(x="1-specificity", y="sensitivity") + facet_wrap(~model.lab, nrow = 1) + labs(color = "predictive model")
ggROC_impw12 <- ggROC_impw12 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm"),
                                legend.position="bottom")


## * Export
if(FALSE){

    ggHist2_w4 <- ggarrange(ggHist2_ccw4 + ggtitle("Complete case analysis"),
                            ggHist2_impw4 + ggtitle("Missing data analysis"),
                            legend = "bottom", common.legend = TRUE)

    ggsave(ggHist2_w4, filename = "./figures/hist-pred-week4.pdf", width = 16, heigh = 7)
    ggsave(ggHist2_w4, filename = "./figures/hist-pred-week4.png", width = 16, heigh = 7)

    ggHist2_w8 <- ggarrange(ggHist2_ccw8 + ggtitle("Complete case analysis"),
                            ggHist2_impw8 + ggtitle("Missing data analysis"),
                            legend = "bottom", common.legend = TRUE)

    ggsave(ggHist2_w8, filename = "./figures/hist-pred-week8.pdf", width = 15, heigh = 7)
    ggsave(ggHist2_w8, filename = "./figures/hist-pred-week8.png", width = 15, heigh = 7)

    ggHist2_w12 <- ggarrange(ggHist2_ccw12 + ggtitle("Complete case analysis"),
                             ggHist2_impw12 + ggtitle("Missing data analysis"),
                             legend = "bottom", common.legend = TRUE)

    ggsave(ggHist2_w12, filename = "./figures/hist-pred-week12.pdf", width = 16, heigh = 7)
    ggsave(ggHist2_w12, filename = "./figures/hist-pred-week12.png", width = 16, heigh = 7)


    ggROC_w4 <- ggarrange(ggROC_ccw4 + ggtitle("Complete case analysis"),
                          ggROC_impw4 + ggtitle("Missing data analysis"),
                          nrow = 2,
                          legend = "bottom", common.legend = TRUE)

    ggsave(ggROC_w4, filename = "./figures/ROC-pred-week4.pdf", width = 14, height = 10)
    ggsave(ggROC_w4, filename = "./figures/ROC-pred-week4.png", width = 14, height = 10)

    ggROC_w8 <- ggarrange(ggROC_ccw8 + ggtitle("Complete case analysis"),
                          ggROC_impw8 + ggtitle("Missing data analysis"),
                          nrow = 2,
                          legend = "bottom", common.legend = TRUE)

    ggsave(ggROC_w8, filename = "./figures/ROC-pred-week8.pdf", width = 14, height = 10)
    ggsave(ggROC_w8, filename = "./figures/ROC-pred-week8.png", width = 14, height = 10)

    ggROC_w12 <- ggarrange(ggROC_ccw12 + ggtitle("Complete case analysis"),
                           ggROC_impw12 + ggtitle("Missing data analysis"),
                           nrow = 2,
                           legend = "bottom", common.legend = TRUE)

    ggsave(ggROC_w12, filename = "./figures/ROC-pred-week12.pdf", width = 14, height = 10)
    ggsave(ggROC_w12, filename = "./figures/ROC-pred-week12.png", width = 14, height = 10)

    ggCali_w4 <- ggarrange(ggCali_ccw4 + ggtitle("Complete case analysis"),
                            ggCali_impw4 + ggtitle("Missing data analysis"),
                            legend = "bottom", common.legend = TRUE)

    ggsave(ggCali_w4, filename = "./figures/cali-pred-week4.pdf", width = 14)
    ggsave(ggCali_w4, filename = "./figures/cali-pred-week4.png", width = 14)

    ggCali_w8 <- ggarrange(ggCali_ccw8 + ggtitle("Complete case analysis"),
                            ggCali_impw8 + ggtitle("Missing data analysis"),
                            legend = "bottom", common.legend = TRUE)

    ggsave(ggCali_w8, filename = "./figures/cali-pred-week8.pdf", width = 14)
    ggsave(ggCali_w8, filename = "./figures/cali-pred-week8.png", width = 14)

    ggCali_w12 <- ggarrange(ggCali_ccw12 + ggtitle("Complete case analysis"),
                            ggCali_impw12 + ggtitle("Missing data analysis"),
                            legend = "bottom", common.legend = TRUE)

    ggsave(ggCali_w12, filename = "./figures/cali-pred-week12.pdf", width = 14)
    ggsave(ggCali_w12, filename = "./figures/cali-pred-week12.png", width = 14)
}

##----------------------------------------------------------------------
### figure-performance.R ends here
