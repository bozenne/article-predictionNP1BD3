### figure-predictionCV.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: mar  3 2022 (18:37) 
## Version: 
## Last-Updated: mar  3 2022 (18:39) 
##           By: Brice Ozenne
##     Update #: 5
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

## ** week 4

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



## ** week 8

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

## ** week 12

## *** CALIBRATION PLOT
ggCali_w8 <- ggplot(dt.CVpred_ccw8,aes(x=prediction, y = outcome))
ggCali_w8 <- ggCali_w8 + geom_smooth() + geom_rug() + geom_abline(slope = 1, intercept = 0, color = "red")
ggCali_w8 <- ggCali_w8 + facet_wrap(~model.lab)

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


##----------------------------------------------------------------------
### figure-predictionCV.R ends here
