### figure-ROC.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: mar  3 2022 (18:37) 
## Version: 
## Last-Updated: mar  3 2022 (18:40) 
##           By: Brice Ozenne
##     Update #: 6
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

## ** week 4
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

## ** week 12

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



##----------------------------------------------------------------------
### figure-ROC.R ends here
