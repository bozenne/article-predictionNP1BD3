### table-performance.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: mar  3 2022 (18:37) 
## Version: 
## Last-Updated: mar 21 2022 (15:28) 
##           By: Brice Ozenne
##     Update #: 13
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
library(officer)

## * Load data
## list.files(path.results)
ePerf.ccw4 <- readRDS(file.path(path.results,"perf-cc-week4.rds"))
ePerf.ccw8 <- readRDS(file.path(path.results,"perf-cc-week8.rds"))
ePerf.ccw12 <- readRDS(file.path(path.results,"perf-cc-week12.rds"))
ePerf.impw4 <- readRDS(file.path(path.results,"perf-imp-week4.rds"))
ePerf.impw8 <- readRDS(file.path(path.results,"perf-imp-week8.rds"))
ePerf.impw12 <- readRDS(file.path(path.results,"perf-imp-week12.rds"))

## * Create table
digits.auc <- 3
digits.brier <- 3

tablePerf.cc <- rbind(cbind(week = 4,  ePerf.ccw4[,.(metric,model,estimate,p.value)]),
                      cbind(week = 8,  ePerf.ccw8[,.(metric,model,estimate,p.value)]),
                      cbind(week = 12,  ePerf.ccw12[,.(metric,model,estimate,p.value)]))
tablePerfW.cc <- dcast(tablePerf.cc, model+week~metric, value.var = c("estimate","p.value"))
tablePerfW.cc$model <- factor(sapply(strsplit(tablePerfW.cc$model, split = "_", fixed = TRUE),"[[",1),
                              levels = c("glm0","glm","rf",""),
                              labels = c("GLM (no biomarker)", "GLM (biomarkers)","RF (biomarkers)",""))
tablePerfW.cc <- tablePerfW.cc[order(tablePerfW.cc$model,tablePerfW.cc$week),]
tablePerfW.cc[,AUC := paste0(round(estimate_auc, digits.auc)," (p=",format.pval(p.value_auc,digits = 2, esp = 0.001),")")]
tablePerfW.cc[,Brier := paste0(round(estimate_brier, digits.brier)," (p=",format.pval(p.value_brier,digits = 2, esp = 0.001),")")]
tablePerfW.cc[,c("estimate_auc","estimate_brier","p.value_auc","p.value_brier") := NULL]
tablePerfW.cc$model[duplicated(tablePerfW.cc$model)] <- ""
tablePerfW.cc

##                 model week             AUC           Brier
## 1: GLM (no biomarker)    4 0.501 (p=0.210) 0.248 (p=0.308)
## 2:                       8  0.56 (p=0.062) 0.255 (p=0.159)
## 3:                      12 0.601 (p=0.032) 0.206 (p=0.038)
## 4:   GLM (biomarkers)    4 0.443 (p=0.656) 0.284 (p=0.626)
## 5:                       8 0.637 (p=0.025) 0.253 (p=0.022)
## 6:                      12 0.585 (p=0.133)  0.23 (p=0.119)
## 7:    RF (biomarkers)    4 0.491 (p=0.444) 0.261 (p=0.466)
## 8:                       8 0.537 (p=0.246) 0.261 (p=0.221)
## 9:                      12 0.689 (p=0.011) 0.194 (p=0.016)

tablePerf.imp <- rbind(cbind(week = 4,  ePerf.impw4[,.(metric,model,estimate,p.value)]),
                       cbind(week = 8,  ePerf.impw8[,.(metric,model,estimate,p.value)]),
                       cbind(week = 12,  ePerf.impw12[,.(metric,model,estimate,p.value)]))
tablePerfW.imp <- dcast(tablePerf.imp, model+week~metric, value.var = c("estimate","p.value"))
tablePerfW.imp$model <- factor(sapply(strsplit(tablePerfW.imp$model, split = "_", fixed = TRUE),"[[",1),
                               levels = c("glm0","glm","rf",""),
                               labels = c("GLM (no biomarker)", "GLM (biomarkers)","RF (biomarkers)",""))
tablePerfW.imp <- tablePerfW.imp[order(tablePerfW.imp$model,tablePerfW.imp$week),]
tablePerfW.imp[,AUC := paste0(round(estimate_auc, digits.auc)," (p=",format.pval(p.value_auc,digits = 2, esp = 0.001),")")]
tablePerfW.imp[,Brier := paste0(round(estimate_brier, digits.brier)," (p=",format.pval(p.value_brier,digits = 2, esp = 0.001),")")]
tablePerfW.imp[,c("estimate_auc","estimate_brier","p.value_auc","p.value_brier") := NULL]
tablePerfW.imp$model[duplicated(tablePerfW.imp$model)] <- ""
tablePerfW.imp
##                 model week             AUC           Brier
## 1: GLM (no biomarker)    4 0.507 (p=0.200) 0.245 (p=0.100)
## 2:                       8 0.513 (p=0.134) 0.259 (p=0.319)
## 3:                      12 0.608 (p=0.013) 0.207 (p=0.024)
## 4:   GLM (biomarkers)    4 0.505 (p=0.600) 0.335 (p=0.800)
## 5:                       8 0.643 (p=0.012) 0.311 (p=0.154)
## 6:                      12 0.605 (p=0.048) 0.263 (p=0.063)
## 7:    RF (biomarkers)    4 0.452 (p=0.900) 0.273 (p=0.900)
## 8:                       8 0.462 (p=0.601) 0.274 (p=0.577)
## 9:                      12 0.573 (p=0.118) 0.214 (p=0.082)

## * Export
if(FALSE){
    table.perf <- body_add_table(x = read_docx(), 
                                 value =  tablePerfW.cc)
    table.perf <- body_add_table(x = table.perf, 
                                 value =  tablePerfW.imp)
    print(table.perf, target = "./table/performance.docx")
}
##----------------------------------------------------------------------
### table-performance.R ends here
