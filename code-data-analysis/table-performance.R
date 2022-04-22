### table-performance.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: mar  3 2022 (18:37) 
## Version: 
## Last-Updated: apr 22 2022 (16:03) 
##           By: Brice Ozenne
##     Update #: 23
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
keep.col <- c("method","metric","model","p.value2","p.value_comp2")

## list.files(path.results)
ePerf.ccw4_IF <- as.data.table(readRDS(file.path(path.results,"perf-cc-week4-IF.rds")))
ePerf.ccw4_perm <- as.data.table(readRDS(file.path(path.results,"perf-cc-week4-perm.rds")))
setkeyv(ePerf.ccw4_IF, c("method","metric","model"))
setkeyv(ePerf.ccw4_perm, c("method","metric","model"))
## range(ePerf.ccw4_IF$estimate - ePerf.ccw4_perm$estimate)
ePerf.ccw4 <- cbind(ePerf.ccw4_IF,
                    p.value2 = ePerf.ccw4_perm$p.value,
                    p.value_comp2 = ePerf.ccw4_perm$p.value_comp)

ePerf.ccw8_IF <- as.data.table(readRDS(file.path(path.results,"perf-cc-week8-IF.rds")))
ePerf.ccw8_perm <- as.data.table(readRDS(file.path(path.results,"perf-cc-week8-perm.rds")))
setkeyv(ePerf.ccw8_IF, c("method","metric","model"))
setkeyv(ePerf.ccw8_perm, c("method","metric","model"))
## range(ePerf.ccw8_IF$estimate - ePerf.ccw8_perm$estimate)
ePerf.ccw8 <- cbind(ePerf.ccw8_IF,
                    p.value2 = ePerf.ccw8_perm$p.value,
                    p.value_comp2 = ePerf.ccw8_perm$p.value_comp)

ePerf.ccw12_IF <- as.data.table(readRDS(file.path(path.results,"perf-cc-week12-IF.rds")))
ePerf.ccw12_perm <- as.data.table(readRDS(file.path(path.results,"perf-cc-week12-perm.rds")))
setkeyv(ePerf.ccw12_IF, c("method","metric","model"))
setkeyv(ePerf.ccw12_perm, c("method","metric","model"))
## range(ePerf.ccw12_IF$estimate - ePerf.ccw12_perm$estimate)
ePerf.ccw12 <- cbind(ePerf.ccw12_IF,
                    p.value2 = ePerf.ccw12_perm$p.value,
                    p.value_comp2 = ePerf.ccw12_perm$p.value_comp)

ePerf.cctraj_IF <- as.data.table(readRDS(file.path(path.results,"perf-cc-traj-IF.rds")))
ePerf.cctraj_perm <- as.data.table(readRDS(file.path(path.results,"perf-cc-traj-perm.rds")))
setkeyv(ePerf.cctraj_IF, c("method","metric","model"))
setkeyv(ePerf.cctraj_perm, c("method","metric","model"))
## range(ePerf.cctraj_IF$estimate - ePerf.cctraj_perm$estimate)
ePerf.cctraj <- cbind(ePerf.cctraj_IF,
                      p.value2 = ePerf.cctraj_perm$p.value,
                      p.value_comp2 = ePerf.cctraj_perm$p.value_comp)

ePerf.impw4_IF <- as.data.table(readRDS(file.path(path.results,"perf-imp-week4-IF.rds")))
ePerf.impw4_perm <- as.data.table(readRDS(file.path(path.results,"perf-imp-week4-perm.rds")))
setkeyv(ePerf.impw4_IF, c("method","metric","model"))
setkeyv(ePerf.impw4_perm, c("method","metric","model"))
## range(ePerf.impw4_IF$estimate - ePerf.impw4_perm$estimate)
ePerf.impw4 <- cbind(ePerf.impw4_IF,
                     p.value2 = ePerf.impw4_perm$p.value,
                     p.value_comp2 = ePerf.impw4_perm$p.value_comp)

ePerf.impw8_IF <- as.data.table(readRDS(file.path(path.results,"perf-imp-week8-IF.rds")))
ePerf.impw8_perm <- as.data.table(readRDS(file.path(path.results,"perf-imp-week8-perm.rds")))
setkeyv(ePerf.impw8_IF, c("method","metric","model"))
setkeyv(ePerf.impw8_perm, c("method","metric","model"))
## range(ePerf.impw8_IF$estimate - ePerf.impw8_perm$estimate)
ePerf.impw8 <- cbind(ePerf.impw8_IF,
                     p.value2 = ePerf.impw8_perm$p.value,
                     p.value_comp2 = ePerf.impw8_perm$p.value_comp)

ePerf.impw12_IF <- as.data.table(readRDS(file.path(path.results,"perf-imp-week12-IF.rds")))
ePerf.impw12_perm <- as.data.table(readRDS(file.path(path.results,"perf-imp-week12-perm.rds")))
setkeyv(ePerf.impw12_IF, c("method","metric","model"))
setkeyv(ePerf.impw12_perm, c("method","metric","model"))
## range(ePerf.impw12_IF$estimate - ePerf.impw12_perm$estimate)
ePerf.impw12 <- cbind(ePerf.impw12_IF,
                     p.value2 = ePerf.impw12_perm$p.value,
                     p.value_comp2 = ePerf.impw12_perm$p.value_comp)

ePerf.imptraj_IF <- as.data.table(readRDS(file.path(path.results,"perf-imp-traj-IF.rds")))
ePerf.imptraj_perm <- as.data.table(readRDS(file.path(path.results,"perf-imp-traj-perm.rds")))
setkeyv(ePerf.imptraj_IF, c("method","metric","model"))
setkeyv(ePerf.imptraj_perm, c("method","metric","model"))
## range(ePerf.cctraj_IF$estimate - ePerf.cctraj_perm$estimate)
ePerf.imptraj <- cbind(ePerf.imptraj_IF,
                      p.value2 = ePerf.imptraj_perm$p.value,
                      p.value_comp2 = ePerf.imptraj_perm$p.value_comp)

## * Create table
digits.auc <- 3
digits.brier <- 3

tablePerf.cc <- rbind(cbind(week = 4,  ePerf.ccw4[method=="cv",.(metric,model,estimate,p.value,p.value2)]),
                      cbind(week = 8,  ePerf.ccw8[method=="cv",.(metric,model,estimate,p.value,p.value2)]),
                      cbind(week = 12,  ePerf.ccw12[method=="cv",.(metric,model,estimate,p.value,p.value2)]),
                      cbind(week = 4812,  ePerf.cctraj[method=="cv",.(metric,model,estimate,p.value,p.value2)]))
tablePerfW.cc <- dcast(tablePerf.cc, model+week~metric, value.var = c("estimate","p.value","p.value2"))
tablePerfW.cc$model <- factor(sapply(strsplit(tablePerfW.cc$model, split = "_", fixed = TRUE),"[[",1),
                              levels = c("glm0","glm","rf",""),
                              labels = c("GLM (no biomarker)", "GLM (biomarkers)","RF (biomarkers)",""))
tablePerfW.cc <- tablePerfW.cc[order(tablePerfW.cc$model,tablePerfW.cc$week),]
tablePerfW.cc[,AUC := paste0(round(estimate_auc, digits.auc),
                             " (p(IF)=",format.pval(p.value_auc,digits = 2, esp = 0.001),
                             ", p(perm)=",format.pval(p.value2_auc,digits = 2, esp = 0.001),")",sep="")]
tablePerfW.cc[,Brier := paste0(round(estimate_brier, digits.brier),
                             " (p(IF)=",format.pval(p.value_brier,digits = 2, esp = 0.001),
                             ", p(perm)=",format.pval(p.value2_brier,digits = 2, esp = 0.001),")",sep="")]
tablePerfW.cc[,c("estimate_auc","estimate_brier","p.value_auc","p.value_brier","p.value2_auc","p.value2_brier") := NULL]
tablePerfW.cc$model[duplicated(tablePerfW.cc$model)] <- ""
tablePerfW.cc

##                  model week                                 AUC                           Brier
##  1: GLM (no biomarker)    4 0.529 (p(IF)=0.6592, p(perm)=0.215) 0.244 (p(IF)=NA, p(perm)=0.328)
##  2:                       8 0.582 (p(IF)=0.2119, p(perm)=0.073) 0.253 (p(IF)=NA, p(perm)=0.206)
##  3:                      12 0.615 (p(IF)=0.1089, p(perm)=0.033) 0.204 (p(IF)=NA, p(perm)=0.037)
##  4:                    4812 0.604 (p(IF)=0.1241, p(perm)=0.043)  0.23 (p(IF)=NA, p(perm)=0.030)
##  5:   GLM (biomarkers)    4 0.443 (p(IF)=0.3290, p(perm)=0.733) 0.285 (p(IF)=NA, p(perm)=0.787)
##  6:                       8 0.644 (p(IF)=0.0338, p(perm)=0.034)  0.25 (p(IF)=NA, p(perm)=0.031)
##  7:                      12 0.583 (p(IF)=0.2447, p(perm)=0.149)  0.23 (p(IF)=NA, p(perm)=0.156)
##  8:                    4812 0.645 (p(IF)=0.0341, p(perm)=0.026) 0.244 (p(IF)=NA, p(perm)=0.035)
##  9:    RF (biomarkers)    4  0.49 (p(IF)=0.8708, p(perm)=0.536)  0.26 (p(IF)=NA, p(perm)=0.549)
## 10:                       8  0.54 (p(IF)=0.5256, p(perm)=0.306)  0.26 (p(IF)=NA, p(perm)=0.286)
## 11:                      12 0.694 (p(IF)=0.0046, p(perm)=0.013) 0.193 (p(IF)=NA, p(perm)=0.020)
## 12:                    4812 0.597 (p(IF)=0.1321, p(perm)=0.104) 0.238 (p(IF)=NA, p(perm)=0.103)

tablePerf.imp <- rbind(cbind(week = 4,  ePerf.impw4[method=="cv",.(metric,model,estimate,p.value,p.value2)]),
                       cbind(week = 8,  ePerf.impw8[method=="cv",.(metric,model,estimate,p.value,p.value2)]),
                       cbind(week = 12,  ePerf.impw12[method=="cv",.(metric,model,estimate,p.value,p.value2)]),
                       cbind(week = 4812,  ePerf.imptraj[method=="cv",.(metric,model,estimate,p.value,p.value2)])
                       )
tablePerfW.imp <- dcast(tablePerf.imp, model+week~metric, value.var = c("estimate","p.value","p.value2"))
tablePerfW.imp$model <- factor(sapply(strsplit(tablePerfW.imp$model, split = "_", fixed = TRUE),"[[",1),
                               levels = c("glm0","glm","rf",""),
                               labels = c("GLM (no biomarker)", "GLM (biomarkers)","RF (biomarkers)",""))
tablePerfW.imp <- tablePerfW.imp[order(tablePerfW.imp$model,tablePerfW.imp$week),]
tablePerfW.imp[,AUC := paste0(round(estimate_auc, digits.auc),
                             " (p(IF)=",format.pval(p.value_auc,digits = 2, esp = 0.001),
                             ", p(perm)=",format.pval(p.value2_auc,digits = 2, esp = 0.001),")",sep="")]
tablePerfW.imp[,Brier := paste0(round(estimate_brier, digits.brier),
                             " (p(IF)=",format.pval(p.value_brier,digits = 2, esp = 0.001),
                             ", p(perm)=",format.pval(p.value2_brier,digits = 2, esp = 0.001),")",sep="")]
tablePerfW.imp[,c("estimate_auc","estimate_brier","p.value_auc","p.value_brier","p.value2_auc","p.value2_brier") := NULL]
tablePerfW.imp$model[duplicated(tablePerfW.imp$model)] <- ""
tablePerfW.imp

##                  model week                                AUC                           Brier
##  1: GLM (no biomarker)    4 0.514 (p(IF)=0.812, p(perm)=0.272) 0.245 (p(IF)=NA, p(perm)=0.179)
##  2:                       8 0.549 (p(IF)=0.420, p(perm)=0.152) 0.253 (p(IF)=NA, p(perm)=0.289)
##  3:                      12 0.622 (p(IF)=0.079, p(perm)=0.031) 0.204 (p(IF)=NA, p(perm)=0.033)
##  4:                    4812 0.586 (p(IF)=0.166, p(perm)=0.056) 0.231 (p(IF)=NA, p(perm)=0.025)
##  5:   GLM (biomarkers)    4 0.489 (p(IF)=0.847, p(perm)=0.521) 0.287 (p(IF)=NA, p(perm)=0.471)
##  6:                       8 0.621 (p(IF)=0.059, p(perm)=0.068) 0.258 (p(IF)=NA, p(perm)=0.057)
##  7:                      12 0.617 (p(IF)=0.098, p(perm)=0.072) 0.227 (p(IF)=NA, p(perm)=0.061)
##  8:                    4812 0.609 (p(IF)=0.080, p(perm)=0.048)  0.26 (p(IF)=NA, p(perm)=0.068)
##  9:    RF (biomarkers)    4  0.52 (p(IF)=0.739, p(perm)=0.390) 0.256 (p(IF)=NA, p(perm)=0.399)
## 10:                       8 0.478 (p(IF)=0.710, p(perm)=0.596) 0.269 (p(IF)=NA, p(perm)=0.615)
## 11:                      12 0.635 (p(IF)=0.044, p(perm)=0.044) 0.203 (p(IF)=NA, p(perm)=0.050)
## 12:                    4812 0.546 (p(IF)=0.444, p(perm)=0.257) 0.247 (p(IF)=NA, p(perm)=0.252)

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
