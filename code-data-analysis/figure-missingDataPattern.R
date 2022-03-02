### figure-missingDataPattern.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: mar  2 2022 (12:14) 
## Version: 
## Last-Updated: mar  2 2022 (12:32) 
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
library(ggplot2)
library(mice)

## * Load data
source(file.path(path.code,"0-data-management.R"))

## subset
name.predictor <- c("sex","age","MR_OFCthick","HAMD17","hsCRP","lvpet","cognitive_cluster","EEG_vigilance","CATS_scoretotal","CAR_AUCi","neuroticism")
dfWR.NP1.pred <- dfWR.NP1[,.SD,.SDcols = c(name.predictor,"Y_w4","Y_w8","Y_w12")]


## * Generate figure
dfW.mdpattern <- md.pattern(dfWR.NP1.pred, plot = FALSE)[,c(name.predictor,"Y_w4","Y_w8","Y_w12")]
rownames(dfW.mdpattern)[NROW(dfW.mdpattern)] <- "total"
md.pattern(dfWR.NP1[,.SD,.SDcols = c(name.predictor,"Y_w4","Y_w8","Y_w12")]) 
##    sex age MR_OFCthick HAMD17 hsCRP Y_w4 lvpet cognitive_cluster Y_w8 EEG_vigilance Y_w12 CATS_scoretotal CAR_AUCi neuroticism   
## 51   1   1           1      1     1    1     1                 1    1             1     1               1        1           1  0
## 8    1   1           1      1     1    1     1                 1    1             1     1               1        1           0  1
## 9    1   1           1      1     1    1     1                 1    1             1     1               1        0           1  1
## 4    1   1           1      1     1    1     1                 1    1             1     1               1        0           0  2
## 3    1   1           1      1     1    1     1                 1    1             1     1               0        1           0  2
## 3    1   1           1      1     1    1     1                 1    1             1     1               0        0           0  3
## 1    1   1           1      1     1    1     1                 1    1             1     0               1        0           0  3
## 1    1   1           1      1     1    1     1                 1    1             1     0               0        0           0  4
## 3    1   1           1      1     1    1     1                 1    1             0     1               1        1           1  1
## 1    1   1           1      1     1    1     1                 1    0             1     1               0        1           0  3
## 1    1   1           1      1     1    1     1                 1    0             1     0               1        0           0  4
## 1    1   1           1      1     1    1     1                 0    1             1     1               0        0           0  4
## 1    1   1           1      1     1    1     1                 0    1             0     1               0        1           0  4
## 1    1   1           1      1     1    1     0                 1    1             1     1               1        1           1  1
## 1    1   1           1      1     1    1     0                 1    1             1     1               0        1           0  3
## 1    1   1           1      1     1    0     1                 1    1             1     0               0        0           0  5
##      0   0           0      0     0    1     2                 2    2             4     4              12       21          26 74

dfL.mdpattern <- reshape2::melt(cbind(index = -(1:NROW(dfW.mdpattern)), n = rownames(dfW.mdpattern), as.data.frame(dfW.mdpattern)), id.vars = c("index","n"))
dfL.mdpattern$value.char <- factor(dfL.mdpattern$value, levels = 0:1, labels = c("missing","available"))
dfL.mdpattern$index.char <- as.factor(dfL.mdpattern$index)
dfL.mdpattern$variable <- gsub("Y_","HAMD6_",dfL.mdpattern$variable, fixed = TRUE)
neworder <- order(dfL.mdpattern[dfL.mdpattern$n=="total","value"],decreasing = TRUE)
dfL.mdpattern$variable2 <- factor(dfL.mdpattern$variable,
                                  levels = as.character(dfL.mdpattern[dfL.mdpattern$n=="total","variable"])[neworder],
                                  labels = paste0(as.character(dfL.mdpattern[dfL.mdpattern$n=="total","variable"]),"\n (missing=",dfL.mdpattern[dfL.mdpattern$n=="total","value"],")")[neworder]
                                  )
dfgg <- dfL.mdpattern[dfL.mdpattern$n!="total",]
dfgg$index.char <- factor(dfgg$index.char, levels = dfgg$index.char[order(as.numeric(dfgg$n)[!duplicated(dfgg$index.char)], decreasing = TRUE)])


gg.mdp <- ggplot(dfgg, aes(x=index.char, y=variable2, fill=value.char)) + geom_tile(color = "gray", size = 2)
gg.mdp <- gg.mdp + labs(fill = "", y = "")
gg.mdp <- gg.mdp + scale_x_discrete("number of patients",
                                    labels = setNames(dfL.mdpattern$n[!duplicated(dfL.mdpattern$index.char)], dfL.mdpattern$index.char[!duplicated(dfL.mdpattern$index.char)]))
gg.mdp <- gg.mdp + theme(text = element_text(size=15),
                 axis.line = element_line(size = 1.25),
                 axis.ticks = element_line(size = 2),
                 axis.ticks.length=unit(.25, "cm"))
gg.mdp

## * Export
if(FALSE){
    ggsave(gg.mdp, filename = "./figures/gg-missingPattern.pdf", width = 12)
    ggsave(gg.mdp, filename = "./figures/gg-missingPattern.png", width = 12)
}

##----------------------------------------------------------------------
### figure-missingDataPattern.R ends here
