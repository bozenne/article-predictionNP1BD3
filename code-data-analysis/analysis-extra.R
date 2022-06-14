### analysis-extra.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: jun 14 2022 (10:06) 
## Version: 
## Last-Updated: jun 14 2022 (10:25) 
##           By: Brice Ozenne
##     Update #: 3
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

## * Path
setwd("c:/Users/hpl802/Documents/Github/article-predictionNP1BD3/")
path.code <- "./code-data-analysis"

## * Packages and function
library(ggfortify)
library(ggplot2)
library(Rtsne)
library(ggpubr)

## * Load data
source(file.path(path.code,"0-data-management.R"))

name.predictor <- c("sex","age","MR_OFCthick","HAMD17","hsCRP","lvpet","cognitive_cluster","EEG_vigilance","CATS_scoretotal","CAR_AUCi","neuroticism")
nameT.predictor <- c("MR_OFCthick","HAMD17","hsCRP","lvpet","cognitive_cluster","EEG_vigilance","CATS_scoretotal","CAR_AUCi","neuroticism")
nameR.predictor <- c("female","age","MR_OFCthick","HAMD17","low_hsCRP","lvpet","cognitive_cluster2","cognitive_cluster3","EEG_vigilance")
nameRT.predictor <- c("MR_OFCthick","HAMD17","low_hsCRP","lvpet","cognitive_cluster2","cognitive_cluster3","EEG_vigilance")

dfWR.NP1$cognitive_cluster <- as.factor(dfWR.NP1$cognitive_cluster)
fff <- as.formula(paste("~1+",paste(name.predictor, collapse = "+")))
M.NP1 <- scale(model.matrix(fff, model.frame(fff, na.action = NULL, dfWR.NP1))[,-1])
MR.NP1 <- M.NP1[,setdiff(colnames(M.NP1),c("CATS_scoretotal","CAR_AUCi","neuroticism"))]

rdHAMD17 <- 100*(dfWR.NP1$HAMD17_w8-dfWR.NP1$HAMD17_w0)/dfWR.NP1$HAMD17_w0


## * pairwise correlation
pdf("./Results/Extra/correlation.pdf")
butils::ggHeatmap(cor(M.NP1, use = "pairwise"), add.text = "fill", round = 2)
dev.off()

## * PCA
PCA10 <- princomp(na.omit(M.NP1))
PCA7 <- princomp(na.omit(MR.NP1))

pdf("./Results/Extra/pca-biomarker.pdf")
ggarrange(autoplot(PCA7) + ggtitle(paste0("PCA on age, sex, and 7 biomarkers, \n complete case analysis (number of observations: ",PCA7$n.obs,")")),
          autoplot(PCA10) + ggtitle(paste0("PCA on age, sex, and 10 biomarkers, \n complete case analysis (number of observations: ",PCA10$n.obs,")")),
          nrow = 1)
dev.off()

## PCA10$sdev
## ggplot(as.data.frame(PCA10$scores), aes(x = Comp.1, y = Comp.2)) + geom_point()

## * T-sne
pdf("./Results/Extra/tsne-biomarker.pdf")
par(mfrow = c(2,4), mar = c(4,2,2,1.5))
plot(Rtsne(na.omit(M.NP1), theta = 0, perplexity = 0.1)$Y, asp=1,
     main ="PCA on age, sex, and 10 biomarkers", xlab = "perplexity 0.1") 
plot(Rtsne(na.omit(M.NP1), theta = 0, perplexity = 1)$Y, asp=1,
     main ="", xlab = "perplexity 1")
plot(Rtsne(na.omit(M.NP1), theta = 0, perplexity = 2.5)$Y, asp=1,
     main ="", xlab = "perplexity 2.5")
plot(Rtsne(na.omit(M.NP1), theta = 0, perplexity = 10)$Y, asp=1,
     main ="", xlab = "perplexity 10")

plot(Rtsne(na.omit(MR.NP1), theta = 0, perplexity = 0.1)$Y, asp=1,
     main = "PCA on age, sex, and 7 biomarkers", xlab = "perplexity 0.1")
plot(Rtsne(na.omit(MR.NP1), theta = 0, perplexity = 1)$Y, asp=1,
     main = "", xlab = "perplexity 1")
plot(Rtsne(na.omit(MR.NP1), theta = 0, perplexity = 2.5)$Y, asp=1,
     main = "", xlab = "perplexity 2.5")
plot(Rtsne(na.omit(MR.NP1), theta = 0, perplexity = 10)$Y, asp=1,
     main = "", xlab = "perplexity 10")
dev.off()

##----------------------------------------------------------------------
### analysis-extra.R ends here
