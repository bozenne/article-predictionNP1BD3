### 1-prediction.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: dec  1 2021 (13:12) 
## Version: 
## Last-Updated: dec  2 2021 (18:44) 
##           By: Brice Ozenne
##     Update #: 34
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
path.FCT <- "./FCT"

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

source(file.path(path.FCT,"runAnalysis.R"))
source(file.path(path.code,"0-data-management.R"))

## * Process data
name.predictor <- c("sex","age","MR_OFCthick","HAMD17","hsCRP","lvpet","cognitive_cluster","EEG_vigilance","CATS_scoretotal","CAR_AUCi","neuroticism")
md.pattern(dfWR.NP1[,.SD,.SDcols = c(name.predictor,"Y_w8","Y_w12")]) 
##    sex age MR_OFCthick HAMD17 Y_w8 hsCRP lvpet cognitive_cluster EEG_vigilance Y_w12 CATS_scoretotal CAR_AUCi neuroticism   
## 48   1   1           1      1    1     1     1                 1             1     1               1        1           1  0
## 8    1   1           1      1    1     1     1                 1             1     1               1        1           0  1
## 7    1   1           1      1    1     1     1                 1             1     1               1        0           1  1
## 4    1   1           1      1    1     1     1                 1             1     1               1        0           0  2
## 2    1   1           1      1    1     1     1                 1             1     1               0        1           0  2
## 2    1   1           1      1    1     1     1                 1             1     1               0        0           0  3
## 1    1   1           1      1    1     1     1                 1             1     0               1        1           1  1
## 1    1   1           1      1    1     1     1                 1             1     0               1        0           0  3
## 2    1   1           1      1    1     1     1                 1             1     0               0        0           0  4
## 3    1   1           1      1    1     1     1                 1             0     1               1        1           1  1
## 1    1   1           1      1    1     1     1                 0             1     1               0        0           0  4
## 1    1   1           1      1    1     1     1                 0             0     1               0        1           0  4
## 1    1   1           1      1    1     1     0                 1             1     1               1        1           1  1
## 1    1   1           1      1    1     1     0                 1             1     1               0        1           0  3
## 1    1   1           1      1    1     0     1                 1             1     1               1        1           1  1
## 1    1   1           1      1    1     0     1                 1             1     1               0        1           0  3
## 1    1   1           1      1    0     1     1                 1             1     1               0        1           0  3
##      0   0           0      0    1     2     2                 2             4     4              11       17          24 67

nameR.predictor <- c("female","age","MR_OFCthick","HAMD17","low_hsCRP","lvpet","cognitive_cluster2","cognitive_cluster3","EEG_vigilance")
md.pattern(dfWR.NP1[,.SD,.SDcols = c(name.predictor,"Y_w8","Y_w12")]) 

##    sex age MR_OFCthick HAMD17 Y_w8 hsCRP lvpet cognitive_cluster EEG_vigilance Y_w12   
## 71   1   1           1      1    1     1     1                 1             1     1  0
## 4    1   1           1      1    1     1     1                 1             1     0  1
## 3    1   1           1      1    1     1     1                 1             0     1  1
## 1    1   1           1      1    1     1     1                 0             1     1  1
## 1    1   1           1      1    1     1     1                 0             0     1  2
## 2    1   1           1      1    1     1     0                 1             1     1  1
## 2    1   1           1      1    1     0     1                 1             1     1  1
## 1    1   1           1      1    0     1     1                 1             1     1  1
##      0   0           0      0    1     2     2                 2             4     4 15

## * Analysis: complete case 
## ** week 8
dfWR.NP1_ccw8 <- dfWR.NP1[rowSums(is.na(dfWR.NP1[,.SD,.SDcols = c("Y_w8",nameR.predictor)]))==0,]
ff_ccw8 <- Y_w8 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance
e.glm0_ccw8 <- glm(Y_w8 ~ female + age, data = dfWR.NP1_ccw8, family = binomial(link = "logit"))
e.glm_ccw8 <- glm(ff_ccw8,
                data = dfWR.NP1_ccw8, family = binomial(link = "logit"))
summary(e.glm_ccw8)
## Coefficients:
##                    Estimate Std. Error z value Pr(>|z|)   
## (Intercept)         9.15328    8.39873   1.090  0.27578   
## female             -0.35933    0.64098  -0.561  0.57507   
## age                 0.05472    0.04217   1.298  0.19443   
## MR_OFCthick        -5.84570    3.18471  -1.836  0.06642 . 
## HAMD17              0.16180    0.09515   1.700  0.08904 . 
## low_hsCRP           1.46954    0.72734   2.020  0.04334 * 
## lvpet              -2.04889    2.23331  -0.917  0.35892   
## cognitive_cluster2 -0.81977    0.69278  -1.183  0.23669   
## cognitive_cluster3 -1.89634    0.75932  -2.497  0.01251 * 
## EEG_vigilance      -1.72063    0.63876  -2.694  0.00707 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## (Dispersion parameter for binomial family taken to be 1)

##     Null deviance: 103.318  on 74  degrees of freedom

anova(e.glm_ccw8,e.glm0_ccw8, test = "Chisq")
## Model 1: Y_w8 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + 
##     cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance
## Model 2: Y_w8 ~ female + age
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)   
## 1        65     80.818                        
## 2        72    100.696 -7  -19.878  0.00584 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


e.ranger_ccw8 <- ranger(ff_ccw8, data = dfWR.NP1_ccw8, importance = "permutation")
importance_pvalues(e.ranger_ccw8, method = "altmann", 
                   formula = ff_ccw8, data = dfWR.NP1_ccw8)
##                     importance    pvalue
## sex               -0.006133188 0.8613861
## age                0.010621040 0.1485149
## MR_OFCthick        0.005533214 0.2772277
## HAMD17             0.008326261 0.1287129
## hsCRP             -0.001772680 0.5544554
## lvpet             -0.006358406 0.7227723
## cognitive_cluster  0.005000458 0.2277228
## EEG_vigilance      0.006514606 0.1980198

## ** week 12

dfWR.NP1_ccw12 <- dfWR.NP1[rowSums(is.na(dfWR.NP1[,.SD,.SDcols = c("Y_w12",nameR.predictor)]))==0,]
ff_ccw12 <- Y_w12 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance
e.glm0_ccw12 <- glm(Y_w12 ~ female + age, data = dfWR.NP1_ccw12, family = binomial(link = "logit"))
e.glm_ccw12 <- glm(ff_ccw12, data = dfWR.NP1_ccw12, family = binomial(link = "logit"))
summary(e.glm_ccw12)
## Coefficients:
##                    Estimate Std. Error z value Pr(>|z|)  
## (Intercept)        15.13771    9.66269   1.567   0.1172  
## female             -0.16310    0.71163  -0.229   0.8187  
## age                 0.12395    0.07958   1.558   0.1193  
## MR_OFCthick        -8.25027    3.58369  -2.302   0.0213 *
## HAMD17              0.17061    0.10751   1.587   0.1125  
## low_hsCRP           1.08090    0.83834   1.289   0.1973  
## lvpet              -0.57038    2.60458  -0.219   0.8267  
## cognitive_cluster2 -1.44482    0.79615  -1.815   0.0696 .
## cognitive_cluster3 -0.80512    0.88884  -0.906   0.3650  
## EEG_vigilance      -0.06684    0.60068  -0.111   0.9114  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## (Dispersion parameter for binomial family taken to be 1)

##     Null deviance: 83.100  on 71  degrees of freedom
anova(e.glm_ccw12,e.glm0_ccw12, test = "Chisq")
## Model 1: Y_w12 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + 
##     cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance
## Model 2: Y_w12 ~ female + age
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1        62     64.376                     
## 2        69     76.256 -7   -11.88   0.1046
## ---

e.ranger_ccw12 <- ranger(ff_ccw12, data = dfWR.NP1_ccw12, importance = "permutation")
importance_pvalues(e.ranger_ccw12, method = "altmann", 
                   formula = ff_ccw12, data = dfWR.NP1_ccw12)
##                     importance     pvalue
## sex               -0.001959833 0.66336634
## age                0.012006112 0.14851485
## MR_OFCthick        0.009389864 0.23762376
## HAMD17             0.014606562 0.07920792
## hsCRP              0.001540193 0.31683168
## lvpet              0.026410873 0.02970297
## cognitive_cluster  0.001450456 0.33663366
## EEG_vigilance      0.001412560 0.29702970

## * Analysis: after multiple imputation
n.imputed <- 100 ## number of imputed datasets

## ** week 8
dfWR.NP1_w8 <- dfWR.NP1[!is.na(dfWR.NP1$Y_w8),]
dfWR.NP1_2impw8 <- dfWR.NP1_w8[,.SD,.SDcols=c("Y_w8", "sex", "age", "MR_OFCthick", "HAMD17", "hsCRP", "lvpet", "cognitive_cluster",
                                            "EEG_vigilance", "CATS_scoretotal", "CAR_AUCi", "neuroticism")]
dfWR.NP1_2impw8$hsCRP <- as.factor(dfWR.NP1_2impw8$hsCRP)
dfWR.NP1_2impw8$cognitive_cluster <- as.factor(dfWR.NP1_2impw8$cognitive_cluster)

Mlink_w8 <- matrix(0, NCOL(dfWR.NP1_2impw8), NCOL(dfWR.NP1_2impw8), dimnames = list(names(dfWR.NP1_2impw8),names(dfWR.NP1_2impw8)))
Mlink_w8[setdiff(names(which(colSums(is.na(dfWR.NP1_2impw8))>0)),"Y_w8"),] <- 1
diag(Mlink_w8) <- 0

dfWRimp.NP1_w8 <- mice(dfWR.NP1_2impw8,
                m=n.imputed,
                maxit = 50, # number of iterations to obtain the imputed dataset
                predictorMatrix = Mlink_w8,
                method = c("","","","","","logreg","norm.predict","polr","norm.predict","norm.predict","norm.predict","norm.predict"), 
                seed = 500, printFlag = FALSE)
summary(dfWRimp.NP1_w8)
str(dfWRimp.NP1_w8$imp)
## stripplot(dfWRimp.NP1_w8, hsCRP ~ .imp, pch=20,cex=2)
## stripplot(dfWRimp.NP1_w8, lvpet ~ .imp, pch=20,cex=2)
## stripplot(dfWRimp.NP1_w8, cognitive_cluster ~ .imp, pch=20,cex=2)
## stripplot(dfWRimp.NP1_w8, EEG_vigilance ~ .imp, pch=20,cex=2)
## stripplot(dfWRimp.NP1_w8, CATS_scoretotal ~ .imp, pch=20,cex=2)
## stripplot(dfWRimp.NP1_w8, CAR_AUCi ~ .imp, pch=20,cex=2)
## stripplot(dfWRimp.NP1_w8, neuroticism ~ .imp, pch=20,cex=2)

e.glm_impw8 <- with(data = dfWRimp.NP1_w8,
                    glm(Y_w8 ~ sex + age + MR_OFCthick + HAMD17 + hsCRP + lvpet + cognitive_cluster + EEG_vigilance + CATS_scoretotal + CAR_AUCi + neuroticism,
                        family = binomial(link = "logit"))
                    )
summary(pool(e.glm_impw8))
##                  term      estimate    std.error   statistic       df    p.value
## 1         (Intercept)  4.1945493456 13.987339952  0.29988185 69.02318 0.76516854
## 2           sexfemale -0.5717989131  1.041637175 -0.54894250 68.81159 0.58482182
## 3                 age  0.0755876324  0.074187435  1.01887378 68.91031 0.31182675
## 4         MR_OFCthick -5.8979895143  4.930933463 -1.19612028 68.90659 0.23574932
## 5              HAMD17  0.1787615813  0.148788588  1.20144686 68.89238 0.23369199
## 6            hsCRPlow  1.6578954500  1.243986222  1.33272814 67.88079 0.18707734
## 7               lvpet -2.5959148089  3.851130291 -0.67406569 69.00085 0.50252154
## 8  cognitive_cluster2 -1.1655821422  1.168489680 -0.99751171 68.86783 0.32200826
## 9  cognitive_cluster3 -2.7735156585  1.405475898 -1.97336408 68.94606 0.05246373
## 10      EEG_vigilance -1.9900447896  1.126093200 -1.76721144 68.88929 0.08162173
## 11    CATS_scoretotal -0.0001989070  0.024957020 -0.00796998 69.03030 0.99366393
## 12           CAR_AUCi  0.0007743798  0.001497696  0.51704746 68.42289 0.60679043
## 13        neuroticism  0.0350596162  0.034299385  1.02216458 69.05222 0.31027079

## ** week 12
dfWR.NP1_w12 <- dfWR.NP1[!is.na(dfWR.NP1$Y_w12),]
dfWR.NP1_2impw12 <- dfWR.NP1_w12[,.SD,.SDcols=c("Y_w12", "sex", "age", "MR_OFCthick", "HAMD17", "hsCRP", "lvpet", "cognitive_cluster",
                                            "EEG_vigilance", "CATS_scoretotal", "CAR_AUCi", "neuroticism")]
dfWR.NP1_2impw12$hsCRP <- as.factor(dfWR.NP1_2impw12$hsCRP)
dfWR.NP1_2impw12$cognitive_cluster <- as.factor(dfWR.NP1_2impw12$cognitive_cluster)

Mlink_w12 <- matrix(0, NCOL(dfWR.NP1_2impw12), NCOL(dfWR.NP1_2impw12), dimnames = list(names(dfWR.NP1_2impw12),names(dfWR.NP1_2impw12)))
Mlink_w12[setdiff(names(which(colSums(is.na(dfWR.NP1_2impw12))>0)),"Y_w12"),] <- 1
diag(Mlink_w12) <- 0

dfWRimp.NP1_w12 <- mice(dfWR.NP1_2impw12,
                m=n.imputed,
                maxit = 50, # number of iterations to obtain the imputed dataset
                predictorMatrix = Mlink_w12,
                method = c("","","","","","logreg","norm.predict","polr","norm.predict","norm.predict","norm.predict","norm.predict"), 
                seed = 500, printFlag = FALSE)
summary(dfWRimp.NP1_w12)
str(dfWRimp.NP1_w12$imp)
## stripplot(dfWRimp.NP1_w12, hsCRP ~ .imp, pch=20,cex=2)
## stripplot(dfWRimp.NP1_w12, lvpet ~ .imp, pch=20,cex=2)
## stripplot(dfWRimp.NP1_w12, cognitive_cluster ~ .imp, pch=20,cex=2)
## stripplot(dfWRimp.NP1_w12, EEG_vigilance ~ .imp, pch=20,cex=2)
## stripplot(dfWRimp.NP1_w12, CATS_scoretotal ~ .imp, pch=20,cex=2)
## stripplot(dfWRimp.NP1_w12, CAR_AUCi ~ .imp, pch=20,cex=2)
## stripplot(dfWRimp.NP1_w12, neuroticism ~ .imp, pch=20,cex=2)

e.glm_impw12 <- with(data = dfWRimp.NP1_w12,
                    glm(Y_w12 ~ sex + age + MR_OFCthick + HAMD17 + hsCRP + lvpet + cognitive_cluster + EEG_vigilance + CATS_scoretotal + CAR_AUCi + neuroticism,
                        family = binomial(link = "logit"))
                    )
summary(pool(e.glm_impw12))
##                  term    estimate    std.error  statistic       df    p.value
## 1         (Intercept) 16.60192030 10.624574423  1.5625963 65.96826 0.12293247
## 2           sexfemale -0.90827463  0.786385086 -1.1549998 65.84322 0.25226499
## 3                 age  0.11489926  0.081182721  1.4153167 66.00335 0.16167681
## 4         MR_OFCthick -9.44685341  3.753564383 -2.5167687 65.85533 0.01428689
## 5              HAMD17  0.14377310  0.111194532  1.2929872 65.86093 0.20053278
## 6            hsCRPlow  0.98423550  0.920667085  1.0690460 64.06755 0.28906030
## 7               lvpet -0.64909275  2.854315308 -0.2274075 66.01747 0.82080967
## 8  cognitive_cluster2 -1.86895994  0.953438494 -1.9602313 65.36569 0.05423371
## 9  cognitive_cluster3 -1.72165818  1.021947040 -1.6846843 65.94132 0.09677729
## 10      EEG_vigilance -0.37342230  0.679826386 -0.5492907 65.96224 0.58466082
## 11    CATS_scoretotal  0.03726550  0.021748474  1.7134766 66.03807 0.09131598
## 12           CAR_AUCi  0.00132186  0.001322469  0.9995395 65.58734 0.32120694
## 13        neuroticism  0.01752110  0.025418000  0.6893186 65.96149 0.49304061

## * Prediction: complete data
## ** week 8
set.seed(10)
ePerf.ccw8 <- as.data.table(performance(list(glm0_ccw8 = e.glm0_ccw8, glm_ccw8 = e.glm_ccw8, rf_ccw8 = ranger(ff_ccw8, data = dfWR.NP1_ccw8, probability = TRUE)),
                                        data = dfWR.NP1_ccw8, fold.number = 100, fold.size = 8))
ePerf.ccw8
##       method metric     model   estimate          se      lower     upper      p.value p.value_comp
##  1: internal    auc glm0_ccw8 0.62769010 0.065372873 0.48583237 0.7404929 0.0753593107           NA
##  2: internal    auc  glm_ccw8 0.81133429 0.052319172 0.68202876 0.8920563 0.0001019312 1.130793e-02
##  3: internal    auc   rf_ccw8 1.00000000 0.000000000 1.00000000 1.0000000 0.0000000000 2.801347e-04
##  4: internal  brier glm0_ccw8 0.23870239 0.009886525 0.22009079 0.2588878           NA           NA
##  5: internal  brier  glm_ccw8 0.17485453 0.020745065 0.13857624 0.2206302           NA 7.973921e-04
##  6: internal  brier   rf_ccw8 0.09426488 0.006777196 0.08187524 0.1085294           NA 1.747974e-05
##  7:       cv    auc glm0_ccw8 0.60504762 0.051618437 0.49616688 0.6975264 0.0581009344           NA
##  8:       cv    auc  glm_ccw8 0.72050000 0.048066159 0.61355669 0.8025301 0.0002337026 6.427393e-02
##  9:       cv    auc   rf_ccw8 0.58383929 0.041934280 0.49706539 0.6608275 0.0578800366 5.176730e-03
## 10:       cv  brier glm0_ccw8 0.25654546 0.023120518 0.21500667 0.3061095           NA           NA
## 11:       cv  brier  glm_ccw8 0.22957101 0.059417334 0.13823243 0.3812626           NA 5.532493e-01
## 12:       cv  brier   rf_ccw8 0.25217593 0.014991855 0.22443972 0.2833398           NA 6.791321e-01

dt.CVpred_ccw8 <- as.data.table(cbind(do.call(rbind,lapply(apply(attr(attr(ePerf.ccw8,"predictions")$cv,"index"),3,list),"[[",1)),
                                 do.call(rbind,lapply(apply(attr(ePerf.ccw8,"predictions")$cv,3,list),"[[",1))))
dt.CVpred_ccw8$Y <- dfWR.NP1_ccw8$Y_w8[dt.CVpred_ccw8$observation]

ggplot(dt.CVpred_ccw8,aes(x=glm0_ccw8, fill = Y, group = Y)) + geom_density(alpha = 0.5, adjust = 0.4) + geom_rug()
ggplot(dt.CVpred_ccw8,aes(x=glm_ccw8, fill = Y, group = Y)) + geom_density(alpha = 0.5, adjust = 0.4) + geom_rug()
ggplot(dt.CVpred_ccw8,aes(x=rf_ccw8, fill = Y, group = Y)) + geom_density(alpha = 0.5, adjust = 0.4) + geom_rug()

ggplot(dt.CVpred_ccw8,aes(x=glm0_ccw8, y = as.numeric(Y))) + geom_smooth() + geom_rug() + geom_abline(slope = 1, intercept = 0, color = "red")
ggplot(dt.CVpred_ccw8,aes(x=glm_ccw8, y = as.numeric(Y))) + geom_smooth() + geom_rug() + geom_abline(slope = 1, intercept = 0, color = "red")
ggplot(dt.CVpred_ccw8,aes(x=rf_ccw8, y = as.numeric(Y))) + geom_smooth() + geom_rug() + geom_abline(slope = 1, intercept = 0, color = "red")

## repeated with permuted endpoint to get ok p-values
n.perm <- 5000
ls.resperm <- vector(mode = "list", length = n.perm)
for(iPerm in 1:n.perm){
    cat(iPerm," ")
    iData <- copy(dfWR.NP1_ccw8)
    iData$Y_w8 <- sample(iData$Y_w8)
    iEPerf <- suppressWarnings(performance(list(glm0_ccw8 = e.glm0_ccw8, glm_ccw8 = e.glm_ccw8, rf_ccw8 = e.ranger_ccw8),
                                           data = iData, fold.number = 100, fold.size = 8, trace = FALSE))
    ls.resperm[[iPerm]] <- cbind(perm = iPerm, iEPerf)
}
dt.resperm <- as.data.table(do.call(rbind, ls.resperm))

dt.resperm[metric == "auc",.(estimate = mean(estimate), type1error = mean(p.value<=0.05)), by = c("method","model")]
##      method     model  estimate type1error
## 1: internal glm0_ccw8 0.5001299 0.05045181
## 2: internal  glm_ccw8 0.5005380 0.05233434
## 3: internal   rf_ccw8 0.5005083 0.06137048
## 4:       cv glm0_ccw8 0.4994576 0.29066265
## 5:       cv  glm_ccw8 0.4992033 0.27334337
## 6:       cv   rf_ccw8 0.5002419 0.25753012

betahat <- ePerf.ccw8[method == "cv" & metric == "auc" & model == "glm_ccw8",estimate]
betahat.perm <- dt.resperm[method == "cv" & metric == "auc" & model == "glm_ccw8",estimate]
mean(betahat<=betahat.perm)
## [1] 0.003388554

betahat <- ePerf.ccw8[method == "cv" & metric == "brier" & model == "glm_ccw8",estimate]
betahat.perm <- dt.resperm[method == "cv" & metric == "brier" & model == "glm_ccw8",estimate]
mean(betahat>=betahat.perm)
 ## [1] 0.003012048

betahat <- ePerf.ccw8[method == "cv" & metric == "auc" & model == "glm0_ccw8",estimate]
betahat.perm <- dt.resperm[method == "cv" & metric == "auc" & model == "glm0_ccw8",estimate]
mean(betahat<=betahat.perm)
## [1] 0.1216114

betahat <- ePerf.ccw8[method == "cv" & metric == "brier" & model == "glm0_ccw8",estimate]
betahat.perm <- dt.resperm[method == "cv" & metric == "brier" & model == "glm0_ccw8",estimate]
mean(betahat<=betahat.perm)
## [1] 0.8189006

## betahat <- ePerf.ccw8[method == "cv" & metric == "auc" & model %in% c("glm0_ccw8","glm_ccw8"),diff(estimate)]
## betahat.perm <- dt.resperm[method == "cv" & metric == "auc" & model %in% c("glm0_ccw8","glm_ccw8"),diff(estimate),by = "perm"][[2]]
## mean(betahat<=betahat.perm)
## ## [1] 0.1393072

## betahat <- ePerf.ccw8[method == "cv" & metric == "brier" & model %in% c("glm0_ccw8","glm_ccw8"),diff(estimate)]
## betahat.perm <- dt.resperm[method == "cv" & metric == "brier" & model %in% c("glm0_ccw8","glm_ccw8"),diff(estimate),by = "perm"][[2]]
## mean(betahat>=betahat.perm)
## ## [1] 0.004141566

## saveRDS(list(estimate = ePerf.ccw8,
##             perm = dt.resperm),
##        file = file.path(path.results,"performance-w8.rds"))
## ePerf.ccw8 <- readRDS(file.path(path.results,"performance-w8.rds"))$estimate
## dt.resperm <- readRDS(file.path(path.results,"performance-w8.rds"))$perm


## as.data.table(performance(list(glm0_ccw8 = e.glm0_ccw8, glm_ccw8 = e.glm_ccw8),
##                           data = dfWR.NP1_ccw8, fold.number = 100, fold.size = 8, individual.fit = TRUE))

## ** week 12
set.seed(10)
ePerf.ccw12 <- as.data.table(performance(list(glm0_ccw12 = e.glm0_ccw12, glm_ccw12 = e.glm_ccw12, rf_ccw12 = ranger(ff_ccw12, data = dfWR.NP1_ccw12, probability = TRUE)),
                                        data = dfWR.NP1_ccw12, fold.number = 100, fold.size = 8))
ePerf.ccw12
##      method metric      model  estimate         se     lower     upper      p.value p.value_comp
##  1: internal    auc glm0_ccw12 0.6603774 0.06489009 0.5168380 0.7703840 3.025560e-02           NA
##  2: internal    auc  glm_ccw12 0.7944389 0.05642037 0.6561561 0.8818996 3.531358e-04 4.727703e-02
##  3: internal    auc   rf_ccw12 0.0000000 0.00000000 0.0000000 0.0000000 0.000000e+00 0.000000e+00
##  4: internal  brier glm0_ccw12 0.1816008 0.01908824 0.1477907 0.2231456           NA           NA
##  5: internal  brier  glm_ccw12 0.1493762 0.02147420 0.1126972 0.1979928           NA 4.420267e-02
##  6: internal  brier   rf_ccw12 0.6554699 0.02475623 0.6087010 0.7058323           NA 0.000000e+00
##  7:       cv    auc glm0_ccw12 0.6200357 0.05189989 0.5098140 0.7124024 3.380535e-02           NA
##  8:       cv    auc  glm_ccw12 0.6058274 0.04587958 0.5097117 0.6888767 3.185505e-02 7.783598e-01
##  9:       cv    auc   rf_ccw12 0.2321845 0.03926630 0.1600416 0.3123321 1.246880e-10 8.623630e-08
## 10:       cv  brier glm0_ccw12 0.2116598 0.02880101 0.1621114 0.2763524           NA           NA
## 11:       cv  brier  glm_ccw12 0.2404126 0.04349642 0.1686376 0.3427363           NA 2.905956e-01
## 12:       cv  brier   rf_ccw12 0.4660306 0.03430842 0.4034134 0.5383671           NA 9.047755e-04
dt.CVpred_ccw12 <- as.data.table(cbind(do.call(rbind,lapply(apply(attr(attr(ePerf.ccw12,"predictions")$cv,"index"),3,list),"[[",1)),
                                 do.call(rbind,lapply(apply(attr(ePerf.ccw12,"predictions")$cv,3,list),"[[",1))))
dt.CVpred_ccw12$Y <- dfWR.NP1_ccw12$Y_w12[dt.CVpred_ccw12$observation]

ggplot(dt.CVpred_ccw12,aes(x=glm0_ccw12, fill = Y, group = Y)) + geom_density(alpha = 0.5, adjust = 0.4) + geom_rug()
ggplot(dt.CVpred_ccw12,aes(x=glm_ccw12, fill = Y, group = Y)) + geom_density(alpha = 0.5, adjust = 0.4) + geom_rug()
ggplot(dt.CVpred_ccw12,aes(x=rf_ccw12, fill = Y, group = Y)) + geom_density(alpha = 0.5, adjust = 0.4) + geom_rug()

ggplot(dt.CVpred_ccw12,aes(x=glm0_ccw12, y = as.numeric(Y))) + geom_smooth() + geom_rug() + geom_abline(slope = 1, intercept = 0, color = "red")
ggplot(dt.CVpred_ccw12,aes(x=glm_ccw12, y = as.numeric(Y))) + geom_smooth() + geom_rug() + geom_abline(slope = 1, intercept = 0, color = "red")
ggplot(dt.CVpred_ccw12,aes(x=rf_ccw12, y = as.numeric(Y))) + geom_smooth() + geom_rug() + geom_abline(slope = 1, intercept = 0, color = "red")

## * Prediction: handling missing values

## ** week 8
set.seed(10)

e.glm0_w8 <- glm(Y_w8 ~ female + age, family = binomial(link = "logit"), data = dfWR.NP1_w8)
e.glm_w8 <- glm(Y_w8 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance + CATS_scoretotal + CAR_AUCi + neuroticism, family = binomial(link = "logit"), data = dfWR.NP1_w8)
e.ranger_w8 <- randomForest(Y_w8f ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance + CATS_scoretotal + CAR_AUCi + neuroticism,
                            data = dfWR.NP1_w8, na.action = na.roughfix)

## performance(e.ranger_w8, fold.number = 100, fold.size = 8)
##     method metric         model   estimate          se      lower     upper   p.value
## 1 internal    auc randomForest1 1.00000000 0.000000000 1.00000000 1.0000000 0.0000000
## 2 internal  brier randomForest1 0.03598857 0.002001488 0.03227197 0.0401332        NA
## 3       cv    auc randomForest1 0.51187202 0.049231003 0.41172790 0.6032751 0.8104758
## 4       cv  brier randomForest1 0.26984952 0.017368838 0.23786698 0.3061323        NA

set.seed(11)
ePerf.w8 <- as.data.table(performance(list(glm0_w8 = e.glm0_w8, glm_w8 = e.glm_w8), ## rf_w8 = e.ranger_w8),
                                         data = dfWR.NP1_w8, fold.number = 100, fold.size = 8, individual.fit = TRUE))
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
### 1-prediction.R ends here
