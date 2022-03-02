### analysis-test.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: mar  2 2022 (15:37) 
## Version: 
## Last-Updated: mar  2 2022 (17:37) 
##           By: Brice Ozenne
##     Update #: 48
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

## * Parameter
n.imputed <- 25 ## number of imputed datasets
n.perm <- 1000 ## number of permutation for testing variable importance in random forest

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
library(ranger)
library(mgcv)
library(mice)
library(multcomp)
source(file.path(path.code,"FCT_glhtPool.R"))
library(ggplot2)
## devtools::install_github("NightingaleHealth/ggforestplot")
library(ggforestplot)

name.predictor <- c("sex","age","MR_OFCthick","HAMD17","hsCRP","lvpet","cognitive_cluster","EEG_vigilance","CATS_scoretotal","CAR_AUCi","neuroticism")
nameT.predictor <- c("MR_OFCthick","HAMD17","hsCRP","lvpet","cognitive_cluster","EEG_vigilance","CATS_scoretotal","CAR_AUCi","neuroticism")
nameR.predictor <- c("female","age","MR_OFCthick","HAMD17","low_hsCRP","lvpet","cognitive_cluster2","cognitive_cluster3","EEG_vigilance")
nameRT.predictor <- c("MR_OFCthick","HAMD17","low_hsCRP","lvpet","cognitive_cluster2","cognitive_cluster3","EEG_vigilance")

## * Load data
source(file.path(path.code,"0-data-management.R"))


## * Complete case 
cat("Hypothesis testing: complete case \n")

## ** Week 4
cat(" - week 4:")

dfWR.NP1_ccw4 <- dfWR.NP1[rowSums(is.na(dfWR.NP1[,.SD,.SDcols = c("Y_w4",nameR.predictor)]))==0,]
dfWR.NP1_ccw4S <- cbind(dfWR.NP1_ccw4[,c("Y_w4","female","low_hsCRP","cognitive_cluster2","cognitive_cluster3")],
                        scale(dfWR.NP1_ccw4[,c("age","MR_OFCthick", "HAMD17", "lvpet", "EEG_vigilance")])
                        )

## *** Logistic
cat(" glm")

ff_ccw4 <- Y_w4 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance

e.glm0_ccw4 <- glm(Y_w4 ~ female + age, data = dfWR.NP1_ccw4, family = binomial(link = "logit"))
e.glm_ccw4 <- glm(ff_ccw4, data = dfWR.NP1_ccw4, family = binomial(link = "logit"))

summary(e.glm_ccw4)
## Coefficients:
##                    Estimate Std. Error z value Pr(>|z|)
## (Intercept)         1.42609    6.87626   0.207    0.836
## female             -0.71403    0.55712  -1.282    0.200
## age                 0.03094    0.03093   1.000    0.317
## MR_OFCthick        -0.61019    2.48044  -0.246    0.806
## HAMD17             -0.02280    0.07997  -0.285    0.776
## low_hsCRP           0.55787    0.64030   0.871    0.384
## lvpet               0.66596    2.01920   0.330    0.742
## cognitive_cluster2 -0.18641    0.56994  -0.327    0.744
## cognitive_cluster3 -0.86503    0.63521  -1.362    0.173
## EEG_vigilance      -0.11693    0.46760  -0.250    0.803

anova(e.glm_ccw4,e.glm0_ccw4, test = "Chisq")
## Analysis of Deviance Table

## Model 1: Y_w4 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + 
##     cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance
## Model 2: Y_w4 ~ female + age
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1        70     99.939                     
## 2        77    103.744 -7  -3.8045    0.802

## table(EEG = dfWR.NP1_ccw8$EEG_vigilance, recovery = dfWR.NP1_ccw8$Y_w8, cluster3 = dfWR.NP1_ccw8$cognitive_cluster3)

## with rescaled predictors
e.glm_ccw4S <- update(e.glm_ccw4, data = dfWR.NP1_ccw4S)

## *** Random forest
cat(" ranger")
e.ranger_ccw4 <- ranger(ff_ccw4, data = dfWR.NP1_ccw4, probability = TRUE)
## ranger(ff_ccw4, data = dfWR.NP1_ccw4, probability = TRUE, num.trees = 5000, mtry = 1, min.node.size = 5)
e.rangerPerm_ccw4 <- importance_pvalues(ranger(ff_ccw4, data = dfWR.NP1_ccw4, importance = "permutation"), method = "altmann", 
                                        formula = ff_ccw4, data = dfWR.NP1_ccw4, num.permutations = n.perm)
e.rangerPerm_ccw4
##                      importance    pvalue
## female             -0.003251493 0.7029703
## age                 0.008970945 0.1980198
## MR_OFCthick         0.002039326 0.3861386
## HAMD17             -0.009179175 0.8316832
## low_hsCRP          -0.004023947 0.8118812
## lvpet              -0.002567274 0.4158416
## cognitive_cluster2 -0.001752197 0.6534653
## cognitive_cluster3  0.004628736 0.1386139
## EEG_vigilance      -0.005700278 0.8019802

cat(" \n")

## ** week 8
cat(" - week 8:")

dfWR.NP1_ccw8 <- dfWR.NP1[rowSums(is.na(dfWR.NP1[,.SD,.SDcols = c("Y_w8",nameR.predictor)]))==0,]
dfWR.NP1_ccw8S <- cbind(dfWR.NP1_ccw8[,c("Y_w8","female","low_hsCRP","cognitive_cluster2","cognitive_cluster3")],
                        scale(dfWR.NP1_ccw8[,c("age","MR_OFCthick", "HAMD17", "lvpet", "EEG_vigilance")])
                        )

## *** Logistic
cat(" glm")

ff_ccw8 <- Y_w8 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance
e.glm0_ccw8 <- glm(Y_w8 ~ female + age, data = dfWR.NP1_ccw8, family = binomial(link = "logit"))
summary(e.glm0_ccw8)
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)
## (Intercept) -0.73199    0.92281  -0.793    0.428
## female      -0.46970    0.52768  -0.890    0.373
## age          0.04435    0.02977   1.490    0.136
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

## table(EEG = dfWR.NP1_ccw8$EEG_vigilance, recovery = dfWR.NP1_ccw8$Y_w8, cluster3 = dfWR.NP1_ccw8$cognitive_cluster3)

## with rescaled predictors
e.glm_ccw8S <- update(e.glm_ccw8, data = dfWR.NP1_ccw8S)

## *** Random Forest
cat(" ranger")
e.ranger_ccw8 <- ranger(ff_ccw8, data = dfWR.NP1_ccw8, probability = TRUE)
## ranger(ff_ccw8, data = dfWR.NP1_ccw8, probability = TRUE, num.trees = 5000, mtry = 1, min.node.size = 5)
e.rangerPerm_ccw8 <- importance_pvalues(ranger(ff_ccw8, data = dfWR.NP1_ccw8, importance = "permutation"), method = "altmann", 
                                        formula = ff_ccw8, data = dfWR.NP1_ccw8, num.permutations = n.perm)

##                       importance     pvalue
## female             -0.0068013913 0.95104895
## age                 0.0073228254 0.23676324
## MR_OFCthick         0.0084958959 0.22877123
## HAMD17              0.0137409374 0.09790210
## low_hsCRP          -0.0020854868 0.58441558
## lvpet              -0.0072505951 0.67032967
## cognitive_cluster2 -0.0009645927 0.49450549
## cognitive_cluster3  0.0060916827 0.08991009
## EEG_vigilance       0.0084285585 0.11788212

## *** Splines
cat(" gam")
e.gam_ccw8 <- gam(Y_w8 ~ female + age + s(lvpet), data = dfWR.NP1_ccw8, family = binomial(link = "logit"))
summary(e.gam_ccw8)
## plot(e.gam_ccw8)

df.grid <- data.frame(lvpet = seq(min(dfWR.NP1_ccw8$lvpet), max(dfWR.NP1_ccw8$lvpet), length.out = 100),
                      age = round(mean(dfWR.NP1_ccw8$age)),
                      female = 1)
df.grid$proba <- predict(e.gam_ccw8, newdata = df.grid, type = "response")


ggSpline <- ggplot(df.grid, aes(x=lvpet,y=proba)) + geom_point() + geom_line() + ylab("Probability of recovery") + ggtitle(paste0(round(mean(dfWR.NP1_ccw8$age))," year-old female"))
ggSpline <- ggSpline + geom_rug(data = cbind(dfWR.NP1_ccw8,proba=rnorm(NROW(dfWR.NP1_ccw8))), sides = "b") + coord_cartesian(ylim = c(0,1)) + xlab("Latent variable PET")
ggSpline <- ggSpline + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm"))

ggCali <- ggplot(dfWR.NP1_ccw8, aes(x = exp(neocortex.log), y = lvpet)) + geom_point() + labs(y="Latent variable PET",x="Neocortex PET binding")
ggCali <- ggCali + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm"))

cat("\n")

## ** week 12
cat(" - week 12:")

dfWR.NP1_ccw12 <- dfWR.NP1[rowSums(is.na(dfWR.NP1[,.SD,.SDcols = c("Y_w12",nameR.predictor)]))==0,]
dfWR.NP1_ccw12S <- cbind(dfWR.NP1_ccw12[,c("Y_w12","female","low_hsCRP","cognitive_cluster2","cognitive_cluster3")],
                      scale(dfWR.NP1_ccw12[,c("age","MR_OFCthick", "HAMD17", "lvpet", "EEG_vigilance")])
                      )

## *** Logistic
cat(" glm")

ff_ccw12 <- Y_w12 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance
e.glm0_ccw12 <- glm(Y_w12 ~ female + age, data = dfWR.NP1_ccw12, family = binomial(link = "logit"))
summary(e.glm0_ccw12)
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)  
## (Intercept)  -2.1459     1.6759  -1.280   0.2004  
## female       -0.2110     0.6229  -0.339   0.7347  
## age           0.1290     0.0647   1.995   0.0461 *
## ---
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

## with rescaled predictors
e.glm_ccw12S <- update(e.glm_ccw12, data = dfWR.NP1_ccw12S)

## *** Random Forests
cat(" ranger")
e.ranger_ccw12 <- ranger(ff_ccw12, data = dfWR.NP1_ccw12, probability = TRUE)
## ranger(ff_ccw12, data = dfWR.NP1_ccw12, probability = TRUE, mtry = 5, num.trees = 5000)
e.rangerPerm_ccw12 <- importance_pvalues(ranger(ff_ccw12, data = dfWR.NP1_ccw12, importance = "permutation"), method = "altmann", 
                                         formula = ff_ccw12, data = dfWR.NP1_ccw12, num.permutations = n.perm)

##                       importance     pvalue
## female             -0.0026347027 0.66833167
## age                 0.0152237002 0.09790210
## MR_OFCthick         0.0111890206 0.14285714
## HAMD17              0.0119828113 0.10489510
## low_hsCRP          -0.0001679358 0.42857143
## lvpet               0.0330879010 0.01398601
## cognitive_cluster2  0.0014107702 0.29070929
## cognitive_cluster3 -0.0005832667 0.45254745
## EEG_vigilance       0.0032876852 0.22777223

e.ranger0_ccw12 <- ranger(Y_w12 ~ sex + age + lvpet, data = dfWR.NP1_ccw12, probability = TRUE)
e.rangerPerm0_ccw12 <- importance_pvalues(ranger(Y_w12 ~ sex + age + lvpet, data = dfWR.NP1_ccw12, importance = "permutation"),
                                          method = "altmann", num.permutations = n.perm,
                                          formula = Y_w12 ~ sex + age + lvpet, data = dfWR.NP1_ccw12)
##         importance     pvalue
## sex   -0.002760504 0.58941059
## age    0.005451897 0.28371628
## lvpet  0.024336625 0.03696304
## *** splines
cat(" gam")
e.gam_ccw12 <- gam(Y_w12 ~ female + age + s(lvpet), data = dfWR.NP1_ccw12, family = binomial(link = "logit"))
summary(e.gam_ccw12)
## plot(e.gam_ccw12)

df.grid <- data.frame(lvpet = seq(min(dfWR.NP1_ccw12$lvpet), max(dfWR.NP1_ccw12$lvpet), length.out = 100),
                      age = round(mean(dfWR.NP1_ccw12$age)),
                      female = 1)
df.grid$proba <- predict(e.gam_ccw12, newdata = df.grid, type = "response")


ggSpline <- ggplot(df.grid, aes(x=lvpet,y=proba)) + geom_point() + geom_line() + ylab("Probability of recovery") + ggtitle(paste0(round(mean(dfWR.NP1_ccw12$age))," year-old female"))
ggSpline <- ggSpline + geom_rug(data = cbind(dfWR.NP1_ccw12,proba=rnorm(NROW(dfWR.NP1_ccw12))), sides = "b") + coord_cartesian(ylim = c(0,1)) + xlab("Latent variable PET")
ggSpline <- ggSpline + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm"))

ggCali <- ggplot(dfWR.NP1_ccw12, aes(x = exp(neocortex.log), y = lvpet)) + geom_point() + labs(y="Latent variable PET",x="Neocortex PET binding")
ggCali <- ggCali + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm"))

## ggpubr::ggarrange(ggSpline,ggCali)
cat("\n")

## * Multiple imputation
cat("\nHypothesis testing: multiple imputation\n")
## ** week 4
cat(" - week 4: original")
dfWR.NP1_w4 <- dfWR.NP1[!is.na(dfWR.NP1$Y_w4),]
dfWR.NP1_2impw4 <- dfWR.NP1_w4[,.SD,.SDcols=c("Y_w4", "sex", "age", "MR_OFCthick", "HAMD17", "hsCRP", "lvpet", "cognitive_cluster",
                                              "EEG_vigilance", "CATS_scoretotal", "CAR_AUCi", "neuroticism")]
dfWR.NP1_2impw4$hsCRP <- as.factor(dfWR.NP1_2impw4$hsCRP)
dfWR.NP1_2impw4$cognitive_cluster <- as.factor(dfWR.NP1_2impw4$cognitive_cluster)

Mlink_w4 <- matrix(0, NCOL(dfWR.NP1_2impw4), NCOL(dfWR.NP1_2impw4), dimnames = list(names(dfWR.NP1_2impw4),names(dfWR.NP1_2impw4)))
Mlink_w4[setdiff(names(which(colSums(is.na(dfWR.NP1_2impw4))>0)),"Y_w4"),] <- 1
diag(Mlink_w4) <- 0

dfWRimp.NP1_w4 <- mice(dfWR.NP1_2impw4,
                       m=n.imputed,
                       maxit = 50, # number of iterations to obtain the imputed dataset
                       predictorMatrix = Mlink_w4,
                       method = c("","","","","","logreg","norm.predict","polr","norm.predict","norm.predict","norm.predict","norm.predict"), 
                       seed = 500, printFlag = FALSE)
## stripplot(dfWRimp.NP1_w4, hsCRP ~ .imp, pch=20,cex=2)
## stripplot(dfWRimp.NP1_w4, lvpet ~ .imp, pch=20,cex=2)
## stripplot(dfWRimp.NP1_w4, cognitive_cluster ~ .imp, pch=20,cex=2)
## stripplot(dfWRimp.NP1_w4, EEG_vigilance ~ .imp, pch=20,cex=2)
## stripplot(dfWRimp.NP1_w4, CATS_scoretotal ~ .imp, pch=20,cex=2)
## stripplot(dfWRimp.NP1_w4, CAR_AUCi ~ .imp, pch=20,cex=2)
## stripplot(dfWRimp.NP1_w4, neuroticism ~ .imp, pch=20,cex=2)

e.glm_impw4 <- with(data = dfWRimp.NP1_w4,
                    glm(Y_w4 ~ sex + age + MR_OFCthick + HAMD17 + hsCRP + lvpet + cognitive_cluster + EEG_vigilance + CATS_scoretotal + CAR_AUCi + neuroticism,
                        family = binomial(link = "logit"))
                    )
summary(pool(e.glm_impw4))
##                  term      estimate   std.error   statistic       df    p.value
## 1         (Intercept) -0.1793690478 7.624470966 -0.02352544 71.93220 0.98129628
## 2           sexfemale -1.2321733186 0.587163801 -2.09851717 71.77338 0.03937915
## 3                 age  0.0275380870 0.034086847  0.80788015 71.97783 0.42182153
## 4         MR_OFCthick -1.2829971752 2.557092139 -0.50174069 71.72269 0.61738669
## 5              HAMD17  0.0598778059 0.082151571  0.72886988 71.95672 0.46844694
## 6            hsCRPlow  0.8959100153 0.696294994  1.28668168 67.55340 0.20259717
## 7               lvpet -0.0738208731 2.088475266 -0.03534678 71.99344 0.97190105
## 8  cognitive_cluster2 -0.5971527133 0.637439757 -0.93679867 67.72045 0.35219105
## 9  cognitive_cluster3 -1.5156140043 0.699476076 -2.16678462 70.56203 0.03362950
## 10      EEG_vigilance -0.2031562616 0.492235816 -0.41272141 71.84613 0.68103980
## 11    CATS_scoretotal -0.0121817332 0.014509607 -0.83956327 72.03766 0.40393095
## 12           CAR_AUCi  0.0008065641 0.000939835  0.85819757 70.69040 0.39368390
## 13        neuroticism  0.0162929025 0.017300270  0.94177160 72.02879 0.34945755

## with rescaled predictors
cat(" centered")
dfWRimp.NP1_w4C <- dfWRimp.NP1_w4
for(iVar in c("age","MR_OFCthick", "HAMD17", "lvpet", "EEG_vigilance","CATS_scoretotal","CAR_AUCi","neuroticism")){
    dfWRimp.NP1_w4C$imp[[iVar]] <- (dfWRimp.NP1_w4$imp[[iVar]]-mean(dfWRimp.NP1_w4C$data[[iVar]], na.rm=TRUE))/sd(dfWRimp.NP1_w4C$data[[iVar]], na.rm=TRUE)
    dfWRimp.NP1_w4C$data[[iVar]] <- (dfWRimp.NP1_w4$data[[iVar]]-mean(dfWRimp.NP1_w4C$data[[iVar]], na.rm=TRUE))/sd(dfWRimp.NP1_w4C$data[[iVar]], na.rm=TRUE)
}

e.glm_impw4C <- with(data = dfWRimp.NP1_w4C,
                     expr = glm(Y_w4 ~ sex + age + MR_OFCthick + HAMD17 + hsCRP + lvpet + cognitive_cluster + EEG_vigilance + CATS_scoretotal + CAR_AUCi + neuroticism,
                                family = binomial(link = "logit"))
                     )


## same p-vlaues except intercept
## summary(e.glm_impw4$analyses[[1]])$coef
## summary(e.glm_impw4C$analyses[[1]])$coef

## summary(pool(eS.glm_impw4S$analyses))
## summary(pool(eS.glm_impw4$analyses))
cat("\n")


## ** week 8
cat(" - week 8: original")
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

## with rescaled predictors
cat(" centered")
dfWRimp.NP1_w8C <- dfWRimp.NP1_w8
for(iVar in c("age","MR_OFCthick", "HAMD17", "lvpet", "EEG_vigilance","CATS_scoretotal","CAR_AUCi","neuroticism")){
    dfWRimp.NP1_w8C$imp[[iVar]] <- (dfWRimp.NP1_w8$imp[[iVar]]-mean(dfWRimp.NP1_w8C$data[[iVar]], na.rm=TRUE))/sd(dfWRimp.NP1_w8C$data[[iVar]], na.rm=TRUE)
    dfWRimp.NP1_w8C$data[[iVar]] <- (dfWRimp.NP1_w8$data[[iVar]]-mean(dfWRimp.NP1_w8C$data[[iVar]], na.rm=TRUE))/sd(dfWRimp.NP1_w8C$data[[iVar]], na.rm=TRUE)
}

e.glm_impw8C <- with(data = dfWRimp.NP1_w8C,
                     expr = glm(Y_w8 ~ sex + age + MR_OFCthick + HAMD17 + hsCRP + lvpet + cognitive_cluster + EEG_vigilance + CATS_scoretotal + CAR_AUCi + neuroticism,
                                family = binomial(link = "logit"))
                     )

cat("\n")

## ** week 12
cat(" - week 12: original ")
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
cat("\n")

## with rescaled predictors
cat(" centered")
dfWRimp.NP1_w12C <- dfWRimp.NP1_w12
for(iVar in c("age","MR_OFCthick", "HAMD17", "lvpet", "EEG_vigilance","CATS_scoretotal","CAR_AUCi","neuroticism")){
    dfWRimp.NP1_w12C$imp[[iVar]] <- (dfWRimp.NP1_w12$imp[[iVar]]-mean(dfWRimp.NP1_w12C$data[[iVar]], na.rm=TRUE))/sd(dfWRimp.NP1_w12C$data[[iVar]], na.rm=TRUE)
    dfWRimp.NP1_w12C$data[[iVar]] <- (dfWRimp.NP1_w12$data[[iVar]]-mean(dfWRimp.NP1_w12C$data[[iVar]], na.rm=TRUE))/sd(dfWRimp.NP1_w12C$data[[iVar]], na.rm=TRUE)
}

e.glm_impw12C <- with(data = dfWRimp.NP1_w12C,
                     expr = glm(Y_w12 ~ sex + age + MR_OFCthick + HAMD17 + hsCRP + lvpet + cognitive_cluster + EEG_vigilance + CATS_scoretotal + CAR_AUCi + neuroticism,
                                family = binomial(link = "logit"))
                     )


cat(" \n")

## * Simultaneous statistical inference
cat("\nHypothesis testing: simulatenous inference \n")

## ** Complete case
cat(" - complete case")

## univariate Wald test
C_cc <- glht(e.glm_ccw4S)$linfct[nameRT.predictor,]
glht_cc <- list(week4 = confint(summary(glht(e.glm_ccw4S,C_cc))),
                week8 = confint(summary(glht(e.glm_ccw8S,C_cc))),
                week12 = confint(summary(glht(e.glm_ccw12S,C_cc))))



dtS.ass_cc <- rbind(cbind(time = "week 4", method = "cc", term = nameRT.predictor,
                          data.table(summary(e.glm_ccw4S)$coef[nameRT.predictor,-3]),
                          glht_cc$week4$confint[,2:3], adj.p.value = glht_cc$week4$test$pvalues),
                    cbind(time = "week 4", method = "cc", term = c("age", "female"),
                          data.table(summary(e.glm_ccw4S)$coef[c("age","female"),-3]),
                          lwr = NA, upr = NA, adj.p.value = NA),
                    cbind(time = "week 8", method = "cc", term = nameRT.predictor,
                          data.table(summary(e.glm_ccw8S)$coef[nameRT.predictor,-3]),
                          glht_cc$week8$confint[,2:3], adj.p.value = glht_cc$week8$test$pvalues),
                    cbind(time = "week 8", method = "cc", term = c("age", "female"),
                          data.table(summary(e.glm_ccw8S)$coef[c("age","female"),-3]),
                          lwr = NA, upr = NA, adj.p.value = NA),
                    cbind(time = "week 12", method = "cc", term = nameRT.predictor,
                          data.table(summary(e.glm_ccw12S)$coef[nameRT.predictor,-3]),
                          glht_cc$week12$confint[,2:3], adj.p.value = glht_cc$week12$test$pvalues),
                    cbind(time = "week 12", method = "cc", term = c("age", "female"),
                          data.table(summary(e.glm_ccw12S)$coef[c("age","female"),-3]),
                          lwr = NA, upr = NA, adj.p.value = NA))

names(dtS.ass_cc)[names(dtS.ass_cc)=="Estimate"] <- "estimate"
names(dtS.ass_cc)[names(dtS.ass_cc)=="Std. Error"] <- "std.error"
names(dtS.ass_cc)[names(dtS.ass_cc)=="Pr(>|z|)"] <- "p.value"
names(dtS.ass_cc)[names(dtS.ass_cc)=="lwr"] <- "lower.adj"
names(dtS.ass_cc)[names(dtS.ass_cc)=="upr"] <- "upper.adj"
dtS.ass_cc$lower <- dtS.ass_cc$estimate + qnorm(0.025) * dtS.ass_cc$std.error
dtS.ass_cc$upper <- dtS.ass_cc$estimate + qnorm(0.975) * dtS.ass_cc$std.error
dtS.ass_cc$time <- factor(dtS.ass_cc$time, levels = rev(as.character(unique(dtS.ass_cc$time))))
dtS.ass_cc$term <- factor(dtS.ass_cc$term,
                          levels = c("age","female","MR_OFCthick","HAMD17","low_hsCRP","lvpet","cognitive_cluster2","cognitive_cluster3","EEG_vigilance","CATS","Cortisol","Neuroticism"),     
                          labels = c("age","female","MR (OFC thickness)","HAMD17","hsCRP","PET (serotonin)","cognition (cluster 2)","cognition (cluster 3)","EEG (vigilance)","CATS","Cortisol","Neuroticism"),     
                          )

dtS.ass_cc <- rbind(dtS.ass_cc,
                    data.frame(time = c("week 4","week 8", "week 12"), method = "cc", term = c("CATS","Cortisol","Neuroticism"),
                               estimate = NA, std.error = NA, p.value = NA, lower.adj = NA, upper.adj = NA, lower = NA, upper = NA, adj.p.value = NA)
                    )


## max test
min(glht_cc$week4$test$pvalues)
## [1] 0.6185829
min(glht_cc$week8$test$pvalues)
## [1] 0.04664495
min(glht_cc$week12$test$pvalues)
## [1] 0.1341767

## multivariate Wald test
summary(glht_cc$week4, test = Chisqtest())
## Global Test:
##   Chisq DF Pr(>Chisq)
## 1 4.324  7     0.7418
summary(glht_cc$week8, test = Chisqtest())
## Global Test:
##   Chisq DF Pr(>Chisq)
## 1 13.66  7    0.05764
summary(glht_cc$week12, test = Chisqtest())
## Global Test:
##   Chisq DF Pr(>Chisq)
## 1 8.718  7     0.2735

## likelihood ratio test
anova(e.glm_ccw4,e.glm0_ccw4, test = "Chisq")
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1        68      95.98                     
## 2        75     100.77 -7  -4.7862    0.686
anova(e.glm_ccw8,e.glm0_ccw8, test = "Chisq")
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)   
## 1        65     80.818                        
## 2        72    100.696 -7  -19.878  0.00584 **
anova(e.glm_ccw12,e.glm0_ccw12, test = "Chisq")
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1        62     64.376                     
## 2        69     76.256 -7   -11.88   0.1046


## ** Missing data
C_imp <- glhtPool(e.glm_impw4C)$linfct[-(1:2),]
glht_imp <- list(week4 = confint(summary(glhtPool(e.glm_impw4C,C_imp))),
                 week8 = confint(summary(glhtPool(e.glm_impw8C,C_imp))),
                 week12 = confint(summary(glhtPool(e.glm_impw12C,C_imp))))

dtS.ass_imp <- rbind(cbind(time = "week 4", method = "MI", data.table(summary(pool(e.glm_impw4C))[-(1:3),], glht_imp$week4$confint[,2:3], adj.p.value = glht_imp$week4$test$pvalues)),
                     cbind(time = "week 4", method = "MI", data.table(summary(pool(e.glm_impw4C))[2:3,],lwr = NA, upr = NA, adj.p.value = NA)),
                     cbind(time = "week 8", method = "MI", data.table(summary(pool(e.glm_impw8C))[-(1:3),], glht_imp$week8$confint[,2:3], adj.p.value = glht_imp$week8$test$pvalues)),
                     cbind(time = "week 8", method = "MI", data.table(summary(pool(e.glm_impw8C))[2:3,],lwr = NA, upr = NA, adj.p.value = NA)),
                     cbind(time = "week 12", method = "MI", data.table(summary(pool(e.glm_impw12C))[-(1:3),], glht_imp$week12$confint[,2:3], adj.p.value = glht_imp$week12$test$pvalues)),
                     cbind(time = "week 12", method = "MI", data.table(summary(pool(e.glm_impw12C))[2:3,],lwr = NA, upr = NA, adj.p.value = NA)))
dtS.ass_imp$lower <- dtS.ass_imp$estimate + qt(0.025, df = dtS.ass_imp$df) * dtS.ass_imp$std.error
dtS.ass_imp$upper <- dtS.ass_imp$estimate + qt(0.975, df = dtS.ass_imp$df) * dtS.ass_imp$std.error
names(dtS.ass_imp)[names(dtS.ass_imp)=="lwr"] <- "lower.adj"
names(dtS.ass_imp)[names(dtS.ass_imp)=="upr"] <- "upper.adj"
dtS.ass_imp$term <- factor(dtS.ass_imp$term,
                          levels = c("sexfemale","age","MR_OFCthick","HAMD17","hsCRPlow","lvpet","cognitive_cluster2","cognitive_cluster3","EEG_vigilance","CATS_scoretotal","CAR_AUCi","neuroticism"),     
                          labels = c("female","age","MR (OFC thickness)","HAMD17","hsCRP","PET (serotonin)","cognition (cluster 2)","cognition (cluster 3)","EEG (vigilance)","CATS","Cortisol","Neuroticism"))
dtS.ass_imp$time <- factor(dtS.ass_imp$time, levels = rev(as.character(unique(dtS.ass_imp$time))))

## max test
min(glht_imp$week4$test$pvalues)
## [1] 0.2965254
min(glht_imp$week8$test$pvalues)
## [1] 0.02180895
min(glht_imp$week12$test$pvalues)
## [1] 0.07408294

## multivariate Wald test
summary(glht_imp$week4, test = Chisqtest())
## Global Test:
##   Chisq DF Pr(>Chisq)
## 1 8.618 10     0.5687
summary(glht_imp$week8, test = Chisqtest())
## Global Test:
##   Chisq DF Pr(>Chisq)
## 1 16.26 10    0.09248
summary(glht_imp$week12, test = Chisqtest())
## Global Test:
##   Chisq DF Pr(>Chisq)
## 1 12.95 10     0.2263


##----------------------------------------------------------------------
### analysis-test.R ends here
