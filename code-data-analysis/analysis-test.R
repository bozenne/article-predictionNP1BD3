### analysis-test.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: mar  2 2022 (15:37) 
## Version: 
## Last-Updated: mar 18 2022 (11:01) 
##           By: Brice Ozenne
##     Update #: 75
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

rm(list=ls())

## * Parameters
n.imputed <- 100 ## number of imputed datasets
n.perm <- 1000 ## number of permutation for testing variable importance in random forest

## * Path
if(system("whoami",intern=TRUE)=="hpl802"){
    ## nothing: on the server
}else if(system("whoami",intern=TRUE)=="unicph\\hpl802"){
    setwd("c:/Users/hpl802/Documents/Github/article-predictionNP1BD3/")
}else{ ## 
    setwd("Vibeke put your path here")
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
## devtools::install_github("NightingaleHealth/ggforestplot")

name.predictor <- c("sex","age","MR_OFCthick","HAMD17","hsCRP","lvpet","cognitive_cluster","EEG_vigilance","CATS_scoretotal","CAR_AUCi","neuroticism")
nameT.predictor <- c("MR_OFCthick","HAMD17","hsCRP","lvpet","cognitive_cluster","EEG_vigilance","CATS_scoretotal","CAR_AUCi","neuroticism")
nameR.predictor <- c("female","age","MR_OFCthick","HAMD17","low_hsCRP","lvpet","cognitive_cluster2","cognitive_cluster3","EEG_vigilance")
nameRT.predictor <- c("MR_OFCthick","HAMD17","low_hsCRP","lvpet","cognitive_cluster2","cognitive_cluster3","EEG_vigilance")

## * Load data
## setwd("h:/SundKonsolidering_BioStatHome/Cluster/BrainDrug-WP3/")
## load(file.path(path.results,"test.Rdata"))
source(file.path(path.code,"0-data-management.R"))
print(dim(dfWR.NP1))

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

## (Dispersion parameter for binomial family taken to be 1)

##     Null deviance: 106.819  on 79  degrees of freedom
## Residual deviance:  99.939  on 70  degrees of freedom

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
##                       importance     pvalue
## female             -0.0004630476 0.45354645
## age                 0.0111673221 0.16983017
## MR_OFCthick         0.0028598950 0.35864136
## HAMD17             -0.0036899090 0.60539461
## low_hsCRP          -0.0029902081 0.73926074
## lvpet               0.0075185611 0.24375624
## cognitive_cluster2 -0.0007050308 0.48451548
## cognitive_cluster3  0.0092599240 0.06293706
## EEG_vigilance      -0.0018836544 0.54645355

cat(" \n")

## ** week 8
cat(" - week 8:")

dfWR.NP1_ccw8 <- dfWR.NP1[rowSums(is.na(dfWR.NP1[,.SD,.SDcols = c("Y_w8",nameR.predictor)]))==0,]
dfWR.NP1_ccw8S <- cbind(dfWR.NP1_ccw8[,c("Y_w8","female","low_hsCRP","cognitive_cluster2","cognitive_cluster3")],
                        scale(dfWR.NP1_ccw8[,c("age","MR_OFCthick", "HAMD17", "lvpet", "EEG_vigilance")])
                        )

## dfWR.NP1[,c("Y_w8","NP1_comment")]
## dfWR.NP1[dfWR.NP1$NP1_comment == "Drop-out at week 7. Suicidal attempt, did not want more medicine.",]

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

## (Dispersion parameter for binomial family taken to be 1)

##     Null deviance: 109.20  on 78  degrees of freedom
## Residual deviance: 105.84  on 76  degrees of freedom
e.glm_ccw8 <- glm(ff_ccw8,
                  data = dfWR.NP1_ccw8, family = binomial(link = "logit"))
summary(e.glm_ccw8)
## Coefficients:
##                    Estimate Std. Error z value Pr(>|z|)  
## (Intercept)         7.74974    7.71591   1.004   0.3152  
## female             -0.45764    0.59550  -0.768   0.4422  
## age                 0.05362    0.03862   1.388   0.1650  
## MR_OFCthick        -4.34680    2.84081  -1.530   0.1260  
## HAMD17              0.08075    0.08310   0.972   0.3312  
## low_hsCRP           0.82377    0.63638   1.294   0.1955  
## lvpet              -1.20918    2.08358  -0.580   0.5617  
## cognitive_cluster2 -0.50806    0.62220  -0.817   0.4142  
## cognitive_cluster3 -1.39105    0.67730  -2.054   0.0400 *
## EEG_vigilance      -1.23341    0.54858  -2.248   0.0246 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## (Dispersion parameter for binomial family taken to be 1)

##     Null deviance: 109.201  on 78  degrees of freedom
## Residual deviance:  92.452  on 69  degrees of freedom

anova(e.glm_ccw8,e.glm0_ccw8, test = "Chisq")
## Model 1: Y_w8 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + 
##     cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance
## Model 2: Y_w8 ~ female + age
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)  
## 1        69     92.452                       
## 2        76    105.838 -7  -13.387  0.06323 .
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
e.rangerPerm_ccw8
##                       importance    pvalue
## female             -0.0046622586 0.8501499
## age                 0.0038266849 0.3256743
## MR_OFCthick         0.0084196899 0.2327672
## HAMD17              0.0025092369 0.3646354
## low_hsCRP          -0.0011115631 0.5254745
## lvpet              -0.0073080258 0.6933067
## cognitive_cluster2  0.0005137546 0.3696304
## cognitive_cluster3  0.0058086586 0.1238761
## EEG_vigilance       0.0034149398 0.2527473

## *** Splines
cat(" gam")
e.gam_ccw8 <- gam(Y_w8 ~ female + age + s(lvpet), data = dfWR.NP1_ccw8, family = binomial(link = "logit"))
summary(e.gam_ccw8)
## plot(e.gam_ccw8)

## Parametric coefficients:
##             Estimate Std. Error z value Pr(>|z|)  
## (Intercept) -0.76237    0.95158  -0.801   0.4230  
## female      -0.71642    0.56185  -1.275   0.2023  
## age          0.05198    0.03070   1.693   0.0904 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Approximate significance of smooth terms:
##            edf Ref.df Chi.sq p-value
## s(lvpet) 2.797  3.566   4.38   0.302

## R-sq.(adj) =  0.0594   Deviance explained = 9.01%
## UBRE = 0.40445  Scale est. = 1         n = 79


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
## (Intercept) -1.76678    1.45414  -1.215   0.2244  
## female      -0.43547    0.60565  -0.719   0.4721  
## age          0.11368    0.05473   2.077   0.0378 *
## ---
e.glm_ccw12 <- glm(ff_ccw12, data = dfWR.NP1_ccw12, family = binomial(link = "logit"))
summary(e.glm_ccw12)
## Coefficients:
##                    Estimate Std. Error z value Pr(>|z|)  
## (Intercept)        15.03965    8.62405   1.744   0.0812 .
## female             -0.39300    0.67454  -0.583   0.5601  
## age                 0.09821    0.06238   1.574   0.1154  
## MR_OFCthick        -6.75349    3.09440  -2.182   0.0291 *
## HAMD17              0.07033    0.09127   0.771   0.4409  
## low_hsCRP           0.59041    0.73156   0.807   0.4196  
## lvpet               1.36541    2.34886   0.581   0.5610  
## cognitive_cluster2 -0.87933    0.69838  -1.259   0.2080  
## cognitive_cluster3 -0.43560    0.77869  -0.559   0.5759  
## EEG_vigilance       0.18026    0.53226   0.339   0.7349  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## (Dispersion parameter for binomial family taken to be 1)

##     Null deviance: 93.903  on 76  degrees of freedom
## Residual deviance: 77.716  on 67  degrees of freedom

##     Null deviance: 83.100  on 71  degrees of freedom
## anova(e.glm_ccw12,e.glm0_ccw12, test = "Chisq")
## Model 1: Y_w12 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + 
##     cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance
## Model 2: Y_w12 ~ female + age
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1        67     77.716                     
## 2        74     86.546 -7  -8.8302   0.2651

## with rescaled predictors
e.glm_ccw12S <- update(e.glm_ccw12, data = dfWR.NP1_ccw12S)

## *** Random Forests
cat(" ranger")
e.ranger_ccw12 <- ranger(ff_ccw12, data = dfWR.NP1_ccw12, probability = TRUE)
## ranger(ff_ccw12, data = dfWR.NP1_ccw12, probability = TRUE, mtry = 5, num.trees = 5000)
e.rangerPerm_ccw12 <- importance_pvalues(ranger(ff_ccw12, data = dfWR.NP1_ccw12, importance = "permutation"), method = "altmann", 
                                         formula = ff_ccw12, data = dfWR.NP1_ccw12, num.permutations = n.perm)

e.rangerPerm_ccw12
##                       importance     pvalue
## female             -0.0024742853 0.67132867
## age                 0.0040111433 0.34065934
## MR_OFCthick         0.0133738696 0.13186813
## HAMD17              0.0004376103 0.45854146
## low_hsCRP          -0.0029191690 0.77022977
## lvpet               0.0243561290 0.02597403
## cognitive_cluster2  0.0043251049 0.15184815
## cognitive_cluster3 -0.0025604673 0.66433566
## EEG_vigilance       0.0017273760 0.28471528

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
## Parametric coefficients:
##             Estimate Std. Error z value Pr(>|z|)  
## (Intercept) -2.60771    1.66854  -1.563   0.1181  
## female      -0.78605    0.71579  -1.098   0.2721  
## age          0.15884    0.06378   2.491   0.0128 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Approximate significance of smooth terms:
##            edf Ref.df Chi.sq p-value  
## s(lvpet) 2.594  3.316  10.26   0.022 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## plot(e.gam_ccw12)


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
## 1         (Intercept) -0.2925515555 7.377320067 -0.03965553 74.00585 0.96847454
## 2           sexfemale -1.1500649634 0.576377337 -1.99533342 73.92037 0.04969227
## 3                 age  0.0306528719 0.033205358  0.92313029 74.01112 0.35893909
## 4         MR_OFCthick -0.5579659292 2.442778524 -0.22841446 73.93571 0.81995462
## 5              HAMD17  0.0269045584 0.077572321  0.34683194 74.02503 0.72970162
## 6            hsCRPlow  0.6504118277 0.653513146  0.99525439 72.36320 0.32292900
## 7               lvpet  0.6475825877 2.001208826  0.32359571 73.93189 0.74715773
## 8  cognitive_cluster2 -0.3329634426 0.600543631 -0.55443672 72.19280 0.58099358
## 9  cognitive_cluster3 -1.3052805776 0.672939553 -1.93966987 72.48699 0.05631125
## 10      EEG_vigilance -0.1954585504 0.483124566 -0.40457175 73.99332 0.68695947
## 11    CATS_scoretotal -0.0144973847 0.014429375 -1.00471328 74.01236 0.31830848
## 12           CAR_AUCi  0.0008668785 0.000917322  0.94500999 73.36810 0.34775562
## 13        neuroticism  0.0104506251 0.016492226  0.63366977 73.99762 0.52825021

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


## same p-values except intercept
## summary(e.glm_impw4$analyses[[1]])$coef
## summary(e.glm_impw4C$analyses[[1]])$coef

## summary(pool(e.glm_impw4$analyses))
## summary(pool(e.glm_impw4C$analyses))
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
##                  term      estimate    std.error  statistic       df    p.value
## 1         (Intercept)  6.8146907992 7.8156561124  0.8719282 72.90729 0.38610994
## 2           sexfemale -0.5775349071 0.5927391411 -0.9743492 72.82231 0.33310871
## 3                 age  0.0545436654 0.0392051209  1.3912383 72.84346 0.16838786
## 4         MR_OFCthick -4.5808015926 2.7216688067 -1.6830856 72.76326 0.09664484
## 5              HAMD17  0.0947528635 0.0798313402  1.1869131 72.95785 0.23911230
## 6            hsCRPlow  0.8939995371 0.6495831068  1.3762666 71.21852 0.17305117
## 7               lvpet -0.7636105932 2.0664223812 -0.3695327 72.96593 0.71280103
## 8  cognitive_cluster2 -0.6991237222 0.6318798394 -1.1064188 72.18586 0.27221893
## 9  cognitive_cluster3 -1.8799217939 0.7109574688 -2.6442113 72.02561 0.01004351
## 10      EEG_vigilance -1.3386060382 0.5634733888 -2.3756331 72.80351 0.02015285
## 11    CATS_scoretotal  0.0038587753 0.0141981465  0.2717802 72.96351 0.78655856
## 12           CAR_AUCi  0.0008391796 0.0008832115  0.9501458 72.24470 0.34520600
## 13        neuroticism  0.0117843042 0.0174388945  0.6757484 73.05572 0.50133395
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
##                  term     estimate  std.error   statistic       df    p.value
## 1         (Intercept) 18.484383637 8.94990069  2.06531718 71.01850 0.04254409
## 2           sexfemale -0.917439878 0.69174839 -1.32626239 70.94739 0.18900657
## 3                 age  0.092336871 0.06141620  1.50346122 70.99552 0.13715573
## 4         MR_OFCthick -8.187374622 3.11868954 -2.62526120 70.99624 0.01059592
## 5              HAMD17  0.070001748 0.08993633  0.77834782 70.94707 0.43895218
## 6            hsCRPlow  0.541703593 0.76362441  0.70938485 70.29063 0.48043219
## 7               lvpet  1.197360971 2.36311674  0.50668719 70.98321 0.61394466
## 8  cognitive_cluster2 -1.232283668 0.77569850 -1.58861165 70.36703 0.11663155
## 9  cognitive_cluster3 -1.232117605 0.84485370 -1.45837985 70.38349 0.14918403
## 10      EEG_vigilance -0.105048546 0.57157022 -0.18378940 71.01714 0.85470244
## 11    CATS_scoretotal  0.035605955 0.01765689  2.01654771 71.06315 0.04752293
## 12           CAR_AUCi  0.001389549 0.00113572  1.22349596 70.88005 0.22519432
## 13        neuroticism -0.001218515 0.02078185 -0.05863362 70.92296 0.95340882
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
## [1] 0.7240508
min(glht_cc$week8$test$pvalues)
## [1] 0.1543613
min(glht_cc$week12$test$pvalues)
## [1] 0.1798999

## multivariate Wald test
summary(glht_cc$week4, test = Chisqtest())
## Global Test:
##   Chisq DF Pr(>Chisq)
## 1 3.469  7     0.8385
summary(glht_cc$week8, test = Chisqtest())
## Global Test:
##   Chisq DF Pr(>Chisq)
## 1 10.61  7     0.1568
summary(glht_cc$week12, test = Chisqtest())
## Global Test:
##   Chisq DF Pr(>Chisq)
## 1 7.135  7      0.415

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
## [1] 0.4105926
min(glht_imp$week8$test$pvalues)
## [1] 0.09046179
min(glht_imp$week12$test$pvalues)
## [1] 0.09403706

## multivariate Wald test
summary(glht_imp$week4, test = Chisqtest())
## Global Test:
##   Chisq DF Pr(>Chisq)
## 1 7.878 10     0.6408
summary(glht_imp$week8, test = Chisqtest())
## Global Test:
##   Chisq DF Pr(>Chisq)
## 1  13.3 10     0.2073
summary(glht_imp$week12, test = Chisqtest())
## Global Test:
##   Chisq DF Pr(>Chisq)
## 1 12.19 10     0.2725

## * export
save.image(file = file.path(path.results,"test.Rdata"))

## save(list = c("dfW.NP1", "dfW.NP1cc", "dfWR.NP1", "dfWR.NP1_2impw12", "dfWR.NP1_2impw4", "dfWR.NP1_2impw8", "dfWR.NP1_ccw12",  "dfWR.NP1_ccw12S", "dfWR.NP1_ccw4",
##               "dfWR.NP1_ccw4S",  "dfWR.NP1_ccw8",   "dfWR.NP1_ccw8S",  "dfWR.NP1_w12",    "dfWR.NP1_w4",     "dfWR.NP1_w8",    
##               "dfWRimp.NP1_w12", "dfWRimp.NP1_w12C",    "dfWRimp.NP1_w4",  "dfWRimp.NP1_w4C", "dfWRimp.NP1_w8",  "dfWRimp.NP1_w8C",    
##               "DS",              "dtS.ass_cc",      "dtS.ass_imp",     "e.gam_ccw12",     "e.gam_ccw8",     "e.glm_ccw12",        
##               "e.glm_ccw12S",    "e.glm_ccw4",      "e.glm_ccw4S",     "e.glm_ccw8",      "e.glm_ccw8S",     "e.glm_impw12",   
##               "e.glm_impw12C",   "e.glm_impw4",     "e.glm_impw4C",    "e.glm_impw8",     "e.glm_impw8C",    "e.glm0_ccw12",   
##               "e.glm0_ccw4",     "e.glm0_ccw8",     "e.ranger_ccw12",  "e.ranger_ccw4",   "e.ranger_ccw8",   "e.ranger0_ccw12",    
##               "e.rangerPerm_ccw12",  "e.rangerPerm_ccw4",   "e.rangerPerm_ccw8",   "e.rangerPerm0_ccw12", "elvm.PET",        "ff_ccw12",       
##               "ff_ccw4",         "ff_ccw8",         "findLevels",      "glht_cc",         "glht_imp",        "glhtPool",       
##               "iObs",            "iPred",      "iVar",      "keep.col",        "lvm.PET",         "Mlink_w12",      
##               "Mlink_w4",        "Mlink_w8",        "n.imputed",       "n.perm",          "name",            "name.predictor", 
##               "nameR.predictor", "nameRT.predictor",    "nameT.predictor", "path.code",       "path.results",    "source.NP1",     
##               "sourceRed.NP1") ,
##      file = file.path(path.results,"analysis-test","test2.Rdata"))


##----------------------------------------------------------------------
### analysis-test.R ends here
