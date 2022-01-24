### 1-prediction.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: dec  1 2021 (13:12) 
## Version: 
## Last-Updated: jan 24 2022 (21:08) 
##           By: Brice Ozenne
##     Update #: 84
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
library(ggplot2)
library(lcmm)
## devtools::install_github("NightingaleHealth/ggforestplot")
library(ggforestplot)

source(file.path(path.FCT,"FCT_forestplot.R"))
source(file.path(path.FCT,"FCT_glhtPool.R"))
source(file.path(path.FCT,"FCT_plotTraj.R"))
source(file.path(path.code,"0-data-management.R"))

## * Description
## ** missing data
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

## ** spaghetti plot
dfWR.NP1$Y_w4812 <- "Switch"
dfWR.NP1$Y_w4812[which(dfWR.NP1$Y_w4*dfWR.NP1$Y_w8*dfWR.NP1$Y_w12==1)] <- "Always responder"
dfWR.NP1$Y_w4812[which((dfWR.NP1$Y_w4==0)*(dfWR.NP1$Y_w8==0)*(dfWR.NP1$Y_w12==0)==1)] <- "Always non-responder"
dfWR.NP1$Y_w4812 <- factor(dfWR.NP1$Y_w4812, levels = c("Always responder","Switch","Always non-responder"))

dfLR.NP1 <- reshape(cbind(dfWR.NP1,Y_w0=0), idvar = c("CIMBI_ID"),
                    direction = "long",
                    varying = list(c("HAMD6_w0","HAMD6_w4","HAMD6_w8","HAMD6_w12"),
                                   c("HAMD17_w0","HAMD17_w4","HAMD17_w8","HAMD17_w12"),
                                   c("Y_w0","Y_w4","Y_w8","Y_w12")),
                    v.names = c("HAMD6","HAMD17","Y"),timevar = "visit")
dfLR.NP1$visit <- factor(dfLR.NP1$visit, levels = 1:4, labels = c("baseline","week 4","week 8","week 12"))
dfLR.NP1$HAMD6.jit <- dfLR.NP1$HAMD6 + runif(NROW(dfLR.NP1),-0.45,0.45)

gg_desc <- ggplot(dfLR.NP1, aes(x=visit, y = HAMD6, group = CIMBI_ID, color = Y_w4812)) + geom_jitter(width = 0.1, height = 0)
## ggplot(dfLR.NP1, aes(x=visit, y = HAMD6, group = CIMBI_ID, color = Y_w4812)) + geom_point(width = 0.1) + facet_wrap(~CIMBI_ID)

gg_desc <- ggplot(dfLR.NP1, aes(x=visit, y = HAMD6, group = CIMBI_ID, color = as.factor(Y))) + geom_jitter(width = 0.1)
## ggplot(dfLR.NP1, aes(x=visit, y = HAMD6, group = CIMBI_ID, color = as.factor(Y))) + geom_point(width = 0.1) + facet_wrap(~CIMBI_ID)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
gg_desc <- ggplot(dfLR.NP1, aes(x=visit, y = HAMD6.jit, group = CIMBI_ID, color = Y_w4812)) + geom_point() + geom_line()
gg_desc <- gg_desc + facet_wrap(~Y_w4812) + guides(color = "none") + scale_color_manual(values = gg_color_hue(3)[c(2,3,1)])
gg_desc <- gg_desc + labs(x="",y = "HAMD-6")
## ggplot(dfLR.NP1, aes(x=visit, y = HAMD17, group = CIMBI_ID, color = as.factor(Y))) + geom_point() + facet_wrap(~CIMBI_ID)

if(FALSE){
    ggsave(gg_desc + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm")),
           filename = "../figures/spaghetti-HAMD6.pdf")
    ggsave(gg_desc + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm")),
           filename = "../figures/spaghetti-HAMD6.png")
}

## ** transition
df.trans <- data.frame(week4 = c("nr2nr" = sum(dfWR.NP1$Y_w4==0, na.rm = TRUE),
                                 "r2nr" = 0,
                                 "nr2r" = sum(dfWR.NP1$Y_w4==1, na.rm = TRUE),
                                 "r2r" = 0),
                       week8 = c("nr2nr" = sum((dfWR.NP1$Y_w4==0)*(dfWR.NP1$Y_w8==0), na.rm = TRUE),
                                 "r2nr" = sum((dfWR.NP1$Y_w4==1)*(dfWR.NP1$Y_w8==0), na.rm = TRUE),
                                 "nr2r" = sum((dfWR.NP1$Y_w4==0)*(dfWR.NP1$Y_w8==1), na.rm = TRUE),
                                 "r2r" = sum((dfWR.NP1$Y_w4==1)*(dfWR.NP1$Y_w8==1), na.rm = TRUE)),
                       week12 = c("nr2nr" = sum((dfWR.NP1$Y_w8==0)*(dfWR.NP1$Y_w12==0), na.rm = TRUE),
                                  "r2nr" = sum((dfWR.NP1$Y_w8==1)*(dfWR.NP1$Y_w12==0), na.rm = TRUE),
                                  "nr2r" = sum((dfWR.NP1$Y_w8==0)*(dfWR.NP1$Y_w12==1), na.rm = TRUE),
                                  "r2r" = sum((dfWR.NP1$Y_w8==1)*(dfWR.NP1$Y_w12==1), na.rm = TRUE))
                       )
df.trans <- rbind(df.trans, total = colSums(df.trans))
df.trans[,1] <- paste(df.trans[,1]," (",round(100*df.trans[,1]/df.trans["total",1],2),"%)", sep = "")
df.trans[,2] <- paste(df.trans[,2]," (",round(100*df.trans[,2]/df.trans["total",2],2),"%)", sep = "")
df.trans[,3] <- paste(df.trans[,3]," (",round(100*df.trans[,3]/df.trans["total",3],2),"%)", sep = "")
##             week4       week8      week12
## nr2nr 51 (58.62%) 27 (32.53%)    16 (20%)
## r2nr       0 (0%)  9 (10.84%)    6 (7.5%)
## nr2r  36 (41.38%)  20 (24.1%) 17 (21.25%)
## r2r        0 (0%) 27 (32.53%) 41 (51.25%)
## total   87 (100%)   83 (100%)   80 (100%)

## ** latent class
dfLR.NP1_NNA <- dfLR.NP1[!is.na(dfLR.NP1$HAMD17)] 

ls.hlme <- list(hlme(HAMD17 ~ visit, subject = "CIMBI_ID", data = dfLR.NP1_NNA,
                     ng = 1, random =~ 1),
                hlme(HAMD17 ~ visit, subject = "CIMBI_ID", data = dfLR.NP1_NNA,
                     ng = 2, nwg = FALSE, mixture =~visit, random =~ 1),
                hlme(HAMD17 ~ visit, subject = "CIMBI_ID", data = dfLR.NP1_NNA,
                     ng = 3, nwg = FALSE, mixture =~visit, random =~ 1),
                hlme(HAMD17 ~ visit, subject = "CIMBI_ID", data = dfLR.NP1_NNA,
                     ng = 4, nwg = FALSE, mixture =~visit, random =~ 1))
     
ggTraj_hlme(ls.hlme[[1]], color = "prob", facet = TRUE, nrow = 1)
ggTraj_hlme(ls.hlme[[2]], color = "prob", facet = TRUE, nrow = 1, order.class = c(2:1))
gg_desc2 <- ggTraj_hlme(ls.hlme[[3]], color = "prob", facet = TRUE, nrow = 1, order.class = c("stable" = 3,"partial recovery"=1,"recovery"=2))
ggTraj_hlme(ls.hlme[[4]], color = "prob", facet = TRUE, nrow = 1, order.class = c(4,3,1,2))
    
compare.hlme <- summarytable(ls.hlme[[1]], bb = ls.hlme[[2]], ls.hlme[[3]], ls.hlme[[4]],
                             which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy"))
colnames(compare.hlme)[1] <- "nb. classes"
colnames(compare.hlme)[3] <- "cv"
colnames(compare.hlme)[4] <- "nb. parameters"
compare.hlme

if(FALSE){
    ggsave(gg_desc2$plot + xlab("") + theme(text = element_text(size=20), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                                 axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm")),
           filename = "../figures/spaghetti-HAMD17.pdf")
    ggsave(gg_desc2$plot + xlab("") + theme(text = element_text(size=20), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                                 axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm")),
           filename = "../figures/spaghetti-HAMD17.png")
}

## * Analysis: complete case 
## ** week 4
dfWR.NP1_ccw4 <- dfWR.NP1[rowSums(is.na(dfWR.NP1[,.SD,.SDcols = c("Y_w4",nameR.predictor)]))==0,]
dfWR.NP1_ccw4S <- cbind(dfWR.NP1_ccw4[,c("Y_w4","female","low_hsCRP","cognitive_cluster2","cognitive_cluster3")],
                      scale(dfWR.NP1_ccw4[,c("age","MR_OFCthick", "HAMD17", "lvpet", "EEG_vigilance")])
                      )

ff_ccw4 <- Y_w4 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance

e.glm0_ccw4 <- glm(Y_w4 ~ female + age, data = dfWR.NP1_ccw4, family = binomial(link = "logit"))
e.glm_ccw4 <- glm(ff_ccw4,
                  data = dfWR.NP1_ccw4, family = binomial(link = "logit"))
summary(e.glm_ccw4)
## Coefficients:
##                     Estimate Std. Error z value Pr(>|z|)
## (Intercept)         2.442441   7.065516   0.346    0.730
## female             -0.731849   0.561789  -1.303    0.193
## age                 0.028382   0.031850   0.891    0.373
## MR_OFCthick        -1.353086   2.593385  -0.522    0.602
## HAMD17              0.003296   0.085150   0.039    0.969
## low_hsCRP           0.861512   0.683635   1.260    0.208
## lvpet               0.141351   2.071846   0.068    0.946
## cognitive_cluster2 -0.411805   0.592115  -0.695    0.487
## cognitive_cluster3 -0.975298   0.649760  -1.501    0.133
## EEG_vigilance      -0.128708   0.476123  -0.270    0.787

anova(e.glm_ccw4,e.glm0_ccw4, test = "Chisq")
## Analysis of Deviance Table

## Model 1: Y_w4 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + 
##     cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance
## Model 2: Y_w4 ~ female + age
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1        68      95.98                     
## 2        75     100.77 -7  -4.7862    0.686

## table(EEG = dfWR.NP1_ccw8$EEG_vigilance, recovery = dfWR.NP1_ccw8$Y_w8, cluster3 = dfWR.NP1_ccw8$cognitive_cluster3)

e.ranger_ccw4 <- ranger(ff_ccw4, data = dfWR.NP1_ccw4, probability = TRUE)
## ranger(ff_ccw4, data = dfWR.NP1_ccw4, probability = TRUE, num.trees = 5000, mtry = 1, min.node.size = 5)
e.rangerPerm_ccw4 <- ranger(ff_ccw4, data = dfWR.NP1_ccw4, importance = "permutation")

importance_pvalues(e.rangerPerm_ccw4, method = "altmann", 
                   formula = ff_ccw4, data = dfWR.NP1_ccw4)
##                      importance     pvalue
## female             -0.003579548 0.74257426
## age                 0.008075597 0.20792079
## MR_OFCthick         0.002276118 0.35643564
## HAMD17             -0.011140327 0.86138614
## low_hsCRP           0.001215267 0.30693069
## lvpet              -0.005461347 0.68316832
## cognitive_cluster2 -0.003157582 0.75247525
## cognitive_cluster3  0.008964127 0.04950495
## EEG_vigilance      -0.007608652 0.91089109

## ** week 8
dfWR.NP1_ccw8 <- dfWR.NP1[rowSums(is.na(dfWR.NP1[,.SD,.SDcols = c("Y_w8",nameR.predictor)]))==0,]
dfWR.NP1_ccw8S <- cbind(dfWR.NP1_ccw8[,c("Y_w8","female","low_hsCRP","cognitive_cluster2","cognitive_cluster3")],
                        scale(dfWR.NP1_ccw8[,c("age","MR_OFCthick", "HAMD17", "lvpet", "EEG_vigilance")])
                        )

ff_ccw8 <- Y_w8 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance
e.glm0_ccw8 <- glm(Y_w8 ~ female + age, data = dfWR.NP1_ccw8, family = binomial(link = "logit"))
summary(e.glm0_ccw8)
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)
## (Intercept) -0.62048    0.92671  -0.670    0.503
## female      -0.40827    0.53158  -0.768    0.442
## age          0.04023    0.02985   1.348    0.178
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

e.ranger_ccw8 <- ranger(ff_ccw8, data = dfWR.NP1_ccw8, probability = TRUE)
## ranger(ff_ccw8, data = dfWR.NP1_ccw8, probability = TRUE, num.trees = 5000, mtry = 1, min.node.size = 5)
e.rangerPerm_ccw8 <- ranger(ff_ccw8, data = dfWR.NP1_ccw8, importance = "permutation")

importance_pvalues(e.ranger_ccw8Perm, method = "altmann", 
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
dfWR.NP1_ccw12S <- cbind(dfWR.NP1_ccw12[,c("Y_w12","female","low_hsCRP","cognitive_cluster2","cognitive_cluster3")],
                      scale(dfWR.NP1_ccw12[,c("age","MR_OFCthick", "HAMD17", "lvpet", "EEG_vigilance")])
                      )

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

e.ranger_ccw12 <- ranger(ff_ccw12, data = dfWR.NP1_ccw12, probability = TRUE)
## ranger(ff_ccw12, data = dfWR.NP1_ccw12, probability = TRUE, mtry = 5, num.trees = 5000)
e.rangerPerm_ccw12 <- ranger(ff_ccw12, data = dfWR.NP1_ccw12, importance = "permutation")

importance_pvalues(e.rangerPerm_ccw12, method = "altmann", 
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
n.imputed <- 25 ## number of imputed datasets

## ** week 4
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
summary(dfWRimp.NP1_w4)
str(dfWRimp.NP1_w4$imp)
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
## ** week 4
## *** assess performance
set.seed(10)
ePerf.ccw4 <- performance(list(glm0_ccw4 = e.glm0_ccw4, glm_ccw4 = e.glm_ccw4, rf_ccw4 = e.ranger_ccw4),
                          data = dfWR.NP1_ccw4, fold.number = 100, fold.size = 0.1)
ePerf.ccw4
##      method metric     model   estimate          se      lower     upper      p.value p.value_comp
## 1  internal    auc glm0_ccw4 0.58680556 0.067302860 0.44366530 0.7049351 2.222716e-01           NA
## 2  internal    auc  glm_ccw4 0.69305556 0.062106054 0.55324473 0.7968469 9.170059e-03 1.111473e-01
## 3  internal    auc   rf_ccw4 0.99791667 0.002612833 0.97586903 0.9998220 3.750462e-06 6.110621e-07
## 4  internal  brier glm0_ccw4 0.22702258 0.017602184 0.19501642 0.2642816           NA           NA
## 5  internal  brier  glm_ccw4 0.21252407 0.019572090 0.17742629 0.2545648           NA 2.141445e-01
## 6  internal  brier   rf_ccw4 0.09768817 0.008326253 0.08265926 0.1154496           NA 1.110223e-15
## 7        cv    auc glm0_ccw4 0.50026389 0.065186250 0.36734343 0.6193880 9.967704e-01           NA
## 8        cv    auc  glm_ccw4 0.46946528 0.061354848 0.34609592 0.5833977 6.146552e-01 6.162546e-01
## 9        cv    auc   rf_ccw4 0.49236458 0.064388406 0.36155437 0.6105074 9.052989e-01 6.606699e-01
## 10       cv  brier glm0_ccw4 0.24690719 0.017305691 0.21521530 0.2832659           NA           NA
## 11       cv  brier  glm_ccw4 0.28051117 0.024045664 0.23712886 0.3318302           NA 3.669000e-02
## 12       cv  brier   rf_ccw4 0.26007552 0.019299611 0.22487116 0.3007913           NA 2.277458e-01

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


## *** CALIBRATION PLOT
ggCali_w4 <- ggplot(dt.CVpred_ccw4,aes(x=prediction, y = outcome))
ggCali_w4 <- ggCali_w4 + geom_smooth() + geom_rug() + geom_abline(slope = 1, intercept = 0, color = "red")
ggCali_w4 <- ggCali_w4 + facet_wrap(~model.lab)

## *** ROC CURVES
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



## ** week 8
## *** assess performance
set.seed(10)
ePerf.ccw8 <- performance(list(glm0_ccw8 = e.glm0_ccw8, glm_ccw8 = e.glm_ccw8, rf_ccw8 = e.ranger_ccw8),
                          data = dfWR.NP1_ccw8, fold.number = 100, fold.size = 0.1)
ePerf.ccw8
##      method metric     model   estimate          se      lower     upper      p.value p.value_comp
## 1  internal    auc glm0_ccw8 0.62769010 0.065372873 0.48583237 0.7404929 0.0753593107           NA
## 2  internal    auc  glm_ccw8 0.81133429 0.052319172 0.68202876 0.8920563 0.0001019312 1.130793e-02
## 3  internal    auc   rf_ccw8 1.00000000 0.000000000 1.00000000 1.0000000 0.0000000000 2.801347e-04
## 4  internal  brier glm0_ccw8 0.23870239 0.009886525 0.22009079 0.2588878           NA           NA
## 5  internal  brier  glm_ccw8 0.17485453 0.021244163 0.13780314 0.2218680           NA 1.016948e-03
## 6  internal  brier   rf_ccw8 0.09426488 0.006777196 0.08187524 0.1085294           NA 2.532701e-05
## 7        cv    auc glm0_ccw8 0.52333572 0.062966746 0.39375920 0.6377029 0.7140942014           NA
## 8        cv    auc  glm_ccw8 0.67472740 0.060196096 0.54138807 0.7770317 0.0125103596 5.671532e-02
## 9        cv    auc   rf_ccw8 0.55047346 0.050755558 0.44573656 0.6433601 0.3335094385 2.656139e-02
## 10       cv  brier glm0_ccw8 0.25887419 0.014290417 0.23232748 0.2884542           NA           NA
## 11       cv  brier  glm_ccw8 0.23957752 0.029954201 0.18750851 0.3061055           NA 4.651872e-01
## 12       cv  brier   rf_ccw8 0.25586413 0.012963184 0.23167750 0.2825758           NA 5.361605e-01

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
           filename = "../figures/hist-predCV-week8.pdf")
    ggsave(ggHist2_w8 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm")),
           filename = "../figures/hist-predCV-week8.png")
}


## *** CALIBRATION PLOT
ggCali_w8 <- ggplot(dt.CVpred_ccw8,aes(x=prediction, y = outcome))
ggCali_w8 <- ggCali_w8 + geom_smooth() + geom_rug() + geom_abline(slope = 1, intercept = 0, color = "red")
ggCali_w8 <- ggCali_w8 + facet_wrap(~model.lab)

## *** ROC CURVES
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

## *** permutation test
n.perm <- 100
ls.resperm <- vector(mode = "list", length = n.perm)

warper_w8 <- function(i, trace = FALSE, fold.number){
    iData <- copy(dfWR.NP1_ccw8)
    iData$Y_w8 <- sample(iData$Y_w8)
    iEPerf <- suppressWarnings(performance(list(glm0_ccw8 = e.glm0_ccw8, glm_ccw8 = e.glm_ccw8, rf_ccw8 = e.ranger_ccw8),
                                           data = iData, fold.number = fold.number, fold.size = 0.1, trace = trace))
    return(cbind(perm = i, as.data.table(iEPerf, type = "metric")))
}
## warper_w8(1, fold.number = 10)

cpus <- 7

cl <- snow::makeSOCKcluster(cpus)
doSNOW::registerDoSNOW(cl)

pb <- txtProgressBar(max = n.perm, style=3)
opts <- list(progress = function(n) setTxtProgressBar(pb, n))

parallel::clusterExport(cl, varlist = c("ff_ccw8","warper_w8"))

ls.perm_w8 <- foreach::`%dopar%`(
                           foreach::foreach(i=1:n.perm, .options.snow=opts, .packages = c("BuyseTest","data.table","ranger")), {
                               warper_w8(i, fold.number = 10)
                           })


dt.resperm_w8 <- as.data.table(do.call(rbind, ls.perm_w8))

dt.resperm_w8[metric == "auc",.(estimate = mean(estimate), sd = sd(estimate), type1error = mean(p.value<=0.05)), by = c("method","model")]
##      method     model  estimate         sd type1error
## 1: internal glm0_ccw8 0.5017719 0.06581663       0.04
## 2: internal  glm_ccw8 0.4985725 0.06846851       0.06
## 3: internal   rf_ccw8 0.5060115 0.06844682       0.06
## 4:       cv glm0_ccw8 0.4027030 0.09982635       0.46
## 5:       cv  glm_ccw8 0.4730674 0.10588802       0.28
## 6:       cv   rf_ccw8 0.4713885 0.09941016       0.29

dtPerfCV.ccw8 <- as.data.table(ePerf.ccw8)[method=="cv"]
dtPerfCV.ccw8[,perm := list(list(dt.resperm_w8[method == "cv" & metric == .SD$metric[1] & model == .SD$model[1],estimate])),by = c("metric","model"),.SDcols = c("metric","model")]

dtPerfCV.ccw8[metric == "auc", .(p.value = mean(estimate <= perm[[1]])), by = "model"]
##        model p.value
## 1: glm0_ccw8    0.11
## 2:  glm_ccw8    0.02
## 3:   rf_ccw8    0.23
dtPerfCV.ccw8[metric == "brier", .(p.value = mean(estimate >= perm[[1]])), by = "model"]
##        model p.value
## 1: glm0_ccw8    0.27
## 2:  glm_ccw8    0.01
## 3:   rf_ccw8    0.19

betahat <- as.data.table(ePerf.ccw8)[method == "cv" & metric == "brier" & model == "glm_ccw8",estimate]
betahat.perm <- dt.resperm[method == "cv" & metric == "brier" & model == "glm_ccw8",estimate]
mean(betahat>=betahat.perm)
## [1] 0.02


## saveRDS(list(estimate = ePerf.ccw8,
##             perm = dt.resperm),
##        file = file.path(path.results,"performance-w8.rds"))
## ePerf.ccw8 <- readRDS(file.path(path.results,"performance-w8.rds"))$estimate
## dt.resperm <- readRDS(file.path(path.results,"performance-w8.rds"))$perm


## as.data.table(performance(list(glm0_ccw8 = e.glm0_ccw8, glm_ccw8 = e.glm_ccw8),
##                           data = dfWR.NP1_ccw8, fold.number = 100, fold.size = 8, individual.fit = TRUE))

## ** week 12
## performance(list(glm0_ccw12 = e.glm0_ccw12, glm_ccw12 = e.glm_ccw12, rf_ccw12 = ranger(ff_ccw12, data = dfWR.NP1_ccw12, probability = FALSE)),
##                           data = dfWR.NP1_ccw12)
set.seed(10)
ePerf.ccw12 <- performance(list(glm0_ccw12 = e.glm0_ccw12, glm_ccw12 = e.glm_ccw12, rf_ccw12 = ranger(ff_ccw12, data = dfWR.NP1_ccw12, probability = TRUE)),
                          data = dfWR.NP1_ccw12, fold.number = 100, fold.size = 0.1)
ePerf.ccw12
##      method metric      model   estimate          se      lower      upper      p.value p.value_comp
## 1  internal    auc glm0_ccw12 0.66037736 0.064890095 0.51683800 0.77038402 0.0302556016           NA
## 2  internal    auc  glm_ccw12 0.79443893 0.056420373 0.65615610 0.88189965 0.0003531358 4.727703e-02
## 3  internal    auc   rf_ccw12 1.00000000 0.000000000 1.00000000 1.00000000 0.0000000000 2.312484e-04
## 4  internal  brier glm0_ccw12 0.18160078 0.019088243 0.14779068 0.22314562           NA           NA
## 5  internal  brier  glm_ccw12 0.14937617 0.021474198 0.11269724 0.19799279           NA 4.420267e-02
## 6  internal  brier   rf_ccw12 0.05981177 0.008366624 0.04546931 0.07867829           NA 7.978087e-08
## 7        cv    auc glm0_ccw12 0.57554121 0.065379483 0.43751627 0.69128809 0.2698480466           NA
## 8        cv    auc  glm_ccw12 0.60582920 0.069408323 0.45637198 0.72602365 0.1559872593 6.387422e-01
## 9        cv    auc   rf_ccw12 0.76768620 0.053208545 0.64277968 0.85372398 0.0002363900 1.115376e-02
## 10       cv  brier glm0_ccw12 0.19544784 0.024274226 0.15321926 0.24931499           NA           NA
## 11       cv  brier  glm_ccw12 0.21388319 0.032330959 0.15904042 0.28763767           NA 3.795072e-01
## 12       cv  brier   rf_ccw12 0.16378866 0.021343825 0.12687061 0.21144949           NA 1.539530e-02

## *** DENSITY PLOT
dt.CVpred_ccw12 <- as.data.table(ePerf.ccw12, type = "prediction-cv", format = "long")
dt.CVpred_ccw12$model.lab <- factor(dt.CVpred_ccw12$model, levels = c("glm0_ccw12","glm_ccw12","rf_ccw12"),
                                   labels = c("logistic model - no biomarkers", "logistic model - biomarkers", "random forest - biomarkers"))
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

## *** ROC CURVES
dt.CVroc_ccw12 <- as.data.table(ePerf.ccw12, type = "roc-cv")                                                      
dt.CVroc_ccw12$model.lab <- factor(dt.CVroc_ccw12$model, levels = c("glm0_ccw12","glm_ccw12","rf_ccw12"),
                                   labels = c("logistic model - no biomarkers", "logistic model - biomarkers", "random forest - biomarkers"))
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


## *** permutation test
n.perm <- 100
ls.resperm <- vector(mode = "list", length = n.perm)

warper_w12 <- function(i, trace = FALSE, fold.number){
    iData <- copy(dfWR.NP1_ccw12)
    iData$Y_w12 <- sample(iData$Y_w12)
    iEPerf <- suppressWarnings(performance(list(glm0_ccw12 = e.glm0_ccw12, glm_ccw12 = e.glm_ccw12, rf_ccw12 = e.ranger_ccw12),
                                           data = iData, fold.number = fold.number, fold.size = 0.1, trace = trace))
    return(cbind(perm = i, as.data.table(iEPerf, type = "metric")))
}
## warper_w12(1, fold.number = 10)

cpus <- 7

cl <- snow::makeSOCKcluster(cpus)
doSNOW::registerDoSNOW(cl)

pb <- txtProgressBar(max = n.perm, style=3)
opts <- list(progress = function(n) setTxtProgressBar(pb, n))

parallel::clusterExport(cl, varlist = c("ff_ccw12","warper_w12"))

system.time(
    ls.perm_w12 <- foreach::`%dopar%`(
                                foreach::foreach(i=1:n.perm, .options.snow=opts, .packages = c("BuyseTest","data.table","ranger")), {
                                    warper_w12(i, fold.number = 10)
                                })
)
## bruger   system forløbet 
##     0.26     0.10   354.00 

dt.resperm_w12 <- as.data.table(do.call(rbind, ls.perm_w12))

dt.resperm_w12[metric == "auc",.(estimate = mean(estimate), sd = sd(estimate), type1error = mean(p.value<=0.05)), by = c("method","model")]
##      method      model  estimate         sd type1error
## 1: internal glm0_ccw12 0.5015889 0.07802765       0.05
## 2: internal  glm_ccw12 0.5047368 0.07940914       0.06
## 3: internal   rf_ccw12 0.5120357 0.08221397       0.09
## 4:       cv glm0_ccw12 0.4244886 0.10874097       0.32
## 5:       cv  glm_ccw12 0.4748888 0.10481419       0.28
## 6:       cv   rf_ccw12 0.4913347 0.10015871       0.19

dtPerfCV.ccw12 <- as.data.table(ePerf.ccw12)[method=="cv"]
dtPerfCV.ccw12[,perm := list(list(dt.resperm_w12[method == "cv" & metric == .SD$metric[1] & model == .SD$model[1],estimate])),by = c("metric","model"),.SDcols = c("metric","model")]

dtPerfCV.ccw12[metric == "auc", .(p.value = mean(estimate <= perm[[1]])), by = "model"]
##         model p.value
## 1: glm0_ccw12    0.10
## 2:  glm_ccw12    0.12
## 3:   rf_ccw12    0.00
dtPerfCV.ccw12[metric == "brier", .(p.value = mean(estimate >= perm[[1]])), by = "model"]
##         model p.value
## 1: glm0_ccw12    0.12
## 2:  glm_ccw12    0.10
## 3:   rf_ccw12    0.00

betahat <- as.data.table(ePerf.ccw12)[method == "cv" & metric == "brier" & model == "rf_ccw12",estimate]
betahat.perm <- dt.resperm_w12[method == "cv" & metric == "brier" & model == "rf_ccw12",estimate]
mean(betahat>=betahat.perm)
## [1] 0.02


## * Prediction: handling missing values

## ** week 8
set.seed(10)

e.glm0_w8 <- glm(Y_w8 ~ female + age, family = binomial(link = "logit"), data = dfWR.NP1_w8)
e.glm_w8 <- glm(Y_w8 ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance + CATS_scoretotal + CAR_AUCi + neuroticism, family = binomial(link = "logit"), data = dfWR.NP1_w8)
e.ranger_w8 <- ranger(formula = Y_w8f ~ female + age + MR_OFCthick + HAMD17 + low_hsCRP + lvpet + cognitive_cluster2 + cognitive_cluster3 + EEG_vigilance + CATS_scoretotal + CAR_AUCi + neuroticism,
                      data = na.omit(dfWR.NP1_w8))

## performance(e.ranger_w8, fold.number = 100, fold.size = 8)
##     method metric         model   estimate          se      lower     upper   p.value
## 1 internal    auc randomForest1 1.00000000 0.000000000 1.00000000 1.0000000 0.0000000
## 2 internal  brier randomForest1 0.03598857 0.002001488 0.03227197 0.0401332        NA
## 3       cv    auc randomForest1 0.51187202 0.049231003 0.41172790 0.6032751 0.8104758
## 4       cv  brier randomForest1 0.26984952 0.017368838 0.23786698 0.3061323        NA

set.seed(11)
ePerf.w8 <- performance(list(glm0_w8 = e.glm0_w8, glm_w8 = e.glm_w8, rf_w8 = e.ranger_w8),
                        data = dfWR.NP1_w8, fold.number = 100, fold.size = 0.1, individual.fit = TRUE)
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

## * Synthesis
## ** Association (complete case)
e.glm_ccw4S <- update(e.glm_ccw4, data = dfWR.NP1_ccw4S)
e.glm_ccw8S <- update(e.glm_ccw8, data = dfWR.NP1_ccw8S)
e.glm_ccw12S <- update(e.glm_ccw12, data = dfWR.NP1_ccw12S)

C <- glht(e.glm_ccw4S)$linfct[-1,]

dtS.ass_cc <- rbind(cbind(time = "week 4", method = "c", cbind(term = rownames(summary(e.glm_ccw4S)$coef)[-1],
                                                                data.table(summary(e.glm_ccw4S)$coef)[-1,-3],
                                                                confint(glht(e.glm_ccw4S,C))$confint[,2:3])),
                    cbind(time = "week 8", method = "cc", cbind(term = rownames(summary(e.glm_ccw8S)$coef)[-1],
                                                                data.table(summary(e.glm_ccw8S)$coef)[-1,-3],
                                                                confint(glht(e.glm_ccw8S,C))$confint[,2:3])),
                    cbind(time = "week 12", method = "cc", cbind(term = rownames(summary(e.glm_ccw12S)$coef)[-1],
                                                                 data.table(summary(e.glm_ccw12S)$coef)[-1,-3],
                                                                 confint(glht(e.glm_ccw12S,C))$confint[,2:3])))
names(dtS.ass_cc)[names(dtS.ass_cc)=="Estimate"] <- "estimate"
names(dtS.ass_cc)[names(dtS.ass_cc)=="Std. Error"] <- "std.error"
names(dtS.ass_cc)[names(dtS.ass_cc)=="Pr(>|z|)"] <- "p.value"
names(dtS.ass_cc)[names(dtS.ass_cc)=="lwr"] <- "lower.adj"
names(dtS.ass_cc)[names(dtS.ass_cc)=="upr"] <- "upper.adj"
dtS.ass_cc$lower <- dtS.ass_cc$estimate + qnorm(0.025) * dtS.ass_cc$std.error
dtS.ass_cc$upper <- dtS.ass_cc$estimate + qnorm(0.975) * dtS.ass_cc$std.error
dtS.ass_cc$time <- factor(dtS.ass_cc$time, levels = rev(as.character(unique(dtS.ass_cc$time))))

dtS.ass_cc$term <- factor(dtS.ass_cc$term,
                          levels = c("female","age","MR_OFCthick","HAMD17","low_hsCRP","lvpet","cognitive_cluster2","cognitive_cluster3","EEG_vigilance","CATS","Cortisol","Neuroticism"),     
                          labels = c("female","age","MR (OFC thickness)","HAMD17","hsCRP","PET (serotonin)","cognition (cluster 2)","cognition (cluster 3)","EEG (vigilance)","CATS","Cortisol","Neuroticism"),     
                          )

dtS.ass_cc <- rbind(dtS.ass_cc,
                    data.frame(time = c("week 4","week 8", "week 12"), method = "cc", term = c("CATS","Cortisol","Neuroticism"), estimate = NA, std.error = NA, p.value = NA, lower.adj = NA, upper.adj = NA, lower = NA, upper = NA)
                    )

ggforest.ass_cc <- forestplot(dtS.ass_cc) + labs(color = "Analysis at:", x = "log odd ratio from logistic model")
ggforest.ass_cc <- ggforest.ass_cc + ggtitle(paste0("Complete case analysis (n=",NROW(dfWR.NP1_ccw4S),"/",NROW(dfWR.NP1_ccw8S),"/",NROW(dfWR.NP1_ccw12S),")"))
ggforest.ass_cc

## ** Association (missing data)

## *** week 4
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

## week 8
dfWRimp.NP1_w8C <- dfWRimp.NP1_w8
for(iVar in c("age","MR_OFCthick", "HAMD17", "lvpet", "EEG_vigilance","CATS_scoretotal","CAR_AUCi","neuroticism")){
    dfWRimp.NP1_w8C$imp[[iVar]] <- (dfWRimp.NP1_w8$imp[[iVar]]-mean(dfWRimp.NP1_w8C$data[[iVar]], na.rm=TRUE))/sd(dfWRimp.NP1_w8C$data[[iVar]], na.rm=TRUE)
    dfWRimp.NP1_w8C$data[[iVar]] <- (dfWRimp.NP1_w8$data[[iVar]]-mean(dfWRimp.NP1_w8C$data[[iVar]], na.rm=TRUE))/sd(dfWRimp.NP1_w8C$data[[iVar]], na.rm=TRUE)
}

e.glm_impw8C <- with(data = dfWRimp.NP1_w8C,
                     expr = glm(Y_w8 ~ sex + age + MR_OFCthick + HAMD17 + hsCRP + lvpet + cognitive_cluster + EEG_vigilance + CATS_scoretotal + CAR_AUCi + neuroticism,
                                family = binomial(link = "logit"))
                     )

## week 12
dfWRimp.NP1_w12C <- dfWRimp.NP1_w12
for(iVar in c("age","MR_OFCthick", "HAMD17", "lvpet", "EEG_vigilance","CATS_scoretotal","CAR_AUCi","neuroticism")){
    dfWRimp.NP1_w12C$imp[[iVar]] <- (dfWRimp.NP1_w12$imp[[iVar]]-mean(dfWRimp.NP1_w12C$data[[iVar]], na.rm=TRUE))/sd(dfWRimp.NP1_w12C$data[[iVar]], na.rm=TRUE)
    dfWRimp.NP1_w12C$data[[iVar]] <- (dfWRimp.NP1_w12$data[[iVar]]-mean(dfWRimp.NP1_w12C$data[[iVar]], na.rm=TRUE))/sd(dfWRimp.NP1_w12C$data[[iVar]], na.rm=TRUE)
}

e.glm_impw12C <- with(data = dfWRimp.NP1_w12C,
                     expr = glm(Y_w12 ~ sex + age + MR_OFCthick + HAMD17 + hsCRP + lvpet + cognitive_cluster + EEG_vigilance + CATS_scoretotal + CAR_AUCi + neuroticism,
                                family = binomial(link = "logit"))
                     )

## assemble
dtS.ass_imp <- rbind(cbind(time = "week 4", method = "MI", data.table(summary(pool(e.glm_impw4C))[-1,],confint(glhtPool(e.glm_impw4C))$confint[,2:3])),
                     cbind(time = "week 8", method = "MI", data.table(summary(pool(e.glm_impw8C))[-1,],confint(glhtPool(e.glm_impw8C))$confint[,2:3])),
                     cbind(time = "week 12", method = "MI", data.table(summary(pool(e.glm_impw12C))[-1,],confint(glhtPool(e.glm_impw12C))$confint[,2:3])))
dtS.ass_imp$lower <- dtS.ass_imp$estimate + qt(0.025, df = dtS.ass_imp$df) * dtS.ass_imp$std.error
dtS.ass_imp$upper <- dtS.ass_imp$estimate + qt(0.975, df = dtS.ass_imp$df) * dtS.ass_imp$std.error
names(dtS.ass_imp)[names(dtS.ass_imp)=="lwr"] <- "lower.adj"
names(dtS.ass_imp)[names(dtS.ass_imp)=="upr"] <- "upper.adj"


dtS.ass_imp$term <- factor(dtS.ass_imp$term,
                          levels = c("sexfemale","age","MR_OFCthick","HAMD17","hsCRPlow","lvpet","cognitive_cluster2","cognitive_cluster3","EEG_vigilance","CATS_scoretotal","CAR_AUCi","neuroticism"),     
                          labels = c("female","age","MR (OFC thickness)","HAMD17","hsCRP","PET (serotonin)","cognition (cluster 2)","cognition (cluster 3)","EEG (vigilance)","CATS","Cortisol","Neuroticism"))
dtS.ass_imp$time <- factor(dtS.ass_imp$time, levels = rev(as.character(unique(dtS.ass_imp$time))))


ggforest.ass_imp <- forestplot(dtS.ass_imp) + labs(color = "Analysis at:", x = "log odd ratio from logistic model")
ggforest.ass_imp <- ggforest.ass_imp + ggtitle(paste0("Multiple imputation (n=",NROW(dfWRimp.NP1_w4C$data),"/",NROW(dfWRimp.NP1_w8C$data),"/",NROW(dfWRimp.NP1_w12C$data),")"))
ggforest.ass_imp




library(ggpubr)
ggforest.ass <- ggarrange(ggforest.ass_cc + coord_cartesian(xlim = c(-5,5)) + ylab(""),
                          ggforest.ass_imp  + coord_cartesian(xlim = c(-5,5)) + ylab(""),
                          common.legend = TRUE, legend = "bottom")
ggforest.ass

## ggsave(ggforest.ass, filename = "figures/forestPlot-OR.pdf")
## ggsave(ggforest.ass, filename = "figures/forestPlot-OR.png")

##----------------------------------------------------------------------
### 1-prediction.R ends here
