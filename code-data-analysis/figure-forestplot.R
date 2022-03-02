### figure-forestplot.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: mar  2 2022 (15:46) 
## Version: 
## Last-Updated: mar  2 2022 (17:36) 
##           By: Brice Ozenne
##     Update #: 14
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
library(multcomp)
library(ggplot2)
library(ggpubr)
source(file.path(path.code,"FCT_forestplot.R"))

## * Load data
source(file.path(path.code,"analysis-test.R"))

## * Generate plot

## ** Logistic
## complete case
gg.forest_cc <- forestplot(dtS.ass_cc) + labs(color = "Analysis at:", x = "log odd ratio from logistic model for recovery")
gg.forest_cc <- gg.forest_cc + ggtitle(paste0("Complete case analysis (n=",NROW(dfWR.NP1_ccw4S),"/",NROW(dfWR.NP1_ccw8S),"/",NROW(dfWR.NP1_ccw12S),")"))
gg.forest_cc

setkeyv(dtS.ass_cc, c("time","term"))
dtS.ass_cc[,.(time,term,estimate,lower,upper,p.value,lower.adj,upper.adj,adj.p.value)]
##        time                  term    estimate       lower       upper     p.value  lower.adj    upper.adj adj.p.value
##  1: week 12                   age  1.04344302 -0.26959673  2.35648278 0.119342547         NA           NA          NA
##  2: week 12                female -0.16310191 -1.55786648  1.23166266 0.818716912         NA           NA          NA
##  3: week 12    MR (OFC thickness) -0.86179387 -1.59548512 -0.12810263 0.021325358 -1.8620137  0.138425954  0.13422109
##  4: week 12                HAMD17  0.56404083 -0.13260625  1.26068791 0.112538398 -0.3856778  1.513759427  0.54070134
##  5: week 12                 hsCRP  1.08090259 -0.56222257  2.72402775 0.197283472 -1.1591219  3.320927094  0.76021163
##  6: week 12       PET (serotonin) -0.06895385 -0.68608310  0.54817540 0.826655395 -0.9102681  0.772360416  0.99999314
##  7: week 12 cognition (cluster 2) -1.44481661 -3.00523278  0.11559956 0.069560045 -3.5720864  0.682453147  0.37667598
##  8: week 12 cognition (cluster 3) -0.80511508 -2.54721796  0.93698779 0.365041788 -3.1800730  1.569842848  0.94873097
##  9: week 12       EEG (vigilance) -0.03618264 -0.67348048  0.60111520 0.911396602 -0.9049922  0.832626880  0.99999994
## 10: week 12           Neuroticism          NA          NA          NA          NA         NA           NA          NA
## 11:  week 8                   age  0.47664094 -0.24330685  1.19658873 0.194427297         NA           NA          NA
## 12:  week 8                female -0.35932885 -1.61562102  0.89696332 0.575073390         NA           NA          NA
## 13:  week 8    MR (OFC thickness) -0.60571994 -1.25249614  0.04105626 0.066424416 -1.4870521  0.275612176  0.35908635
## 14:  week 8                HAMD17  0.56066052 -0.08555265  1.20687369 0.089040138 -0.3199044  1.441225422  0.45177119
## 15:  week 8                 hsCRP  1.46954263  0.04398767  2.89509760 0.043337596 -0.4729954  3.412080691  0.25102693
## 16:  week 8       PET (serotonin) -0.25704505 -0.80619130  0.29210120 0.358921367 -1.0053413  0.491251239  0.94369513
## 17:  week 8 cognition (cluster 2) -0.81977323 -2.17760298  0.53805652 0.236688505 -2.6700253  1.030478797  0.82346635
## 18:  week 8 cognition (cluster 3) -1.89634154 -3.38458419 -0.40809889 0.012510207 -3.9243012  0.131618136  0.08057073
## 19:  week 8       EEG (vigilance) -0.91233551 -1.57616465 -0.24850637 0.007066751 -1.8169049 -0.007766138  0.04668763
## 20:  week 8              Cortisol          NA          NA          NA          NA         NA           NA          NA
## 21:  week 4                   age  0.23389737 -0.28055767  0.74835241 0.372876010         NA           NA          NA
## 22:  week 4                female -0.73184867 -1.83293581  0.36923848 0.192673850         NA           NA          NA
## 23:  week 4    MR (OFC thickness) -0.14116450 -0.67145644  0.38912745 0.601847680 -0.8657026  0.583373614  0.99813431
## 24:  week 4                HAMD17  0.01084126 -0.53814179  0.55982431 0.969125451 -0.7392345  0.760917045  1.00000000
## 25:  week 4                 hsCRP  0.86151224 -0.47838786  2.20141234 0.207599685 -0.9691938  2.692218325  0.79120472
## 26:  week 4       PET (serotonin)  0.01729582 -0.47958023  0.51417187 0.945606823 -0.6615861  0.696177781  1.00000000
## 27:  week 4 cognition (cluster 2) -0.41180547 -1.57233046  0.74871952 0.486753476 -1.9974313  1.173820328  0.98921254
## 28:  week 4 cognition (cluster 3) -0.97529753 -2.24880377  0.29820871 0.133352482 -2.7152897  0.764694600  0.61858285
## 29:  week 4       EEG (vigilance) -0.06850206 -0.56517040  0.42816627 0.786910683 -0.7471002  0.610096096  0.99997595
## 30:  week 4                  CATS          NA          NA          NA          NA         NA           NA          NA
##        time                  term    estimate       lower       upper     p.value  lower.adj    upper.adj adj.p.value

## missing value
gg.forest_imp <- forestplot(dtS.ass_imp) + labs(color = "Analysis at:", x = "log odd ratio from logistic model for recovery")
gg.forest_imp <- gg.forest_imp + ggtitle(paste0("Multiple imputation (n=",NROW(dfWRimp.NP1_w4C$data),"/",NROW(dfWRimp.NP1_w8C$data),"/",NROW(dfWRimp.NP1_w12C$data),")"))
gg.forest_imp

setkeyv(dtS.ass_imp, c("time","term"))
dtS.ass_imp[,.(time,term,estimate,lower,upper,p.value,lower.adj,upper.adj,adj.p.value)]
##        time                  term     estimate       lower       upper     p.value  lower.adj     upper.adj adj.p.value
##  1: week 12                female -0.898052812 -2.35778862  0.56168300 0.223680983         NA            NA          NA
##  2: week 12                   age  0.944057537 -0.27621471  2.16432978 0.127217090         NA            NA          NA
##  3: week 12    MR (OFC thickness) -1.015594624 -1.76088413 -0.27030512 0.008320140 -2.0874030  0.0562137156  0.07414347
##  4: week 12                HAMD17  0.449987098 -0.21449416  1.11446835 0.180958963 -0.5055683  1.4055424915  0.81446712
##  5: week 12                 hsCRP  1.001687386 -0.71921167  2.72258645 0.249341554 -1.4729426  3.4763174119  0.91174164
##  6: week 12       PET (serotonin) -0.078573011 -0.73343590  0.57628988 0.811416615 -1.0203322  0.8631861492  0.99999981
##  7: week 12 cognition (cluster 2) -1.916496381 -3.68833534 -0.14465743 0.034446856 -4.4642907  0.6312979092  0.26488947
##  8: week 12 cognition (cluster 3) -1.727339880 -3.62252836  0.16784860 0.073330910 -4.4525715  0.9978917346  0.47904896
##  9: week 12       EEG (vigilance) -0.204889126 -0.86832309  0.45854484 0.539623282 -1.1589929  0.7492146244  0.99893424
## 10: week 12                  CATS  0.718229801 -0.05524125  1.49170085 0.068213132 -0.3941193  1.8305789295  0.45470877
## 11: week 12              Cortisol  0.466265253 -0.36354628  1.29607679 0.265992614 -0.7271099  1.6596404027  0.92728983
## 12: week 12           Neuroticism  0.349656909 -0.55468782  1.25400164 0.442895467 -0.9508809  1.6501946957  0.99376244
## 13:  week 8                female -0.606523958 -1.88875471  0.67570679 0.348594058         NA            NA          NA
## 14:  week 8                   age  0.625488803 -0.13658342  1.38756102 0.106093214         NA            NA          NA
## 15:  week 8    MR (OFC thickness) -0.615175146 -1.25320090  0.02285061 0.058543617 -1.5319204  0.3015701341  0.40427550
## 16:  week 8                HAMD17  0.618934031 -0.00689762  1.24476568 0.052506763 -0.2800916  1.5179596320  0.37152090
## 17:  week 8                 hsCRP  1.691274796  0.15452069  3.22802890 0.031511194 -0.5152520  3.8978015492  0.24407741
## 18:  week 8       PET (serotonin) -0.337471332 -0.94147723  0.26653457 0.268864033 -1.2053254  0.5303827225  0.92906529
## 19:  week 8 cognition (cluster 2) -1.122716525 -2.56174658  0.31631353 0.124151811 -3.1899884  0.9445553917  0.67266494
## 20:  week 8 cognition (cluster 3) -2.766429587 -4.51155599 -1.02130319 0.002331061 -5.2735195 -0.2593396467  0.02186628
## 21:  week 8       EEG (vigilance) -1.025533211 -1.73954202 -0.31152440 0.005518995 -2.0514358  0.0003693958  0.05013556
## 22:  week 8                  CATS -0.005758783 -0.59021735  0.57869979 0.984373951 -0.8455738  0.8340561953  1.00000000
## 23:  week 8              Cortisol  0.237121591 -0.39690422  0.87114740 0.457876039 -0.6731297  1.1473728572  0.99506997
## 24:  week 8           Neuroticism  0.666163789 -0.13539667  1.46772425 0.101863773 -0.4856121  1.8179396532  0.59703851
## 25:  week 4                female -1.221835167 -2.39156833 -0.05210200 0.040873554         NA            NA          NA
## 26:  week 4                   age  0.218251228 -0.32285950  0.75936196 0.424020165         NA            NA          NA
## 27:  week 4    MR (OFC thickness) -0.141776468 -0.67977369  0.39622075 0.600966603 -0.9172243  0.6336713691  0.99979913
## 28:  week 4                HAMD17  0.190980874 -0.33616179  0.71812354 0.472495393 -0.5688184  0.9507801257  0.99707326
## 29:  week 4                 hsCRP  0.969374585 -0.41287130  2.35162047 0.166340939 -1.0222712  2.9610203474  0.80209042
## 30:  week 4       PET (serotonin) -0.007519834 -0.52609109  0.51105143 0.977018061 -0.7549542  0.7399145113  1.00000000
## 31:  week 4 cognition (cluster 2) -0.603538625 -1.86886690  0.66178965 0.344723445 -2.4264705  1.2193932998  0.97765924
## 32:  week 4 cognition (cluster 3) -1.484978275 -2.88386437 -0.08609218 0.037799594 -3.5002829  0.5303263111  0.29653799
## 33:  week 4       EEG (vigilance) -0.108042382 -0.61781570  0.40173094 0.673922709 -0.8428205  0.6267357046  0.99997193
## 34:  week 4                  CATS -0.240642458 -0.80361820  0.32233328 0.396979577 -1.0520857  0.5708007498  0.98968700
## 35:  week 4              Cortisol  0.285025984 -0.34134327  0.91139523 0.367338867 -0.6177017  1.1877536546  0.98387712
## 36:  week 4           Neuroticism  0.310795862 -0.35045963  0.97205135 0.351919927 -0.6423187  1.2639104238  0.97984991
##        time                  term     estimate       lower       upper     p.value  lower.adj     upper.adj adj.p.value

## together
ggforest.ass <- ggarrange(gg.forest_cc + coord_cartesian(xlim = c(-5,5)) + ylab(""),
                          gg.forest_imp  + coord_cartesian(xlim = c(-5,5)) + ylab(""),
                          common.legend = TRUE, legend = "bottom")
ggforest.ass

## ** Random forest
df.ipranger <- rbind(data.frame(time = "week4", param = rownames(e.rangerPerm_ccw4), e.rangerPerm_ccw4),
                     data.frame(time = "week8", param = rownames(e.rangerPerm_ccw8), e.rangerPerm_ccw8),
                     data.frame(time = "week12", param = rownames(e.rangerPerm_ccw12), e.rangerPerm_ccw12))
rownames(df.ipranger) <- NULL
df.ipranger$significance <- droplevels(cut(df.ipranger$pvalue, c(0,0.001,0.01,0.05,0.1,0.5,1)))
df.ipranger$param <- factor(df.ipranger$param,
                            levels = c("female","age","MR_OFCthick","HAMD17","low_hsCRP","lvpet","cognitive_cluster2","cognitive_cluster3","EEG_vigilance"),     
                            labels = c("female","age","MR (OFC thickness)","HAMD17","hsCRP","PET (serotonin)","cognition (cluster 2)","cognition (cluster 3)","EEG (vigilance)"))

ggVI <- ggplot(df.ipranger, aes(x=param, y = importance, group = time, color = time)) + geom_point(aes(size = 1/pvalue, shape = significance)) + geom_line()
ggVI <- ggVI + scale_size_continuous(breaks = 1/c(0.001,0.01,0.05,0.1,0.5,1), labels = c("(0,0.001]","(0.001,0.01]","(0.01,0.05]","(0.05,0.1]","(0.1,0.5]","(0.5,1]"), name = "significance")
ggVI <- ggVI + guides(size = "none")
ggVI <- ggVI + labs(x = "",y = "variable importance", shape = "statistical\nsignificance") + theme(text = element_text(size=20), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                                                          axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm"))
## ggsave(ggVI, filename = "figures/variableImportance.pdf", width = 12)
## ggsave(ggVI, filename = "figures/variableImportance.png", width = 12)


## * Export
if(FALSE){
    ggsave(ggforest.ass, filename = "./figures/gg-forestplot-OR.pdf", width = 12)
    ggsave(ggforest.ass, filename = "./figures/gg-forestplot-OR.png", width = 12)

    ggsave(ggVI, filename = "./figures/variableImportance.pdf", width = 12)
    ggsave(ggVI, filename = "./figures/variableImportance.png", width = 12)
}

##----------------------------------------------------------------------
### figure-forestplot.R ends here
