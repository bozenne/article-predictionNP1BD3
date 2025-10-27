### figure-forestplot.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: mar  2 2022 (15:46) 
## Version: 
## Last-Updated: okt 27 2025 (18:59) 
##           By: Brice Ozenne
##     Update #: 33
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
## devtools::install_github("NightingaleHealth/ggforestplot")
source(file.path(path.code,"FCT_forestplot.R"))

## * Load data
load(file.path(path.results,"test.Rdata"))
old2new.names <- c("age" = "Age",
                   "female" = "Female",
                   "PET (serotonin)" = "5-HT4R binding (PET)",
                   "MR (OFC thickness)" = "OFC thickness (MR)",
                   "EEG (vigilance)" = "Vigilance (EEG)",
                   "hsCRP" = "hs-CRP (blood)",
                   "Cortisol" = "CAR (saliva)",
                   "cognition (cluster 2)" = "Cluster 2 (cognition)",
                   "cognition (cluster 3)" = "Cluster 3 (cognition)",
                   "HAMD17" = "HAMD17 (depressive symptoms)",
                   "Neuroticism" = "Neuroticism (personality)",
                   "CATS" = "CAT (Childhood trauma)")


dtS.ass_cc$term2 <- factor(dtS.ass_cc$term, levels = levels(dtS.ass_cc$term),
                           labels = old2new.names[levels(dtS.ass_cc$term)])
dtS.ass_cc$term2 <- factor(dtS.ass_cc$term2, levels = old2new.names)
dtS.ass_imp$term2 <- factor(dtS.ass_imp$term, levels = levels(dtS.ass_imp$term),
                          labels = old2new.names[levels(dtS.ass_cc$term)])
dtS.ass_imp$term2 <- factor(dtS.ass_imp$term2, levels = old2new.names)

## * Generate plot

## ** Logistic
## *** complete case
gg.forest_cctraj <- forestplot(dtS.ass_cc, term = "term2", reorder = FALSE) + labs(color = "Analysis at:", x = "log odds ratio from logistic model for recovery")
gg.forest_cctraj <- gg.forest_cctraj + ggtitle(paste0("Complete case analysis (n=",NROW(dfWR.NP1_ccw4S),"/",NROW(dfWR.NP1_ccw8S),"/",NROW(dfWR.NP1_ccw12S),"/",NROW(dfWR.NP1_cctrajS),")"))
gg.forest_cctraj

gg.forest_cc <- forestplot(dtS.ass_cc[time!="trajectory"], term = "term2", reorder = FALSE) + labs(color = "Analysis at:", x = "log odds ratio from logistic model for recovery")
gg.forest_cc <- gg.forest_cc + ggtitle(paste0("Complete case analysis (n=",NROW(dfWR.NP1_ccw4S),"/",NROW(dfWR.NP1_ccw8S),"/",NROW(dfWR.NP1_ccw12S),")"))
gg.forest_cc

setkeyv(dtS.ass_cc, c("time","term2"))
dtS.ass_cc[,.(time,term2,estimate,lower,upper,p.value,lower.adj,upper.adj,adj.p.value)]
##           time                        term2    estimate      lower       upper    p.value  lower.adj upper.adj adj.p.value
##         <fctr>                       <fctr>       <num>      <num>       <num>      <num>      <num>     <num>       <num>
##  1: trajectory                          Age  0.27975595 -0.2440280  0.80353989 0.29517902         NA        NA          NA
##  2: trajectory                       Female -1.31928328 -2.4356229 -0.20294365 0.02054335         NA        NA          NA
##  3: trajectory           OFC thickness (MR) -0.57903818 -1.1942064  0.03613007 0.06505917 -1.4193293 0.2612529   0.3643020
##  4: trajectory HAMD17 (depressive symptoms) -0.14949517 -0.6876291  0.38863873 0.58610865 -0.8845609 0.5855706   0.9975014
##  5: trajectory               hs-CRP (blood)  1.07690702 -0.2730594  2.42687345 0.11793043 -0.7670841 2.9208981   0.5687535
##  6: trajectory         5-HT4R binding (PET) -0.03633292 -0.5527661  0.48010029 0.89032700 -0.7417565 0.6690907   0.9999998
##  7: trajectory        Cluster 2 (cognition) -0.46571118 -1.6763351  0.74491279 0.45086576 -2.1193669 1.1879446   0.9824984
##  8: trajectory        Cluster 3 (cognition) -0.74402259 -2.0672980  0.57925284 0.27045906 -2.5515550 1.0635098   0.8791945
##  9: trajectory              Vigilance (EEG) -0.56626760 -1.1663803  0.03384506 0.06439629 -1.3859935 0.2534583   0.3612880
## 10: trajectory       CAT (Childhood trauma)          NA         NA          NA         NA         NA        NA          NA
## 11: trajectory                 CAR (saliva)          NA         NA          NA         NA         NA        NA          NA
## 12: trajectory    Neuroticism (personality)          NA         NA          NA         NA         NA        NA          NA
## 13:    week 12                          Age  0.81290854 -0.1991656  1.82498268 0.11542653         NA        NA          NA
## 14:    week 12                       Female -0.39299838 -1.7150671  0.92907029 0.56014963         NA        NA          NA
## 15:    week 12           OFC thickness (MR) -0.70720797 -1.3423103 -0.07210565 0.02907366 -1.5741454 0.1597295   0.1799001
## 16:    week 12 HAMD17 (depressive symptoms)  0.23537353 -0.3632756  0.83402269 0.44093952 -0.5818040 1.0525511   0.9789569
## 17:    week 12               hs-CRP (blood)  0.59040715 -0.8434181  2.02423243 0.41963444 -1.3668158 2.5476301   0.9729095
## 18:    week 12         5-HT4R binding (PET)  0.16805457 -0.3985637  0.73467279 0.56103147 -0.6053996 0.9415088   0.9959440
## 19:    week 12        Cluster 2 (cognition) -0.87932730 -2.2481214  0.48946682 0.20799391 -2.7477803 0.9891257   0.7846151
## 20:    week 12        Cluster 3 (cognition) -0.43559532 -1.9617932  1.09060261 0.57589054 -2.5189102 1.6477196   0.9967966
## 21:    week 12              Vigilance (EEG)  0.09762557 -0.4673671  0.66261827 0.73486298 -0.6736097 0.8688609   0.9998751
## 22:    week 12       CAT (Childhood trauma)          NA         NA          NA         NA         NA        NA          NA
## 23:    week 12                 CAR (saliva)          NA         NA          NA         NA         NA        NA          NA
## 24:    week 12    Neuroticism (personality)          NA         NA          NA         NA         NA        NA          NA
## 25:     week 8                          Age  0.46077421 -0.1897010  1.11124937 0.16502410         NA        NA          NA
## 26:     week 8                       Female -0.45764039 -1.6248000  0.70951925 0.44219197         NA        NA          NA
## 27:     week 8           OFC thickness (MR) -0.45455237 -1.0367947  0.12768999 0.12598465 -1.2493351 0.3402304   0.5898954
## 28:     week 8 HAMD17 (depressive symptoms)  0.28155670 -0.2863378  0.84945121 0.33118413 -0.4936407 1.0567541   0.9306381
## 29:     week 8               hs-CRP (blood)  0.82376823 -0.4235222  2.07105871 0.19550977 -0.8788302 2.5263667   0.7625702
## 30:     week 8         5-HT4R binding (PET) -0.15011091 -0.6570793  0.35685750 0.56168810 -0.8421419 0.5419201   0.9960788
## 31:     week 8        Cluster 2 (cognition) -0.50805665 -1.7275439  0.71143065 0.41418546 -2.1727027 1.1565894   0.9716107
## 32:     week 8        Cluster 3 (cognition) -1.39105193 -2.7185345 -0.06356935 0.03999301 -3.2031156 0.4210118   0.2394098
## 33:     week 8              Vigilance (EEG) -0.65938997 -1.2341938 -0.08458618 0.02455161 -1.4440188 0.1252388   0.1544359
## 34:     week 8       CAT (Childhood trauma)          NA         NA          NA         NA         NA        NA          NA
## 35:     week 8                 CAR (saliva)          NA         NA          NA         NA         NA        NA          NA
## 36:     week 8    Neuroticism (personality)          NA         NA          NA         NA         NA        NA          NA
## 37:     week 4                          Age  0.25381585 -0.2434057  0.75103744 0.31706887         NA        NA          NA
## 38:     week 4                       Female -0.71403058 -1.8059686  0.37790741 0.19996806         NA        NA          NA
## 39:     week 4           OFC thickness (MR) -0.06335028 -0.5680834  0.44138283 0.80568234 -0.7531670 0.6264665   0.9999877
## 40:     week 4 HAMD17 (depressive symptoms) -0.07653123 -0.6026031  0.44954061 0.77554514 -0.7955115 0.6424491   0.9999663
## 41:     week 4               hs-CRP (blood)  0.55787451 -0.6970852  1.81283422 0.38360448 -1.1572740 2.2730230   0.9626538
## 42:     week 4         5-HT4R binding (PET)  0.08161689 -0.4034035  0.56663724 0.74154105 -0.5812585 0.7444923   0.9999097
## 43:     week 4        Cluster 2 (cognition) -0.18640813 -1.3034792  0.93066291 0.74361982 -1.7131047 1.3402885   0.9999146
## 44:     week 4        Cluster 3 (cognition) -0.86503090 -2.1100260  0.37996420 0.17326226 -2.5665608 0.8364990   0.7240491
## 45:     week 4              Vigilance (EEG) -0.06211710 -0.5489636  0.42472937 0.80253070 -0.7274883 0.6032541   0.9999862
## 46:     week 4       CAT (Childhood trauma)          NA         NA          NA         NA         NA        NA          NA
## 47:     week 4                 CAR (saliva)          NA         NA          NA         NA         NA        NA          NA
## 48:     week 4    Neuroticism (personality)          NA         NA          NA         NA         NA        NA          NA
##           time                         term    estimate      lower       upper    p.value  lower.adj upper.adj adj.p.value

## *** missing value
gg.forest_imptraj <- forestplot(dtS.ass_imp, term = "term2", reorder = FALSE) + labs(color = "Analysis at:", x = "log odds ratio from logistic model for recovery")
gg.forest_imptraj <- gg.forest_imptraj + ggtitle(paste0("Multiple imputation (n=",NROW(dfWRimp.NP1_w4C$data),"/",NROW(dfWRimp.NP1_w8C$data),"/",NROW(dfWRimp.NP1_w12C$data),"/",NROW(dfWRimp.NP1_trajC$data),")"))
gg.forest_imptraj

gg.forest_imp <- forestplot(dtS.ass_imp[time!="trajectory"], term = "term2", reorder = FALSE) + labs(color = "Analysis at:", x = "log odds ratio from logistic model for recovery")
gg.forest_imp <- gg.forest_imp + ggtitle(paste0("Multiple imputation (n=",NROW(dfWRimp.NP1_w4C$data),"/",NROW(dfWRimp.NP1_w8C$data),"/",NROW(dfWRimp.NP1_w12C$data),")"))
gg.forest_imp

setkeyv(dtS.ass_imp, c("time","term2"))
dtS.ass_imp[,.(time,term2,estimate,lower,upper,p.value,lower.adj,upper.adj,adj.p.value)]
##           time                        term2     estimate        lower        upper     p.value    lower.adj  upper.adj adj.p.value
##         <fctr>                       <fctr>        <num>        <num>        <num>       <num>        <num>      <num>       <num>
##  1: trajectory                          Age -1.686694064 -2.925477272 -0.447910856 0.008284188           NA         NA          NA
##  2: trajectory                       Female  0.276194477 -0.271723585  0.824112539 0.318494902           NA         NA          NA
##  3: trajectory           OFC thickness (MR) -0.566732901 -1.181790365  0.048324563 0.070380409 -1.452753857 0.31928806  0.48177015
##  4: trajectory HAMD17 (depressive symptoms)  0.025725243 -0.549365474  0.600815961 0.929225742 -0.802720677 0.85417116  1.00000000
##  5: trajectory               hs-CRP (blood)  1.253839863 -0.153317665  2.660997391 0.079946120 -0.773390954 3.28107068  0.52693001
##  6: trajectory         5-HT4R binding (PET)  0.048260858 -0.454110341  0.550632057 0.848744807 -0.675460236 0.77198195  0.99999998
##  7: trajectory        Cluster 2 (cognition) -0.785645174 -2.139416471  0.568126123 0.250978513 -2.733349473 1.16205912  0.92302992
##  8: trajectory        Cluster 3 (cognition) -1.039017852 -2.393127870  0.315092166 0.130431392 -2.987458173 0.90942247  0.71094160
##  9: trajectory              Vigilance (EEG) -0.606743948 -1.227163176  0.013675280 0.055133566 -1.500556688 0.28706879  0.40191812
## 10: trajectory       CAT (Childhood trauma) -0.029002047 -0.090079392  0.032075298 0.347080265 -0.116952688 0.05894859  0.97786958
## 11: trajectory                 CAR (saliva)  0.003301743 -0.002914839  0.009518325 0.293144971 -0.005644909 0.01224839  0.95436284
## 12: trajectory    Neuroticism (personality) -0.003265826 -0.014806270  0.008274618 0.574591571 -0.019890752 0.01335910  0.99960559
## 13:    week 12                          Age -0.917439878 -2.296764726  0.461884971 0.189006569           NA         NA          NA
## 14:    week 12                       Female  0.736976668 -0.240428887  1.714382223 0.137155729           NA         NA          NA
## 15:    week 12           OFC thickness (MR) -0.870213721 -1.531160705 -0.209266737 0.010595919 -1.821034944 0.08060750  0.09396276
## 16:    week 12 HAMD17 (depressive symptoms)  0.228647719 -0.357101333  0.814396771 0.438952178 -0.613985585 1.07128102  0.99386047
## 17:    week 12               hs-CRP (blood)  0.541703593 -0.981186574  2.064593761 0.480432191 -1.648705652 2.73211284  0.99698565
## 18:    week 12         5-HT4R binding (PET)  0.149692250 -0.439387364  0.738771864 0.613944664 -0.697739739 0.99712424  0.99981908
## 19:    week 12        Cluster 2 (cognition) -1.232283668 -2.779223684  0.314656348 0.116631552 -3.457326677 0.99275934  0.65758928
## 20:    week 12        Cluster 3 (cognition) -1.232117605 -2.916963774  0.452728564 0.149184031 -3.655528008 1.19129280  0.75138497
## 21:    week 12              Vigilance (EEG) -0.055485349 -0.657447066  0.546476369 0.854702437 -0.921456311 0.81048561  0.99999999
## 22:    week 12       CAT (Childhood trauma)  0.693980650  0.007789692  1.380171608 0.047522927 -0.293172009 1.68113331  0.34996954
## 23:    week 12                 CAR (saliva)  0.462824947 -0.291467530  1.217117423 0.225194320 -0.622249860 1.54789975  0.88903712
## 24:    week 12    Neuroticism (personality) -0.023381728 -0.818535021  0.771771566 0.953408822 -1.167248190 1.12048473  1.00000000
## 25:     week 8                          Age -0.577534907 -1.758910876  0.603841062 0.333108711           NA         NA          NA
## 26:     week 8                       Female  0.451931362 -0.195499434  1.099362158 0.168387864           NA         NA          NA
## 27:     week 8           OFC thickness (MR) -0.486117432 -1.061776655  0.089541791 0.096644839 -1.315318633 0.34308377  0.59434177
## 28:     week 8 HAMD17 (depressive symptoms)  0.320979351 -0.217996206  0.859954908 0.239112300 -0.455416318 1.09737502  0.91059355
## 29:     week 8               hs-CRP (blood)  0.893999537 -0.401163526  2.189162600 0.173051171 -0.970918096 2.75891717  0.81271899
## 30:     week 8         5-HT4R binding (PET) -0.096167633 -0.614831706  0.422496441 0.712801028 -0.843305951 0.65097069  0.99999133
## 31:     week 8        Cluster 2 (cognition) -0.699123722 -1.958697754  0.560450309 0.272218928 -2.513216242 1.11496880  0.93997382
## 32:     week 8        Cluster 3 (cognition) -1.879921794 -3.297180996 -0.462662592 0.010043514 -3.921041870 0.16119828  0.09045036
## 33:     week 8              Vigilance (EEG) -0.698474885 -1.284475193 -0.112474576 0.020152847 -1.542579663 0.14562989  0.17149993
## 34:     week 8       CAT (Childhood trauma)  0.074790483 -0.473661785  0.623242751 0.786558564 -0.715257445 0.86483841  0.99999954
## 35:     week 8                 CAR (saliva)  0.281525297 -0.309097697  0.872148292 0.345206001 -0.569127065 1.13217766  0.97701584
## 36:     week 8    Neuroticism (personality)  0.226125522 -0.440782096  0.893033139 0.501333952 -0.734578302 1.18682935  0.99819131
## 37:     week 4                          Age -1.150064963 -2.298542434 -0.001587493 0.049692270           NA         NA          NA
## 38:     week 4                       Female  0.242497114 -0.280922871  0.765917100 0.358939094           NA         NA          NA
## 39:     week 4           OFC thickness (MR) -0.058764139 -0.571392786  0.453864509 0.819954620 -0.797784173 0.68025590  0.99999993
## 40:     week 4 HAMD17 (depressive symptoms)  0.088121294 -0.418131177  0.594373766 0.729701621 -0.641721309 0.81796390  0.99999584
## 41:     week 4               hs-CRP (blood)  0.650411828 -0.652231115  1.953054770 0.322928997 -1.226836615 2.52766027  0.97074004
## 42:     week 4         5-HT4R binding (PET)  0.080616162 -0.415786180  0.577018503 0.747157732 -0.635010953 0.79624328  0.99999785
## 43:     week 4        Cluster 2 (cognition) -0.332963443 -1.530070671  0.864143786 0.580993581 -2.058054368 1.39212748  0.99969475
## 44:     week 4        Cluster 3 (cognition) -1.305280578 -2.646607101  0.036045946 0.056311248 -3.238332326 0.62777117  0.41057468
## 45:     week 4              Vigilance (EEG) -0.101380661 -0.600688092  0.397926770 0.686959469 -0.821205772 0.61844445  0.99998234
## 46:     week 4       CAT (Childhood trauma) -0.281353376 -0.839330756  0.276624005 0.318308480 -1.085763288 0.52305654  0.96887739
## 47:     week 4                 CAR (saliva)  0.288736155 -0.320148090  0.897620400 0.347755621 -0.588935750 1.16640806  0.97933042
## 48:     week 4    Neuroticism (personality)  0.200533947 -0.430035548  0.831103443 0.528250206 -0.708525615 1.10959351  0.99904117
##           time                        term2     estimate        lower        upper     p.value    lower.adj  upper.adj adj.p.value

## POSTER
new.levels <- c("PET (serotonin)", "MR (OFC thickness)", "EEG (vigilance)", "cognition (cluster 2)", "cognition (cluster 3)", "Cortisol", "hsCRP",
                                      "HAMD17", "CATS", "Neuroticism", "age", "female")
dtS.ass_imp$term3 <- factor(dtS.ass_imp$term, levels = new.levels)
                                       
gg.forest_imp2 <- forestplot(dtS.ass_imp[time!="trajectory"], reorder = FALSE, term = "term3") + labs(color = "Analysis at:", x = "log odds ratio for recovery", y = "")
gg.forest_imp2 <- gg.forest_imp2 + ggtitle(paste0("Multiple imputation (n=",NROW(dfWRimp.NP1_w4C$data),"/",NROW(dfWRimp.NP1_w8C$data),"/",NROW(dfWRimp.NP1_w12C$data),")"))
gg.forest_imp2 <- gg.forest_imp2 + coord_flip() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + scale_x_continuous(breaks=seq(-4,3,1))
gg.forest_imp2 <- gg.forest_imp2 + ggplot2::theme(panel.grid.major.y = element_line(colour = "gray50", size = 0.25, linetype = 2),
                                                panel.grid.minor.y = element_blank(), 
                                                panel.grid.major.x = element_blank(),
                                                panel.grid.minor.x = element_blank())
gg.forest_imp2

## ggsave(gg.forest_imp2, filename = "./figures/gg-forestplot-OR2.pdf", width = 12)
## ggsave(gg.forest_imp2, filename = "./figures/gg-forestplot-OR2.png", width = 12)


## *** together
ggforest.ass <- ggarrange(gg.forest_cc + coord_cartesian(xlim = c(-5,5)) + ylab(""),
                          gg.forest_imp  + coord_cartesian(xlim = c(-5,5)) + ylab(""),
                          common.legend = TRUE, legend = "bottom")
ggforest.ass

ggforest.asstraj <- ggarrange(gg.forest_cctraj + coord_cartesian(xlim = c(-5,5)) + ylab(""),
                              gg.forest_imptraj  + coord_cartesian(xlim = c(-5,5)) + ylab(""),
                              common.legend = TRUE, legend = "bottom")
ggforest.asstraj

## ** Random forest
df.ipranger <- rbind(data.frame(time = "week4", param = rownames(e.rangerPerm_ccw4), e.rangerPerm_ccw4),
                     data.frame(time = "week8", param = rownames(e.rangerPerm_ccw8), e.rangerPerm_ccw8),
                     data.frame(time = "week12", param = rownames(e.rangerPerm_ccw12), e.rangerPerm_ccw12),
                     data.frame(time = "traj", param = rownames(e.rangerPerm_cctraj), e.rangerPerm_cctraj))
rownames(df.ipranger) <- NULL
df.ipranger$significance <- droplevels(cut(df.ipranger$pvalue, c(0,0.001,0.01,0.05,0.1,0.5,1)))
df.ipranger$param <- factor(df.ipranger$param,
                            levels = c("female","age","MR_OFCthick","HAMD17","low_hsCRP","lvpet","cognitive_cluster2","cognitive_cluster3","EEG_vigilance"),     
                            labels = c("female","age","MR (OFC thickness)","HAMD17","hsCRP","PET (serotonin)","cognition (cluster 2)","cognition (cluster 3)","EEG (vigilance)"))
df.ipranger$term2 <- droplevels(factor(df.ipranger$param, levels = names(old2new.names), labels = old2new.names))
df.ipranger
##      time                 param    importance     pvalue significance                        term2
## 1   week4                female -0.0050503515 0.87712288      (0.5,1]                       Female
## 2   week4                   age  0.0146193600 0.13286713    (0.1,0.5]                          Age
## 3   week4    MR (OFC thickness)  0.0050171775 0.31168831    (0.1,0.5]           OFC thickness (MR)
## 4   week4                HAMD17 -0.0106773975 0.88411588      (0.5,1] HAMD17 (depressive symptoms)
## 5   week4                 hsCRP -0.0018443538 0.58041958      (0.5,1]               hs-CRP (blood)
## 6   week4       PET (serotonin)  0.0043395432 0.32067932    (0.1,0.5]         5-HT4R binding (PET)
## 7   week4 cognition (cluster 2) -0.0011411421 0.50149850      (0.5,1]        Cluster 2 (cognition)
## 8   week4 cognition (cluster 3)  0.0074573257 0.07892108   (0.05,0.1]        Cluster 3 (cognition)
## 9   week4       EEG (vigilance) -0.0039026217 0.71028971      (0.5,1]              Vigilance (EEG)
## 10  week8                female -0.0052458023 0.87412587      (0.5,1]                       Female
## 11  week8                   age  0.0116987320 0.14385614    (0.1,0.5]                          Age
## 12  week8    MR (OFC thickness)  0.0076690057 0.25974026    (0.1,0.5]           OFC thickness (MR)
## 13  week8                HAMD17  0.0055072981 0.25674326    (0.1,0.5] HAMD17 (depressive symptoms)
## 14  week8                 hsCRP -0.0042825411 0.83916084      (0.5,1]               hs-CRP (blood)
## 15  week8       PET (serotonin) -0.0103063187 0.77922078      (0.5,1]         5-HT4R binding (PET)
## 16  week8 cognition (cluster 2) -0.0005347263 0.44255744    (0.1,0.5]        Cluster 2 (cognition)
## 17  week8 cognition (cluster 3)  0.0098227172 0.03896104  (0.01,0.05]        Cluster 3 (cognition)
## 18  week8       EEG (vigilance)  0.0055136599 0.18881119    (0.1,0.5]              Vigilance (EEG)
## 19 week12                female -0.0035052660 0.77222777      (0.5,1]                       Female
## 20 week12                   age -0.0012492394 0.51648352      (0.5,1]                          Age
## 21 week12    MR (OFC thickness)  0.0162901528 0.08991009   (0.05,0.1]           OFC thickness (MR)
## 22 week12                HAMD17  0.0005552010 0.43556444    (0.1,0.5] HAMD17 (depressive symptoms)
## 23 week12                 hsCRP -0.0032353282 0.83016983      (0.5,1]               hs-CRP (blood)
## 24 week12       PET (serotonin)  0.0220453069 0.03696304  (0.01,0.05]         5-HT4R binding (PET)
## 25 week12 cognition (cluster 2)  0.0028633418 0.21178821    (0.1,0.5]        Cluster 2 (cognition)
## 26 week12 cognition (cluster 3) -0.0008492188 0.49850150    (0.1,0.5]        Cluster 3 (cognition)
## 27 week12       EEG (vigilance)  0.0041917164 0.21078921    (0.1,0.5]              Vigilance (EEG)
## 28   traj                female  0.0063222447 0.11288711    (0.1,0.5]                       Female
## 29   traj                   age -0.0051448292 0.63236763      (0.5,1]                          Age
## 30   traj    MR (OFC thickness)  0.0251098173 0.03696304  (0.01,0.05]           OFC thickness (MR)
## 31   traj                HAMD17 -0.0037318881 0.61838162      (0.5,1] HAMD17 (depressive symptoms)
## 32   traj                 hsCRP  0.0029331137 0.17482517    (0.1,0.5]               hs-CRP (blood)
## 33   traj       PET (serotonin)  0.0097394583 0.18381618    (0.1,0.5]         5-HT4R binding (PET)
## 34   traj cognition (cluster 2) -0.0007894867 0.49750250    (0.1,0.5]        Cluster 2 (cognition)
## 35   traj cognition (cluster 3)  0.0003355789 0.35364635    (0.1,0.5]        Cluster 3 (cognition)
## 36   traj       EEG (vigilance)  0.0010230632 0.35064935    (0.1,0.5]              Vigilance (EEG)
ggVI <- ggplot(df.ipranger[df.ipranger$time!="traj",], aes(x=term2, y = importance, group = time, color = time)) + geom_point(aes(size = 1/pvalue, shape = significance)) + geom_line()
ggVI <- ggVI + scale_size_continuous(breaks = 1/c(0.001,0.01,0.05,0.1,0.5,1), labels = c("(0,0.001]","(0.001,0.01]","(0.01,0.05]","(0.05,0.1]","(0.1,0.5]","(0.5,1]"), name = "significance")
ggVI <- ggVI + guides(size = "none", color = guide_legend(override.aes = list(size=1.5)), shape = guide_legend(override.aes = list(size=3)))
ggVI <- ggVI + labs(x = "",y = "variable importance", shape = "statistical\nsignificance")
ggVI <- ggVI + theme(text = element_text(size=20), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                     axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm"))

## * Export
if(FALSE){
    ggsave(ggforest.ass, filename = "./figures/gg-forestplot-OR.pdf", width = 12, height = 6)
    ggsave(ggforest.ass, filename = "./figures/gg-forestplot-OR.png", width = 12, height = 6)

    ggsave(ggforest.asstraj, filename = "./figures/gg-forestplot-OR-traj.pdf", width = 12, height = 6)
    ggsave(ggforest.asstraj, filename = "./figures/gg-forestplot-OR-traj.png", width = 12, height = 6)

    ggsave(ggVI, filename = "./figures/variableImportance.pdf", width = 12, height = 9)
    ggsave(ggVI, filename = "./figures/variableImportance.png", width = 12, height = 9)
}

##----------------------------------------------------------------------
### figure-forestplot.R ends here
