### figure-forestplot.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: mar  2 2022 (15:46) 
## Version: 
## Last-Updated: mar 18 2022 (11:39) 
##           By: Brice Ozenne
##     Update #: 19
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
load(file.path(path.results,"test.Rdata"))

## * Generate plot

## ** Logistic
## complete case
gg.forest_cc <- forestplot(dtS.ass_cc) + labs(color = "Analysis at:", x = "log odd ratio from logistic model for recovery")
gg.forest_cc <- gg.forest_cc + ggtitle(paste0("Complete case analysis (n=",NROW(dfWR.NP1_ccw4S),"/",NROW(dfWR.NP1_ccw8S),"/",NROW(dfWR.NP1_ccw12S),")"))
gg.forest_cc

setkeyv(dtS.ass_cc, c("time","term"))
dtS.ass_cc[,.(time,term,estimate,lower,upper,p.value,lower.adj,upper.adj,adj.p.value)]
##        time                  term    estimate      lower       upper    p.value  lower.adj upper.adj adj.p.value
##  1: week 12                   age  0.81290854 -0.1991656  1.82498268 0.11542653         NA        NA          NA
##  2: week 12                female -0.39299838 -1.7150671  0.92907029 0.56014963         NA        NA          NA
##  3: week 12    MR (OFC thickness) -0.70720797 -1.3423103 -0.07210565 0.02907366 -1.5739076 0.1594917   0.1798999
##  4: week 12                HAMD17  0.23537353 -0.3632756  0.83402269 0.44093952 -0.5815799 1.0523269   0.9789573
##  5: week 12                 hsCRP  0.59040715 -0.8434181  2.02423243 0.41963444 -1.3662789 2.5470932   0.9729092
##  6: week 12       PET (serotonin)  0.16805457 -0.3985637  0.73467279 0.56103147 -0.6051875 0.9412966   0.9959440
##  7: week 12 cognition (cluster 2) -0.87932730 -2.2481214  0.48946682 0.20799391 -2.7472678 0.9886132   0.7846058
##  8: week 12 cognition (cluster 3) -0.43559532 -1.9617932  1.09060261 0.57589054 -2.5183387 1.6471481   0.9967966
##  9: week 12       EEG (vigilance)  0.09762557 -0.4673671  0.66261827 0.73486298 -0.6733982 0.8686493   0.9998751
## 10: week 12           Neuroticism          NA         NA          NA         NA         NA        NA          NA
## 11:  week 8                   age  0.46077421 -0.1897010  1.11124937 0.16502410         NA        NA          NA
## 12:  week 8                female -0.45764039 -1.6248000  0.70951925 0.44219197         NA        NA          NA
## 13:  week 8    MR (OFC thickness) -0.45455237 -1.0367947  0.12768999 0.12598465 -1.2492643 0.3401595   0.5898969
## 14:  week 8                HAMD17  0.28155670 -0.2863378  0.84945121 0.33118413 -0.4935716 1.0566850   0.9306340
## 15:  week 8                 hsCRP  0.82376823 -0.4235222  2.07105871 0.19550977 -0.8786785 2.5262149   0.7625632
## 16:  week 8       PET (serotonin) -0.15011091 -0.6570793  0.35685750 0.56168810 -0.8420802 0.5418584   0.9960788
## 17:  week 8 cognition (cluster 2) -0.50805665 -1.7275439  0.71143065 0.41418546 -2.1725544 1.1564411   0.9716108
## 18:  week 8 cognition (cluster 3) -1.39105193 -2.7185345 -0.06356935 0.03999301 -3.2029541 0.4208503   0.2394224
## 19:  week 8       EEG (vigilance) -0.65938997 -1.2341938 -0.08458618 0.02455161 -1.4439488 0.1251689   0.1543613
## 20:  week 8              Cortisol          NA         NA          NA         NA         NA        NA          NA
## 21:  week 4                   age  0.25381585 -0.2434057  0.75103744 0.31706887         NA        NA          NA
## 22:  week 4                female -0.71403058 -1.8059686  0.37790741 0.19996806         NA        NA          NA
## 23:  week 4    MR (OFC thickness) -0.06335028 -0.5680834  0.44138283 0.80568234 -0.7531695 0.6264689   0.9999877
## 24:  week 4                HAMD17 -0.07653123 -0.6026031  0.44954061 0.77554514 -0.7955141 0.6424516   0.9999663
## 25:  week 4                 hsCRP  0.55787451 -0.6970852  1.81283422 0.38360448 -1.1572800 2.2730290   0.9626538
## 26:  week 4       PET (serotonin)  0.08161689 -0.4034035  0.56663724 0.74154105 -0.5812609 0.7444946   0.9999097
## 27:  week 4 cognition (cluster 2) -0.18640813 -1.3034792  0.93066291 0.74361982 -1.7131101 1.3402938   0.9999146
## 28:  week 4 cognition (cluster 3) -0.86503090 -2.1100260  0.37996420 0.17326226 -2.5665668 0.8365050   0.7240508
## 29:  week 4       EEG (vigilance) -0.06211710 -0.5489636  0.42472937 0.80253070 -0.7274906 0.6032564   0.9999862
## 30:  week 4                  CATS          NA         NA          NA         NA         NA        NA          NA
##        time                  term    estimate      lower       upper    p.value  lower.adj upper.adj adj.p.value

## missing value
gg.forest_imp <- forestplot(dtS.ass_imp) + labs(color = "Analysis at:", x = "log odd ratio from logistic model for recovery")
gg.forest_imp <- gg.forest_imp + ggtitle(paste0("Multiple imputation (n=",NROW(dfWRimp.NP1_w4C$data),"/",NROW(dfWRimp.NP1_w8C$data),"/",NROW(dfWRimp.NP1_w12C$data),")"))
gg.forest_imp

setkeyv(dtS.ass_imp, c("time","term"))
dtS.ass_imp[,.(time,term,estimate,lower,upper,p.value,lower.adj,upper.adj,adj.p.value)]
##        time                  term    estimate        lower        upper    p.value  lower.adj  upper.adj adj.p.value
##  1: week 12                female -0.91743988 -2.296764726  0.461884971 0.18900657         NA         NA          NA
##  2: week 12                   age  0.73697667 -0.240428887  1.714382223 0.13715573         NA         NA          NA
##  3: week 12    MR (OFC thickness) -0.87021372 -1.531160705 -0.209266737 0.01059592 -1.8211883 0.08076082  0.09403706
##  4: week 12                HAMD17  0.22864772 -0.357101333  0.814396771 0.43895218 -0.6141215 1.07141690  0.99386231
##  5: week 12                 hsCRP  0.54170359 -0.981186574  2.064593761 0.48043219 -1.6490589 2.73246604  0.99698507
##  6: week 12       PET (serotonin)  0.14969225 -0.439387364  0.738771864 0.61394466 -0.6978764 0.99726089  0.99981907
##  7: week 12 cognition (cluster 2) -1.23228367 -2.779223684  0.314656348 0.11663155 -3.4576855 0.99311813  0.65755046
##  8: week 12 cognition (cluster 3) -1.23211761 -2.916963774  0.452728564 0.14918403 -3.6559188 1.19168357  0.75139652
##  9: week 12       EEG (vigilance) -0.05548535 -0.657447066  0.546476369 0.85470244 -0.9215959 0.81062525  0.99999999
## 10: week 12                  CATS  0.69398065  0.007789692  1.380171608 0.04752293 -0.2933312 1.68129249  0.34992677
## 11: week 12              Cortisol  0.46282495 -0.291467530  1.217117423 0.22519432 -0.6224248 1.54807472  0.88903756
## 12: week 12           Neuroticism -0.02338173 -0.818535021  0.771771566 0.95340882 -1.1674326 1.12066918  1.00000000
## 13:  week 8                female -0.57753491 -1.758910876  0.603841062 0.33310871         NA         NA          NA
## 14:  week 8                   age  0.45193136 -0.195499434  1.099362158 0.16838786         NA         NA          NA
## 15:  week 8    MR (OFC thickness) -0.48611743 -1.061776655  0.089541791 0.09664484 -1.3153682 0.34313335  0.59435171
## 16:  week 8                HAMD17  0.32097935 -0.217996206  0.859954908 0.23911230 -0.4554627 1.09742145  0.91059402
## 17:  week 8                 hsCRP  0.89399954 -0.401163526  2.189162600 0.17305117 -0.9710296 2.75902869  0.81271604
## 18:  week 8       PET (serotonin) -0.09616763 -0.614831706  0.422496441 0.71280103 -0.8433506 0.65101536  0.99999133
## 19:  week 8 cognition (cluster 2) -0.69912372 -1.958697754  0.560450309 0.27221893 -2.5133247 1.11507728  0.93996471
## 20:  week 8 cognition (cluster 3) -1.87992179 -3.297180996 -0.462662592 0.01004351 -3.9211639 0.16132034  0.09046179
## 21:  week 8       EEG (vigilance) -0.69847488 -1.284475193 -0.112474576 0.02015285 -1.5426301 0.14568037  0.17155060
## 22:  week 8                  CATS  0.07479048 -0.473661785  0.623242751 0.78655856 -0.7153047 0.86488565  0.99999954
## 23:  week 8              Cortisol  0.28152530 -0.309097697  0.872148292 0.34520600 -0.5691779 1.13222853  0.97701174
## 24:  week 8           Neuroticism  0.22612552 -0.440782096  0.893033139 0.50133395 -0.7346358 1.18688679  0.99819203
## 25:  week 4                female -1.15006496 -2.298542434 -0.001587493 0.04969227         NA         NA          NA
## 26:  week 4                   age  0.24249711 -0.280922871  0.765917100 0.35893909         NA         NA          NA
## 27:  week 4    MR (OFC thickness) -0.05876414 -0.571392786  0.453864509 0.81995462 -0.7977520 0.68022368  0.99999993
## 28:  week 4                HAMD17  0.08812129 -0.418131177  0.594373766 0.72970162 -0.6416895 0.81793208  0.99999584
## 29:  week 4                 hsCRP  0.65041183 -0.652231115  1.953054770 0.32292900 -1.2267548 2.52757843  0.97074518
## 30:  week 4       PET (serotonin)  0.08061616 -0.415786180  0.577018503 0.74715773 -0.6349798 0.79621208  0.99999785
## 31:  week 4 cognition (cluster 2) -0.33296344 -1.530070671  0.864143786 0.58099358 -2.0579792 1.39205228  0.99969478
## 32:  week 4 cognition (cluster 3) -1.30528058 -2.646607101  0.036045946 0.05631125 -3.2382481 0.62768690  0.41059256
## 33:  week 4       EEG (vigilance) -0.10138066 -0.600688092  0.397926770 0.68695947 -0.8211744 0.61841307  0.99998233
## 34:  week 4                  CATS -0.28135338 -0.839330756  0.276624005 0.31830848 -1.0857282 0.52302147  0.96887611
## 35:  week 4              Cortisol  0.28873615 -0.320148090  0.897620400 0.34775562 -0.5888975 1.16636980  0.97932856
## 36:  week 4           Neuroticism  0.20053395 -0.430035548  0.831103443 0.52825021 -0.7084860 1.10955388  0.99904150
##        time                  term    estimate        lower        upper    p.value  lower.adj  upper.adj adj.p.value
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
df.ipranger
##      time                 param    importance     pvalue significance
## 1   week4                female -0.0004630476 0.45354645    (0.1,0.5]
## 2   week4                   age  0.0111673221 0.16983017    (0.1,0.5]
## 3   week4    MR (OFC thickness)  0.0028598950 0.35864136    (0.1,0.5]
## 4   week4                HAMD17 -0.0036899090 0.60539461      (0.5,1]
## 5   week4                 hsCRP -0.0029902081 0.73926074      (0.5,1]
## 6   week4       PET (serotonin)  0.0075185611 0.24375624    (0.1,0.5]
## 7   week4 cognition (cluster 2) -0.0007050308 0.48451548    (0.1,0.5]
## 8   week4 cognition (cluster 3)  0.0092599240 0.06293706   (0.05,0.1]
## 9   week4       EEG (vigilance) -0.0018836544 0.54645355      (0.5,1]
## 10  week8                female -0.0046622586 0.85014985      (0.5,1]
## 11  week8                   age  0.0038266849 0.32567433    (0.1,0.5]
## 12  week8    MR (OFC thickness)  0.0084196899 0.23276723    (0.1,0.5]
## 13  week8                HAMD17  0.0025092369 0.36463536    (0.1,0.5]
## 14  week8                 hsCRP -0.0011115631 0.52547453      (0.5,1]
## 15  week8       PET (serotonin) -0.0073080258 0.69330669      (0.5,1]
## 16  week8 cognition (cluster 2)  0.0005137546 0.36963037    (0.1,0.5]
## 17  week8 cognition (cluster 3)  0.0058086586 0.12387612    (0.1,0.5]
## 18  week8       EEG (vigilance)  0.0034149398 0.25274725    (0.1,0.5]
## 19 week12                female -0.0024742853 0.67132867      (0.5,1]
## 20 week12                   age  0.0040111433 0.34065934    (0.1,0.5]
## 21 week12    MR (OFC thickness)  0.0133738696 0.13186813    (0.1,0.5]
## 22 week12                HAMD17  0.0004376103 0.45854146    (0.1,0.5]
## 23 week12                 hsCRP -0.0029191690 0.77022977      (0.5,1]
## 24 week12       PET (serotonin)  0.0243561290 0.02597403  (0.01,0.05]
## 25 week12 cognition (cluster 2)  0.0043251049 0.15184815    (0.1,0.5]
## 26 week12 cognition (cluster 3) -0.0025604673 0.66433566      (0.5,1]
## 27 week12       EEG (vigilance)  0.0017273760 0.28471528    (0.1,0.5]

ggVI <- ggplot(df.ipranger, aes(x=param, y = importance, group = time, color = time)) + geom_point(aes(size = 1/pvalue, shape = significance)) + geom_line()
ggVI <- ggVI + scale_size_continuous(breaks = 1/c(0.001,0.01,0.05,0.1,0.5,1), labels = c("(0,0.001]","(0.001,0.01]","(0.01,0.05]","(0.05,0.1]","(0.1,0.5]","(0.5,1]"), name = "significance")
ggVI <- ggVI + guides(size = "none", color = guide_legend(override.aes = list(size=1.5)), shape = guide_legend(override.aes = list(size=3)))
ggVI <- ggVI + labs(x = "",y = "variable importance", shape = "statistical\nsignificance")
ggVI <- ggVI + theme(text = element_text(size=20), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
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
