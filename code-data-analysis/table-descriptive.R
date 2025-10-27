### table-descriptive.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: okt 27 2025 (12:40) 
## Version: 
## Last-Updated: okt 27 2025 (17:07) 
##           By: Brice Ozenne
##     Update #: 9
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

library(LMMstar)

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
library(officer)

## * Load data
source(file.path(path.code,"0-data-management.R"))
dfWR.NP1$depression_history <- factor(dfWR.NP1$mdd_diagnosis_screening, levels = c("F32.1","F32.2","F33.1","F33.2"),
                                      labels = c("first episode","first episode","recurrent","recurrent"))

## * Table
summarize(age~1, data = dfWR.NP1, na.rm = TRUE)
##   observed missing     mean     sd      min       q1   median       q3      max
## 1       90       0 26.98067 8.2041 18.23682 22.11225 23.99316 28.43053 57.30595
summarize(sex+depression_history~1, data = dfWR.NP1, na.rm = TRUE, FUN = "sum")
##                            outcome observed missing      mean sd min q1 median q3 max sum
## 1                         sex:male       90       0 0.2777778 NA   0 NA     NA NA   1  25
## 2                       sex:female       90       0 0.7222222 NA   0 NA     NA NA   1  65
## 3 depression_history:first episode       90       0 0.4333333 NA   0 NA     NA NA   1  39
## 4     depression_history:recurrent       90       0 0.5666667 NA   0 NA     NA NA   1  51
summarize(lvpet+MR_OFCthick+EEG_vigilance+CAR_AUCi+HAMD17_w0+neuroticism+CATS_scoretotal~1, data = dfWR.NP1, na.rm = TRUE, digits = 3)
##           outcome observed missing     mean      sd       min      q1  median      q3      max
## 1           lvpet       88       2  -0.4302   0.124    -0.826  -0.488  -0.422  -0.351  -0.0977
## 2     MR_OFCthick       90       0   2.5758   0.106     2.318   2.510   2.566   2.639   2.8888
## 3   EEG_vigilance       86       4  -0.0174   0.516    -1.500   0.000   0.000   0.000   1.5000
## 4        CAR_AUCi       69      21 181.3469 333.076 -1070.300  79.100 221.870 381.075 768.9000
## 5       HAMD17_w0       90       0  22.8556   3.371    18.000  20.000  22.000  25.000  31.0000
## 6     neuroticism       64      26 120.3750  19.189    67.000 108.750 119.000 134.000 155.0000
## 7 CATS_scoretotal       78      12  30.1026  19.407     0.000  16.000  23.000  41.500  81.0000

summarize(cognitive_cluster+low_hsCRP~1, data = transform(dfWR.NP1,
                                                          cognitive_cluster = as.factor(cognitive_cluster),
                                                          low_hsCRP = as.factor(low_hsCRP)),
          FUN = "sum", na.rm = TRUE)
##               outcome observed missing      mean        sd min q1 median q3 max sum
## 1 cognitive_cluster:1       88       2 0.4090909 0.4944837   0  0      0  1   1  36
## 2 cognitive_cluster:2       88       2 0.3068182 0.4638161   0  0      0  1   1  27
## 3 cognitive_cluster:3       88       2 0.2840909 0.4535648   0  0      0  1   1  25
## 4         low_hsCRP:0       88       2 0.2159091 0.4138094   0  0      0  0   1  19
## 5         low_hsCRP:1       88       2 0.7840909 0.4138094   0  1      1  1   1  69

##----------------------------------------------------------------------
### table-descriptive.R ends here
