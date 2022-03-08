### analysis-latentClass.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: mar  2 2022 (12:31) 
## Version: 
## Last-Updated: mar  4 2022 (10:11) 
##           By: Brice Ozenne
##     Update #: 13
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

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
library(lcmm)
library(data.table)

## * Load data
source(file.path(path.code,"0-data-management.R"))

## reshape
dfLR.NP1 <- reshape(cbind(dfWR.NP1,Y_w0=0), idvar = c("CIMBI_ID"),
                    direction = "long",
                    varying = list(c("HAMD6_w0","HAMD6_w4","HAMD6_w8","HAMD6_w12"),
                                   c("HAMD17_w0","HAMD17_w4","HAMD17_w8","HAMD17_w12"),
                                   c("Y_w0","Y_w4","Y_w8","Y_w12")),
                    v.names = c("HAMD6","HAMD17","Y"),timevar = "visit")
dfLR.NP1$visit <- factor(dfLR.NP1$visit, levels = 1:4, labels = c("baseline","week 4","week 8","week 12"))
dfLR.NP1_NNA <- dfLR.NP1[!is.na(dfLR.NP1$HAMD17)] 

## * Latent classes
## ** Homogenous residual variance-covariance
ls.hlme <- list(hlme(HAMD17 ~ visit, subject = "CIMBI_ID", data = dfLR.NP1_NNA,
                     ng = 1, random =~ 1),
                hlme(HAMD17 ~ visit, subject = "CIMBI_ID", data = dfLR.NP1_NNA,
                     ng = 2, nwg = FALSE, mixture =~visit, random =~ 1),
                hlme(HAMD17 ~ visit, subject = "CIMBI_ID", data = dfLR.NP1_NNA,
                     ng = 3, nwg = FALSE, mixture =~visit, random =~ 1),
                hlme(HAMD17 ~ visit, subject = "CIMBI_ID", data = dfLR.NP1_NNA,
                     ng = 4, nwg = FALSE, mixture =~visit, random =~ 1))
     
compare.hlme <- summarytable(ls.hlme[[1]], bb = ls.hlme[[2]], ls.hlme[[3]], ls.hlme[[4]],
                             which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy"))
colnames(compare.hlme)[1] <- "nb. classes"
colnames(compare.hlme)[3] <- "cv"
colnames(compare.hlme)[4] <- "nb. parameters"
compare.hlme
##         nb. classes     loglik cv nb. parameters      AIC      BIC    SABIC   entropy
## [[                1 -1027.4100  1              6 2066.820 2081.819 2062.882 1.0000000
## ls.hlme           2  -990.2726  1             11 2002.545 2030.043 1995.326 0.8147255
## 1                 3  -981.8465  1             16 1995.693 2035.690 1985.193 0.7259010
## [[                4  -978.0521  1             21 1998.104 2050.600 1984.323 0.7358319


## ** Heterogenenous residual variance-covariance
if(FALSE){
    ls.hlme2 <- list(hlme(HAMD17 ~ visit, subject = "CIMBI_ID", data = dfLR.NP1_NNA,
                          ng = 1, random =~ 1),
                     hlme(HAMD17 ~ visit, subject = "CIMBI_ID", data = dfLR.NP1_NNA,
                          ng = 2, nwg = TRUE, mixture =~visit, random =~ 1),
                     hlme(HAMD17 ~ visit, subject = "CIMBI_ID", data = dfLR.NP1_NNA,
                          ng = 3, nwg = TRUE, mixture =~visit, random =~ 1),
                     hlme(HAMD17 ~ visit, subject = "CIMBI_ID", data = dfLR.NP1_NNA,
                          ng = 4, nwg = TRUE, mixture =~visit, random =~ 1))
     
    compare.hlme2 <- summarytable(ls.hlme2[[1]], bb = ls.hlme2[[2]], ls.hlme2[[3]], ls.hlme2[[4]],
                                  which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy"))
    colnames(compare.hlme2)[1] <- "nb. classes"
    colnames(compare.hlme2)[3] <- "cv"
    colnames(compare.hlme2)[4] <- "nb. parameters"
    compare.hlme2
    ##          nb. classes     loglik cv nb. parameters      AIC      BIC    SABIC   entropy
    ## [[                 1 -1027.4100  1              6 2066.820 2081.819 2062.882 1.0000000
    ## ls.hlme2           2  -990.0760  1             12 2004.152 2034.150 1996.277 0.7786856
    ## 1                  3  -981.3224  1             18 1998.645 2043.641 1986.832 0.7336236
    ## [[                 4  -979.4068  1             24 2006.814 2066.809 1991.063 0.7441485
}


##----------------------------------------------------------------------
### analysis-latentClass.R ends here
