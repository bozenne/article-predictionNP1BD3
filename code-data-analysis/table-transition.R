### table-transition.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: mar  2 2022 (12:26) 
## Version: 
## Last-Updated: mar  2 2022 (12:32) 
##           By: Brice Ozenne
##     Update #: 5
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
library(officer)

## * Load data
source(file.path(path.code,"0-data-management.R"))

## * Transition
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
df.trans
##             week4       week8      week12
## nr2nr 52 (58.43%) 30 (34.48%) 20 (23.53%)
## r2nr       0 (0%)  9 (10.34%)   6 (7.06%)
## nr2r  37 (41.57%) 20 (22.99%)    17 (20%)
## r2r        0 (0%) 28 (32.18%) 42 (49.41%)
## total   89 (100%)   87 (100%)   85 (100%)

## * Export
table.trans <- body_add_table(x = read_docx(), 
                              value =  df.trans)
print(table.trans, target = "./table/transition.docx")

##----------------------------------------------------------------------
### table-transition.R ends here
