### table-transition.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: mar  2 2022 (12:26) 
## Version: 
## Last-Updated: mar 23 2022 (09:27) 
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

## * Censoring

table(dfWR.NP1$Y_w4, useNA = "always")
## FALSE  TRUE  <NA> 
##    52    37     1 
dfWR.NP1[is.na(dfWR.NP1$Y_w4),"NP1_comment"]
##                                                                                    NP1_comment
## 1: Exclusion at week 3. No contact between week 3 and 8. Not documented compliant to medicine.

table(dfWR.NP1$Y_w8, useNA = "always")
## FALSE  TRUE  <NA> 
##    40    48     2 
dfWR.NP1[is.na(dfWR.NP1$Y_w8),"NP1_comment"]
## 1: Drop-out week 8. Came back for follow-up at week 12.
## 2:                       Drop-out week 7. Lost contact.

table(dfWR.NP1$Y_w12, useNA = "always")
## FALSE  TRUE  <NA> 
##    26    60     4 
dfWR.NP1[is.na(dfWR.NP1$Y_w12),"NP1_comment"]
##                                                                                    NP1_comment
## 1:                                              Drop-out in week 8. No week 8 sample acquired.
## 2:                                                                  Drop-out, week 12 missing.
## 3:                                                              Drop-out week 7. Lost contact.
## 4: Exclusion at week 3. No contact between week 3 and 8. Not documented compliant to medicine.


## * Transition
df.trans <- data.frame(week4 = c("nr2nr" = sum(dfWR.NP1$Y_w4==0, na.rm = TRUE),
                                 "r2nr" = 0,
                                 "nr2r" = sum(dfWR.NP1$Y_w4==1, na.rm = TRUE),
                                 "r2r" = 0),
                       "4->8" = c("nr2nr" = sum((dfWR.NP1$Y_w4==0)*(dfWR.NP1$Y_w8==0), na.rm = TRUE),
                                  "r2nr" = sum((dfWR.NP1$Y_w4==1)*(dfWR.NP1$Y_w8==0), na.rm = TRUE),
                                  "nr2r" = sum((dfWR.NP1$Y_w4==0)*(dfWR.NP1$Y_w8==1), na.rm = TRUE),
                                  "r2r" = sum((dfWR.NP1$Y_w4==1)*(dfWR.NP1$Y_w8==1), na.rm = TRUE)),
                       week8 = c("nr2nr" = sum(dfWR.NP1$Y_w8==0, na.rm = TRUE),
                                 "r2nr" = 0,
                                 "nr2r" = sum(dfWR.NP1$Y_w8==1, na.rm = TRUE),
                                 "r2r" = 0),
                       "8->12" = c("nr2nr" = sum((dfWR.NP1$Y_w8==0)*(dfWR.NP1$Y_w12==0), na.rm = TRUE),
                                   "r2nr" = sum((dfWR.NP1$Y_w8==1)*(dfWR.NP1$Y_w12==0), na.rm = TRUE),
                                   "nr2r" = sum((dfWR.NP1$Y_w8==0)*(dfWR.NP1$Y_w12==1), na.rm = TRUE),
                                   "r2r" = sum((dfWR.NP1$Y_w8==1)*(dfWR.NP1$Y_w12==1), na.rm = TRUE)),
                       week12 = c("nr2nr" = sum(dfWR.NP1$Y_w12==0, na.rm = TRUE),
                                  "r2nr" = 0,
                                  "nr2r" = sum(dfWR.NP1$Y_w12==1, na.rm = TRUE),
                                  "r2r" = 0),
                       check.names = FALSE
                       )
df.trans <- rbind(df.trans, total = colSums(df.trans))
df.trans[,1] <- paste(df.trans[,1]," (",round(100*df.trans[,1]/df.trans["total",1],2),"%)", sep = "")
df.trans[,2] <- paste(df.trans[,2]," (",round(100*df.trans[,2]/df.trans["total",2],2),"%)", sep = "")
df.trans[,3] <- paste(df.trans[,3]," (",round(100*df.trans[,3]/df.trans["total",3],2),"%)", sep = "")
df.trans[,4] <- paste(df.trans[,4]," (",round(100*df.trans[,4]/df.trans["total",4],2),"%)", sep = "")
df.trans[,5] <- paste(df.trans[,5]," (",round(100*df.trans[,5]/df.trans["total",5],2),"%)", sep = "")
df.trans
##             week4        4->8       week8       8->12      week12
## nr2nr 52 (58.43%) 30 (34.48%) 40 (45.45%) 20 (23.53%) 26 (30.23%)
## r2nr       0 (0%)  9 (10.34%)      0 (0%)   6 (7.06%)      0 (0%)
## nr2r  37 (41.57%) 20 (22.99%) 48 (54.55%)    17 (20%) 60 (69.77%)
## r2r        0 (0%) 28 (32.18%)      0 (0%) 42 (49.41%)      0 (0%)
## total   89 (100%)   87 (100%)   88 (100%)   85 (100%)   86 (100%)

## * Export
if(FALSE){
    table.trans <- body_add_table(x = read_docx(), 
                                  value =  df.trans)
    print(table.trans, target = "./table/transition.docx")
}

## * all comments
table(dfWR.NP1[,"NP1_comment"])
## Drop-out after week 8. Psychotic depression and suicidal. 
## 1 
## Drop-out at week 7. Suicidal attempt, did not want more medicine. 
## 1 
## Drop-out in week 8. No week 8 sample acquired. 
## 1 
## Drop-out week 7. Lost contact. 
## 1 
## Drop-out week 8. Came back for follow-up at week 12. 
## 1 
## Drop-out, week 12 missing. 
## 1 
## Excluded at week 7, adverse side-effects to SNRI. 
## 1 
## Exclusion after week 1, suicidal and psychotic. Hospitalized. 
## 1 
## Exclusion at week 1, spontaneous remission (< 2 weeks between inclusion HAMD17 and baseline assessments). Can use baseline assessments were the patient flfilled diagnostic criteria for MDD. 
## 1 
## Exclusion at week 3. No contact between week 3 and 8. Not documented compliant to medicine. 
## 1 
## Femicept has been prescribed by doctor, but test subject says no to taking birth control pills 
## 1 
## Last menstrual period 1½ years ago 
## 1 
## Menstrual cycle irregular 
## 2 
## No medicine intake for 1 week around week 7 - week 8. Can be part of response-category but not use of serum-level of medicine at week 8. 
## 1 
## Non compliant to medicine baes on blood measures at week 8 
## 1 
## Non compliant to medicine intake based on escitalopram at week 8 
## 1 
## Non compliant to medicine, based on serum measures at week 8 
## 1 
## Non compliant to medicine. Was ordinated Duloxetin at week 4, but had no serum-levels at week 8 of Duloxetin, only S-Escitalopram. Not valid for response categorization. 
## 1 
## Tracer production failure for PET, missing PET. 
## 1 

##----------------------------------------------------------------------
### table-transition.R ends here
