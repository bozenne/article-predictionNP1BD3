### 0-read-data.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: nov  1 2021 (13:10) 
## Version: 
## Last-Updated: nov  1 2021 (14:14) 
##           By: Brice Ozenne
##     Update #: 12
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

library(data.table)
library(dplyr)
if(system("whoami",intern=TRUE)=="unicph\\hpl802"){
    setwd("c:/Users/hpl802/Documents/Projects/BD-WP3-prediction/")
}else{ ## 
    setwd("Vibeke put you path here")
}
## load("data/DS9.Rdata")
## names(DS)
## DS$CPR <- NULL
## write.csv(DS, file.path("data/DS9.csv"))

## ** Original data
## NOTE: CPR column has been deleted
source.NP1 <- read.csv("source/DS9 NP1.csv", sep = ";", na.strings = c(""))
names(source.NP1)
grep("5HTT", names(source.NP1), value = TRUE)
## [1] "SLC6A4_5HTTLPR"                "SLC6A4_5HTTLPR_A_G__s_allele_" "SLC6A4_5HTTLPR_A_G__l_allele_"

## ** Data cleaning
## done by emily's script
dfW.NP1 <- read.csv("data/DS9.csv")
dfW.NP1$CIMBI_ID

## data includes the following outliers:
# CIMBI 55896 --> SRT score is 1253.38 (16.1 IQR above mean). True data so should not necessarily be excluded but check effect on estimate
# CIMBI 55003 --> SRT score is 778.59 (8.1 IQR above mean). True data so should not necessarily be excluded but check effect on estimate

## ** Build key variables


## Sex (men=0; women=1)
dfW.NP1$sex <- factor(dfW.NP1$sex, levels = 0:1, labels = c("male", "female"))

## Use of contraceptive (0=no; 1=yes; N/A = 3)
## table(dfW.NP1$contraceptive_use,dfW.NP1$sex, useNA = "always")
dfW.NP1$contraceptive_use <- factor(dfW.NP1$contraceptive_use,  levels = 0:2, labels = c('no' = 0, 'yes' = 1, 'male' = 3))

# 5-HTTLPR S-carriers (0=LL; 1=S-carrier)
dfW.NP1$gene5HTTLPR <- factor(dfW.NP1$SLC6A4_5HTTLPR_genotype, levels = 0:2, labels = c('ll', 'sl', 'ss'))
dfW.NP1$SLC6A4_5HTTLPR_genotype <- NULL
dfW.NP1$gene5HTTLPR.ll <- factor(dfW.NP1$gene5HTTLPR, levels = 0:2, labels = c('ll', 'sx', 'sx'))

## [TODO] info is missing 
# HTTLPR LALA (0=non-LALA; 1=LALA)
## dfW.NP1$HTTLPR_LALA <- recode(dfW.NP1$SLC6A4.5HTTLPR.A.G..l.allele., 'A' = 0, 'G' = 0, .missing= 0, 'AA' = 1)


## Val/MET genotype (0=Val-Val; 1=Met-carrier)
dfW.NP1$geneBDNF <- factor(dfW.NP1$BDNF_genotype, levels = 0:2, c('val/val', 'val/met', 'met/met'))
dfW.NP1$BDNF_genotype <- NULL
dfW.NP1$metCarrier <- factor(dfW.NP1$geneBDNF, levels = 0:2, c('valval', 'valx', 'valx'))

## Week 8 compliance (0=no; yes=1)
dfW.NP1$compliance_wk8 <- tolower(dfW.NP1$compliance_wk8) 

## MR [TODO: ask Emily about IC and ICV]
dfW.NP1$hippocampus_vol_av <- (dfW.NP1$hippocampus_vol_L + dfW.NP1$hippocampus_vol_R)/2
dfW.NP1$hippocampus_vol_rel <- dfW.NP1$hippocampus_vol_av/dfW.NP1$ICV


grep("IC", names(dfW.NP1), value = TRUE)
## First episode vs recurrent (0=first-episode; 1=recurrent)
## dfW.NP1[, grep("mini", names(dfW.NP1), value = TRUE)] ## [TODO

## Shift to duloxetine (0=escitalopram; 1=duloxetine)

## Cortisol sample day (0=day off; 1=study day; 2=working day)
dfW.NP1$_dulox


# Verbal Affective Memory Task 26 (VAMT-26) affective memory bias in percentage points
dfW.NP1$VAMT_bias <- ((dfW.NP1$VAMT...Immediate.recall.of.positive.words..sum.A1.A5.+dfW.NP1$VAMT...Short.term.memory.of.positive.words..A6.+dfW.NP1$VAMT...Delayed.recall.of.positive.words..A7.)/70*100)-((dfW.NP1$VAMT...Immediate.recall.of.negative.words..sum.A1.A5.+dfW.NP1$VAMT...Short.term.memory.of.negative.words..A6.+dfW.NP1$VAMT...Delayed.recall.of.negative.words..A7.)/70*100)

# Verbal Affective Memory Task 26 (VAMT-26) total memory score (range 0-26)
dfW.NP1$VAMT_total <- (dfW.NP1$VAMT...Immediate.recall.of.all.words..sum.A1.A5. + dfW.NP1$VAMT...Short.term.memory.of.all.words..A6.+dfW.NP1$VAMT...Delayed.recall.of.all.words..A7.)/7

# Emotion Recognition Task eyes (ERT) emotion recognition bias in percentage points 
dfW.NP1$ERT_hit_bias <- (dfW.NP1$Emoticom.ER...Hit.rate.for.Eyes.Happy-dfW.NP1$Emoticom.ER...Hit.rate.for.Eyes.Sad)*100

# Emotion Recognition Task eyes (ERT) emotion misattribution bias in percentage points
dfW.NP1$ERT_FA_bias <- (dfW.NP1$Emoticom.ER...False.alarm.rate.for.Eyes.Happy-dfW.NP1$Emoticom.ER...False.alarm.rate.for.Faces.Sad)*100

# Intensity Morphing (IM) emotion detection threshold bias for increase condition in percentage point
dfW.NP1$IM_inc_bias <- ((dfW.NP1$Mean.intensity.threshold...Sad..ascending.-1)/14*100)-((dfW.NP1$Mean.intensity.threshold...Happy..ascending.-1)/14*100)

# Moral Emotion (ME) guilt ratings (range 1-7)
dfW.NP1$ME_guilt <- (dfW.NP1$Emoticom.MJ...Mean.rating..1.7...Guilty..Agent.Intentional+dfW.NP1$Emoticom.MJ...Mean.rating..1.7...Guilty..Agent.Unintentional+NP1.tall.raw$Emoticom.MJ...Mean.rating..1.7...Guilty..Victim.Intentional+dfW.NP1$Emoticom.MJ...Mean.rating..1.7...Guilty..Victim.Unintentional)/4

# Moral Emotion (ME) guilt ratings (range 1-7)
dfW.NP1$ME_shame <- (dfW.NP1$Emoticom.MJ...Mean.rating..1.7...Ashamed..Agent.Intentional+dfW.NP1$Emoticom.MJ...Mean.rating..1.7...Ashamed..Agent.Unintentional+NP1.tall.raw$Emoticom.MJ...Mean.rating..1.7...Ashamed..Victim.Intentional+dfW.NP1$Emoticom.MJ...Mean.rating..1.7...Ashamed..Victim.Unintentional)/4

# Social Information Preference (SIP) information type choice in percentage points
dfW.NP1$SIP_information <- dfW.NP1$Emoticom.SIP...Relative.proportion.....of.faces.chosen + dfW.NP1$Emoticom.SIP...Relative.proportion.....of.thoughts.chosen - dfW.NP1$Emoticom.SIP...Relative.proportion.....of.facts.chosen

# Social Information Preference (SIP) interpretation bias in percentage points
dfW.NP1$SIP_bias <- dfW.NP1$Emoticom.SIP...Relative.proportion.....of.positive.outcomes - dfW.NP1$Emoticom.SIP...Relative.proportion.....of.negative.outcomes




mean(dfW.NP1$SIP_bias, na.rm = TRUE)
dfW.NP1$IM_inc_bias



## ** add cluster analysis outcome
## ** add fMRI from Patrick

##----------------------------------------------------------------------
### 0-read-data.R ends here
