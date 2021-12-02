### 0-read-data.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: nov  1 2021 (13:10) 
## Version: 
## Last-Updated: dec  2 2021 (15:40) 
##           By: Brice Ozenne
##     Update #: 54
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

library(data.table)
library(pbapply)
library(lava)
if(system("whoami",intern=TRUE)=="unicph\\hpl802"){
    setwd("c:/Users/hpl802/Documents/Github/article-predictionNP1BD3/")
}else{ ## 
    setwd("Vibeke put you path here")
}
## load("data/DS9.Rdata")
## names(DS)
## DS$CPR <- NULL
## write.csv(DS, file.path("data/DS9.csv"))

## * Original data
## NOTE: CPR column has been deleted
source.NP1 <- as.data.table(read.csv("source/DS9_NP1_in.csv", sep = ";", na.strings = c("")))
## source.NP1 <- read.xlsx("source/DS9_NP1_in.xlsx", sheetIndex = 1)
## names(dfW.NP1)

## * Data cleaning
## done by emily's script
load("source/DS9_full.Rdata")
## DS$CPR <- NULL 
## save(DS, file = "source/DS9_full.Rdata")
dfW.NP1 <- as.data.table(DS)
dfW.NP1$CIMBI_ID <- as.numeric(as.character(dfW.NP1$CIMBI_ID))
setkeyv(dfW.NP1,"CIMBI_ID")

sourceRed.NP1 <- source.NP1[source.NP1$cimbi_id %in% dfW.NP1$CIMBI_ID, .SD[1], by = "cimbi_id"]
setkeyv(sourceRed.NP1,"cimbi_id")
## data includes the following outliers:
# CIMBI 55896 --> SRT score is 1253.38 (16.1 IQR above mean). True data so should not necessarily be excluded but check effect on estimate
# CIMBI 55003 --> SRT score is 778.59 (8.1 IQR above mean). True data so should not necessarily be excluded but check effect on estimate
## all(dfW.NP1$CIMBI_ID==sourceRed.NP1$cimbi_id)

## table(source.NP1[source.NP1$cimbi_id %in% dfW.NP1[which(dfW.NP1$SLC6A4_5HTTLPR_genotype==1),CIMBI_ID],"SLC6A4_5HTTLPR"][[1]])



findLevels <- function(data1, data2, col1, col2){
    if(missing(col2)){col2 <- col1}
    keep.id <- as.character(data1[, CIMBI_ID[1], by = col1][[2]])
    data1.red <- data1[data1$CIMBI_ID %in% keep.id, .SD, .SDcols = c("CIMBI_ID",col1)]
    data2.red <- data2[data2$cimbi_id %in% keep.id, .SD[1], by = "cimbi_id", .SDcols = col2]
    setkeyv(data1.red,"CIMBI_ID")
    setkeyv(data2.red,"cimbi_id")
    
    out <- list(data1.red,data2.red)
    names(out) <- c(as.character(match.call()$data1), as.character(match.call()$data2))
    return(out)
}

## * Build key variables
keep.col <- c("CIMBI_ID","sex","age")

## ** Sex (men=1; women=2)
## findLevels(data1 = dfW.NP1, data2 = source.NP1, col1 = "sex")
## $dfW.NP1
##    CIMBI_ID sex
## 1:    55144   1
## 2:    55896   2

## $source.NP1
##       sex cimbi_id
## 1:   Male    55144
## 2: Female    55896

dfW.NP1$sex <- factor(dfW.NP1$sex, levels = 1:2, labels = c("male", "female"))
## table(sourceRed.NP1$sex,dfW.NP1$sex)

## ** Use of contraceptive (0=no; 1=yes;2=; 2/NA = male)
## WARNING ISSUE HERE
## findLevels(data1 = dfW.NP1, data2 = source.NP1, col1 = "contraceptive_use", col2 = "contraceptive")
## dfW.NP1$contraceptive_use[is.na(dfW.NP1$contraceptive_use)] <- 2
## dfW.NP1$contraceptive_use <- factor(dfW.NP1$contraceptive_use,  levels = 0:2, labels = c('no', 'yes', 'male'))
## table(sourceRed.NP1$contraceptive,dfW.NP1$contraceptive_use, useNA = "always")

## ** 5-HTTLPR S-carriers (0=LL; 1=S-carrier)
## WARNING ISSUE HERE
## findLevels(data1 = dfW.NP1, data2 = source.NP1, col1 = "SLC6A4_5HTTLPR_genotype", col2 = "SLC6A4_5HTTLPR")
## dfW.NP1$SLC6A4_5HTTLPR_genotype <- NULL
## dfW.NP1$gene5HTTLPR.ll <- factor(dfW.NP1$SLC6A4_5HTTLPR_genotype, levels = 0:2, labels = c('ll', 'sl', 'ss'))

## table(sourceRed.NP1$SLC6A4_5HTTLPR, dfW.NP1$SLC6A4_5HTTLPR_genotype, useNA = "always")
##       0  1 <NA>
## ll    7 26    0
## sl   49  0    0
## ss   14  0    0
## <NA>  0  0    2


## ** HTTLPR LALA (0=non-LALA; 1=LALA)
## [TODO] info is missing 
## dfW.NP1$HTTLPR_LALA <- recode(dfW.NP1$SLC6A4.5HTTLPR.A.G..l.allele., 'A' = 0, 'G' = 0, .missing= 0, 'AA' = 1)

## ** Val/MET genotype (0=Val-Val; 1=Met-carrier)
## dfW.NP1$geneBDNF <- factor(dfW.NP1$BDNF_genotype, levels = 1:3, c('val/val', 'val/met', 'met/met'))
## dfW.NP1$BDNF_genotype <- NULL
## dfW.NP1$metCarrier <- factor(dfW.NP1$geneBDNF, levels = 0:2, c('valval', 'valx', 'valx'))

## ** Week 8 compliance (0=no; yes=1)
## dfW.NP1$compliance_wk8 <- tolower(dfW.NP1$compliance_wk8) 

## ** MR [TODO: ask Emily about IC and ICV]
dfW.NP1$MR_OFCthick <- (dfW.NP1$MR_lat.orbitofrontal_thickness_L+dfW.NP1$MR_lat.orbitofrontal_thickness_R+dfW.NP1$MR_med.orbitofrontal_thickness_L+dfW.NP1$MR_med.orbitofrontal_thickness_R)/4

## dfW.NP1$hippocampus_vol_av <- (dfW.NP1$MR_hippocampus_vol_L + dfW.NP1$MR_hippocampus_vol_R)/2
## dfW.NP1$hippocampus_vol_rel <- dfW.NP1$hippocampus_vol_av/dfW.NP1$MR_ICV
keep.col <- c(keep.col, "MR_OFCthick")

## ** blood
## hsCRP (high sensitivity CRP, i.e. inflammation)
## over 20 -> treated as NA
## between 3-20 -> high low grade
## <3 -> low low grade
dfW.NP1$hsCRP <- "" ## empty just to create column
dfW.NP1$hsCRP[dfW.NP1$lab_hsCRP == "<=0.30"] <- "low"
dfW.NP1$hsCRP[is.na(dfW.NP1$lab_hsCRP)] <- "excluded"
dfW.NP1$hsCRP[which(dfW.NP1$lab_hsCRP != "<=0.30")] <- as.character(cut(as.numeric(dfW.NP1$lab_hsCRP[which(dfW.NP1$lab_hsCRP != "<=0.30")]), breaks = c(0,3,20,Inf), labels = c("low","high","excluded"), right = FALSE))
dfW.NP1$hsCRP[dfW.NP1$hsCRP=="excluded"] <- NA

keep.col <- c(keep.col, "hsCRP")

## CAR (AUCi)
keep.col <- c(keep.col, "CAR_AUCi")

## ** pet
## dfW.NP1$age
dfW.NP1$female <- as.numeric(dfW.NP1$sex=="female")
## dfW.NP1$injected_mass
## dfW.NP1$MR_scanner

## source.NP1$SB_neocortex_BPnd_MGCS
dfW.NP1$neocortex.log <- log(dfW.NP1$SB_neocortex_BPnd_NPV)
dfW.NP1$hippocampus.log <- log(dfW.NP1$SB_hippocampus_BPnd_NPV)
dfW.NP1$caudate.log <- log(dfW.NP1$SB_caudate_BPnd_NPV)
dfW.NP1$putamen.log <- log(dfW.NP1$SB_putamen_BPnd_NPV)

## NOTE: no need to adjust on MR because all subjects (i.e. only cases) scanned in same scaner
## temporary remove gene effect
lvm.PET <- lvm(neocortex.log ~ eta + age + female + injected_mass,
               hippocampus.log ~ eta + age + female + injected_mass,
               caudate.log ~ eta + age + female + injected_mass,
               putamen.log ~ eta + age + female + injected_mass,
               eta  ~ 1
               )
latent(lvm.PET) <- ~eta

dfW.NP1cc <- dfW.NP1[!is.na(dfW.NP1$neocortex.log),.SD,.SDcols = c("CIMBI_ID",manifest(lvm.PET))]

elvm.PET <- pblapply(1:NROW(dfW.NP1cc), function(iObs){ ## iObs <- 2
    estimate(lvm.PET, data = dfW.NP1cc[-iObs,], control = list(constrain = TRUE))
})

dfW.NP1$lvpet <- as.numeric(NA)
for(iObs in 1:NROW(dfW.NP1cc)){ ## iObs <- 1
    iPred <- predict(elvm.PET[[iObs]], data = dfW.NP1cc[iObs,,drop=FALSE], x = manifest(elvm.PET[[iObs]]),  y = latent(elvm.PET[[iObs]]))
    dfW.NP1[dfW.NP1$CIMBI_ID==dfW.NP1cc[iObs,CIMBI_ID], lvpet := as.double(iPred)]
}

keep.col <- c(keep.col, "lvpet", "neocortex.log", "hippocampus.log", "caudate.log", "putamen.log")

## ** EEG
dfW.NP1$EEG_vigilance <- dfW.NP1$EEG_vigilance_slope_B1_bl
keep.col <- c(keep.col, "EEG_vigilance")

## ** Cognition
keep.col <- c(keep.col, "cognitive_cluster")

## ## Verbal Affective Memory Task 26 (VAMT-26) affective memory bias in percentage points
## dfW.NP1$VAMT_bias <- ((dfW.NP1$VAMT...Immediate.recall.of.positive.words..sum.A1.A5.+dfW.NP1$VAMT...Short.term.memory.of.positive.words..A6.+dfW.NP1$VAMT...Delayed.recall.of.positive.words..A7.)/70*100)-((dfW.NP1$VAMT...Immediate.recall.of.negative.words..sum.A1.A5.+dfW.NP1$VAMT...Short.term.memory.of.negative.words..A6.+dfW.NP1$VAMT...Delayed.recall.of.negative.words..A7.)/70*100)

## ## Verbal Affective Memory Task 26 (VAMT-26) total memory score (range 0-26)
## dfW.NP1$VAMT_total <- (dfW.NP1$VAMT...Immediate.recall.of.all.words..sum.A1.A5. + dfW.NP1$VAMT...Short.term.memory.of.all.words..A6.+dfW.NP1$VAMT...Delayed.recall.of.all.words..A7.)/7

## ## Emotion Recognition Task eyes (ERT) emotion recognition bias in percentage points 
## dfW.NP1$ERT_hit_bias <- (dfW.NP1$Emoticom.ER...Hit.rate.for.Eyes.Happy-dfW.NP1$Emoticom.ER...Hit.rate.for.Eyes.Sad)*100

## ## Emotion Recognition Task eyes (ERT) emotion misattribution bias in percentage points
## dfW.NP1$ERT_FA_bias <- (dfW.NP1$Emoticom.ER...False.alarm.rate.for.Eyes.Happy-dfW.NP1$Emoticom.ER...False.alarm.rate.for.Faces.Sad)*100

## ## Intensity Morphing (IM) emotion detection threshold bias for increase condition in percentage point
## dfW.NP1$IM_inc_bias <- ((dfW.NP1$Mean.intensity.threshold...Sad..ascending.-1)/14*100)-((dfW.NP1$Mean.intensity.threshold...Happy..ascending.-1)/14*100)

## ## Moral Emotion (ME) guilt ratings (range 1-7)
## dfW.NP1$ME_guilt <- (dfW.NP1$Emoticom.MJ...Mean.rating..1.7...Guilty..Agent.Intentional+dfW.NP1$Emoticom.MJ...Mean.rating..1.7...Guilty..Agent.Unintentional+NP1.tall.raw$Emoticom.MJ...Mean.rating..1.7...Guilty..Victim.Intentional+dfW.NP1$Emoticom.MJ...Mean.rating..1.7...Guilty..Victim.Unintentional)/4

## ## Moral Emotion (ME) guilt ratings (range 1-7)
## dfW.NP1$ME_shame <- (dfW.NP1$Emoticom.MJ...Mean.rating..1.7...Ashamed..Agent.Intentional+dfW.NP1$Emoticom.MJ...Mean.rating..1.7...Ashamed..Agent.Unintentional+NP1.tall.raw$Emoticom.MJ...Mean.rating..1.7...Ashamed..Victim.Intentional+dfW.NP1$Emoticom.MJ...Mean.rating..1.7...Ashamed..Victim.Unintentional)/4

## ## Social Information Preference (SIP) information type choice in percentage points
## dfW.NP1$SIP_information <- dfW.NP1$Emoticom.SIP...Relative.proportion.....of.faces.chosen + dfW.NP1$Emoticom.SIP...Relative.proportion.....of.thoughts.chosen - dfW.NP1$Emoticom.SIP...Relative.proportion.....of.facts.chosen

## ## Social Information Preference (SIP) interpretation bias in percentage points
## dfW.NP1$SIP_bias <- dfW.NP1$Emoticom.SIP...Relative.proportion.....of.positive.outcomes - dfW.NP1$Emoticom.SIP...Relative.proportion.....of.negative.outcomes

## ** Psychometric
dfW.NP1$HAMD17 <- dfW.NP1$HAMD17_total_bl
dfW.NP1$neuroticism <- dfW.NP1$NEO_neuroticism

dfW.NP1$PBI_care <- apply(cbind(dfW.NP1$PBI_care_dad,dfW.NP1$PBI_care_mom),1,mean,na.rm=TRUE)
dfW.NP1$PBI_care[is.na(dfW.NP1$PBI_care)] <- NA

## library(ggplot2)
## library(ggpubr)
## gg1 <- ggplot(dfW.NP1, aes(x=PBI_care)) + geom_histogram() + geom_rug() + ggtitle("PBI_care")
## gg2 <- ggplot(dfW.NP1, aes(x=CATS_scoretotal)) + geom_histogram() + geom_rug() + ggtitle("CATS_scoretotal")
## gg3 <- ggplot(dfW.NP1, aes(x=CATS_scoretotal,y=PBI_care)) + geom_point()
## ggarrange(gg1,gg2,gg3)
## "Let's go with CATS, it does not look too bad" (Vibeke Dam onsdag 21-12-01 at 15:01)
keep.col <- c(keep.col, "HAMD17","neuroticism", "CATS_scoretotal")

## ** fMRI from Patrick


## ** outcome


dfW.NP1$dHAMD6_w8 <- (dfW.NP1$HAMD6_total_wk8-dfW.NP1$HAMD6_total_bl)/dfW.NP1$HAMD6_total_bl
dfW.NP1$Y_w8 <- dfW.NP1$dHAMD6_w8 <= -0.5

dfW.NP1$dHAMD6_w12 <- (dfW.NP1$HAMD6_total_wk12-dfW.NP1$HAMD6_total_bl)/dfW.NP1$HAMD6_total_bl
dfW.NP1$Y_w12 <- dfW.NP1$dHAMD6_w12 <= -0.5

keep.col <- c(keep.col,"dHAMD6_w8","dHAMD6_w12","Y_w8","Y_w12")

## * Reduced dataset
keep.col %in% names(dfW.NP1)
dfWR.NP1 <- dfW.NP1[!is.na(Y_w8) | !is.na(Y_w12),.SD,.SDcols = c(keep.col,"female")]
dfWR.NP1$cognitive_cluster2 <- as.numeric(dfWR.NP1$cognitive_cluster=="2")
dfWR.NP1$cognitive_cluster3 <- as.numeric(dfWR.NP1$cognitive_cluster=="3")
dfWR.NP1$low_hsCRP <- as.numeric(dfWR.NP1$hsCRP=="low")
dfWR.NP1$Y_w8f <- as.factor(dfWR.NP1$Y_w8)

## table(is.na(dfW.NP1$Y_w8) + is.na(dfW.NP1$Y_w12) > 1) 
##----------------------------------------------------------------------
### 0-read-data.R ends here
