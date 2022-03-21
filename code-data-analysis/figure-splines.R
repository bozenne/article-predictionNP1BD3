### figure-splines.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: mar  9 2022 (12:19) 
## Version: 
## Last-Updated: mar 21 2022 (15:21) 
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
library(ggplot2)
library(ggpubr)
library(mgcv)

## * Load data
load(file.path(path.results,"test.Rdata"))

## * Generate plot
## ** week 8
df.gridw8 <- data.frame(lvpet = seq(min(dfWR.NP1_ccw8$lvpet), max(dfWR.NP1_ccw8$lvpet), length.out = 100),
                      age = round(mean(dfWR.NP1_ccw8$age)),
                      female = 1)
ppw8 <- predict(e.gam_ccw8, newdata = df.gridw8, type = "response", se.fit = TRUE)

df.gridw8$proba <- ppw8$fit
df.gridw8$probaLower <- pmax(0, ppw8$fit + qnorm(0.025) * ppw8$se.fit)
df.gridw8$probaUpper <- pmin(1, ppw8$fit + qnorm(0.975) * ppw8$se.fit)

ggSpline.w8 <- ggplot(df.gridw8, aes(x=lvpet,y=proba)) + geom_point() + geom_line() + ylab("Probability of recovery") + ggtitle(paste0(round(mean(dfWR.NP1_ccw8$age))," year-old female"))
ggSpline.w8 <- ggSpline.w8 + geom_ribbon(aes(ymin = probaLower, ymax = probaUpper), alpha = 0.05)
ggSpline.w8 <- ggSpline.w8 + geom_rug(data = cbind(dfWR.NP1_ccw8,proba=rnorm(NROW(dfWR.NP1_ccw8))), sides = "b") + coord_cartesian(ylim = c(0,1)) + xlab("Latent variable PET")
ggSpline.w8 <- ggSpline.w8 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm"))

ggCali.w8 <- ggplot(dfWR.NP1_ccw8, aes(x = exp(neocortex.log), y = lvpet)) + geom_point() + labs(y="Latent variable PET",x="Neocortex PET binding")
ggCali.w8 <- ggCali.w8 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm"))

## ** week 12
df.gridw12 <- data.frame(lvpet = seq(min(dfWR.NP1_ccw12$lvpet), max(dfWR.NP1_ccw12$lvpet), length.out = 100),
                      age = round(mean(dfWR.NP1_ccw12$age)),
                      female = 1)
ppw12 <- predict(e.gam_ccw12, newdata = df.gridw12, type = "response", se.fit = TRUE)

df.gridw12$proba <- ppw12$fit
df.gridw12$probaLower <- pmax(0, ppw12$fit + qnorm(0.025) * ppw12$se.fit)
df.gridw12$probaUpper <- pmin(1, ppw12$fit + qnorm(0.975) * ppw12$se.fit)

ggSpline.w12 <- ggplot(df.gridw12, aes(x=lvpet,y=proba)) + geom_point() + geom_line() + ylab("Probability of recovery") + ggtitle(paste0(round(mean(dfWR.NP1_ccw12$age))," year-old female"))
ggSpline.w12 <- ggSpline.w12 + geom_ribbon(aes(ymin = probaLower, ymax = probaUpper), alpha = 0.05)
ggSpline.w12 <- ggSpline.w12 + geom_rug(data = cbind(dfWR.NP1_ccw12,proba=rnorm(NROW(dfWR.NP1_ccw12))), sides = "b") + coord_cartesian(ylim = c(0,1)) + xlab("Latent variable PET")
ggSpline.w12 <- ggSpline.w12 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm"))

ggCali.w12 <- ggplot(dfWR.NP1_ccw12, aes(x = exp(neocortex.log), y = lvpet)) + geom_point() + labs(y="Latent variable PET",x="Neocortex PET binding")
ggCali.w12 <- ggCali.w12 + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm"))

## * export
if(FALSE){
    ggsave(ggpubr::ggarrange(ggSpline.w8,ggSpline.w12), filename = "./figures/gg-spline-w8.pdf", width = 12, height = 7)
    ggsave(ggpubr::ggarrange(ggSpline.w8,ggSpline.w12), filename = "./figures/gg-spline-w8.png", width = 12, height = 7)

    ggsave(ggpubr::ggarrange(ggSpline.w8,ggCali.w8), filename = "./figures/gg-spline-w8.pdf", width = 12, height = 7)
    ggsave(ggpubr::ggarrange(ggSpline.w8,ggCali.w8), filename = "./figures/gg-spline-w8.png", width = 12, height = 7)

    ggsave(ggpubr::ggarrange(ggSpline.w12,ggCali.w12), filename = "./figures/gg-spline-w12.pdf", width = 12, height = 7)
    ggsave(ggpubr::ggarrange(ggSpline.w12,ggCali.w12), filename = "./figures/gg-spline-w12.png", width = 12, height = 7)
}
##----------------------------------------------------------------------
### figure-splines.R ends here
