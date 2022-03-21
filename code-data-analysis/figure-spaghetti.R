### figure-spaghetti.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: mar  2 2022 (12:20) 
## Version: 
## Last-Updated: mar 21 2022 (16:35) 
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

## * Load data
source(file.path(path.code,"0-data-management.R"))

## add group
dfWR.NP1$Y_w4812 <- "Switch"
dfWR.NP1$Y_w4812[which(dfWR.NP1$Y_w4*dfWR.NP1$Y_w8*dfWR.NP1$Y_w12==1)] <- "Always responder"
dfWR.NP1$Y_w4812[which((dfWR.NP1$Y_w4==0)*(dfWR.NP1$Y_w8==0)*(dfWR.NP1$Y_w12==0)==1)] <- "Always non-responder"
dfWR.NP1$Y_w4812 <- factor(dfWR.NP1$Y_w4812, levels = c("Always responder","Switch","Always non-responder"))

dfLR.NP1 <- reshape(cbind(dfWR.NP1,Y_w0=0), idvar = c("CIMBI_ID"),
                    direction = "long",
                    varying = list(c("HAMD6_w0","HAMD6_w4","HAMD6_w8","HAMD6_w12"),
                                   c("HAMD17_w0","HAMD17_w4","HAMD17_w8","HAMD17_w12"),
                                   c("Y_w0","Y_w4","Y_w8","Y_w12")),
                    v.names = c("HAMD6","HAMD17","Y"),timevar = "visit")
dfLR.NP1$visit <- factor(dfLR.NP1$visit, levels = 1:4, labels = c("baseline","week 4","week 8","week 12"))
dfLR.NP1$HAMD6.jit <- dfLR.NP1$HAMD6 + runif(NROW(dfLR.NP1),-0.45,0.45)

## * Generate figure
gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
}
gg.spa <- ggplot(dfLR.NP1, aes(x=visit, y = HAMD6.jit, group = CIMBI_ID, color = Y_w4812)) + geom_point() + geom_line()
gg.spa <- gg.spa + facet_wrap(~Y_w4812) + guides(color = "none") + scale_color_manual(values = gg_color_hue(3)[c(2,3,1)])
gg.spa <- gg.spa + labs(x="",y = "HAMD-6")
gg.spa <- gg.spa + theme(text = element_text(size=20), axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## * Export
if(FALSE){
    ggsave(gg.spa, filename = "./figures/spaghetti-HAMD6.pdf", width = 14)
    ggsave(gg.spa, filename = "./figures/spaghetti-HAMD6.png", width = 14)
}


##----------------------------------------------------------------------
### figure-spaghetti.R ends here
