### figure-latentClass.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: mar  2 2022 (12:30) 
## Version: 
## Last-Updated: mar  2 2022 (15:37) 
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
library(ggplot2)
source(file.path(path.code,"FCT_plotTraj.R"))

## * Load data
source(file.path(path.code,"analysis-latentClass.R"))

## ggTraj_hlme(ls.hlme[[1]], color = "prob", facet = TRUE, nrow = 1)
## ggTraj_hlme(ls.hlme[[2]], color = "prob", facet = TRUE, nrow = 1, order.class = c(2:1))
gg.traj <- ggTraj_hlme(ls.hlme[[3]], color = "prob", facet = TRUE, nrow = 1, order.class = c("stable" = 3,"partial recovery"=1,"recovery"=2))
## ggTraj_hlme(ls.hlme[[4]], color = "prob", facet = TRUE, nrow = 1, order.class = c(4,3,1,2))


if(FALSE){
    ggsave(gg.traj$plot + xlab("") + theme(text = element_text(size=20), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                                 axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm")),
           filename = "./figures/spaghetti-HAMD17.pdf")
    ggsave(gg.traj$plot + xlab("") + theme(text = element_text(size=20), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                                 axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm")),
           filename = "./figures/spaghetti-HAMD17.png")
}


##----------------------------------------------------------------------
### figure-latentClass.R ends here
