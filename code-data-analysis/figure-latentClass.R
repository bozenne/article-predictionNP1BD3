### figure-latentClass.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: mar  2 2022 (12:30) 
## Version: 
## Last-Updated: mar 21 2022 (15:15) 
##           By: Brice Ozenne
##     Update #: 7
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
##         G     loglik conv npm      AIC      BIC    SABIC   entropy
## [[      1 -1027.4100    1   6 2066.820 2081.819 2062.882 1.0000000
## ls.hlme 2  -990.2726    1  11 2002.545 2030.043 1995.326 0.8147255
## 1       3  -981.8465    1  16 1995.693 2035.690 1985.193 0.7259010
## [[      4  -978.0521    1  21 1998.104 2050.600 1984.323 0.7358319

## ggTraj_hlme(ls.hlme[[1]], color = "prob", facet = TRUE, nrow = 1)
## ggTraj_hlme(ls.hlme[[2]], color = "prob", facet = TRUE, nrow = 1, order.class = c(2:1))
gg.traj <- ggTraj_hlme(ls.hlme[[3]], color = "prob", facet = TRUE, nrow = 1, order.class = c("stable" = 3,"partial recovery"=1,"recovery"=2))
## ggTraj_hlme(ls.hlme[[4]], color = "prob", facet = TRUE, nrow = 1, order.class = c(4,3,1,2))


if(FALSE){
    ggsave(gg.traj$plot + xlab("") + theme(text = element_text(size=25), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                                 axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm")),
           filename = "./figures/spaghetti-HAMD17.pdf", width = 12)
    ggsave(gg.traj$plot + xlab("") + theme(text = element_text(size=25), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                                 axis.line = element_line(size = 1.25), axis.ticks = element_line(size = 2), axis.ticks.length=unit(.25, "cm")),
           filename = "./figures/spaghetti-HAMD17.png", width = 12)
}


##----------------------------------------------------------------------
### figure-latentClass.R ends here
