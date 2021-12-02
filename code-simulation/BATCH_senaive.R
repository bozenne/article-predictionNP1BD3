### BATCH_senaive.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: aug 16 2021 (18:01) 
## Version: 
## Last-Updated: aug 24 2021 (15:04) 
##           By: Brice Ozenne
##     Update #: 16
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

## * library
library(BuyseTest)
library(pbapply)
library(data.table)
source("simData.R")
source("runAnalysis.R")

## * run
mygrid <- expand.grid(n.obs = c(50,80,250,500,1000),
                    n.fold = c(10,50,100))

ff <- Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10

warper <- function(i, n.obs, formula, n.fold){
    data <- simData(n.obs, p.noise = 8, scenario = "linear", effect = 1, sigma = 1, plot = 0)
    ## data$Y <- as.factor(data$Y)
    e.glm <- do.call(glm, list(formula = formula, data = data, family = binomial(link = "logit")))
    e.perf <- performance(e.glm, data = data, fold.number = n.fold, name.response = "Y", trace = FALSE)
    e.auc <- as.data.frame(attr(e.perf,"auc")$logit1[attr(e.perf,"auc")$logit1$fold != "global",])
    e.brier <- as.data.frame(attr(e.perf,"brier")$logit1[attr(e.perf,"brier")$logit1$fold != "global",])
    out1 <- cbind(sim = i,
                  n.obs = n.obs,
                  n.fold = n.fold,
                  e.perf[e.perf$metric == "auc" & e.perf$method == "cv",],
                  estimate.naive = mean(e.auc$estimate),
                  se.naive = sd(e.auc$estimate)/sqrt(length(e.auc$estimate)-1))
    out2 <- cbind(sim = i,
                  n.obs = n.obs,
                  n.fold = n.fold,
                  e.perf[e.perf$metric == "brier" & e.perf$method == "cv",],
                  estimate.naive = mean(e.brier$estimate),
                  se.naive = sd(e.brier$estimate)/sqrt(length(e.brier$estimate)-1))
    return(rbind(out1,out2))
}

## warper(i=1, n.obs = 100, formula = ff, n.fold = 10)

n.sim <- 2500
n.cpus <- 25
ls.res <- pblapply(1:n.sim, function(i){
    res <- NULL
    for(g in 1:NROW(mygrid)){
        res <- rbind(res, warper(i=i, n.obs = mygrid$n.obs[g], formula = ff, n.fold = mygrid$n.fold[g]))
    }
    return(res)
}, cl = n.cpus)

saveRDS(ls.res, file = "Results/res-senaive2.rds")

## * Display
library(ggplot2)

ls.res <- readRDS(file = "Results/res-senaive2.rds")
dt.res <- as.data.table(do.call(rbind, ls.res))
## range(dt.res$estimate - dt.res$estimate.naive)

## ** histogram
gg.histAUC <- ggplot(dt.res[metric == "auc"], aes(estimate)) + geom_histogram() + facet_grid(n.fold ~ n.obs, labeller = label_both)
gg.histBrier <- ggplot(dt.res[metric == "brier"], aes(estimate)) + geom_histogram() + facet_grid(n.fold ~ n.obs, labeller = label_both)
ggsave(gg.histAUC, filename = "Results/gg-histCV-AUC.pdf")
ggsave(gg.histBrier, filename = "Results/gg-histCV-brier.pdf")

## ** se
dtS.res <- dt.res[, .(rep = .N, se.empir = sd(estimate,na.rm=TRUE), se = mean(se,na.rm=TRUE), se.naive = mean(se.naive,na.rm=TRUE)), by = c("n.obs","n.fold","metric")]
print(dtS.res)
dtSL.res <- melt(dtS.res, id.vars = c("n.obs","n.fold","rep","metric"))


dtSL.res[, fold := factor(paste0("10 fold CV with ",n.fold," replicates"),paste0("10 fold CV with ",unique(sort(n.fold))," replicates"))]
dtSL.res[, estimator := factor(variable, levels = c("se.empir","se","se.naive"), labels = c("empirical","modeled, accounting for correlation","modeled, naive"))]
gg.se <- ggplot(dtSL.res, aes(x = as.factor(n.obs), y = value, group = estimator, color = estimator)) + geom_point(size = 3) + geom_line(size = 1.25)
gg.se <- gg.se + facet_grid(metric~fold) + ylab("standard error") + xlab("sample size")
gg.se <- gg.se + theme(text = element_text(size=15),
                       legend.position="bottom",
                       legend.direction = "horizontal")
gg.se

ggsave(gg.se, filename = "Results/gg-seCV-AUCbrier.pdf")

## ** coverage
dt.res[, GS := mean(estimate, na.rm = TRUE), by = c("n.obs","n.fold","metric")]
dt.res[, c("lower.naive","upper.naive") := as.numeric(NA)]
dt.res[metric == "brier", c("lower.naive","upper.naive") := .(estimate*exp(qnorm(0.025)*se.naive/estimate),estimate*exp(qnorm(0.975)*se.naive/estimate))]
dt.res[metric == "auc", c("lower.naive","upper.naive") := .(estimate^exp(-qnorm(0.975)*se.naive/(estimate*log(estimate))),estimate^exp(-qnorm(0.025)*se.naive/(estimate*log(estimate))))]
## dt.res[2,.(estimate*exp(qnorm(0.025)*se/estimate),estimate*exp(qnorm(0.975)*se/estimate),lower,upper)]

dt.res[, coverage := (GS >= lower)*(GS <= upper)]
dt.res[, coverage.naive := (GS >= lower.naive)*(GS <= upper.naive)]
dtC.res <- dt.res[, .(rep = .N, coverage = mean(coverage, na.rm = TRUE), coverage.naive = mean(coverage.naive, na.rm = TRUE)), by = c("n.obs","n.fold","metric")]
dtC.res[, fold := factor(paste0("10 fold CV with ",n.fold," replicates"),paste0("10 fold CV with ",unique(sort(n.fold))," replicates"))]

gg.cov <- ggplot(dtC.res, aes(x = as.factor(n.obs)))
gg.cov <- gg.cov + geom_point(size = 3, aes(y=coverage, color = "accounting for correlation")) + geom_line(size = 1.25, aes(y=coverage, group = metric, color = "accounting for correlation"))
gg.cov <- gg.cov + geom_point(size = 3, aes(y=coverage.naive, color = "naive")) + geom_line(size = 1.25, aes(y=coverage.naive, group = metric, color = "naive"))
gg.cov <- gg.cov + facet_grid(metric~fold) + ylab("coverage") + xlab("sample size")
gg.cov <- gg.cov + geom_abline(slope = 0, intercept = 0.95)
gg.cov <- gg.cov + theme(text = element_text(size=15),
                 legend.position="bottom",
                 legend.direction = "horizontal")
gg.cov
ggsave(gg.cov, filename = "Results/gg-covCV-AUCbrier.pdf")


##----------------------------------------------------------------------
### BATCH_senaive.R ends here
