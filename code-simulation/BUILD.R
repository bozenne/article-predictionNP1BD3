### BUILD.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: aug  3 2021 (09:28) 
## Version: 
## Last-Updated: aug 26 2021 (13:33) 
##           By: Brice Ozenne
##     Update #: 23
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

library(data.table)
library(ggplot2)
library(ggthemes)
library(butils)
path <- "P:/Cluster/BrainDrug-WP3/"
setwd(path)

gg_fill_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 30, c = 100)[1:n]
}

path.results <- file.path(path,"Results")
path.scenario <- file.path(path.results,"scenario")

## * Example 1
cat("\n Example 1 \n")
## ** load
dtI.scenario <- butils::sinkDirectory(path.scenario, string.keep = "inference_([0-9]+)\\(tempo\\)")
dtP.scenario <- butils::sinkDirectory(path.scenario, string.keep = "performance_([0-9]+)\\(tempo\\)")
## table(dtP.scenario$effect.factor,dtP.scenario$effect,dtP.scenario$scenario)
## grep("inference_([0-9]+)\\(tempo\\)",c("simul_inference_1(tempo).rds","simul_performance_1(tempo).rds",
##                                        "simul_inference_1.rds","simul_performance_1.rds",
##                                        "simul_inference_10(tempo).rds","simul_performance_10(tempo).rds"), fixed = FALSE)

## ** process
## unique(dtP.scenario[,.N,by = "run"][["N"]])
dtP.scenario[, effect.factor := factor(effect, levels = sort(unique(effect)), c("no","moderate","high")), by = "scenario"]
dtP.scenario[, model.factor := factor(model, levels = c("null","logit","RF"), c("null","logistic regression","random forest")), by = "scenario"]
dtP.scenario[, scenario.factor := factor(scenario, levels = c("linear","interaction","abnormal"), paste0("scenario: ",c("linear","interaction","abnormal"))), by = "scenario"]
dtP.scenario[, method.factor := factor(method, levels = c("internal","cv","external"), c("internal assessment","cross-validation","external assessment")), by = "scenario"]

dtP.scenario[, run := .GRP, by = c("sim","n","effect","scenario","sigma","model","metric","iFile")]
dtP.scenario[, diff := estimate - .SD[method == "external"][["estimate"]], by = "run"]
## dtP.scenario[method=="external",which(diff!=0)]
## saveRDS(dtP.scenario, file = file.path(path.results,"res-sim.rds"))

## ** rejection rate
dtIS.scenario <- dtI.scenario[,.(rep = .N, repGLMNET = sum(!is.na(X1.X2)*(model=="glmnet")),
                                 powerX1 = mean(X1<0.05, na.rm=TRUE), powerX2 = mean(X2<0.05, na.rm=TRUE), powerX12 = mean((X1<0.05)*(X2<0.05), na.rm=TRUE),
                                 type1 = mean(X3<0.05, na.rm=TRUE),
                                 powerInteraction = mean(X1.X2<0.05, na.rm=TRUE), type1Interaction = mean(Xm1.Xm2<0.05, na.rm=TRUE)),
                              by = c("effect","model","scenario")]

dtIS.scenario$effect.char <- factor(dtIS.scenario$effect, levels = c(0,0.5,0.75,1,1.5), c("null","weak","weak","strong","strong"))
dtIS.scenario$scenario.f <- factor(dtIS.scenario$scenario, levels = c("linear","interaction","abnormal"), c("scenario: linear","scenario: interaction","scenario: abnormal"))

ggPower <- ggplot(dtIS.scenario, aes(x=effect.char, y = powerX1, group = model, color = model))
ggPower <- ggPower + facet_wrap(~scenario.f) + geom_abline(slope=0, intercept = 0.05, size = 1, color = "black") + geom_abline(slope=0, intercept = 0.1, size = 1, color = "gray") 
ggPower <- ggPower + geom_point(size = 3) + geom_line(size = 1.25)
ggPower <- ggPower + ylab("rejection rate (X1)") + xlab("effect") + theme(text = element_text(size=15),
                                                                     legend.position="bottom",
                                                                     legend.direction = "horizontal")
ggPower <- ggPower + scale_y_continuous(breaks = round(sort(c(seq(0,1,length.out = 5),0.05,0.1)),2))
ggPower
## ggsave(ggPower, filename = file.path(path.results,"figure-rejectionRate-n80.pdf"))

ggPower2 <- ggplot(dtIS.scenario, aes(x=effect.char, y = powerX12, group = model, color = model))
ggPower2 <- ggPower2 + facet_wrap(~scenario.f) + geom_abline(slope=0, intercept = 0.05, size = 1, color = "black")
ggPower2 <- ggPower2 + geom_point(size = 3) + geom_line(size = 1.25)
ggPower2 <- ggPower2 + ylab("rejection rate (X1 and X2)") + xlab("effect") + theme(text = element_text(size=15),
                                                                     legend.position="bottom",
                                                                     legend.direction = "horizontal")
ggPower2 <- ggPower2 + scale_y_continuous(breaks = round(sort(c(seq(0,1,length.out = 5),0.05)),2))
ggPower2
## ggsave(ggPower2, filename = file.path(path.results,"figure-rejectionRate2-n80.pdf"))


ggGLMNET <- ggplot(dtIS.scenario[model=="glmnet", .(scenario.f, effect.char, occurence = repGLMNET/rep)], aes(x = effect.char, y = occurence, group=scenario.f))
ggGLMNET <- ggGLMNET + geom_point() + geom_line()
ggGLMNET <- ggGLMNET + facet_wrap(~scenario.f)
ggGLMNET <- ggGLMNET + xlab("effect") + ylab("AUC(RF)>AUC(logit) & important variable(s) in RF")
ggGLMNET
## ggsave(ggGLMNET, filename = file.path(path.results,"figure-occurenceGLMNET-n80.pdf"))

## ** performance

ggAUC.estimate <- ggplot(dtP.scenario[metric == "auc" & model != "null" & n == 80],
                         aes(x = effect.factor, y = estimate, fill = model.fullname, color = model.fullname))
ggAUC.estimate <- ggAUC.estimate + facet_grid(method.factor ~ scenario.factor)
ggAUC.estimate <- ggAUC.estimate + geom_boxplot() + geom_abline(slope = 0, intercept = 0.5, color = "red")
ggAUC.estimate <- ggAUC.estimate + theme(legend.position="bottom",
                                         legend.direction = "horizontal",
                                         text = element_text(size=15), 
                                         axis.line = element_line(size = 1.25),
                                         axis.ticks = element_line(size = 2),
                                         axis.ticks.length=unit(.25, "cm"))
ggAUC.estimate <- ggAUC.estimate + ylab("AUC") + xlab("Effect of X1 and X2")
ggAUC.estimate <- ggAUC.estimate + scale_color_manual("Predictive model", values = gg_color_hue(8)) + scale_fill_manual("Predictive model", values = gg_fill_hue(8))
ggAUC.estimate

## ggsave(ggAUC.estimate, filename = file.path(path.results,"figure-auc-n80.pdf"))

ggBrier.estimate <- ggplot(dtP.scenario[metric == "brier" & n == 80],
                           aes(x = effect.factor, y = estimate, fill = model.fullname, color = model.fullname))
ggBrier.estimate <- ggBrier.estimate + facet_grid(method.factor ~ scenario.factor)
ggBrier.estimate <- ggBrier.estimate + geom_boxplot()
ggBrier.estimate <- ggBrier.estimate + theme(legend.position="bottom",
                                         legend.direction = "horizontal",
                                         text = element_text(size=15), 
                                         axis.line = element_line(size = 1.25),
                                         axis.ticks = element_line(size = 2),
                                         axis.ticks.length=unit(.25, "cm"))
ggBrier.estimate <- ggBrier.estimate + ylab("Brier") + xlab("Effect of X1 and X2")
ggBrier.estimate <- ggBrier.estimate + scale_color_manual("Predictive model", values = gg_color_hue(8)) + scale_fill_manual("Predictive model", values = gg_fill_hue(8))
ggBrier.estimate
## ggsave(ggBrier.estimate, filename = file.path(path.results,"figure-brier-n80.pdf"))

## ** Optimism
ggAUC.optimism <- ggplot(dtP.scenario[metric == "auc" & method != "external" & model != "null" & n == 80],
                         aes(x = effect.factor, y = diff, fill = model.fullname, color = model.fullname))
ggAUC.optimism <- ggAUC.optimism + facet_grid(method.factor ~ scenario.factor)
ggAUC.optimism <- ggAUC.optimism + geom_boxplot() + geom_abline(slope = 0, intercept = 0, color = "red")
ggAUC.optimism <- ggAUC.optimism + theme(legend.position="bottom",
                                         legend.direction = "horizontal",
                                         text = element_text(size=15), 
                                         axis.line = element_line(size = 1.25),
                                         axis.ticks = element_line(size = 2),
                                         axis.ticks.length=unit(.25, "cm"))
ggAUC.optimism <- ggAUC.optimism + ylab("AUC vs. AUC(external)") + xlab("Effect of X1 and X2")
ggAUC.optimism <- ggAUC.optimism + scale_color_manual("Predictive model", values = gg_color_hue(8)) + scale_fill_manual("Predictive model", values = gg_fill_hue(8))
ggAUC.optimism
## ggsave(ggAUC.optimism, filename = file.path(path.results,"figure-aucOptimism-n80.pdf"))

ggBrier.optimism <- ggplot(dtP.scenario[metric == "brier" & method != "external" & n == 80], aes(x = effect.factor, y = diff, fill = model.factor, color = model.factor)) + facet_grid(method.factor ~ scenario.factor)
ggBrier.optimism <- ggBrier.optimism + geom_boxplot()
ggBrier.optimism <- ggBrier.optimism + scale_fill_manual("Predictive model", values = ggthemes::colorblind_pal()(8)[-1])
ggBrier.optimism <- ggBrier.optimism + theme(legend.position="bottom",
                                         legend.direction = "horizontal",
                                         text = element_text(size=15), 
                                         axis.line = element_line(size = 1.25),
                                         axis.ticks = element_line(size = 2),
                                         axis.ticks.length=unit(.25, "cm"))
ggBrier.optimism <- ggBrier.optimism + ylab("Brier vs. Brier(external)") + xlab("Effect of X1 and X2") + geom_abline(slope = 0, intercept = 0, color = "red")
ggBrier.optimism <- ggBrier.optimism + scale_color_manual(values = gg_color_hue(3)) + scale_fill_manual(values = gg_fill_hue(3))
ggBrier.optimism
## ggsave(ggBrier.optimism, filename = file.path(path.results,"figure-brierOptimism-n80.pdf"))
##----------------------------------------------------------------------
### BUILD.R ends here
