## * Header 
## path <- "p:/Cluster/BrainDrug-WP3/"
## setwd(path)
## source("BATCH_scenario.R")
## sbatch -a 1-1 -J 'scenario' --output=/dev/null --error=/dev/null R CMD BATCH --vanilla BATCH_scenario.R /dev/null 

rm(list = ls())
gc()

## * seed
iter_sim <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
n.iter_sim <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_COUNT"))
if(is.na(iter_sim)){iter_sim <- 1}
if(is.na(n.iter_sim)){n.iter_sim <- 10}
cat("iteration ",iter_sim," over ",n.iter_sim,"\n",sep="")

set.seed(1)
seqSeed <- sample(1:max(1e5,n.iter_sim),size=n.iter_sim,replace=FALSE)
iSeed <- seqSeed[iter_sim]
set.seed(iSeed)

cat("seed: ",iSeed,"\n")

## * prepare export
path <- "."
path.res <- file.path(path,"Results","scenario")
if(dir.exists(path.res)==FALSE){
    if(dir.exists(file.path(path,"Results"))==FALSE){
    dir.create(file.path(path,"Results"))
    }
    dir.create(path.res)
}
path.output <- file.path(path,"output","scenario")
if(dir.exists(path.output)==FALSE){
    if(dir.exists(file.path(path,"output"))==FALSE){
    dir.create(file.path(path,"output"))
    }
    dir.create(path.output)
}

## * libraries
library(data.table)
setDTthreads(1)
library(mvtnorm)
library(forestControl)
library(selectiveInference)
library(pROC)
library(ranger)
library(splines)
library(BuyseTest)
source("simData.R")
source("runAnalysis.R")

## * settings
n.sim <- 25
n.obs <- c(80,400)
n.GS <- 8000
n.foldCV <- c(80,40,10)
size.foldCV <- 0.1
## size.foldCV <- c(0.05,0.1,0.2)

param.linear <- data.frame(effect = c(1.5,0.5,0), sigma = 1)
n.param.linear <- NROW(param.linear)

param.abnormal <- data.frame(effect = c(1,0.5,0), sigma = c(1.4,1.1,1))
n.param.abnormal <- NROW(param.abnormal)

param.interaction <- data.frame(effect = c(1.5,0.75,0), sigma = c(1.5,1.5,1))
n.param.interaction <- NROW(param.interaction)

## * prepare
res.inference <- NULL
res.performance <- NULL

dataEx1.validation <- lapply(1:n.param.linear, function(iE){
    simData(n.GS, p.noise = 8, scenario = "linear", effect = param.linear[iE,"effect"], sigma = param.linear[iE,"sigma"], plot = 0)
})
dataEx2.validation <- lapply(1:n.param.abnormal, function(iE){
    simData(n.GS, p.noise = 8, scenario = "abnormal", effect = param.abnormal[iE,"effect"], sigma = param.abnormal[iE,"sigma"], plot = 0)
})
dataEx3.validation <- lapply(1:n.param.interaction, function(iE){
    simData(n.GS, p.noise = 8, scenario = "interaction", effect = param.interaction[iE,"effect"], sigma = param.interaction[iE,"sigma"], plot = 0)
})

## * run
cat("\n")
for(iN in 1:length(n.obs)){ ## iN <- 1

    for(iSim in 1:n.sim){## iSim <- 1
        cat(iSim)
    
        ## Scenario 1
        for(iE in 1:n.param.linear){ ## iE <- 1
            cat(".")
            iData <- simData(n.obs[iN], p.noise = 8, scenario = "linear", effect = param.linear[iE,"effect"], sigma = param.linear[iE,"sigma"], plot = 0)
            iRes <- try(runAnalysis(data = iData,  data.validation = dataEx1.validation[[iE]], fold.number = n.foldCV, fold.size = size.foldCV,
                                    formula.mean = Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10,
                                    RF_num.trees = c(100,500,1000), RF_mtry = c(3,5),
                                    model = c("null","logistic","RF","glmnet"), trace = FALSE))



            if(!inherits(iRes,"try-error")){
                res.inference <- rbind(res.inference,
                                       cbind(sim = iSim, n = n.obs[iN], scenario = "linear", effect = param.linear[iE,"effect"], sigma = param.linear[iE,"sigma"],
                                             iRes$inference))
                res.performance <- rbind(res.performance,
                                         cbind(sim = iSim, n = n.obs[iN], scenario = "linear", effect = param.linear[iE,"effect"], sigma = param.linear[iE,"sigma"],
                                               iRes$performance))
                ## cbind(sim = iSim, scenario = "linear", iRes$performance)
            }
        }

        ## Scenario 2
        for(iE in 1:n.param.abnormal){ ## iE <- 1
            cat("*")
            iData <- simData(n.obs[iN], p.noise = 8, scenario = "abnormal", effect = param.abnormal[iE,"effect"], sigma = param.abnormal[iE,"sigma"], plot = 0)
            iRes <- try(runAnalysis(data = iData,  data.validation = dataEx2.validation[[iE]], fold.number = n.foldCV, fold.size = size.foldCV,
                                    formula.mean = Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10,
                                    RF_num.trees = c(100,500,1000), RF_mtry = c(3,5),
                                    model = c("null","logistic","RF","glmnet"), trace = FALSE))
        
            if(!inherits(iRes,"try-error")){
                res.inference <- rbind(res.inference,
                                       cbind(sim = iSim, n = n.obs[iN], scenario = "abnormal", effect = param.abnormal[iE,"effect"], sigma = param.abnormal[iE,"sigma"],
                                             iRes$inference))
                res.performance <- rbind(res.performance,
                                         cbind(sim = iSim, n = n.obs[iN], scenario = "abnormal", effect = param.abnormal[iE,"effect"], sigma = param.abnormal[iE,"sigma"],
                                               iRes$performance))
            }
        }

        ## Scenario 3
        for(iE in 1:n.param.interaction){ ## iE <- 1
            cat("-")
            iData <- simData(n.obs[iN], p.noise = 8, scenario = "interaction", effect = param.interaction[iE,"effect"], sigma = param.interaction[iE,"sigma"], plot = 0)
            iRes <- try(runAnalysis(data = iData,  data.validation = dataEx3.validation[[iE]], fold.number = n.foldCV, size.foldCV,
                                    formula.mean = Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10,
                                    RF_num.trees = c(100,500,1000), RF_mtry = c(3,5),
                                    model = c("null","logistic","RF","glmnet"), trace = FALSE))
            ## iRes$performance
            ## iRes$fit$RF
            
            if(!inherits(iRes,"try-error")){
                res.inference <- rbind(res.inference,
                                       cbind(sim = iSim, n = n.obs[iN], scenario = "interaction", effect = param.interaction[iE,"effect"], sigma = param.interaction[iE,"sigma"],
                                             iRes$inference))
                res.performance <- rbind(res.performance,
                                         cbind(sim = iSim, n = n.obs[iN], scenario = "interaction", effect = param.interaction[iE,"effect"], sigma = param.interaction[iE,"sigma"],
                                               iRes$performance))
            }
        }
        cat("\n")

        ## export
        saveRDS(res.inference, file = file.path(path.res,paste0("simul_inference_",iter_sim,"(tempo).rds")))
        saveRDS(res.performance, file = file.path(path.res,paste0("simul_performance_",iter_sim,"(tempo).rds")))
    }
        
}
cat("\n")

## * export
saveRDS(res.inference, file = file.path(path.res,paste0("simul_inference_",iter_sim,".rds")))
saveRDS(res.performance, file = file.path(path.res,paste0("simul_performance_",iter_sim,".rds")))

## * R version
print(sessionInfo())

## * gather and process results
if(FALSE){
path <- "p:/Cluster/BrainDrug-WP3/"
setwd(path)

path.scenario <- file.path("Results","scenario")
allRes.tempo <- butils::sinkDirectory(path.scenario, string.keep = "tempo")
allRes.final <- butils::sinkDirectory(path.scenario, string.exclude = "tempo")
}

	
