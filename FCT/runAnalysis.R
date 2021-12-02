### runAnalysis.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: Jun 28 2021 (17:27) 
## Version: 
## Last-Updated: aug 23 2021 (19:06) 
##           By: Brice Ozenne
##     Update #: 216
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

## * runAnalysis (documentation)

## * runAnalysis (examples)
##' set.seed(10)
##' 
##' ## validation dataset
##' dv <- simData(1000, p.noise = 8, scenario = "linear", effect = 1, plot = 2, plot.nbins = 20)
##'
##' ## observed dataset
##' df <- simData(100, p.noise = 8, scenario = "linear", effect = 1, plot = 2, plot.nbins = 20)
##'
##' runAnalysis(data = df, data.validation = dv,
##'             formula.mean = Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10,
##'             model = c("logistic","RF"))

## * runAnalysis (code)
runAnalysis <- function(data,
                        formula.mean,
                        model, data.validation = NULL, threshold = NULL, fold.size = round(NROW(data)/10), fold.number = 0,
                        RF_num.trees = 500, RF_mtry = 3, RF_min.node.size = NULL, RF_max.depth = NULL, RF_importance = "impurity_corrected",
                        trace = TRUE){

    require(forestControl)
    require(selectiveInference)
    require(pROC)
    require(BuyseTest)
    require(ranger)
    require(splines)
    require(data.table)
    
    ## ** normalize user input
    if(any(all.vars(formula.mean) %in% names(data) == FALSE)){
        stop("Incorrect \'formula.mean\' argument. Contain variables not in argument \'data\'.\n")
    }
    if(!is.null(data.validation) && any(all.vars(formula.mean) %in% names(data.validation) == FALSE)){
        stop("Incorrect \'data.validation\' argument. Should contain variables in argument \'formula.mean\'.\n")
    }
    model <-  match.arg(model, choices = c("null","logistic","RF", "glmnet", "superLearner"), several.ok = TRUE)
    n.model <- length(model)
    model.inference <- setdiff(model,"null")
    n.model.inference <- length(model.inference)
    
    RF_importance <-  match.arg(RF_importance, choices = c("permutation","impurity_corrected","fpr"), several.ok = TRUE)
    RF_importance2 <- switch(RF_importance,
                             "permutation" = "altmann",
                             "impurity_corrected" = "janitza",
                             "fpr" = "fpr")
    RF_importance <- switch(RF_importance,
                            "permutation" = "permutation",
                            "impurity_corrected" = "impurity_corrected",
                            "fpr" = "none")
    name.outcome <- all.vars(formula.mean)[1]
    if(is.null(threshold)){
        threshold  <- 0.5
    }
    test.CV <- (fold.number>0)
    
    ## ** prepare output
    performance.col <- c("brier","brier.se","brier.lower","brier.upper","auc","auc.se","auc.lower","auc.upper","threshold","se","sp")
    out <- list(formula = formula.mean,
                fit = NULL,
                inference = data.table(matrix(as.numeric(NA), nrow = n.model.inference, ncol = 13, dimnames = list(NULL,c("model",paste0("X",1:10),"X1.X2","Xm1.Xm2")))),
                performance = NULL)
    out$inference$model <- model.inference
    
    ## *** fit models
    if("null" %in% model){
        out$fit$null <- do.call(stats::glm, args = list(formula = as.formula(paste0(name.outcome,"~1")), data = data, family = stats::binomial(link = "logit")))
    }
    if("logistic" %in% model){
        out$fit$logit <- do.call(stats::glm, args = list(formula = formula.mean, data = data, family = stats::binomial(link = "logit")))
    }
    if("RF" %in% model){
        grid.hyperparam <- expand.grid(num.trees = RF_num.trees,
                                       mtry = RF_mtry)
        name.RF <- paste0("RF-",grid.hyperparam$num.trees,"trees","-",grid.hyperparam$mtry,"mtry")
        out$fit <- c(out$fit,
                     setNames(lapply(1:NROW(grid.hyperparam), function(iGrid){
                         do.call(ranger::ranger, args = list(formula.mean,
                                                             data = data,
                                                             num.trees = grid.hyperparam[iGrid,"num.trees"],
                                                             mtry = grid.hyperparam[iGrid,"mtry"],
                                                             keep.inbag = TRUE, 
                                                             min.node.size = RF_min.node.size,
                                                             max.depth = RF_max.depth))
                     }), name.RF))
    }else{
        name.RF <- NULL
    }

    ## *** predictive performance
    out$performance <- performance(out$fit, data = data, newdata = data.validation, fold.size = fold.size[1], fold.number = fold.number,
                                   name.response = name.outcome, trace = trace, simplify = FALSE)
    if(length(fold.size)>1 && any(fold.number>0)){
        simpleFit <- out$fit[names(out$fit) %in% c("null","logit",name.RF[1])]
        for(iSize in fold.size[-1]){
            iPerformance <- performance(simpleFit, data = data, fold.size = iSize, fold.number = fold.number,
                                        name.response = name.outcome, trace = trace)
            out$performance <- rbind(out$performance,iPerformance[iPerformance$method!="internal",])
        }
    }
    out$performance$method.fullname <- paste0(out$performance$method,
                                              ifelse(is.na(out$performance$foldCV.number),"",paste0("-",out$performance$foldCV.number,"number")),
                                              ifelse(is.na(out$performance$foldCV.size),"",paste0("-",out$performance$foldCV.size,"size")))
    if(any(fold.number>0)){
        method.ref <- paste0("cv-",fold.number[1],"number-",na.omit(unique(out$performance$foldCV.size))[1],"size")
    }else if(!is.null(data.validation)){
        method.ref <- "external"
    }else{
        method.ref <- "internal"
    }
    out$performance <- as.data.table(out$performance)

    if("RF" %in% model){
        if(length(name.RF)>1 && length(fold.number)>1){
            index1 <- which(out$performance$foldCV.number %in% fold.number[-1])
            index2 <- which(out$performance$model %in% name.RF[-1])
            out$performance <- out$performance[-intersect(index1,index2)]
        }
        out$performance[, model.fullname := model]
        out$performance[, model := sapply(strsplit(model.fullname,split = "-", fixed = TRUE),"[[",1)]
        out$performance[, c("ntree","mtry") := as.numeric(NA)]
        out$performance[model=="RF", ntree := as.numeric(gsub("trees","",sapply(strsplit(model.fullname,split = "-", fixed = TRUE),"[[",2)))]
        out$performance[model=="RF", mtry := as.numeric(gsub("mtry","",sapply(strsplit(model.fullname,split = "-", fixed = TRUE),"[[",3)))]
    }
    ## out$Score <- riskRegression::Score(out$fit[names(out$fit!="RF")], formula = as.formula(paste0(name.outcome,"~1")), data = data)

    ## *** statistical inference
    if("logistic" %in% model){
        out$inference[out$inference$model=="logistic",c(paste0("X",1:10)) := as.list(summary(out$fit$logit)$coef[paste0("X",1:10),"Pr(>|z|)"])]
        out$inference[out$inference$model=="logistic",c("X1.X2","Xm1.Xm2") := 1]
    }
    if("RF" %in% model){
        attr(out$fit[[name.RF[1]]],"inference") <- do.call(ranger::ranger, args = list(formula.mean,
                                                                                       data = data,
                                                                                       num.trees = RF_num.trees[1],
                                                                                       mtry = RF_mtry[1],
                                                                                       min.node.size = RF_min.node.size,
                                                                                       max.depth = RF_max.depth,
                                                                                       importance = RF_importance))

        if(RF_importance2=="fpr"){
            out$fit[[name.RF[1]]]$importance_pvalues <-  as.data.frame(forestControl::fpr_fs(attr(out$fit[[name.RF[1]]],"inference")))
            out$inference[out$inference$model=="RF",c(paste0("X",1:10)) := as.list(out$fit[[name.RF[1]]]$importance_pvalues[,"fpr"])]
            out$inference[out$inference$model=="RF",c("X1.X2","Xm1.Xm2")] <- 1
        }else{
            out$fit[[name.RF[1]]]$importance_pvalues <-  try(suppressWarnings(ranger::importance_pvalues(attr(out$fit[[name.RF[1]]],"inference"),
                                                                                                         formula = formula.mean,
                                                                                                         data = data,
                                                                                                         method = RF_importance2,
                                                                                                         num.permutation = 1000)))
            if(inherits(out$fit[[name.RF[1]]]$importance_pvalues,"try-error")){
                model <- setdiff(model,"glmnet")
            }else{
                out$inference[out$inference$model=="RF", c(paste0("X",1:10)) := as.list(out$fit[[name.RF[1]]]$importance_pvalues[,"pvalue"])]
                out$inference[out$inference$model=="RF",c("X1.X2","Xm1.Xm2") := 1]
            }
        }
    }
        
    ## *** post selection inference
    if("glmnet" %in% model){
        ## check that RF identify some predictors and is better than a simple logistic model
        keep.var <- paste0("X",1:10)[which(out$inference[out$inference$model == "RF",paste0("X",1:10)] < 0.005)]
        ## WARNING: DO NOT JUST USE model AS IT IS AN ARGUMENT OF THE FUNCTION AND THAT WOULD LEAD TO CONFUSION
        iAUC_logit  <- out$performance[model.fullname == "logit" & metric == "auc" & method.fullname == method.ref,estimate]
        iAUC_RF <- out$performance[model.fullname == name.RF[1] & metric == "auc"  & method.fullname == method.ref,estimate]
        ipAUC_RF <- out$performance[model.fullname == name.RF[1] & metric == "auc"  & method.fullname == method.ref,p.value_comp]

        out$RF_better_logit <- (iAUC_RF > iAUC_logit) && (ipAUC_RF < 0.05)
        if((length(keep.var)>0) && out$RF_better_logit){ ## keep.var <- c("X1","X2","X3")
            ## main.terms <- paste0("s(",keep.var,")")
            ## interaction.terms <- apply(.Xinteraction(keep.var), 2, function(iVec){paste0("s(",iVec[1],",",iVec[2],")")})
            ## ff.gam <- as.formula(paste(all.vars(formula.mean)[1],"~",paste(c(main.terms, interaction.terms), collapse = "+")))
            ## out$fit$gam <- try(mgcv::gam(ff.gam, data = data, family = stats::binomial(link = "logit")), silent = TRUE)
            main.terms <- paste0("splines::bs(",keep.var,")")
            if(length(keep.var)>1){
                interaction.terms <- apply(.Xinteraction(keep.var), 2, function(iVec){paste0(iVec[1],":",iVec[2])})
                ff.glmnet <- as.formula(paste(name.outcome,"~0+",paste(c(main.terms, interaction.terms), collapse = "+")))
                ff.terms <- terms(ff.glmnet)
            }else{
                ff.glmnet <- as.formula(paste(name.outcome,"~0+",paste(main.terms, collapse = "+")))
            }

            Y.glmnet <- data[[name.outcome]]
            X.glmnet <- scale(model.matrix(ff.glmnet, data), center = TRUE, scale = TRUE)
            colnames(X.glmnet) <- paste0("V",1:NCOL(X.glmnet))

            index.interaction <- which(attr(X.glmnet,"assign") > length(keep.var))
            index.main <- which(attr(X.glmnet,"assign") <= length(keep.var))
            
            out$fit$glmnet <- glmnet::cv.glmnet(y = Y.glmnet, x = X.glmnet, standardize = FALSE, family = "binomial")
            lambda.glmnet <- out$fit$glmnet$lambda.min

                                        # ## https://stackoverflow.com/questions/68147109/r-how-to-translate-lasso-lambda-values-from-the-cv-glmnet-function-into-the-s
            beta.glmnet <- as.numeric(coef(out$fit$glmnet, s = lambda.glmnet, exact = TRUE, y = data[[name.outcome]], x = X.glmnet))
            SI.glmnet <- selectiveInference::fixedLassoInf(x = X.glmnet, y = data[[name.outcome]], beta = beta.glmnet, lambda = lambda.glmnet*NROW(data), family = "binomial")
            pvalue.glmnet <- rep(1,length(beta.glmnet)-1)
            pvalue.glmnet[which(abs(beta.glmnet[-1])>1e-10)] <- SI.glmnet$pv
            ## according to documentation
            ## beta.glmnet <- coef(out$fit$glmnet, s = lambda.glmnet/NROW(data), exact = TRUE, y = data[[name.outcome]], x = X.glmnet) 
            ## SI.glmnet <- selectiveInference::fixedLassoInf(x = X.glmnet, y = data[[name.outcome]], beta = beta.glmnet, lambda = lambda.glmnet, family = "binomial")
            
            out$inference[out$inference$model=="glmnet", c(keep.var) := as.list(tapply(pvalue.glmnet[index.main],attr(X.glmnet,"assign")[index.main],min))]
            out$inference[out$inference$model=="glmnet",setdiff(paste0("X",1:10),keep.var)]  <- 1
            if(length(index.interaction)>0){
                if(all(c("X1","X2") %in% rownames(attr(ff.terms,"factors")))){
                    test.interactionX12  <- colSums(attr(ff.terms,"factors")[c("X1","X2"),attr(X.glmnet,"assign")[index.interaction],drop=FALSE]>0)
                }else{
                    test.interactionX12 <- 0
                }
                if(any(test.interactionX12==2)){
                    out$inference[out$inference$model=="glmnet","X1.X2"]  <- pvalue.glmnet[index.interaction[test.interactionX12==2]]
                }else{
                    out$inference[out$inference$model=="glmnet","X1.X2"] <- 1
                }
                if(all(test.interactionX12==2)){
                    out$inference[out$inference$model=="glmnet","Xm1.Xm2"]  <- 1
                }else{
                    out$inference[out$inference$model=="glmnet","Xm1.Xm2"]  <- min(pvalue.glmnet[index.interaction[test.interactionX12!=2]])
                }
            }else{
                out$inference[out$inference$model=="glmnet",c("X1.X2","Xm1.Xm2")]  <- 1
            }
        }
    }


    ## ** export
    class(out) <- append("compareScore",class(out))
    return(out)
}

## * print.compareScore
print.compareScore <- function(x, ...){
    cat("     ** Comparing predictive models based on the formula ** \n        ")
    print(x$formula)

    cat("\n Inference (p-value) \n")
    print(x$inference)

    cat("\n Performance \n")
    print(x$performance)
}

## * .Xinteraction
.Xinteraction <- function(x){ ## x <- c("X1","X2")
    n <- length(x)
    ls <- lapply(1:n, function(k){ rbind(x[k], x[k:n])})
    out <- array(unlist(ls), dim = c(2, n * (n + 1)/2))
    out <- out[,colSums(apply(out,2,duplicated))==0,drop=FALSE]
    return(out)
}





##----------------------------------------------------------------------
### runAnalysis.R ends here
