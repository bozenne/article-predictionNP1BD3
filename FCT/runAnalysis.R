### runAnalysis.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: Jun 28 2021 (17:27) 
## Version: 
## Last-Updated: Jun 28 2021 (18:25) 
##           By: Brice Ozenne
##     Update #: 14
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
##' df <- simData(100, p.noise = 8, scenario = "linear", effect = 1, plot = 2, plot.nbins = 20)
##'
##' runAnalysis(df,
##'             formula.mean = Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10,
##'             step = c("logistic","RF"))

## * runAnalysis (code)
runAnalysis <- function(data,
                        formula.mean,
                        step,
                        RF_num.trees = 500, RF_mtry = NULL, RF_min.node.size = NULL, RF_max.depth = NULL, RF_importance = "impurity_corrected"){

    ## ** normalize user input
    if(any(all.vars(formula.mean) %in% names(data) == FALSE)){
        stop("Incorrect \'formula.mean\' argument. Contain variables not in argument \'data\'.\n")
    }
    step <-  match.arg(step, choices = c("logistic","RF", "glmnet", "superLearner"), several.ok = TRUE)
    RF_importance <-  match.arg(RF_importance, choices = c("permutation","impurity_corrected","fpr"), several.ok = TRUE)
    RF_importance2 <- switch(RF_importance,
                             "permutation" = "altmann",
                             "impurity_corrected" = "janitza",
                             "fpr" = "fpr")
    RF_importance <- switch(RF_importance,
                             "permutation" = "permutation",
                             "impurity_corrected" = "impurity_corrected",
                             "fpr" = "none")

    ## *** warper
    warper <- function(dataTrain, dataValid){
        out <- list(fit = NULL,
                    inference = data.frame(matrix(nrow = 4, ncol = 13, dimnames = list(NULL,c("model",paste0("X",1:10),"X1.X2","Xm1.Xm2")))),
                    performance = NULL)
        out$inference$model <- c("logistic","RF","glmnet","superLearner")
        
        ## **** logistic model
        if("logistic" %in% step){
            out$fit$logit <- stats::glm(formula.mean, data = dataTrain, family = stats::binomial(link = "logit"))
            out$inference[out$inference$model=="logistic",paste0("X",1:10)] <- summary(out$fit$logit)$coef[paste0("X",1:10),"Pr(>|z|)"]
            out$inference[out$inference$model=="logistic",c("X1.X2","Xm1.Xm2")] <- 1
        }
        ## **** random forest
        if("RF" %in% step){
            out$fit$RF <- ranger::ranger(formula.mean, data = dataTrain, num.trees = RF_num.trees, mtry = RF_mtry, min.node.size = RF_min.node.size, max.depth = RF_max.depth, importance = RF_importance)
        }
    
        ## **** glmnet
        if("RF" %in% step && "glmnet" %in% step){
            if(RF_importance2=="fpr"){
                out$fit$RFimportance <-  as.data.frame(forestControl::fpr_fs(out$fit$RF))
                out$inference[out$inference$model=="RF",paste0("X",1:10)] <- out$fit$RFimportance[,"fpr"]
                keep.var <- out$fit$RFimportance$variable[which(out$fit$RFimportance[,"fpr"]<=0.05)]
            }else{
                out$fit$RFimportance <-  suppressWarnings(ranger::importance_pvalues(out$fit$RF, formula = formula.mean, data = dataTrain, method = RF_importance2, num.permutation = 1000))
                out$inference[out$inference$model=="RF",paste0("X",1:10)] <- out$fit$RFimportance[,"pvalue"]
                keep.var <- names(which(out$fit$RFimportance[,"pvalue"]<=0.05))
            }
            ## main.terms <- paste0("s(",keep.var,")")
            ## interaction.terms <- apply(.Xinteraction(keep.var), 2, function(iVec){paste0("s(",iVec[1],",",iVec[2],")")})
            ## ff.gam <- as.formula(paste(all.vars(formula.mean)[1],"~",paste(c(main.terms, interaction.terms), collapse = "+")))
            ## out$fit$gam <- try(mgcv::gam(ff.gam, data = data, family = stats::binomial(link = "logit")), silent = TRUE)
            main.terms <- paste0("bs(",keep.var,")")
            interaction.terms <- apply(.Xinteraction(keep.var), 2, function(iVec){paste0(iVec[1],":",iVec[2])})
            ff.glmnet <- as.formula(paste(all.vars(formula.mean)[1],"~",paste(c(main.terms, interaction.terms), collapse = "+")))
            out$fit$glmnet <- glmnet::cv.glmnet(y = dataTrain[[all.vars(formula.mean)[1]]], x = model.matrix(ff.glmnet, dataTrain))

            ## get p-value from selective inference
            #lasso at fixed lambda- logistic family#set.seed(43)#   n = 50#   p = 10#   sigma = 1#    x = matrix(rnorm(n*p),n,p)x=scale(x,TRUE,TRUE)##     beta = c(3,2,rep(0,p-2))#    y = x%*%beta + sigma*rnorm(n)#    y=1*(y>mean(y))# first run glmnet#    gfit = glmnet(x,y,standardize=FALSE,family="binomial")# extract coef for a given lambda; note the 1/n factor!# (and here  we DO  include the intercept term)#    lambda = .8#    beta = coef(gfit, s=lambda/n, exact=TRUE)#    # compute fixed lambda p-values and selection intervals#    out = fixedLassoInf(x,y,beta,lambda,family="binomial")#    out
        }
    }


    ## TODO1: extract conclusion for each analysis:
    ## - selected variable for glm, RF
    ## - selected variable/interactions for glmnet
    ## - predictive performance on the validation set (brier,auc)

    ## TODO2: add the cv loop

    ## TODO3: overall calibration
}

## * .Xinteraction
.Xinteraction <- function(x){
    n <- length(x)
    ls <- lapply(1:n, function(k){ rbind(x[k], x[k:n])})
    out <- array(unlist(ls), dim = c(2, n * (n + 1)/2))
    out <- out[,colSums(apply(out,2,duplicated))==0]
    return(out)
}

##----------------------------------------------------------------------
### runAnalysis.R ends here
