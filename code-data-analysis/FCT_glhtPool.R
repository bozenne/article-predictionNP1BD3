### FCT_glhtPool.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: jan 24 2022 (18:39) 
## Version: 
## Last-Updated: mar  2 2022 (16:57) 
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

## * glhtPool
glhtPool <- function(object, C = NULL){
    
    require(lava)
    
    ## ** contrast
    all.coef <- names(coef(object$analyses[[1]]))
    n.coef <- length(all.coef)
    if(is.null(C)){
        C <- matrix(0, nrow = n.coef, ncol = n.coef, dimnames = list(all.coef, all.coef))
        diag(C) <- 1
        C <- C[-1,]
    }

    ## ** extract all
    all.estimate <- lapply(object$analyses,coef)
    all.vcov <- lapply(object$analyses,vcov)
    n.imputed <- length(object$analyses)

    ## ** pool
    pooled.mice <- summary(pool(object))

    pooled.estimate <- colMeans(do.call(rbind,all.estimate))
    if(any(abs(pooled.estimate-pooled.mice$estimate)>1e-6)){
        warning("Descrepancy between mice and manual calculations for the estimates. \n")
    }
    
    pooled.vcovW <- Reduce("+",all.vcov)/n.imputed
    pooled.vcovB <- Reduce("+",lapply(all.estimate, function(iCoef){tcrossprod(iCoef-pooled.estimate)}))/(n.imputed-1)
    pooled.vcovE <- pooled.vcovB/n.imputed
    pooled.vcov <- pooled.vcovW + pooled.vcovB + pooled.vcovE
    if(any(abs(sqrt(diag(pooled.vcov))-pooled.mice$std.error)>1e-6)){
        warning("Descrepancy between mice and manual calculations for the standard error. \n")
    }
    
    ## glht object
    out <- list(linfct = C,
                rhs = rep(0,NROW(C)),
                coef = pooled.estimate,
                vcov = pooled.vcov,
                df = floor(median(pooled.mice$df)),
                alternative = "two.sided")
    class(out) <- "glht"

    return(out)
}

##----------------------------------------------------------------------
### FCT_glhtPool.R ends here
