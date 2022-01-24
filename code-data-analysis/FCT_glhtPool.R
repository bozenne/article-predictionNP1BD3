### FCT_glhtPool.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: jan 24 2022 (18:39) 
## Version: 
## Last-Updated: jan 24 2022 (18:43) 
##           By: Brice Ozenne
##     Update #: 3
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

glhtPool <- function(object){
    all.coef <- setdiff(names(coef(object$analyses[[1]])),"(Intercept)")
    n.coef <- length(all.coef)
    
    ## contrast
    C <- matrix(0, nrow = n.coef, ncol = n.coef, dimnames = list(all.coef, all.coef))
    diag(C) <- 1

    ## estimates
    pooled <- summary(pool(object))
    pooled <- pooled[pooled[,"term"] != "(Intercept)",]

    ## glht object
    out <- list(linfct = C,
                rhs = rep(0,NROW(C)),
                coef = pooled[,"estimate"],
                vcov = tcrossprod(pooled[,"std.error"]) * Reduce("+",lapply(object$analyses,function(iO){cov2cor(vcov(iO)[pooled[,"term"],pooled[,"term"]])}))/length(object$analyses),
                df = round(median(pooled[,"df"])),
                alternative = "two.sided")
    class(out) <- "glht"

    return(out)
}

##----------------------------------------------------------------------
### FCT_glhtPool.R ends here
