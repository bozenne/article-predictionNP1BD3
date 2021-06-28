### simData.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: Jun 28 2021 (09:35) 
## Version: 
## Last-Updated: Jun 28 2021 (15:19) 
##           By: Brice Ozenne
##     Update #: 83
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:
##' @title Simulate Data
##' @description Simulate two informative biomarkers and a number of non-informative biomarker of treatment response.
##' 
##' @param n [integer, >0] number of observations
##' @param p.noise [integer, >0] number of irrelevant covariates
##' @param scenario [character] type of association between the biomarkers and the treatment response. Can be linear, quadratic, or normal/abnormal
##' @param effect [numeric, >0] how strongly the biomarkers predicts treatment response (in term of log odd-ratio). 0 means no association between any of the biomarkers and treatment response
##' @param rho [numeric] how correlated the biomarkers are. When correlated, the biomakers are divided into two groups of biomarkers, each containing an informative one. Biomarkers are correlated withing group but not between groups.
##' @param plot [logical] should a map of the simulated risk be output with respect to the informative biomarkers.
##'
##' @examples
##' set.seed(10)
##' 
##' #### linear effect ####
##' ## strong
##' df <- simData(100000, p.noise = 5, scenario = "linear", effect = 2, plot = 2, plot.nbins = 20)
##' df <- simData(100000, p.noise = 5, scenario = "linear", effect = 1, plot = 2, plot.nbins = 20)
##' ## weak
##' df <- simData(100000, p.noise = 5, scenario = "linear", effect = 0.25, plot = 2, plot.nbins = 20)
##' ## null
##' df <- simData(100000, p.noise = 5, scenario = "linear", effect = 0, plot = 2, plot.nbins = 20)
##' df <- simData(100000, p.noise = 5, scenario = "linear", effect = 0, shift = -1, plot = 2, plot.nbins = 20)
##' 
##' #### normal / abnormal ####
##' ## strong
##' df <- simData(100000, p.noise = 5, scenario = "abnormal", effect = 2, plot = 2, plot.nbins = 20)
##' df <- simData(100000, p.noise = 5, scenario = "abnormal", effect = 1, plot = 2, plot.nbins = 20)
##' ## weak
##' df <- simData(100000, p.noise = 5, scenario = "abnormal", effect = 0.25, plot = 2, plot.nbins = 20)
##' ## null
##' df <- simData(100000, p.noise = 5, scenario = "abnormal", effect = 0, plot = 2, plot.nbins = 20)
##' df <- simData(100000, p.noise = 5, scenario = "abnormal", effect = 0, shift = -1, plot = 2, plot.nbins = 20)
##' 
##' #### interaction ####
##' ## strong
##' df <- simData(100000, p.noise = 5, scenario = "interaction", effect = 2, plot = 2, plot.nbins = 20)
##' ## weak
##' df <- simData(100000, p.noise = 5, scenario = "interaction", effect = 1, plot = 2, plot.nbins = 20)
##' ## null
##' df <- simData(100000, p.noise = 5, scenario = "interaction", effect = 0, plot = 2, plot.nbins = 20)
##' df <- simData(100000, p.noise = 5, scenario = "interaction", effect = 0, shift = -1, plot = 2, plot.nbins = 20)
##'
##'
##' 

simData <- function(n, p.noise, scenario, effect, shift = 0, rho = 0, sigma = 1,
                    plot = FALSE, plot.nbins = 20, plot.xlim = c(-2,2), plot.ylim = c(-2,2)){

    ## ** load dependencies
    requireNamespace("mvtnorm")

    ## ** normalize user-input
    if(length(rho)==1){
        rho <- rep(rho,2)
    }else if(length(rho)!=2){
        stop("Argument \'rho\' must have length 2.\n")
    }
    if(length(sigma)==1){
        sigma <- rep(sigma,2+p.noise)
    }else if(length(sigma)==2){
        sigma.save <- sigma
        sigma <- rep(NA, 2+p.noise)
        sigma[1:(2+p.noise) %% 2 == 1] <- sigma.save[1]
        sigma[1:(2+p.noise) %% 2 == 2] <- sigma.save[2]
    }else{
        stop("Argument \'sigma\' must have length ",2+p.noise,".\n")
    }

    if(length(effect)!=1){
        stop("Argument \'effect\' must have length 1. \n")
    }
    
    ## ** generate names for the covariates
    allX <- c("X1","X2")
    n.allX <- length(allX)
    if(p.noise>0){
        allX <- c(allX,paste("X",2+(1:p.noise),sep=""))
    }
    X.cor1 <- allX[1:n.allX %% 2 == 1]
    p.cor1 <- length(X.cor1)
    M.cor1 <- matrix(rho[1], ncol = p.cor1, nrow = p.cor1)
    diag(M.cor1) <- 1
    M.cor1 <- M.cor1 *  sigma[1:n.allX %% 2 == 1]

    X.cor2 <- allX[1:n.allX %% 2 == 0]
    p.cor2 <- length(X.cor2)
    M.cor2 <- matrix(rho[2], ncol = p.cor2, nrow = p.cor2)
    diag(M.cor2) <- 1
    M.cor2 <- M.cor2 * sigma[1:n.allX %% 2 == 0]

    allBeta <- c(rep(effect,2),rep(0,p.noise))

    ## ** simulate data
    Xsim.cor1 <- mvtnorm::rmvnorm(n, mean = rep(0, p.cor1), sigma = M.cor1)
    colnames(Xsim.cor1) <- X.cor1
    Xsim.cor2 <- mvtnorm::rmvnorm(n, mean = rep(0, p.cor2), sigma = M.cor2)
    colnames(Xsim.cor2) <- X.cor2
    X.sim <- cbind(Xsim.cor1,Xsim.cor2)[,allX,drop=FALSE]

    if(scenario == "linear"){
        mu <- X.sim %*% allBeta + shift
    }else if(scenario == "abnormal"){
        mu  <- (X.sim^2 %*% allBeta) + shift - sum(allBeta)
    }else if(scenario == "interaction"){
        ## factor <- c(1,-1)[(sign(X.sim[,1])<0)*(sign(X.sim[,2])<0)+1]
        ## mu  <- factor * X.sim[,1] * X.sim[,2] * allBeta[1] * allBeta[2] + shift
        mu  <- pmin(X.sim[,1],X.sim[,2]) * effect + shift
    }
    Y <- rbinom(n, p = 1/(1+exp(-mu)), size = 1)

    out <- data.frame(Y=Y,X.sim)
    ## ** plot
    if(plot>0){
        requireNamespace("ggplot2")
        if(is.null(plot.xlim)){plot.xlim <- range(X.sim[,"X1"])}
        if(is.null(plot.ylim)){plot.ylim <- range(X.sim[,"X2"])}
        X1.grid <- c(-Inf,seq(floor(plot.xlim[1]), ceiling(plot.xlim[2]), length = plot.nbins),Inf)
        X2.grid <- c(-Inf,seq(floor(plot.ylim[1]), ceiling(plot.ylim[2]), length = plot.nbins),Inf)

        X1.interval <- findInterval(X.sim[,"X1"], vec = X1.grid)
        X2.interval <- findInterval(X.sim[,"X2"], vec = X2.grid)
        allLevels <- sort(unique(c(X1.interval,X2.interval)))
        
        table.grid0 <- table(factor(X1.interval[Y==0], levels = allLevels),
                             factor(X2.interval[Y==0], levels = allLevels))
        table.grid1 <- table(factor(X1.interval[Y==1], levels = allLevels),
                             factor(X2.interval[Y==1], levels = allLevels))
        p.grid <- table.grid1/(table.grid0+table.grid1)
        p.grid[table.grid0+table.grid1==0] <- NA

        arr.ind <- which(table.grid0>=0, arr.ind = TRUE)
        grid <- data.frame(X1 = X1.grid[arr.ind[,1]], X2 = X2.grid[arr.ind[,2]],
                           X1.interval = arr.ind[,1], X2.interval = arr.ind[,2],
                           Probability = p.grid[which(table.grid0>=0)])
        gridR <- grid[(grid$X1.interval %in% 2:(length(X1.grid)-1)) & (grid$X2.interval %in% 2:(length(X2.grid)-1)),]
        gg <- ggplot2::ggplot(gridR, ggplot2::aes(x=X1,y=X2,fill=Probability)) + ggplot2::geom_raster()
        gg  <-  gg + ggplot2::scale_fill_gradientn(colours = c("green", "yellow", "orange", "red"), values = c(0,0.5,0.75,1), limits = c(0,1))
        gg  <-  gg + ggplot2::theme(legend.key.size = ggplot2::unit(3,"line"))
        gg  <-  gg + ggplot2::ggtitle(paste0("Prevalence = ",mean(Y)))
        attr(out,"plot") <- gg
        attr(out,"grid") <- grid
        attr(out,"gridR") <- gridR
        if(plot>1) {
            print(gg)
        }
    }
    
    ## ** export
    return(out)
}

##----------------------------------------------------------------------
### simData.R ends here
