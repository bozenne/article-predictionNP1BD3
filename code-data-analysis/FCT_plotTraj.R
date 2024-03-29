### FCT_plotTraj.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: jan 24 2022 (13:51) 
## Version: 
## Last-Updated: mar 22 2022 (11:18) 
##           By: Brice Ozenne
##     Update #: 64
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

## * ggTraj_hlme
ggTraj_hlme <- function(object, alpha = 0.8, color = "class", plot = TRUE, order.class = NULL,
                        facet = FALSE, nrow = NULL){

    nClass <- object$ng
    idvar <- colnames(object$pred)[1]
    name.y <- all.vars(object$call$fixed)[1]
    name.x <- all.vars(object$call$fixed)[2]
    data <- eval(object$call$data)
    value.x <- unique(data[[name.x]])

    ## ** gather informations
    ## get coef
    object.coef <- coef(object)

    object.membcoef <- object.coef[1:(nClass-1)]
    object.proba <- c(exp(object.membcoef),1)/sum(c(exp(object.membcoef),1))
    object.prob.round <- round(100*object.proba,2)

    name.regcoef <- object$Xnames
    object.regcoef <- object.coef[nClass:length(object.coef)]
    if(nClass==1){
        dt.regcoef <- data.table(class = 1,
                                 time = value.x,
                                 obs = c(object.regcoef["intercept"],object.regcoef["intercept"] + object.regcoef[setdiff(name.regcoef,"intercept")])
                                 )
    }else{
        dt.regcoef <- do.call(rbind,lapply(1:nClass, function(iC){ ## iC <- 1
            alpha <- object.regcoef[paste0("intercept"," class",iC)]
            beta <- c(0,object.regcoef[paste0(setdiff(name.regcoef,"intercept")," class",iC)])
            data.table(class = iC, time = 1:length(name.regcoef), obs = alpha + beta)
        }))
    }
    dt.regcoef[, class.char := as.character(class)]

    ## ** get groups
    dt.pred <- merge(as.data.table(object$pred),as.data.table(object$pprob), by = idvar)
    dt.pred$time <- NA
    for(iId in unique(dt.pred[[idvar]])){
        dt.pred$time[dt.pred[[idvar]]==iId] <- data[[name.x]][data[[idvar]]==iId]
    }
    dt.pred[, class.char := as.character(class)]

    if(color == "rgb"){
        if(is.null(dt.pred$prob2)){dt.pred$prob2 <- 0}
        if(is.null(dt.pred$prob3)){dt.pred$prob3 <- 0}
    }else if(color == "prob"){
        vec.color <- c("Blue", "Green", "Red", "Orange", "Purple", "Brown", "Gray")
        if(nClass>7){
            stop("Argument \'color\' equals  to \"prob\" supports max. 7 classes \n")
        }
    }
    
    ## ** create color
    dt.pred[, prob :=  as.numeric(NA)]
    dt.pred[, rgb :=  as.character(NA)]
    
    for(iC in 1:nClass){
        dt.pred[class==iC,prob := .SD[[paste0("prob",iC)]]]
        if(color == "rgb"){
            dt.pred[, rgb := rgb(pmax(0,prob1*alpha),pmax(0,prob2*alpha),pmax(prob3*alpha))]
            dt.regcoef[class==iC, rgb := rgb(iC==1,iC==2,iC==3)]
        }else if(color == "prob"){
            dt.pred[class==iC, rgb := ggthemes::tableau_seq_gradient_pal(vec.color[iC])(prob*alpha)]
            dt.regcoef[class==iC, rgb := ggthemes::tableau_seq_gradient_pal(vec.color[iC])(1)]
        }
    }

    ## ** create plot
    if(is.null(names(order.class))){
        dt.pred[,class.char := paste0("Class ",class," (",object.prob.round[class],"%)")]
        dt.regcoef[,class.char := paste0("Class ",class," (",object.prob.round[class],"%)")]

        if(!is.null(order.class)){
            dt.regcoef$class.char <- factor(dt.regcoef$class.char, levels = levels(factor(dt.pred$class.char))[order.class])
            dt.pred$class.char <- factor(dt.pred$class.char, levels = levels(factor(dt.pred$class.char))[order.class])
        }
        
    }else{
        name.class <- names(sort(order.class))
        name.order.class <- paste0(name.class[order.class]," (",object.prob.round[order.class],"%)")
        dt.pred[,class.char := factor(paste0(name.class[class]," (",object.prob.round[class],"%)"),name.order.class)]
        dt.regcoef[,class.char := factor(paste0(name.class[class]," (",object.prob.round[class],"%)"),name.order.class)]
        
    }
    if(is.factor(dt.regcoef$time)){
        dt.regcoef$time <- as.numeric(dt.regcoef$time)
    }

    if(color == "class"){
        gg <- ggplot(mapping = aes(x = time, y = obs))
        gg <- gg + geom_line(data = dt.pred, mapping = aes_string(group = idvar, color = "class.char"), alpha = 0.25)
        gg <- gg + geom_line(data = dt.regcoef, mapping = aes(group = class.char, color = class.char), alpha = 1, size = 3)
        gg <- gg + labs(color = "Class") + ylab(name.y)        
    }else if(color %in% c("prob","rgb")){
        idvar2 <- paste0(idvar,".char")
        dt.pred[, c(idvar2) := as.character(.SD[[idvar]])]
        dt.regcoef[,c(idvar2) := paste0("G",.SD$class)]
        dt.color <- rbind(dt.pred[,.(rgb=rgb[1]),by=idvar2],
                          dt.regcoef[,.(rgb=rgb[1]),by=idvar2])
        gg <- ggplot(mapping = aes(x = time, y = obs))
        gg <- gg + geom_line(data = dt.pred, mapping = aes_string(group = idvar, color = idvar2))
        gg <- gg + geom_line(data = dt.regcoef, mapping = aes_string(group = "class.char", color = idvar2), size = 3)
        gg <- gg + scale_color_manual(values = setNames(dt.color$rgb, dt.color[[idvar2]]))
        gg <- gg + guides(color = "none")
        gg <- gg + labs(color = "Class") + ylab(name.y)
    
    }else{
        stop("not available - only \"class\" or \"rgb\" \n")
    }
    if(!is.numeric(value.x)){
        gg <- gg + scale_x_continuous(breaks = as.numeric(value.x), labels = as.character(value.x))
    }
    if(facet){
        gg <- gg + facet_wrap(~class.char, nrow = nrow) + guides(color = "none")
    }
    
    ## ** display
    if(plot){
        print(gg)
    }

    ## ** export
    return(invisible(list(plot = gg,
                          data = dt.pred,
                          data2 = dt.regcoef)))
    
}


##----------------------------------------------------------------------
### FCT_plotTraj.R ends here
