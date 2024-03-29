### FCT_forestplot.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: jan 24 2022 (17:36) 
## Version: 
## Last-Updated: sep 16 2022 (09:20) 
##           By: Brice Ozenne
##     Update #: 22
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:


forestplot <- function(data, size = c(1,1.5,0.75), term = "term", reorder = TRUE){
    require(ggforestplot)
    gg_color_hue <- function(n) {
        hues = seq(15, 375, length = n + 1)
        hcl(h = hues, l = 65, c = 100)[1:n]
    }
    index.keep <- data[[term]] != "(Intercept)"
    data <- data[index.keep]
    if(reorder){
        data[[term]] <- factor(data[[term]], levels = sort(levels(data[[term]]), decreasing = TRUE))
        setkeyv(data,c("method",term))
    }

    ## based on ggforestplot::forestplot
    ggForest <- ggplot(data, aes_string(x = "estimate", y = term))
    ggForest <- ggForest + ggforestplot::theme_forest() + ggforestplot::scale_colour_ng_d() + ggforestplot::scale_fill_ng_d()
    ggForest <- ggForest + ggforestplot::geom_stripes()
    ggForest <- ggForest + geom_vline(xintercept = 0, linetype = "solid", size = size[3], colour = "black")
    ggForest <- ggForest + ggforestplot::geom_effect(ggplot2::aes(xmin = lower, xmax = upper, 
                                                                  colour = time, filled = TRUE), size = size[2], 
                                                     position = ggstance::position_dodgev(height = 0.5))
    ggForest <- ggForest + ggforestplot::geom_effect(ggplot2::aes(xmin = lower.adj, xmax = upper.adj, 
                                                                  colour = time, filled = TRUE), size = size[1],
                                                     position = ggstance::position_dodgev(height = 0.5))
    ggForest <- ggForest + ggplot2::scale_shape_manual(values = c(21L, 22L, 23L, 
                                                                  24L, 25L)) + guides(colour = guide_legend(reverse = TRUE), 
                                                                                      shape = guide_legend(reverse = TRUE))
    ggForest <- ggForest + ggplot2::scale_color_manual(values = gg_color_hue(length(unique(data$time))))
    return(ggForest)
}

##----------------------------------------------------------------------
### FCT_forestplot.R ends here
