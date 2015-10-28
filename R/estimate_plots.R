#' A function to parameter estimate plots with 95% confidence bounds for up to two models we wish to compare.
#'
#' @param model1
#' @param model2
#' @param type
#' @return Prints the plot.
#' @export
estimate_plots <- function(model1,
                          model2 = NULL,
                          type = c("generic","glm/lm","zero-inflated" )){

    type <- type[1]

    if(type == "zero-inflated"){
        if(class(model1) == "zeroinfl"){
            #we are ok
        }else{
            stop("If you are specifying a type of zero-inflated, then you must provide a model object estimated by the zeroinfl() function, which is part of the pscl package.")
        }
        mod1names <- c(paste0(names(model1$coefficients$count),"_count"), paste0(names(model1$coefficients$zero),"_zero") )[-c(1,length(model1$coefficients$count) + 1)]
        mod1coefs <- as.numeric(c(model1$coefficients$count, model1$coefficients$zero))[-c(1,length(model1$coefficients$count) + 1)]
        mod1ses <-  as.numeric(sqrt(diag(model1$vcov)))[-c(1,length(model1$coefficients$count) + 1)]
        #if we are providing a comparison model
        if(!is.null(model2)){
            if(class(model2) == "zeroinfl"){
                #we are ok
            }else{
                stop("If you are specifying a type of zero-inflated, then you must provide a model object estimated by the zeroinfl() function, which is part of the pscl package.")
            }
            mod2names <- c(paste0(names(model2$coefficients$count),"_count"), paste0(names(model2$coefficients$zero),"_zero") )[-c(1,length(model2$coefficients$count) + 1)]
            mod2coefs <- as.numeric(c(model2$coefficients$count, model2$coefficients$zero))[-c(1,length(model2$coefficients$count) + 1)]
            mod2ses <-  as.numeric(sqrt(diag(model2$vcov)))[-c(1,length(model2$coefficients$count) + 1)]
        }
    }

    if(type == "glm/lm"){
        if((class(model1) == "glm" | class(model1) == "lm")  ){
            #we are ok
        }else{
            stop("If you are specifying a type of glm/lm then you must provide a model object estimated by lm() or glm().")
        }
        mod1names <- names(model1$coefficients)
        mod1coefs <- summary(model1)$coefficients[, 1]
        mod1ses <-  summary(model1)$coefficients[, 2]
        #if we are providing a comparison model
        if(!is.null(model2)){
            if(class(model2) == "glm" | class(model2) == "lm"){
                #we are ok
            }else{
                stop("If you are specifying a type of glm/lm then you must provide a model object estimated by lm() or glm().")
            }
            mod2names <- names(model2$coefficients)
            mod2coefs <- summary(model2)$coefficients[, 1]
            mod2ses <-  summary(model2)$coefficients[, 2]
        }
    }

    if(type == "generic"){
        mod1names <- model1$varnames
        mod1coefs <- model1$coefficients
        mod1ses <-  model1$se
        #if we are providing a comparison model
        if(!is.null(model2)){
            mod2names <- model2$varnames
            mod2coefs <- model2$coefficients
            mod2ses <-  model2$se
        }
    }

    #define colors
    UMASS_BLUE <- rgb(51,51,153,255,maxColorValue = 255)
    UMASS_RED <- rgb(153,0,51,255,maxColorValue = 255)
    Model <- Variable <- Coefficient <- SE <- NULL
    modelFrame <- data.frame(Variable = mod1names ,
                             Coefficient = mod1coefs,
                             SE = mod1ses,
                             Model = "Model 1"
    )
    data <- data.frame(modelFrame)

    if(!is.null(model2)){
        ModelFrame2 <- data.frame(Variable = mod2names ,
                                  Coefficient = mod2coefs,
                                  SE = mod2ses,
                                  Model = "Model 2"
        )
        data <- rbind(data,ModelFrame2)
    }


    # Plot
    zp1 <- ggplot2::ggplot(data, ggplot2::aes(colour = Model))

    if(is.null(model2)){
        zp1 <- zp1 + ggplot2::scale_color_manual(values = UMASS_BLUE)
    }else{
        zp1 <- zp1 + ggplot2::scale_color_manual(values = c(UMASS_BLUE,UMASS_RED))
    }

    zp1 <- zp1 + ggplot2::geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
    zp1 <- zp1 + ggplot2::geom_linerange( ggplot2::aes(x = Variable,
                                                       ymin = Coefficient - SE*(-qnorm((1-0.9)/2)),
                                                       ymax = Coefficient + SE*(-qnorm((1-0.9)/2))),
                                          lwd = 1,
                                          position = ggplot2::position_dodge(width = 1/2))
    zp1 <- zp1 + ggplot2::geom_pointrange(ggplot2::aes(x = Variable,
                                                       y = Coefficient,
                                                       ymin = Coefficient - SE*(-qnorm((1-0.95)/2)),
                                                       ymax = Coefficient + SE*(-qnorm((1-0.95)/2))),
                                          lwd = 1/2,
                                          position = ggplot2::position_dodge(width = 1/2),
                                          shape = 21, fill = "WHITE")
    if(is.null(model2)){
        zp1 <- zp1  + ggplot2::theme_bw() +
            ggplot2::coord_flip() +
            ggplot2::theme(legend.position="none")
    }else{
        zp1 <- zp1  + ggplot2::theme_bw() +
            ggplot2::coord_flip()
    }
    print(zp1)
}
