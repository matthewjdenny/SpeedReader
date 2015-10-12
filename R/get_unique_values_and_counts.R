get_unique_values_and_counts <- function(metadata,
                                         variable_names = NA,
                                         threshold = 0){
    #takes a varialbe names vector
    if(length(variable_names) == 1){
        UNIVARIATE = TRUE
    }else{
        UNIVARIATE = FALSE
        NUM_VARS = length(variable_names)
        if(length(threshold)==1){
            threshold = rep(threshold,NUM_VARS)
        }
    }

    #if we are only making a 1 variable contingency table
    if(UNIVARIATE){
        colindex <- which(colnames(metadata)==variable_names)
        unique_values <- unique(metadata[,colindex])
        #if we are making a multi-variate contingency table
    }else{
        colindexes <- rep(0,NUM_VARS)
        unique_value_list <- vector(length = NUM_VARS, mode = "list")
        #populate the list
        for(i in 1:NUM_VARS){
            colindex <- which(colnames(metadata)==variable_names[i])
            colindexes[i] <- colindex
            unique_value_list[[i]] <- unique(metadata[,colindex])
        }
    }#end multivariate conditional statement

    if(UNIVARIATE){
        use <- NULL
        for(j in 1:length(unique_values)){
            catsum <- length(which(metadata[,colindex] == unique_values[j]))
            if(catsum > threshold){
                use <- c(use,j)
            }
            cat("Variable:",variable_names,"Category:",unique_values[j],"Num Obs:",catsum,"\n")
        }
    }else{
        uselist <- vector(length = NUM_VARS, mode = "list")
        for(i in 1:NUM_VARS){
            use <- NULL
            colindex <- colindexes[i]
            unique_values <- unique_value_list[[i]]
            for(j in 1:length(unique_values)){
                catsum <- length(which(metadata[,colindex] == unique_values[j]))
                if(catsum > threshold[i]){
                    use <- c(use,j)
                }
                cat("Variable:",variable_names[i],"Category:",unique_values[j],"Num Obs:",catsum,"\n")
            }
            uselist[[i]] <- use
        }
    }

    #now return stuff
    if(UNIVARIATE){
        return(use)
    }else{
        return(uselist)
    }
}
