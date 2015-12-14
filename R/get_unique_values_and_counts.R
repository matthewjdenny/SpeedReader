#' Find unique values and the counts of those variables for a set of variables in a data.frame. Useful in PMI analysis and for exploring document metadata.
#'
#' @param metadata A data.frame containing document covariates.
#' @param variable_names A string or vector of strings givign the column names of covariates we would like to get unique values for.
#' @param threshold Defaults to 0, the number of times a unique value of a variable must appear in order to be included in the returned list object. Allows the user to ignore very infrequent values.
#' @return A vector (or list of vectors) of unique variable values that occur more than "threshold" times.
#' @export
get_unique_values_and_counts <- function(metadata,
                                         variable_names = NA,
                                         threshold = 0){
	#make sure we are working with a dataframe
	if(class(metadata) != "data.frame"){
		metadata <- data.frame(metadata, stringsAsFactors = FALSE)
	}

    #takes a variable names vector
    if(length(variable_names) == 1){
        UNIVARIATE = TRUE
    }else{
        UNIVARIATE = FALSE
        NUM_VARS = length(variable_names)
        if(length(threshold)==1){
            threshold = rep(threshold,NUM_VARS)
        }
		# deal with incorrect threshold sepcification
		if(length(threshold) != length(variable_names)){
			stop("You must either specify a single value for threshold or a vector of the same length as variable_names.")
		}
    }

    #if we are only making a 1 variable contingency table
    if(UNIVARIATE){
        colindex <- which(colnames(metadata)==variable_names)
		if(length(colindex) == 0){
			stop(paste("You have specified a covariate:",variable_names,"that does not exist!"))
		}
        unique_values <- unique(metadata[,colindex])
        #if we are making a multi-variate contingency table
    }else{
        colindexes <- rep(0,NUM_VARS)
        unique_value_list <- vector(length = NUM_VARS, mode = "list")
        #populate the list
        for(i in 1:NUM_VARS){
            colindex <- which(colnames(metadata)==variable_names[i])
			if(length(colindex) == 0){
				stop(paste("You have specified a covariate:",variable_names,"that does not exist!"))
			}
            colindexes[i] <- colindex
            unique_value_list[[i]] <- unique(metadata[,colindex])
        }
    }#end multivariate conditional statement

    if(UNIVARIATE){
        use <- NULL
        for(j in 1:length(unique_values)){
            catsum <- length(which(metadata[,colindex] == unique_values[j]))
            if(catsum >= threshold){
                use <- c(use,j)
            }
            cat("Variable:",variable_names,"Category:",unique_values[j],"Num Obs:",catsum,"\n")
        }
		if(length(use) == 0){
			stop(paste("You have specified a threshold such that there are no valid categories of variable:",variable_names))
		}
		unique_values <- unique_values[use]
		ret <- list(indexes = use,
		            values = unique_values)
    }else{
        uselist <- vector(length = NUM_VARS, mode = "list")
        for(i in 1:NUM_VARS){
            use <- NULL
            colindex <- colindexes[i]
            unique_values <- unique_value_list[[i]]
            for(j in 1:length(unique_values)){
                catsum <- length(which(metadata[,colindex] == unique_values[j]))
                if(catsum >= threshold[i]){
                    use <- c(use,j)
                }
                cat("Variable:",variable_names[i],"Category:",unique_values[j],"Num Obs:",catsum,"\n")
            }
			if(length(use) == 0){
				stop(paste("You have specified a threshold such that there are no valid categories of variable:",variable_names[i]))
			}
            uselist[[i]] <- use
			unique_value_list[[i]] <- unique_value_list[[i]][use]
        }
        ret <- list(indexes = uselist,
                    values = unique_value_list)
    }

    #now return  a list of unique values and their indexes
    return(ret)
}
