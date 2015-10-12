generate_contingency_table  <- function(metadata,
                                        doc_word_matrix,
                                        vocab ,
                                        variable_indexes = NA,
                                        use = "all",
                                        variable_names = NA,
                                        USE_INDEXES = FALSE){
    #     metadata -- a matrix containing metadata abotu every bill
    #     doc_word_matrix -- a documents (row) by word types (columns) matrix
    #     variable_indexes= NA -- if we are using numeric indexing then the indexes of each variable to use
    #     use = "all"  -- a list containing the indexes of the unique independent variables we want to use for constructing the contingency table.
    #     variable_names = NA -- a vector of names to be used to get variables
    #     vocab = the vocab associated with the doc-word matrix

    Num_Docs = nrow(doc_word_matrix)
    Vocab_Size = ncol(doc_word_matrix)

    #determine which kind of variable indexing we are doing and how many variables we are creating a contingency table over
    if(USE_INDEXES){
        USE_NUMERIC = TRUE
        if(length(variable_indexes) == 1){
            UNIVARIATE = TRUE
        }else{
            UNIVARIATE = FALSE
            NUM_VARS = length(variable_indexes)
        }
    }else{
        USE_NUMERIC = FALSE
        if(length(variable_names) == 1){
            UNIVARIATE = TRUE
        }else{
            UNIVARIATE = FALSE
            NUM_VARS = length(variable_names)
        }
    }

    cat("Getting Unique Values...\n")
    if(USE_NUMERIC){
        #if we are only making a 1 variable contingency table
        if(UNIVARIATE){
            colindex = variable_indexes
            if(use == "all"){
                unique_values <- unique(metadata[,variable_indexes])
            }else{
                unique_values <- unique(metadata[,variable_indexes])
                unique_values  <- unique_values[use]
            }
            #if we are making a multi-variate contingency table
        }else{
            unique_value_list <- vector(length = NUM_VARS, mode = "list")
            colindexes = variable_indexes
            #populate the list
            for(i in 1:NUM_VARS){
                if(length(use[[i]])== 1){
                    unique_value_list[[i]] <- unique(metadata[,variable_indexes[i]])
                }else{
                    unique_values <- unique(metadata[,variable_indexes[i]])
                    unique_value_list[[i]] <- unique_values[use[[i]]]
                }
            }
        }#end multivariate conditional statement
        #if we are using names
    }else{
        #if we are only making a 1 variable contingency table
        if(UNIVARIATE){
            colindex <- which(colnames(metadata)==variable_names)
            if(use == "all"){
                unique_values <- unique(metadata[,colindex])
            }else{
                unique_values <- unique(metadata[,colindex])
                unique_values  <- unique_values[use]
            }
            #if we are making a multi-variate contingency table
        }else{
            colindexes <- rep(0,NUM_VARS)
            unique_value_list <- vector(length = NUM_VARS, mode = "list")
            #populate the list
            for(i in 1:NUM_VARS){
                colindex <- which(colnames(metadata)==variable_names[i])
                colindexes[i] <- colindex
                if(length(use[[i]]) == 1){
                    unique_value_list[[i]] <- unique(metadata[,colindex])
                }else{
                    unique_values <- unique(metadata[,colindex])
                    unique_value_list[[i]] <- unique_values[use[[i]]]
                }
            }
        }#end multivariate conditional statement
    }#end of finding unique values for contingency table conditional

    #testing
    #unique_value_list<- list(c(1,2,3),c("dog","cat","Fish"),c(1,2,3,4,5,6,7))
    cat("Building Unique Values Lookup... \n")
    #determine number of rows in contingency table and build a category combination lookup if necessary
    if(UNIVARIATE){
        Num_Categories = length(unique_values)
        contingency_table <- matrix(0,ncol = Vocab_Size,nrow = Num_Categories)
    }else{
        Num_Categories = length(unique_value_list[[1]])
        for(i in 2:NUM_VARS){
            Num_Categories = Num_Categories*length(unique_value_list[[i]])
        }
        contingency_table <- matrix(0,ncol = Vocab_Size,nrow = Num_Categories)
        cat("The contingency table has",Num_Categories,"rows and",Vocab_Size,"columns. \n")

        #now generate facotrial category names and lookup talbe
        times_repeat <- rep(1,NUM_VARS)
        for(i in 1:(NUM_VARS-1)){
            for(j in 1:i){
                times_repeat[j] = times_repeat[j]*length(unique_value_list[[i+1]])
            }
        }
        cat_list <- vector(length = NUM_VARS, mode = "list")
        for(i in 1:NUM_VARS){
            cat_vec <- rep(unique_value_list[[i]][1],times_repeat[i])
            for(j in 2:length(unique_value_list[[i]])){
                cat_vec <- c(cat_vec,rep(unique_value_list[[i]][j],times_repeat[i]))
            }
            len <- Num_Categories/length(cat_vec)
            cat_list[[i]] <- rep(cat_vec,len)
        }

        #now cbind everything together
        Category_Combination_Lookup <- cat_list[[1]]
        for(i in 2:NUM_VARS){
            Category_Combination_Lookup <- data.frame(Category_Combination_Lookup ,cat_list[[i]],stringsAsFactors = F)
        }

        #now populate cateogry names vector
        Cateogry_Names <- rep("",Num_Categories)
        for(i in 1:Num_Categories){
            Cateogry_Names[i] <- paste0(Category_Combination_Lookup[i,],collapse = "_")
        }
    }#end_multivariate


    cat("Compiling Contingency Table...\n")

    #populate contingency tables
    if(UNIVARIATE){
        Cateogry_Names <- unique_values
        for(i in 1:Num_Categories){
            cat("Currently compiling contingency table row for:",unique_values[i],"\n")
            indexes <- which(metadata[,colindex] == unique_values[i])
            cur <- doc_word_matrix[indexes,]
            contingency_table[i,] <- col_sums(cur)
        }
        rownames(contingency_table) <- Cateogry_Names
        colnames(contingency_table) <- vocab
        #if multivariate
    }else{
        for(i in 1:Num_Categories){
            cat("Currently compiling contingency table row for:",Cateogry_Names[i],"\n")
            #now generate the conditional contingency tables
            indexes <- which(metadata[,colindexes[1]] == Category_Combination_Lookup[i,1])
            cur <- doc_word_matrix[indexes,]
            met <- metadata[indexes,]
            NONE = FALSE
            for(j in 2:NUM_VARS){
                #if we have not already gotten to an empty category
                if(!NONE){
                    indexes <- which(met[,colindexes[j]] == Category_Combination_Lookup[i,j])
                    if(length(indexes) > 0){
                        cur <- cur[indexes,]
                        met <- met[indexes,]
                    }else{
                        NONE = TRUE
                    }
                }
            }

            if(!NONE){
                cat("There were",nrow(cur),"observations for category:",Cateogry_Names[i],"\n")
                row <- apply(cur,2,sum)
                #cat("length of current colsum:",length(row), "length of contingency", length(contingency_table[i,])) #for debugging
                contingency_table[i,] <- row
            }else{
                cat("There were no observations for category:",Cateogry_Names[i],"\n")
            }
        }
        rownames(contingency_table) <- Cateogry_Names
        colnames(contingency_table) <- vocab
    }

    #return everything
    return(list(contingency_table,vocab,Cateogry_Names))
}
