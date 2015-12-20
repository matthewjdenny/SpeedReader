pmi <- function(contingency_table,
                          to_print = 20,
                          threshold = 5,
                          EVERY_CATEGORY_COUNTS = F){

    Names <- rownames(contingency_table)
    Terms <- colnames(contingency_table)
    categories <- length(contingency_table[,1])
    check  <- function(index){
        return(length(which(temp == keep1[index])))
    }
    print(paste("Thresholding for terms that appear at least",threshold,"times") )

    if(EVERY_CATEGORY_COUNTS){
        for(i in 1:categories){
            print(i)
            if(i == 1){
                keep1 <- which(contingency_table[1,] >= threshold)
            }else{
                temp <- which(contingency_table[i,] >= threshold)
                temp2 <- unlist(sapply(1:length(keep1),check))
                keep1 <- keep1[which(temp2 > 0)]
            }
        }
    }else{
        colsums <- apply(contingency_table,2,sum)
        keep1 <- which(colsums >= threshold)
    }

    contingency_table <- contingency_table[,keep1]
    cat("Reduced Number of Columns in contingency Table:",ncol(contingency_table), "\n")

    #allocate tables
    unique_terms <- length(contingency_table[1,])
    table_sum <- sum(contingency_table)
    colsums <- apply(contingency_table,2,sum)
    rowsums <- apply(contingency_table,1,sum)
    pmi_table <- matrix(0,nrow = categories,ncol = unique_terms )
    distinctiveness_table <- matrix(0,nrow = categories,ncol = unique_terms )
    saliency_table <- matrix(0,nrow = categories,ncol = unique_terms )

    #generate tables
    print("generating token pmi tables")
    for(i in 1:length(contingency_table[,1])){
        print(i)
        for(j in 1:length(contingency_table[1,])){
            pmi_table[i,j] <- log((contingency_table[i,j]/table_sum)/((colsums[j]/table_sum)*(rowsums[i]/table_sum)))
            distinctiveness_table[i,j] <- (contingency_table[i,j]/colsums[j])*log((contingency_table[i,j]/colsums[j])/(rowsums[i]/table_sum))
            saliency_table[i,j] <- (colsums[j]/table_sum)*distinctiveness_table[i,j]
        }
    }

    Terms <- Terms[keep1]


    ## get token top and bottom words
    top_terms <- matrix(0,nrow = categories,ncol = length(pmi_table[1,]) )
    for(i in 1:categories){
        top_terms[i,] <- order(pmi_table[i,],decreasing = T)
    }
    pmi_ranked_terms <- matrix("",nrow = categories,ncol = length(pmi_table[1,]) )
    for(i in 1:categories){
        for(j in 1:length(pmi_table[1,])){
            pmi_ranked_terms[i,j] <- Terms[top_terms[i,j]]
        }
    }
    ranked_pmi <-  matrix(0,nrow = categories,ncol = length(pmi_table[1,]) )
    for(i in 1:categories){
        ranked_pmi[i,] <- pmi_table[i,top_terms[i,]]
    }
    print("Top terms By Category:")


    for(i in 1:categories){
        cat("Category: ",Names[i], "\n")

        for(j in 1:to_print){
            cat(Terms[top_terms[i,j]],"    ePMI:",exp(pmi_table[i,top_terms[i,j]]), ": Local Count --",contingency_table[i,top_terms[i,j]], "Global Count --",sum(contingency_table[,top_terms[i,j]]),"\n")
        }
        cat("\n\n")
    }

    distinctiveness <- apply(distinctiveness_table,2,sum)
    saliency <- apply(saliency_table,2,sum)

    dist_terms <- Terms[order(distinctiveness ,decreasing = T)]
    non_dist_terms <- Terms[order(distinctiveness ,decreasing = F)]

    sal_terms <- Terms[order(saliency ,decreasing = T)]
    non_sal_terms <- Terms[order(saliency ,decreasing = F)]

    print("High Distinctiveness")
    for(i in 1:to_print){
        cat(dist_terms[i], ", ")
    }
    cat("\n\n")

    print("Low Distinctiveness")
    for(i in 1:to_print){
        cat(non_dist_terms[i], ", ")
    }
    cat("\n\n")


    print("High Salience")
    for(i in 1:to_print){
        cat(sal_terms[i], ", ")
    }
    cat("\n\n")
    print("Low Salience")
    for(i in 1:to_print){
        cat(non_sal_terms[i], ", ")
    }
    cat("\n\n")

    return(list(pmi_table, pmi_ranked_terms,ranked_pmi,dist_terms, non_dist_terms, sal_terms, non_sal_terms, Names, Terms ))
}
