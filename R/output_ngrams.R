output_ngrams <- function(NGrams,
                          combine_ngrams,
                          directory){
    setwd(directory)
    # loop over NGrams
    if(length(NGrams) > 50){
        printseq <- round(seq(1,length(NGrams), length.out = 51)[2:51],0)
    } else {
        printseq <- 1:length(NGrams)
    }
    print_counter <- 1
    for (i in 1:length(NGrams)) {
        if (printseq[print_counter] == i) {
            cat(".")
            print_counter <- print_counter + 1
        }
        current <- NGrams[[i]]
        output <- list()
        if (length(names(current$ngrams)) > 0) {
            if (combine_ngrams) {
                for (j in 1:length(current$ngrams)) {
                    if (j == 1) {
                        temp <- paste0(current$ngrams[[j]],collapse = " ")
                    } else {
                        temp <- paste(temp,paste0(current$ngrams[[j]],collapse = " "))
                    }
                }
                output$ngrams <- temp
            } else {
                for (j in 1:length(current$ngrams)) {
                    temp <- paste0(current$ngrams[[j]],collapse = " ")
                    name <- names(current$ngrams)[j]
                    ind <- length(output) + 1
                    output[[ind]] <- temp
                    names(output)[j] <- name
                }
            }
        }
        if (!is.null(current$jk_filtered)) {
            if (!is.na(current$jk_filtered)) {
                output$jk_filtered <- paste0(current$jk_filtered,collapse = " ")
            }
        }

        if (!is.null(current$verb_filtered)) {
            if (!is.na(current$verb_filtered)) {
                output$verb_filtered <- paste0(current$verb_filtered,collapse = " ")
            }
        }

        if (!is.null(current$phrases)) {
            if (!is.na(current$phrases)) {
                output$phrases <- paste0(current$phrases,collapse = " ")
            }
        }

        # if we are on the first iteration create the filenames
        if (i == 1) {
            filenames <- rep("",length(output))
            for (j in 1:length(filenames)) {
                filenames[j] <- paste("NGrams_For_Vocabulary_",
                                      names(output)[j],".txt",sep = "")
            }
        }
        # now write everything to file
        for (j in 1:length(output)) {
            write.table(x = output[[j]],
                        file = filenames[j],
                        sep = "",
                        col.names = FALSE,
                        row.names = FALSE,
                        append = TRUE)
        }
    }
    cat("\n")
    # return the filenames so we can use them
    return(list(filenames = filenames,
                types = names(output)))
}
