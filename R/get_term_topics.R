# function to extract term topics
get_term_topics <- function(num_topics,
                            unzip_command = "gunzip -k") {

    # unzip the file, save it back in the same location as a .zip
    system(paste(unzip_command, "output_state.txt.gz"))

    # Read in the output state (which has token topic counts):
    hyperparameters <- readLines("output_state.txt", n = 3)

    alpha <- stringr::str_split(hyperparameters[2]," ")[[1]]

    alpha <- alpha[-c(1:2)]
    rem <- which(alpha == "")
    if (length(rem > 0)) {
        alpha <- alpha[-rem]
    }

    alpha <- as.numeric(alpha)

    beta <- stringr::str_split(hyperparameters[3]," ")[[1]]

    beta <- beta[-c(1:2)]
    rem <- which(beta == "")
    if (length(rem > 0)) {
        beta <- beta[-rem]
    }

    beta <- as.numeric(beta)

    # Read in the output state (which has token topic counts):
    output <- data.table::fread("output_state.txt",
                                header = F,
                                skip = 3,
                                sep = " ",
                                stringsAsFactors = FALSE)

    cat("\nNumber of columns", ncol(output),"number of rows", nrow(output),"\n")

    cat("Completed reading in token topic assignments...\n")

    # give column names
    colnames(output) <- c("doc", "source", "pos", "typeindex", "type", "topic")

    # reduce to the relevant columns
    output <- output[,c(4:6)]

    # Get the number of unique word types
    num_terms <- length(unique(output$type))

    # create a term-topic matrix
    term_topics <- matrix(0, nrow = num_terms, ncol = num_topics)



    # order the terms by type then by topic assignment
    ordering <- order(output$typeindex,
                      output$topic,
                      decreasing = FALSE)

    output <- output[ordering,]

    utypes <- unique(output$typeindex)

    starts <- match(utypes, output$typeindex)
    stops <- c(starts[-1]-1,length(output$typeindex))

    # create a blank vector to store the terms
    terms <- output$type[starts]

    for (i in 1:length(starts)) {
        if (i %% 10000 == 0) {
            cat("Forming topic-word type counts for word type:",i,"of",length(starts),"\n")
        }
        # generate a table with counts in topic indices
        tab <- table(output$topic[starts[i]:stops[i]])
        # get the topic indices and term counts:
        insert_counts <- as.numeric(tab)
        insert_inds <- as.numeric(names(tab))

        if (length(insert_counts) > 0) {
            term_topics[i, insert_inds+1] <- insert_counts
        }
    }

    # set row and column names
    rownames(term_topics) <- terms
    colnames(term_topics) <- paste("Topic", 1:num_topics,
                                   sep = "_")


    ret <- list(topic_term_counts = term_topics,
                alpha = alpha,
                beta = beta)

    return(ret)


}



