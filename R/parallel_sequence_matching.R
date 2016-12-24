parallel_sequence_matching <- function(x,
                                       start_stop_lookup,
                                       input_directory,
                                       filenames,
                                       doc_pairs,
                                       ngram_size,
                                       output_directory,
                                       documents) {

    document_vector <- FALSE
    if (!is.null(documents[1])) {
        document_vector <- TRUE
    }

    # subset the lookup based on the index
    start <- start_stop_lookup[x,1]
    stop <- start_stop_lookup[x,2]
    doc_pairs <- doc_pairs[start:stop,]

    # get the number of comparisons
    num_comp <- nrow(doc_pairs)

    # create a blank data.frame to store results
    ret <- data.frame(addition_granularity = rep(0,num_comp),
                      deletion_granularity = rep(0,num_comp),
                      addition_scope = rep(0,num_comp),
                      deletion_scope = rep(0,num_comp),
                      average_addition_size = rep(0,num_comp),
                      average_deletion_size = rep(0,num_comp),
                      scope = rep(0,num_comp),
                      average_edit_size = rep(0,num_comp),
                      prop_deletions = rep(0,num_comp),
                      prop_additions  = rep(0,num_comp),
                      prop_changes = rep(0,num_comp),
                      num_match_blocks_v1 = rep(0,num_comp),
                      max_match_length_v1 = rep(0,num_comp),
                      min_match_length_v1 = rep(0,num_comp),
                      mean_match_length_v1 = rep(0,num_comp),
                      median_match_length_v1 = rep(0,num_comp),
                      match_length_variance_v1 = rep(0,num_comp),
                      num_nonmatch_blocks_v1 = rep(0,num_comp),
                      max_nonmatch_length_v1 = rep(0,num_comp),
                      min_nonmatch_length_v1 = rep(0,num_comp),
                      mean_nonmatch_length_v1 = rep(0,num_comp),
                      median_nonmatch_length_v1 = rep(0,num_comp),
                      nonmatch_length_variance_v1 = rep(0,num_comp),
                      total_ngrams_v1 = rep(0,num_comp),
                      num_match_blocks_v2 = rep(0,num_comp),
                      max_match_length_v2 = rep(0,num_comp),
                      min_match_length_v2 = rep(0,num_comp),
                      mean_match_length_v2 = rep(0,num_comp),
                      median_match_length_v2 = rep(0,num_comp),
                      match_length_variance_v2 = rep(0,num_comp),
                      num_nonmatch_blocks_v2 = rep(0,num_comp),
                      max_nonmatch_length_v2 = rep(0,num_comp),
                      min_nonmatch_length_v2 = rep(0,num_comp),
                      mean_nonmatch_length_v2 = rep(0,num_comp),
                      median_nonmatch_length_v2 = rep(0,num_comp),
                      nonmatch_length_variance_v2 = rep(0,num_comp),
                      total_ngrams_v2 = rep(0,num_comp))

    # set working directory to
    if (!is.null(input_directory)) {
        setwd(input_directory)
    }


    # now we loop through the pairings
    for (j in 1:num_comp) {

        # progress
        if (j %% 10000 == 0) {
            cat("Comparison",j,"of",num_comp,"\n")
        }

        if (document_vector) {
            # read in the documents
            document_1 <- documents[doc_pairs[j,1]]
            document_2 <- documents[doc_pairs[j,2]]
        } else {
            # read in the documents
            document_1 <- readLines(filenames[doc_pairs[j,1]])
            document_2 <- readLines(filenames[doc_pairs[j,2]])
        }

        # do the matching
        results <- ngram_sequence_matching(
            document_1,
            document_2,
            ngram_size = ngram_size,
            use_hashmap = TRUE,
            tokenized_strings_provided = FALSE)

        # store
        ret[j,] <- results$match_sequence_statistics

    }

    # add in document indicies for later lookups
    ret$doc_1_ind <- doc_pairs[,1]
    ret$doc_2_ind <- doc_pairs[,2]

    # if we are using filenames, then store those as well
    if (!document_vector) {
        ret$doc_1_file <- filenames[doc_pairs[,1]]
        ret$doc_2_file <- filenames[doc_pairs[,2]]
    }

    # save or return
    if (!is.null(output_directory)) {
        setwd(output_directory)
        save(ret, file = paste("Document_Similarity_Results_",x,".RData",sep = ""))
        return(i)
    } else {
        return(ret)
    }

}

