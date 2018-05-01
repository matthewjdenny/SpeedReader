parallel_sequence_matching <- function(x,
                                       start_stop_lookup,
                                       input_directory,
                                       filenames,
                                       doc_pairs,
                                       ngram_size,
                                       output_directory,
                                       documents,
                                       prehash,
                                       ngram_match_only,
                                       add_ngram_comparisons = NULL,
                                       unigram_similarity_threshold = NULL,
                                       dont_use_lookup = FALSE,
                                       doc_lengths = NULL) {

    document_vector <- FALSE
    if (!is.null(documents[1])) {
        document_vector <- TRUE
        filenames <- rep("",length(documents))
    }

    # logical to see if we should perform comparison:
    perform_comparison <- TRUE

    # subset the lookup based on the index
    if (!dont_use_lookup) {
        start <- start_stop_lookup[x,1]
        stop <- start_stop_lookup[x,2]
        doc_pairs <- doc_pairs[start:stop,]
    }

    # determine which files should be loaded in
    load_inds <- unique(c(doc_pairs[,1],doc_pairs[,2]))
    cat("Reading in",length(load_inds),"documents...\n")

    if (!dont_use_lookup) {
        load_inds <- 1:length(filenames)
    }

    # get the number of comparisons
    num_comp <- nrow(doc_pairs)

    if (prehash) {
        temp_num_comp <- num_comp
        num_comp <- 2
    }

    if (ngram_match_only) {
        ret <- data.frame(prop_a_in_b = rep(0,num_comp),
                          prop_b_in_a = rep(0,num_comp))
    } else {
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
    }


    if (prehash) {
        num_comp <- temp_num_comp
    }

    # set working directory to
    if (!is.null(input_directory)) {
        setwd(input_directory)
    }

    if (prehash) {


        if (is.null(doc_lengths)) {
            cat("Generating document lengths\n")
            docs2 <- rep("",length(filenames))
            doc_lengths <- rep(0,length(filenames))
            for (l in 1:length(filenames)) {
                if (l %in% load_inds) {
                    if (document_vector) {
                        # read in the documents
                        temp <- documents[l]
                    } else {
                        # read in the documents
                        temp <- readLines(filenames[l])
                    }
                    if (length(temp) > 1) {
                        doc <- paste0(temp,collapse = " ")
                    } else {
                        doc <- temp
                    }

                    doc <- stringr::str_replace_all(doc, "[\\s]+", " ")[[1]]
                    doc <- stringr::str_split(doc, " ")[[1]]
                    # docs[[l]] <- doc
                    doc_lengths[l] <- length(doc)
                    doc <- paste0(doc,collapse = " ")
                    docs2[l] <- doc
                } else {
                    doc_lengths[l] <- 0
                }
            }
        } else {
            cat("Document lengths provided\n")
            docs2 <- documents
        }
        #docs <- vector(mode = "list", length = length(filenames))

        cat("Summary of document lengths (unigrams):\n")
        print(summary(doc_lengths))


        # if we are using a threshold, then do the unigram checking here:
        if (!is.null(unigram_similarity_threshold)) {
            ignore_documents <- FALSE
            to_ignore <- c(-1,-1)
            check <- which(doc_lengths == 0)
            if (length(check) > 0) {
                print("The following number documents were removed:")
                print(length(check))
                rem1 <- which(doc_pairs[,1] %in% check)
                rem2 <- which(doc_pairs[,2] %in% check)
                rem <- unique(c(rem1,rem2))
                if (length(rem) > 0) {
                    doc_pairs <- doc_pairs[-rem,]
                }
                # order things to make checking faster
                check <- check[order(check,decreasing = FALSE)]
                ignore_documents <- TRUE
                to_ignore <- check
                to_ignore <- c(to_ignore,-1,-1)
            }

            unigram_check <- Efficient_Block_Hash_Ngrams(
                docs2,
                length(docs2),
                doc_pairs - 1,
                1, # ngram size
                ignore_documents,
                to_ignore - 1)

            # find the indexes of the threshold matches, then subset doc_pairs
            # to only those:
            indsa <- which(unigram_check[,1] >= unigram_similarity_threshold)
            indsb <- which(unigram_check[,2] >= unigram_similarity_threshold)
            inds <- unique(c(indsa,indsb))
            doc_pairs <- doc_pairs[inds,]

            # check to see if there were no matches:
            if (nrow(doc_pairs) < 1) {
                perform_comparison <- FALSE
                # return a dummy dataset:
                ret <- ret[1,]
            }
        }

        # only do the actual comparisons if we found any unigram matches over
        # the threshold.
        if (perform_comparison) {
            if (ngram_match_only) {
                ignore_documents <- FALSE
                to_ignore <- c(-1,-1)
                check <- which(doc_lengths == 0)
                if (length(check) > 0) {
                    print("The following number documents were removed:")
                    print(length(check))
                    rem1 <- which(doc_pairs[,1] %in% check)
                    rem2 <- which(doc_pairs[,2] %in% check)
                    rem <- unique(c(rem1,rem2))
                    if (length(rem) > 0) {
                        doc_pairs <- doc_pairs[-rem,]
                    }
                    # order things to make checking faster
                    check <- check[order(check,decreasing = FALSE)]
                    ignore_documents <- TRUE
                    to_ignore <- check
                    to_ignore <- c(to_ignore,-1,-1)
                }

                cnms <- colnames(ret)
                ret <- Efficient_Block_Hash_Ngrams(
                    docs2,
                    length(docs2),
                    doc_pairs - 1,
                    ngram_size,
                    ignore_documents,
                    to_ignore - 1)
                colnames(ret) <- cnms
                ret <- as.data.frame(ret)
            } else {
                # remove any documents that are shorter than the ngram length and
                # let the user know
                ignore_documents <- FALSE
                to_ignore <- c(-1,-1)
                check <- which(doc_lengths == 0)
                if (length(check) > 0) {
                    print("The following number documents were removed:")
                    print(length(check))
                    rem1 <- which(doc_pairs[,1] %in% check)
                    rem2 <- which(doc_pairs[,2] %in% check)
                    rem <- unique(c(rem1,rem2))
                    if (length(rem) > 0) {
                        doc_pairs <- doc_pairs[-rem,]
                    }
                    # order things to make checking faster
                    check <- check[order(check,decreasing = FALSE)]
                    ignore_documents <- TRUE
                    to_ignore <- check
                    to_ignore <- c(to_ignore,-1,-1)
                }


                cnms <- colnames(ret)
                # ret <- Efficient_Block_Sequential_String_Set_Hash_Comparison(
                #     docs,
                #     length(docs),
                #     doc_pairs - 1,
                #     ngram_size,
                #     ignore_documents,
                #     to_ignore - 1)

                ret <- String_Input_Sequential_String_Set_Hash_Comparison(
                    docs2,
                    length(docs2),
                    doc_pairs - 1,
                    ngram_size,
                    ignore_documents,
                    to_ignore - 1)
                colnames(ret) <- cnms
                ret <- as.data.frame(ret)
            }
        }

    } else {
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
    }

    # again, only add stuff in if we are actually doing a comparison:
    if (perform_comparison) {
        # add in document indicies for later lookups
        ret$doc_1_ind <- doc_pairs[,1]
        ret$doc_2_ind <- doc_pairs[,2]

        # if we are using filenames, then store those as well
        if (!document_vector) {
            ret$doc_1_file <- filenames[doc_pairs[,1]]
            ret$doc_2_file <- filenames[doc_pairs[,2]]
        }

        # now if we want to tack on some n-gram comparisons:
        if (!is.null(add_ngram_comparisons)) {
            for (j in 1:length(add_ngram_comparisons)) {
                cat("Adding n-gram comparison size:",add_ngram_comparisons[j],"\n")
                results <- parallel_sequence_matching(
                    x = x,
                    start_stop_lookup = start_stop_lookup,
                    input_directory = input_directory,
                    filenames = filenames,
                    doc_pairs = doc_pairs,
                    ngram_size = ngram_size,
                    output_directory = output_directory,
                    documents = documents,
                    prehash = prehash,
                    ngram_match_only = TRUE,
                    add_ngram_comparisons = NULL,
                    dont_use_lookup = TRUE)

                # get only the first two columns
                results <- results[,1:2]
                colnames(results) <- c(paste("ngram_",
                                             add_ngram_comparisons[j],
                                             "_prop_a_in_b",sep = ""),
                                       paste("ngram_",
                                             add_ngram_comparisons[j],
                                             "_prop_b_in_a",sep = ""))

                ret <- cbind(ret,results)
            } # end of n-gram size loop.
        }
    }

    # save or return
    if (!is.null(output_directory)) {
        setwd(output_directory)
        save(ret, file = paste("Document_Similarity_Results_",x,".RData",sep = ""))
        return(x)
    } else {
        return(ret)
    }

}

