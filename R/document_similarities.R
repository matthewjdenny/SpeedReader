#' @title Calculate sequence based document similarities
#' @description Calculates a number of similarity and difference statistics
#' between two document versions based on n-gram sequence matching
#'
#' @param filenames An optional character vector of filenames (with .txt
#' extension), one per document. One of filenames and documents must be provided.
#' @param documents An optional character vector of documents with one entry per
#'  document. One of filenames and documents must be provided.
#' @param input_directory If filenames are provided, then a valid directory path
#' to the directory where the document text files are located must be provided.
#' @param ngram_size The length of n-grams on which to base comparisons. Defaults
#' to 10, but 5 may be appropriate if stopwords have been removed.
#' @param  output_directory An optional directory where chunks of results will be
#' saved in the form: Document_Similarity_Results_x.RData. If NULL, then a
#' data.frame is returned from the function
#' @param doc_pairs An optional two column matrix indicating the document
#' indicies in filenames or documents to be compared in each comparison. This
#' will be automatically generated to include all pairs but, can be user
#' specified if only a subset of pairs are desired. If providing filenames, it
#' is also possible to use the filenames to generate this matrix, but this will
#' provide slower performance.
#' @param cores The number of cores to be used for parallelization. Defaults to
#' 1 but can be any number less than or equal to the number of logical cores
#' available on your computer.
#' @param max_block_size Defaults to NULL, but can be set to an integer value
#' indicating the maximum number of pairs to be compared in each parallel
#' process. Can be useful to limit the intermediate data.frame sizes. A maximum
#' of 10-50 million is suggested.
#' @param prehash Logical which defaults to FALSE. If TRUE, then a pre-hashing
#' scheme is used which may greatly speed up computation but dramatically
#' increase memory usage as well.
#' @param ngram_match_only Defaults to FALSE. If TRUE, then only the proportion
#' of n-grams in version a that are also present in version b and vice-versa
#' are calculated. Can be a useful first step when searching for near-exact
#' matches.
#' @param document_block_size Overrides other arguments, breaks up documents
#' into `document_block_size` chunks to be compared. This argument is suggested
#' if a large number of comparisons are to be completed. By only comparing
#' subsets of documents at a time, the process is memory-optimized so that very
#' large sets of documents can be compared. Automatically sets `prehash = TRUE`,
#' `doc_pairs = NULL`, and suggests that `output_directory` be set (due to the
#' large number of comparisons, it is likely the resulting data.frame will be
#' too large to hold in memory). Defaults to NULL.
#' @param add_ngram_comparisons Defaults to NULL, but can optionally be a
#' numeric vector containing n-gram sizes on which to compare documents. If this
#' argument is provided, then a_in_b and b_in_a comparisons will be appended to
#' the output for each n-gram size.
#' @param unigram_similarity_threshold Defaults to NULL. If not NULL, can be any
#' number greater than 0 and less than 1. This argument allows the user to first
#' filter potential document comparisons to those where atleast one of the
#' documents contains more than unigram_similarity_threshold proportion of the
#' unigrams in the other version. So for example if this argument were set to
#' 0.8, then only those documents with a unigram similarity of 0.8 would be
#' given a full comparison. This approach is particularly useful if one is
#' looking for very similar documents, such as hitchhiker bills.
#' @param doc_lengths Defaults to NULL. If not NULL, then this must be a numeric
#' vector of length equal to the number of input documents, giving the number of
#' tokens in each.
#' @return A data.frame or NULL if output_directory is not NULL.
#' @export
document_similarities <- function(filenames = NULL,
                                  documents = NULL,
                                  input_directory = NULL,
                                  ngram_size = 10,
                                  output_directory = NULL,
                                  doc_pairs = NULL,
                                  cores = 1,
                                  max_block_size = NULL,
                                  prehash = FALSE,
                                  ngram_match_only = FALSE,
                                  document_block_size = NULL,
                                  add_ngram_comparisons = NULL,
                                  unigram_similarity_threshold = NULL,
                                  doc_lengths = NULL) {

    # start timing
    ptm <- proc.time()

    parallel <- FALSE
    if (cores > 1) {
        parallel <- TRUE
    }

    # check the input parameters
    if (is.null(filenames) & is.null(documents)) {
        stop("One of filenames and documents must be a non-null character vector...")
    }

    if (!is.null(filenames) & !is.null(documents)) {
        stop("Only one of filenames and documents may be non-null...")
    }

    if (!is.null(filenames) & is.null(input_directory)) {
        stop("If filenames is non-null, input_directory must be non-null ...")
    }

    if (!is.null(unigram_similarity_threshold)) {
        if (!is.numeric(unigram_similarity_threshold)) {
            stop("unigram_similarity_threshold must be a number between 0 and 1!" )
        }
        if (unigram_similarity_threshold >= 1 | unigram_similarity_threshold <= 0 ) {
            stop("unigram_similarity_threshold must be a number between 0 and 1!" )
        }
        if (!prehash) {
            prehash <- TRUE
            cat("Because unigram_similarity_threshold was set, setting prehash = TRUE.\n")
        }
    }

    # make sure that these are numeric and rounded:
    if (!is.null(add_ngram_comparisons)) {
        if (class(add_ngram_comparisons) != "numeric" &
            class(add_ngram_comparisons) != "integer") {
            stop("add_ngram_comparisons must be a numeric vector, e.g. c(1,2,3).")
        }
        # make sure that we have whole numbers:
        add_ngram_comparisons <- as.numeric(add_ngram_comparisons)
        add_ngram_comparisons <- round(add_ngram_comparisons)
    }

    # determine the number of docs
    if (is.null(filenames)){
        using_files <- FALSE
        num_docs <- length(documents)
    } else {
        using_files <- TRUE
        num_docs <- length(filenames)
    }

    # Deal with the case where we are automatically blocking:
    if (!is.null(document_block_size)) {
        if (!prehash) {
            prehash <- TRUE
            cat("Because document_block_size was set, setting prehash = TRUE.\n")
        }
        if (is.null(output_directory)) {
            warning("output_directory should be provided when document_block_size is used due to the large number of comparisons. If more than 2.2 billion comparisons would be generated, this will cause R to crash...")
        }
        if (!is.null(doc_pairs)) {
            doc_pairs <- NULL
            cat("Because document_block_size was set, setting doc_pairs = NULL.\n")
        }

        if (class(document_block_size) != "numeric") {
            stop("document_block_size must be a single number of class numeric")
        }
        if (document_block_size < 10) {
            stop("document_block_size should be between 1000 and 20,000 for most applications. Specified size is too small.")
        }

        # Here we need to split up the blocks we are working with and then call
        # document_similarities() recursively.
        num_chunks <- ceiling(num_docs/document_block_size)

        # we will use ret as a blank object to rbind onto
        ret <- NULL
        chunk_counter <- 1
        for (k in 1:num_chunks) {
            # get the doc indices of the first block
            strt <- ((k - 1) * document_block_size) + 1
            nd <- min(k * document_block_size, num_docs)
            first_chunk <- strt:nd
            # only do the lower diagonal comparisons:
            for (l in 1:k) {
                # let the user know where the block process is going...
                cat("\n\nCurrently comparing block",k,"to block",l,"\n\n")

                if (l == k) {
                    comps <- t(combn(first_chunk,2))
                } else {
                    # get the doc indices of second block
                    strt <- ((l - 1) * document_block_size) + 1
                    nd <- min(l * document_block_size, num_docs)
                    second_chunk <- strt:nd
                    comps2 <- data.frame(expand.grid(list(first = first_chunk,
                                                          second = second_chunk)))
                    comps2 <- as.matrix(comps2)
                    # need to do a column switch so the right document is first
                    comps <- comps2
                    comps[,1] <- comps2[,2]
                    comps[,2] <- comps2[,1]

                }

                # now run the comparison:
                cur_results <- document_similarities(
                    filenames = filenames,
                    documents = documents,
                    input_directory = input_directory,
                    ngram_size = ngram_size,
                    output_directory = NULL, #set to null as we want to return to this process.
                    doc_pairs = comps, # use the current doc pairs
                    cores = cores,
                    max_block_size = max_block_size,
                    prehash = prehash,
                    ngram_match_only = ngram_match_only,
                    document_block_size = NULL,
                    add_ngram_comparisons = add_ngram_comparisons,
                    unigram_similarity_threshold = unigram_similarity_threshold,
                    doc_lengths = doc_lengths)

                # now either rbind the results or
                if (is.null(output_directory)) {
                    ret <- rbind(ret,cur_results)
                } else {
                    setwd(output_directory)
                    save(ret, file = paste("Document_Similarity_Results_",chunk_counter,".RData",sep = ""))
                }

                # increment the chunk counter
                chunk_counter <- chunk_counter + 1
            }
        }

    } else {
        # create document pairs matrix
        if (is.null(doc_pairs)) {
            doc_pairs <- t(combn(1:num_docs,2))
        } else {
            if (ncol(doc_pairs) < 2) {
                stop("doc_pairs must have two columns (one for the index or file name of each document), and one row for each pair to be compared.")
            }
            if (ncol(doc_pairs) > 2) {
                cat("Only using the first two columns of doc_pairs...\n")
                doc_pairs <- doc_pairs[,c(1,2)]
            }

            # now make sure that doc_pairs is a matrix.
            doc_pairs <- as.matrix(doc_pairs)

            # if we are using file names, then convert the filename pairs
            # to numeric indices. Only do this if we detect that the first column
            # of the matrix is of class character.
            if (using_files) {
                if (class(doc_pairs[1,1]) == "character") {
                    cat("Converting document name pairs to numeric indices. This",
                        "may take a while... It is more efficient to provide a",
                        "matrix with document index pairs.\n")
                    temp <- matrix(0,
                                   nrow = nrow(doc_pairs),
                                   ncol = ncol(doc_pairs))

                    # loop through and repopulate:
                    for (i in 1:nrow(doc_pairs)) {
                        for (j in 1:ncol(doc_pairs)) {
                            temp[i,j] <- which(filenames == doc_pairs[i,j])
                        }
                    }

                    # replace with the index version.
                    doc_pairs <- temp
                }
            }
        }


        # determine how many chunks should be generated
        if (is.null(max_block_size)) {
            # split up the blocks evenly among cores
            block_size <- ceiling(nrow(doc_pairs)/cores)
        } else {
            block_size <- max_block_size
        }


        start_stop_lookup <- NULL
        start <- 1
        stop <- block_size
        done <- FALSE
        # populate a lookup matrix
        while (!done) {
            if (start > nrow(doc_pairs)) {
                break
            } else {
                if (stop >= nrow(doc_pairs)) {
                    stop <- nrow(doc_pairs)
                    done <- TRUE
                }
                cur <- c(start,stop)
                start_stop_lookup <- rbind(start_stop_lookup,cur)
                start <- stop + 1
                stop <- stop + block_size
            }
        }

        start_stop_lookup <- as.matrix(start_stop_lookup)

        if (parallel) {
            vec <- 1:nrow(start_stop_lookup)
            cat("Comparing",nrow(doc_pairs),"document pairs on", cores,
                "cores with block size", block_size,". This may take a while...\n")

            # initializes snowfall session
            cl <- parallel::makeCluster(getOption("cl.cores", cores))

            results <- parallel::clusterApplyLB(
                cl = cl,
                x = vec,
                fun = parallel_sequence_matching,
                start_stop_lookup = start_stop_lookup,
                input_directory = input_directory,
                filenames = filenames,
                doc_pairs = doc_pairs,
                ngram_size = ngram_size,
                output_directory = output_directory,
                documents = documents,
                prehash = prehash,
                ngram_match_only = ngram_match_only,
                add_ngram_comparisons = add_ngram_comparisons,
                unigram_similarity_threshold = unigram_similarity_threshold,
                doc_lengths = doc_lengths)

            # stop the cluster when we are done
            parallel::stopCluster(cl)

            cat("Calculating similarities complete at:",proc.time(),"\n")

            ret <- NULL
            # put in a whole bunch of checks to make sure we do not rbind
            # together things with wrong number of columns or no rows:
            if (is.null(output_directory)){
                for (i in 1:length(results)) {
                    if (ncol(results[[i]]) > 0 & nrow(results[[i]]) > 0) {
                        if (i > 1 & !is.null(ret)) {
                            if (ncol(results[[i]]) == ncol(ret)) {
                                ret <- rbind(ret,results[[i]])
                            }
                        } else {
                            ret <- rbind(ret,results[[i]])
                        }
                    }
                }
            }

        } else {
            ret <- NULL
            #start_stop_lookup <- matrix(c(1,nrow(doc_pairs)),ncol = 2)
            for (i in 1:nrow(start_stop_lookup)) {
                cat("Currently working on block:",i,"of",nrow(start_stop_lookup),"\n")
                temp <- parallel_sequence_matching(
                    x = i,
                    start_stop_lookup = start_stop_lookup,
                    input_directory = input_directory,
                    filenames = filenames,
                    doc_pairs = doc_pairs,
                    ngram_size = ngram_size,
                    output_directory = output_directory,
                    documents = documents,
                    prehash = prehash,
                    ngram_match_only = ngram_match_only,
                    add_ngram_comparisons = add_ngram_comparisons,
                    unigram_similarity_threshold = unigram_similarity_threshold,
                    doc_lengths = doc_lengths)
                ret <- rbind(ret,temp)
            }

        }
    }

    t2 <- proc.time() - ptm
    cat("Complete in:",t2[[3]],"seconds...\n")

    return(ret)
}
