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
#' specified if only a subset of pairs are desired.
#' @param parallel Logical indicating whether parallelization should be used.
#' Defaults to FALSE.
#' @param cores The number of cores to be used for parallelization. Defaults to
#' 1 but can be any number less than or equal to the number of logical cores
#' available on your computer.
#' @param max_block_size Defaults to NULL, but can be set to an integer value
#' indicating the maximum number of pairs to be compared in each parallel
#' process. Can be usefull to limit the intermediate data.frame sizes. A maximum
#' of 10-50 million is suggested.
#' @param prehash Logical which defaults to FALSE. If TRUE, then a prehashing
#' scheme is used which may greatly speed up computation but dramatically
#' increase memory usage as well.
#' @return A data.frame or NULL if output_directory is not NULL.
#' @export
document_similarities <- function(filenames = NULL,
                                  documents = NULL,
                                  input_directory = NULL,
                                  ngram_size = 10,
                                  output_directory = NULL,
                                  doc_pairs = NULL,
                                  parallel = TRUE,
                                  cores = 1,
                                  max_block_size = NULL,
                                  prehash = FALSE) {

    # start timing
    ptm <- proc.time()

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

    # determine the number of docs
    if (is.null(filenames)){
        using_files <- FALSE
        num_docs <- length(documents)
    } else {
        using_files <- TRUE
        num_docs <- length(filenames)
    }

    # create document pairs matrix
    if (is.null(doc_pairs)) {
        doc_pairs <- t(combn(1:num_docs,2))
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
            if (stop > nrow(doc_pairs)) {
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

        # intitalizes snowfall session
        cl <- parallel::makeCluster(getOption("cl.cores", cores))

        results <- parallel::clusterApplyLB(cl = cl,
                                            x = vec,
                                            fun = parallel_sequence_matching,
                                            start_stop_lookup = start_stop_lookup,
                                            input_directory = input_directory,
                                            filenames = filenames,
                                            doc_pairs = doc_pairs,
                                            ngram_size = ngram_size,
                                            output_directory = output_directory,
                                            documents = documents,
                                            prehash = prehash)

        # stop the cluster when we are done
        parallel::stopCluster(cl)

        cat ("Calculting similarities complete at:",proc.time(),"\n")

        ret <- NULL
        if (!is.null(output_directory)){
            for (i in 1:length(results)) {
                ret <- rbind(ret,results[[i]])
            }
        }

    } else {
        start_stop_lookup <- matrix(c(1,nrow(doc_pairs)),ncol = 2)
        ret <- parallel_sequence_matching(1,
                                          start_stop_lookup,
                                          input_directory,
                                          filenames,
                                          doc_pairs,
                                          ngram_size,
                                          output_directory,
                                          documents,
                                          prehash)
    }

    t2 <- proc.time() - ptm
    cat("Complete in:",t2[[3]],"seconds...\n")

    return(ret)
}
