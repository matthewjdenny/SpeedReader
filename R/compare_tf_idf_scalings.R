#' A function that performs a bunch of different forms of TF-IDF scaling to a
#' document-term matrix.
#'
#' @param document_term_matrix A document-term matrix as a dense or simple
#' triplet matrix, with documents as rows, and vocabulary terms as columns. Note
#' that internally, this function will turn the matrix into a dense matrix, so
#' this function should only be used with moderately sized matrices.
#' @return A list object containing a number of document-term matrices, each
#' with a different form of TF-IDF scaling applied. Each list entry contains a
#' list object with the rescaled doc-term matrix as the second entry, and a
#' description of the method applied as the first entry.
#' @export
compare_tf_idf_scalings <- function(document_term_matrix){

    cat("All formulations are taken from the Wikipedia TF-IDF page...\n")

    # determine whether we are working with a sparse matrix
    if(class(document_term_matrix) == "simple_triplet_matrix"){
        warning("Transforming sparse matrix to dense matrix, this can cause memory overflow issues for large document-term matrices.")
        document_term_matrix <- sparse_to_dense_matrix(document_term_matrix)
    }

    num_doc <- nrow(document_term_matrix)
    vocab_size <- ncol(document_term_matrix)
    binary_doc_term <- document_term_matrix
    binary_doc_term[which(binary_doc_term > 0)] <- 1
    term_doc_counts <- colSums(binary_doc_term)
    # doc_sums <- rowSums(document_term_matrix)
    max_tf_doc <- apply(document_term_matrix,1,max)

    double_normalization <- function(vec, cur_max_tf_doc) {
        vec <- as.numeric(vec)
        for (i in 1:length(vec)) {
            if (vec[i] > 0) {
                vec[i] <- 0.5 + 0.5*(vec[i]/cur_max_tf_doc)
            } else {
                vec[i] <- 0.5
            }
        }
        return(vec)
    }

    # list to save each dtm in
    dtm_list <- list()

    cat("Raw TF...\n")
    # first method, raw term frequency (don't do anything)
    dtm_list$raw_tf <- list(scaling = "raw term frequencies",
                            dtm = document_term_matrix)

    cat("Binary TF...\n")
    # binary indicators for a term in each document
    temp_dtm <- document_term_matrix
    temp_dtm[which(temp_dtm > 0)] <- 1
    dtm_list$binary_tf <- list(scaling = "binary term frequencies",
                               dtm = temp_dtm)

    cat("log normalized TF...\n")
    # log normalized tf
    temp_dtm <- document_term_matrix
    temp_dtm[which(temp_dtm > 0)] <- log(temp_dtm[which(temp_dtm > 0)]) + 1
    dtm_list$log_norm_tf <- list(scaling = "log normalized term frequencies",
                                 dtm = temp_dtm)

    cat("double normalized (0.5) TF...\n")
    # double normalized 0.5
    temp_dtm <- document_term_matrix
    for (i in 1:num_doc) {
        temp_dtm[i,] <- double_normalization(temp_dtm[i,],
                                             max_tf_doc[i])
    }
    dtm_list$double_normalized_tf <- list(
        scaling = "double normalized (0.5) term frequencies",
        dtm = temp_dtm)

    cat("TF IDF...\n")
    # no-log idf
    temp_dtm <- document_term_matrix
    for (i in 1:num_doc) {
        for(j in 1:vocab_size) {
            temp_dtm[i,j] <- temp_dtm[i,j] * (num_doc/(1+term_doc_counts[j]))
        }
    }
    dtm_list$idf <- list(
        scaling = "TF - idf scaling: (N/(1+ df))",
        dtm = temp_dtm)

    cat("TF log IDF...\n")
    # log idf
    temp_dtm <- document_term_matrix
    for (i in 1:num_doc) {
        for(j in 1:vocab_size) {
            if (temp_dtm[i,j]) {
                temp_dtm[i,j] <- temp_dtm[i,j] * log(num_doc/(1+term_doc_counts[j]))
            }
        }
    }
    dtm_list$log_idf <- list(
        scaling = "TF - log idf scaling: log(N/(1+ df))",
        dtm = temp_dtm)

    cat("TF smooth log IDF...\n")
    # smooth log idf
    temp_dtm <- document_term_matrix
    for (i in 1:num_doc) {
        for(j in 1:vocab_size) {
            temp_dtm[i,j] <- temp_dtm[i,j] * log(1 + num_doc/(1 + term_doc_counts[j]))
        }
    }
    dtm_list$smooth_log_idf <- list(
        scaling = "TF - smooth log idf scaling: log(1 + (N/(1+ df)))",
        dtm = temp_dtm)

    cat("max smooth log IDF...\n")
    # max log idf
    temp_dtm <- document_term_matrix
    for (i in 1:num_doc) {
        max_vec <- rep(0,vocab_size)
        for(j in 1:vocab_size) {
            max_vec[j] <- binary_doc_term[i,j]*term_doc_counts[j]
        }
        max_df_i <- max(max_vec)
        for(j in 1:vocab_size) {
            temp_dtm[i,j] <- temp_dtm[i,j] * log(1 + max_df_i/(1 + term_doc_counts[j]))
        }
    }
    dtm_list$max_smooth_log_idf <- list(
        scaling = "TF - smooth log idf scaling with max tf in document: log(1 + (max_{d}df/(1+ df)))",
        dtm = temp_dtm)

    cat("TF probabilitic log IDF...\n")
    # probabilistic log idf
    temp_dtm <- document_term_matrix
    for (i in 1:num_doc) {
        for(j in 1:vocab_size) {
            if (temp_dtm[i,j]) {
                temp_dtm[i,j] <- temp_dtm[i,j] * log((num_doc - term_doc_counts[j])/term_doc_counts[j])
            }
        }
    }
    dtm_list$prob_log_idf <- list(
        scaling = "TF - probabilistic log idf scaling: log((N - df)/df)",
        dtm = temp_dtm)

    return(dtm_list)
}
