#' Only to be used internally. A function to generate a sparse large document term matrix in parallel.
#'
#' @param file The path to a block of document term vectors.
#' @param aggregate_vocabulary Defaults to aggregate_vocabulary. This is set internally inside the generate_sparse_large_document_term_matrix() function.
#' @return A sparse document term matrix object.
#' @export
sparse_doc_term_parallel <- function(file,
                                     aggregate_vocabulary = aggregate_vocabulary){
    load(file)
    current_document_lengths <- unlist(lapply(document_term_vector_list, length))
    cat("Total terms in current block:",sum(current_document_lengths),"\n")
    current_dw <- generate_document_term_matrix(document_term_vector_list,
                                                vocabulary = aggregate_vocabulary,
                                                document_term_count_list = document_term_count_list)

    #turn into simple triplet matrix and rbind to what we already have
    cat(str(current_dw),"\n")
    current_dw <- slam::as.simple_triplet_matrix(current_dw)
    cat(str(current_dw),"\n")
    return(current_dw)
}
