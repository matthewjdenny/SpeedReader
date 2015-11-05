#' Only to be used internally. A function to generate a sparse large document term matrix in parallel.
#'
#' @param file The path to a block of document term vectors.
#' @param vocabulary This is set internally inside the generate_sparse_large_document_term_matrix() function.
#' @return A sparse document term matrix object.
#' @export
sparse_doc_term_parallel <- function(file,
                                     vocabulary){
    document_term_vector_list = document_term_count_list = NULL
    load(file)
    current_document_lengths <- unlist(lapply(document_term_vector_list, length))
    cat("Total terms in current block:",sum(current_document_lengths),"\n")
    current_dw <- generate_document_term_matrix(
        document_term_vector_list,
        vocabulary = vocabulary,
        document_term_count_list = document_term_count_list,
        return_sparse_matrix = TRUE)
    return(current_dw)
}
