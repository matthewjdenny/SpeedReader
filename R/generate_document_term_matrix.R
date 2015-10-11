#' A function to generate a document term matrix from a list of document term vectors.
#'
#' @param document_term_vector_list A list of term vectors, one per document, that we wish to turn into a document term matrix.
#' @param vocabulary An optional vocabulary vector which will be used to form the document term matrix. Defaults to NULL, in which case a vocabulary vector will be generated internally.
#' @return A dense document term matrix object with the vocabulary as column names.
#' @export
generate_document_term_matrix <- function(document_term_vector_list,
                                          vocabulary = NULL){

    #if a vocabulary was not supplied, then we generate it.
    if(is.null(vocabulary)){
        vocab <- count_words(document_term_vector_list,
                             maximum_vocabulary_size = -1)
        vocabulary <- vocab$unique_words
    }

    number_of_documents <- length(document_term_vector_list)
    number_of_unique_words <- length(vocabulary)
    document_lengths <- unlist(lapply(document_term_vector_list, length))

    document_term_matrix <- Generate_Document_Term_Matrix(
        number_of_documents,
        number_of_unique_words,
        vocabulary,
        document_term_vector_list,
        document_lengths)

    colnames(document_term_matrix) <- vocabulary

    return(document_term_matrix)
}
