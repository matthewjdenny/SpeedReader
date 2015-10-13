#' A function to generate a document term matrix from a list of document term vectors.
#'
#' @param document_term_vector_list A list of term vectors, one per document, that we wish to turn into a document term matrix.
#' @param vocabulary An optional vocabulary vector which will be used to form the document term matrix. Defaults to NULL, in which case a vocabulary vector will be generated internally.
#' @param document_term_count_list A list of vectors of word counts can optionally be provided, in which case we will aggregate over them. This can be useful if we wish to store documents in a memory efficent way. Defaults to NULL.
#' @return A dense document term matrix object with the vocabulary as column names.
#' @export
generate_document_term_matrix <- function(document_term_vector_list,
                                          vocabulary = NULL,
                                          document_term_count_list = NULL){

    #if a vocabulary was not supplied, then we generate it.
    if(is.null(vocabulary)){
        vocab <- count_words(document_term_vector_list,
                             maximum_vocabulary_size = -1,
                             document_term_count_list)
        vocabulary <- vocab$unique_words
    }

    number_of_documents <- length(document_term_vector_list)
    number_of_unique_words <- length(vocabulary)
    document_lengths <- unlist(lapply(document_term_vector_list, length))

    using_wordcounts <- 0
    # if we are providing word counts
    if(!is.null(document_term_count_list)){
        using_wordcounts <- 1
        if(typeof(document_term_count_list) == "numeric"){
            document_term_count_list <- as.integer(document_term_count_list)
            document_term_count_list <- list(document_term_count_list)
        }else if(typeof(document_term_count_list) == "integer"){
            document_term_count_list <- list(document_term_count_list)
        }else if(typeof(document_term_count_list) != "list"){
            stop("document_term_count_list must be a list object containing integer vectors or a single integer or numeric vector.")
        }
        if(length(document_term_count_list) != length(document_term_vector_list)){
            stop("document_term_vector_list and document_word_list must be the same length.")
        }
    }else{
        document_term_count_list <- as.list(rep(0,number_of_documents))
    }

    document_term_matrix <- Generate_Document_Term_Matrix(
        number_of_documents,
        number_of_unique_words,
        vocabulary,
        document_term_vector_list,
        document_lengths,
        using_wordcounts,
        document_term_count_list)

    colnames(document_term_matrix) <- vocabulary

    return(document_term_matrix)
}
