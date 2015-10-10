#' A function to combine multiple document term matrices into a single aggregate document term matrix.
#'
#' @param document_term_matrix_list A list of document term matricies -- preferrably generated using generate_document_term_matrix(), each of which corresponds to a vocabulary in vocabulary_list.
#' @param vocabulary_list A list of string vectors containing the vocabularies associated with each document term matrix. The j'th entry in each of these vectors should correspond to j'th column in the assoicated document term matrix. Defaults to NULL. If use_column_names_as_vocabularies = TRUE, then vocabularies will be extracted from document term matrices, otherwise these must be provided.
#' @param use_column_names_as_vocabularies Deafults to FALSE, if TRUE then the function will attempt to extract vocabularies from the column names of each document term matrix.
#' @return An aggregate document term matrix with columns named for each word in the vocabulary and columns ordered from most frequently used to least frequently used terms.
#' @export
combine_document_term_matrices <- function(
    document_term_matrix_list,
    vocabulary_list = NULL,
    use_column_names_as_vocabularies = FALSE){

    number_of_corpora <- length(document_term_matrix_list)

    # extract vocabularies from doc-term matrices.
    if(use_column_names_as_vocabularies){
        vocabulary_list <- vector(mode = "list",
                                  length = number_of_corpora)
        for(i in 1:number_of_corpora){
            vocabulary_list[[i]] <- colnames(document_term_matrix_list[[i]])
        }
    }

    if(length(vocabulary_list) != length(document_term_matrix_list)){
        stop("You must provide a document_term_matrix_list and a vocabulary_list with the same number of entries.")
    }

    if(length(document_term_matrix_list) < 2){
        stop("You must provide atleast two document term matrices to combine.")
    }

    # get the unique words
    unique_words <- unique(unlist(vocabulary_list))

    result <- Combine_Document_Term_Matrices(
        document_word_matrix_list,
        vocabulary_list,
        unique_words,
        number_of_corpora)

    # get the counts of each word
    all_word_counts <- apply(result[[1]],2,sum)
    ordering <- order(all_word_counts, decreasing = T)

    # order columns by counts, give columns names.
    return_matrix <- result[[1]][,ordering]
    colnames(return_matrix) <- result[[2]][ordering]

    return(return_matrix)
}
