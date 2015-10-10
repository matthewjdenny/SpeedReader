#' A function to efficiently form aggregate word counts and a common vocabulary vector from an unordered list of document term vectors.
#'
#' @param document_word_list A list of string vectors from which we wish to find a unique vocabulary and counts for all unique words.
#' @param maximum_vocabulary_size A number larger than maximum vocabulary size we expect to find. Defaults to 1,000,000 but can be adjusted appropriately to conserve memory, or if more unique words are expected. The reason we specify this number beforehand is becasue all word count vectors are pre-allocated to improve performance over a growing vector.
#' @param existing_vocabulary An existing vocabulary vector we wish to add to. Defaults to NULL in which case a new word count and vocabulry is generated.
#' @param existing_word_counts A vector of existing word counts that must also be provided if we are specifying existing_vocabulary. Defaults to NULL in which case a new word count and vocabulry is generated.
#' @return A list object with a unique_words field containing a vector of all unique word types, in descending order of their frequency, as well as a word_counts field containing word counts for each of those words, in the same order, and a total_unique_words field -- the size of the vocabulary.
#' @export
count_words <- function(document_word_list,
                        maximum_vocabulary_size = 1000000,
                        existing_vocabulary = NULL,
                        existing_word_counts = NULL){

    # allocate internal variables
    number_of_documents <- length(document_word_list)

    # get the document lengths
    document_lengths <- unlist(lapply(document_word_list, length))

    counts <- Count_Words(number_of_documents,
                          document_word_list,
                          document_lengths,
                          maximum_vocabulary_size)

    ordering <- order(counts[[3]],decreasing = TRUE)

    result <- list(unique_words = counts[[2]][ordering],
                   word_counts = counts[[3]][ordering],
                   total_unique_words = counts[[1]])

    return(result)
}
