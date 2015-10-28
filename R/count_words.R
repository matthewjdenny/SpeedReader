#' A function to efficiently form aggregate word counts and a common vocabulary vector from an unordered list of document term vectors.
#'
#' @param document_term_vector_list A list of string vectors (or a single string vector) from which we wish to find a unique vocabulary and counts for all unique words.
#' @param maximum_vocabulary_size A number larger than maximum vocabulary size we expect to find. Defaults to 1,000,000 but can be adjusted appropriately to conserve memory, or if more unique words are expected. The reason we specify this number beforehand is becasue all word count vectors are pre-allocated to improve performance over a growing vector.
#' @param existing_vocabulary An existing vocabulary vector we wish to add to. Defaults to NULL in which case a new word count and vocabulry is generated.
#' @param existing_word_counts A vector of existing word counts that must also be provided if we are specifying existing_vocabulary. Defaults to NULL in which case a new word count and vocabulry is generated.
#' @param document_term_count_list A list of vectors of word counts can optionally be provided, in which case we will aggregate over them. This can be useful if we wish to store documents in a memory efficent way. Defaults to NULL.
#' @return A list object with a unique_words field containing a vector of all unique word types, in descending order of their frequency, as well as a word_counts field containing word counts for each of those words, in the same order, and a total_unique_words field -- the size of the vocabulary.
#' @export
count_words <- function(document_term_vector_list,
                        maximum_vocabulary_size = 1000000,
                        existing_vocabulary = NULL,
                        existing_word_counts = NULL,
                        document_term_count_list = NULL){

    if(typeof(document_term_vector_list) == "character"){
        document_term_vector_list <- list(document_term_vector_list)
    }else if(typeof(document_term_vector_list) != "list"){
        stop("document_term_vector_list must be a list object containing character vectors or a single character vector.")
    }

    # allocate internal variables
    number_of_documents <- length(document_term_vector_list)

    # get the document lengths
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
            stop("document_term_count_list and document_term_vector_list must be the same length.")
        }
    }else{
        document_term_count_list <- as.list(rep(0,number_of_documents))
    }

    #if we are adding to a current vocabulary, then initialize everything
    if(!is.null(existing_vocabulary) & !is.null(existing_word_counts)){
        add_to_vocabulary <- 1
        existing_vocabulary <- as.character(existing_vocabulary)
        existing_word_counts <- as.numeric(existing_word_counts)
        existing_vocabulary_size <- length(existing_vocabulary)
    }else{
        # make sure we pass in something valid
        add_to_vocabulary <- 0
        existing_vocabulary <- rep("ERROR",2)
        existing_word_counts <- rep(0,2)
        existing_vocabulary_size <- 0
    }

    # if maximum_vocabulary_size == -1 then set to the sum of all tokens in
    # all documents.
    if(maximum_vocabulary_size == -1){
        maximum_vocabulary_size <- sum(document_lengths) + existing_vocabulary_size
        if(maximum_vocabulary_size > 2147000000){
            maximum_vocabulary_size <- 2147000000
            warning("You have specified a vocabulary size of greater than 2,147,000,000. R can only handle vectors of length 2,147,483,648 so considder switching to another programming language to simply using C++. If you beleive your actual vocabulary size is smaller than this, then set it manually.")
        }
    }

    print_counter = 0
    if(length(number_of_documents) > 1){
        print_counter = 1
    }

    counts <- Count_Words(number_of_documents,
                          document_term_vector_list,
                          document_lengths,
                          maximum_vocabulary_size,
                          add_to_vocabulary,
                          existing_word_counts,
                          existing_vocabulary,
                          existing_vocabulary_size,
                          using_wordcounts,
                          document_term_count_list,
                          print_counter)

    ordering <- order(counts[[3]],decreasing = TRUE)

    result <- list(unique_words = counts[[2]][ordering],
                   word_counts = counts[[3]][ordering],
                   total_unique_words = counts[[1]])

    #check to make sure that we did not inadvertently run out of space in our initially allocated vector.
    if(counts[[1]] >= (maximum_vocabulary_size -1)){
        stop("You have specified a maximum_vocabulary_size that is too small. Considder increasing it or setting it to -1, in which case the total number of tokens in all documents will be used.")
    }

    # let the user know what they are doing incase they thought they were
    # providing a vocabulary but did not.
    if(existing_vocabulary[1] != "ERROR" & existing_word_counts[1] != 0){
        if(print_counter == 1){
            cat("You provided an existing vocabulary and this was added to.\n")
        }
    }else{
        if(print_counter == 1){
            cat("You did not provide an existing vocabulary, so a new one was created.\n")
        }
    }

    return(result)
}
