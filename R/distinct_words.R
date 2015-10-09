#' A function to find (semi)-distinct words in a list of term vectors.
#'
#' @param word_vector_list A list of character vectors we wish to find distinctive words in.
#' @param threshold An integer > 0 indicating the number of times a word must appear more than to be included in the vector we return. Defaults to threshold = 1, meaning all words that appear 1 or less times in the other term vectors we pass in will be removed from them before they are compared against the current vector. In this way we can get pseudo-distinct words, perhaps preventing us from removing really distinctive words that appear only threshold or less times in most term vectors, but lots of times in one vector in particular.
#' @return A list of distinct word vectors.
#' @export
distinct_words <- function(word_vector_list, 
                           threshold = 1){
  # determine the number of word vectors we are dealing with, by taking the 
  # length of the list tha was passed in
  nvects <- length(word_vector_list)
  # create an index vector
  cur <- 1:nvects
  # create a blank list object we can populate with distinct word vectors
  dist_vects <- vector(mode = "list", length = nvects)
  if(!is.null(names(word_vector_list))){
    names(dist_vects) <- names(word_vector_list)
  }
  # loop over all of the word vectors in 'word_vector_list'
  for(i in cur){
    # pull out the words in the current list entry.
    distinct_words <- word_vector_list[[i]]
    # now loop over the other word vectors to find distinct words
    for(j in cur[-i]){
      # use 'threshold' and  the frequency_threshold() function to remove all 
      # words that appear less than 'threshold' times in the other vectors 
      # before we compare them to the current word vector.
      check <- frequency_threshold(word_vector_list[[j]],threshold)
      # remove all entries (words) in distinct_words that also appear in check.
      if(length(check) > 0){
        rem <- which(distinct_words %in% unique(check))
        if(length(rem) > 0){
          distinct_words <- distinct_words[-rem]
        }
      }
    }
    # store the distinct_words vector in the dist_vects list object we will be 
    # returning
    dist_vects[[i]] <- as.character(distinct_words)
  }
  return(dist_vects)
}