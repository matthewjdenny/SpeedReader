#' A function to generate an ordered word count dataframe from a raw vector of words.
#'
#' @param word_vector A character vector of words we wish to count and order.
#' @param weighting a vector of weightings to be given to counts of a particular string. Defaults to NULL meaning no weightings will be used.
#' @return A data.frame with 'word' and a 'count' fields, with rows ordered by word frequency.
#' @export
order_by_counts <- function(word_vector,
                            weighting = NULL){
  # get all of the unique words in the vector we passed in
  vocab <- unique(as.character(word_vector))
  # create an empty vector to store counts of these words
  counts <- rep(0,length(vocab))
  # loop over all unique words
  for(i in 1:length(vocab)){
    # store the number of times the unique word appears in the vector we passed
    # in.
    if(is.null(weighting)){
      counts[i] <- length(which(word_vector == vocab[i]))
    }else{
      counts[i] <- sum(weighting[which(word_vector == vocab[i])])
    }
    
  }
  # create a data.frame with a 'word' and a 'count' field.
  ret <- data.frame(word = vocab,count = counts, stringsAsFactors = F)
  # get the (descending) order of words by their frequency in the vector we 
  # passed in
  ordering <- order(counts,decreasing = T)
  # order what we pass back
  ret <- ret[ordering,]
  return(ret)
}