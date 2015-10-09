#' A function to frequency threshold a vector of strings.
#'
#' @param word_vector A character vector of words we wish to frequency threshold.
#' @param threshold An integer > 0 indicating the number of times a word must appear more than to be included in the vector we return. Defaults to threshold = 1, meaning all words that appear 1 or less times in the vector we pass in will be removed.
#' @return A word vector with all words appearing 'threshold' or less times removed.
#' @export
frequency_threshold <- function(word_vector, 
                                threshold = 1){
  # deal with the case where the user was a dummy.
  if(threshold < 0){
    stop("You must specify a threshold >= 0 or all words will be removed.")
  }
  # get all of the unique words in the vector we passed in
  vocab <- unique(as.character(word_vector))
  # create an empty vector to store counts of these words
  counts <- rep(0,length(vocab))
  # loop over all unique words
  for(i in 1:length(vocab)){
    # store the number of times the unique word appears in the vector we passed
    # in.
    counts[i] <- length(which(word_vector == vocab[i]))
  }
  # get the indicies of those unique words that appear 'threshold' or less times
  remove <- which(counts <= threshold)
  # if there are any words that appear less than 'threshold' times then we go 
  # ahead and figure out which entries in word_vector shoudl be removed.
  if(length(remove) > 0){
    # create an indicator vector that stores a value of 1 if the i'th entry in 
    # word_vector appears 'threshold' or less times, and zero otherwise.
    rem <- rep(0,length(word_vector)) 
    # populate rem by looping over the word vector
    for(i in 1:length(word_vector)){
      # for each entry in word_vector, check to see if it is a match to any of 
      # the words we want to remove, and note it if TRUE. Note that this is not 
      # very efficient, but prevents a crash if we remove all words.
      for(j in 1:length(remove)){
        if(word_vector[i] == vocab[remove[j]]){
          rem[i] <- 1
        }
      }
    }
    # remove all of the entries in word_vector where rem[i] == 1
    word_vector <- word_vector[-which(rem == 1)]
  }
  # deal with the case where we remove all words.
  if(length(word_vector) < 1){
    cat("There are no words left, considder setting a lower threshold.")
    word_vector <- NULL
  }
  return(word_vector)
}