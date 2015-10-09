#' A function to calculate topic coherence for a given topic using the formulation in "Optimizing Semantic Coherence in Topic Models" available here: <http://dirichlet.net/pdf/mimno11optimizing.pdf>
#'
#' @param top_words A string vector of top words associated with a topic. If numeric_top_words == TRUE then a numeric vector of word indicies.
#' @param document_term_matrix A numeric matrix or data.frame with dimensions number of documents X vocabulary length, where each entry is the count of word j in document i.
#' @param vocabulary A string vector containing all words in the vocabulary. The vocaublary vector must have the same number of entries as the number of columns in the document_term_matrix, and the word indicated by entries in the i'th column of document_term_matrix must correspond to the i'th entry in vocabulary. If numeric_top_words == TRUE then it is not necessary to supply.
#' @param numeric_top_words Defaults to FALSE. If TRUE, then the function expects a vector of word indicies instead of a string vector of actual words.
#' @param K The number of top words to use in calculating the topic coherence. Defaults to the lneght of top_words. Common values are usually in the range of 10-20.
#' @return The coherence score for the given topic.
#' @export
topic_coherence <- function(top_words,
                            document_term_matrix,
                            vocabulary = NULL,
                            numeric_top_words = FALSE,
                            K = length(top_words)){

  # make sure the data is the right format
  vocabulary <- as.character(vocabulary)

  # perform some basic checks and throw errors if we see something weird.
  if(is.null(vocabulary) & !numeric_top_words){
      stop("You must provide a vocabulary vector!")
  }
  if(K > length(top_words)){
      K <- length(top_words)
      warining(paste("You must select a value for K that is less than length(top_words). K has automatically been set to :",K,sep = " "))
  }
  if(length(vocabulary) != ncol(document_term_matrix)){
      stop("The vocaublary vector must have the same number of entries as the number of columns in the document_term_matrix, and the word indicated by entries in the i'th column of document_term_matrix must correspond to the i'th entry in vocabulary.")
  }

  #if we are only using the K top words then reduce our top words vector
  top_words <- top_words[1:K]

  # binarize the document term matrix
  document_term_matrix <- matrix(as.numeric(document_term_matrix > 0),
                                 nrow = nrow(document_term_matrix),
                                 ncol = ncol(document_term_matrix))
  coherence_score <- 0
  for(i in 2:length(top_words)){
    for(j in 1:(i-1)){
      # we can either look up against vocab or just use indexes
      if(numeric_top_words){
        jindex <- top_words[j]
        iindex <- top_words[i]
      }else{
        jindex <- which(vocabulary == top_words[j])
        iindex <- which(vocabulary == top_words[i])
      }

      document_frequency <- sum(document_term_matrix[,jindex])
      j_positive <- which(document_term_matrix[,jindex] > 0)
      i_positive <- which(document_term_matrix[,iindex] > 0)
      co_document_frequency <- sum(i_positive %in% j_positive)

      coherence_score <- coherence_score + log((co_document_frequency + 1)/document_frequency)

    }
  }
  if(is.infinite(coherence_score)){
    coherence_score <- NA
    warning("The coherence score was not finite. Make sure that all words in your vocabulary appear atleast once.")
  }
  return(coherence_score)
}




