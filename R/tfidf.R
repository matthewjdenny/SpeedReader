#' A function to calculate TF-IDF and other related statistics on a set of documents.
#'
#' @param document_term_matrix document_term_matrix A numeric matrix or data.frame with dimensions number of documents X vocabulary length, where each entry is the count of word j in document i.
#' @param vocabulary A string vector containing all words in the vocabulary. The vocaublary vector must have the same number of entries as the number of columns in the document_term_matrix, and the word indicated by entries in the j'th column of document_term_matrix must correspond to the j'th entry in vocabulary.
#' @param remove_documents_with_no_terms Defualts to FALSE, if TRUE then all words in the vocabulary that appear zero times in the selected set of documents will be removed.
#' @param only_calculate_corpus_level_statistics Defaults to TRUE. If FALSE then tfidf scores will be calculated for every token in every document.
#' @param display_rankings If TRUE then the function will print out the top_words_to_display number of words ranked by TF-IDF.
#' @param top_words_to_display The number of top ranked words to print out if display_rankings == TRUE.
#' @return A list object.
#' @export
tfidf <- function(document_term_matrix,
                  vocabulary,
                  remove_documents_with_no_terms = FALSE,
                  only_calculate_corpus_level_statistics = TRUE,
                  display_rankings = TRUE,
                  top_words_to_display = 40){

    sparse_matrix <- FALSE
    if(class(document_term_matrix) == "simple_triplet_matrix"){
        sparse_matrix <- TRUE
    }

  if(min(document_term_matrix) < 0){
    stop(paste("The minimum value for a term frequency matrix must be zero! The minimum value of the document term matrix you supplied was:",min(document_term_matrix)))
  }

  if(remove_documents_with_no_terms){
    zero_documents <- which(rowSums(document_term_matrix) == 0)
    if(length(zero_documents) > 0 ){
      cat("Removing",length(zero_documents),"documents with no words in them.\n")
      document_term_matrix <- document_term_matrix[-zero_documents,]
    }
  }

  if(only_calculate_corpus_level_statistics) {
      return_list = list()
      if(sparse_matrix){
          document_frequency <- rep(0, ncol(document_term_matrix))
          printseq <- round(seq(1,length(document_term_matrix$i), length.out = 101)[2:101],0)
          # fast C++ implementation
          document_frequency <- Sparse_Document_Frequencies(
              length(document_term_matrix$j),
              document_term_matrix$j,
              document_frequency,
              printseq,
              length(printseq))
          return_list$document_frequency <- as.numeric(document_frequency)
          cat("Document frequency vector characteristics...\n")
          print(str(return_list$document_frequency))

      }else{
          return_list$document_frequency = calculate_document_frequency(document_term_matrix)
      }
      return_list$inverse_document_frequency = log(nrow(document_term_matrix)/as.numeric(return_list$document_frequency))
      if(sparse_matrix){
          return_list$document_word_counts = as.numeric(slam::row_sums(document_term_matrix))
          return_list$corpus_term_frequency = as.numeric(slam::col_sums(document_term_matrix))
          print(str(return_list$document_word_counts))
          print(str(return_list$corpus_term_frequency))
      }else{
          return_list$document_word_counts = apply(document_term_matrix,1,sum)
          return_list$corpus_term_frequency = apply(document_term_matrix,2,sum)
      }

      return_list$tfidf = return_list$corpus_term_frequency*return_list$inverse_document_frequency
      cat("Charactersitics of TF-IDF vector...\n")
      print(str(return_list$tfidf))
      return_list$vocabulary = vocabulary

  }else{
      if(sparse_matrix){
          stop("Full term-level TF-IDF not implemented for sparse matrices. This would likely break your computer. Set only_calculate_corpus_level_statistics = TRUE to proceed.")
      }else{
          to_return <- Calculate_TFIDF(document_term_matrix)

          return_list = list()

          return_list$document_frequency = as.numeric(to_return[[3]])
          return_list$inverse_document_frequency = log(nrow(document_term_matrix)/as.numeric(to_return[[3]]))
          return_list$document_word_counts = as.numeric(to_return[[4]])
          return_list$corpus_term_frequency = as.numeric(to_return[[5]])/sum(document_term_matrix)
          return_list$document_level_term_frequency = to_return[[2]]
          return_list$tfidf_dw = to_return[[1]]
          return_list$tfidf = return_list$corpus_term_frequency*return_list$inverse_document_frequency
          return_list$vocabulary = vocabulary
      }
  }


  #now generate a rank ordered dataset
  ranking <- order(return_list$tfidf, decreasing = T)
  print(length(ranking))
  return_list$tfidf_rankings <- data.frame(tfidf = return_list$tfidf[ranking],
                               term = vocabulary[ranking],
                               doc_freq = return_list$document_frequency[ranking],
                               term_freq = return_list$corpus_term_frequency[ranking],
                               idf = return_list$inverse_document_frequency[ranking],
                               stringsAsFactors = F)

  if(display_rankings){
      print("Top words as ranked by corpus-level TF-IDF.")
      print(head(return_list$tfidf_rankings,n = top_words_to_display))

      print("Top words as ranked by IDF.")
      ranking <- order(return_list$tfidf_rankings$idf, decreasing = T)
      print(head(return_list$tfidf_rankings[ranking,],n = top_words_to_display))

      print("Top words as ranked by term frequency.")
      ranking <- order(return_list$tfidf_rankings$term_freq, decreasing = T)
      print(head(return_list$tfidf_rankings[ranking,],n = top_words_to_display))
  }

  return(return_list)
}
