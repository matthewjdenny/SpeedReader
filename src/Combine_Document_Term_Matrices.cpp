#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
List Combine_Document_Term_Matrices(
    List document_word_matrix_list,
    List vocabularies,
    std::vector<std::string> unique_words,
    int number_of_corpora
){
  
  int vocab_size = unique_words.size();
  int ndoc = 0;
  
  //get the number of rows
  for(int i = 0; i < number_of_corpora; ++i){
    arma::mat current = document_word_matrix_list[i];
    ndoc += current.n_rows;
  }
  
  arma::mat aggregate_doc_term_matrix = arma::zeros(ndoc, vocab_size);
  
  // calculate df/idf
  int document_counter  = 0;
  for(int i = 0; i < number_of_corpora; ++i){
    Rcpp::Rcout << "Current Corpus: " << i << std::endl;
    arma::mat current = document_word_matrix_list[i];
    int current_docs = current.n_rows;
    int current_vocab_size = current.n_cols;
    std::vector<std::string> current_vocab = vocabularies[i];
    
    arma::vec indexes = arma::zeros(current_vocab_size);
    for(int k = 0; k < current_vocab_size; ++k){
      int already = 0;
      int counter = 0;
      while(already == 0){
        if(current_vocab[k] == unique_words[counter]){
          indexes[k] = counter;
          already = 1;
        }
        counter +=1;
      }
    }
    
    
    for(int j = 0; j < current_docs; ++j){
      for(int k = 0; k < current_vocab_size; ++k){
        aggregate_doc_term_matrix(document_counter,indexes[k]) += current(j,k);
      }
      document_counter += 1;
    }
  } 
  
  
  List return_list(2);
  return_list[0] = aggregate_doc_term_matrix;
  return_list[1] = unique_words;
  return return_list;
}


