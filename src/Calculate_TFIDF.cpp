#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
List Calculate_TFIDF(
    arma::mat document_word_matrix
){
  
  int ndoc = document_word_matrix.n_rows;
  int vocab_size = document_word_matrix.n_cols;
  arma::colvec rowsums = sum(document_word_matrix,1);
  arma::rowvec colsums = sum(document_word_matrix,0);
  
  arma::mat term_frequency = arma::zeros(ndoc, vocab_size);
  arma::vec document_frequency = arma::zeros(vocab_size);
  arma::mat tfidf = arma::zeros(ndoc, vocab_size);
  
  // calculate df/idf
  for(int i = 0; i < ndoc; ++i){
    for(int j = 0; j < vocab_size; ++j){
      if(document_word_matrix(i,j) > 0){
        document_frequency[j] += 1;
      }
    }
  } 
  for(int i = 0; i < ndoc; ++i){
    if(rowsums[i] > 0){
      for(int j = 0; j < vocab_size; ++j){
        term_frequency(i,j) = double(document_word_matrix(i,j))/double(rowsums[i]);
        double idf = log(double(ndoc)/double(document_frequency[j]));
        tfidf(i,j) = idf*term_frequency(i,j);
      }
    }
  } 
  
  List return_list(5);
  return_list[0] = tfidf;
  return_list[1] = term_frequency;
  return_list[2] = document_frequency;
  return_list[3] = rowsums;
  return_list[4] = colsums;
  return return_list;
}


