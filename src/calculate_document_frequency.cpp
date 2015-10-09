#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
arma::vec calculate_document_frequency(
        arma::mat document_word_matrix
){

    int ndoc = document_word_matrix.n_rows;
    int vocab_size = document_word_matrix.n_cols;
    arma::vec document_frequency = arma::zeros(vocab_size);

    // calculate df/idf
    for(int i = 0; i < ndoc; ++i){
        for(int j = 0; j < vocab_size; ++j){
            if(document_word_matrix(i,j) > 0){
                document_frequency[j] += 1;
            }
        }
    }

    return document_frequency;
}
