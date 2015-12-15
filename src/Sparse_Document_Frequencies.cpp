#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
arma::vec Sparse_Document_Frequencies(
        int length_sparse_counts,
        arma::vec sparse_counts,
        arma::vec document_frequencies,
        arma::vec print_sequence,
        int print_sequence_length
){
    int print_sequence_counter = 0;
    for(int i = 0; i < length_sparse_counts; ++i){
       if(i == print_sequence[print_sequence_counter]){
           Rcpp::Rcout << print_sequence_counter+1 << "/" << print_sequence_length << " complete..." << std::endl;
       }
       int index = sparse_counts[i] -1;
       document_frequencies[index] += 1;
    }
    return document_frequencies;
}
