#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using std::log;

// [[Rcpp::export]]
List Sparse_PMI_Statistics(
        int length_sparse_counts,
        int table_sum,
        arma::vec colsums,
        arma::vec rowsums,
        arma::vec sparse_col_indexes,
        arma::vec sparse_row_indexes,
        arma::vec sparse_counts,
        arma::vec print_sequence,
        int print_sequence_length
){
    // Allocate sparse vectors to hold values.
    arma::vec pmi = arma::zeros(length_sparse_counts);
    arma::vec distinctiveness = arma::zeros(length_sparse_counts);
    arma::vec saliency = arma::zeros(length_sparse_counts);

    int print_sequence_counter = 0;
    for(int i = 0; i < length_sparse_counts; ++i){
        if(i == print_sequence[print_sequence_counter]){
            Rcpp::Rcout << print_sequence_counter+1 << "/" << print_sequence_length
            << " complete..." << std::endl;
            print_sequence_counter +=1;
        }
        pmi[i] = log( (double(sparse_counts[i]) / double(table_sum) ) *
            ((double(colsums[sparse_col_indexes[i] -1]) / double(table_sum)) *
            (double(rowsums[sparse_row_indexes[i] -1]) / double(table_sum))));

        distinctiveness[i] = (double(sparse_counts[i]) / double(colsums[sparse_col_indexes[i] -1])) *
            log((double(sparse_counts[i]) / double(colsums[sparse_col_indexes[i] -1])) /
            (double(rowsums[sparse_row_indexes[i] -1]) / double(table_sum)));

        saliency[i] =  (double(colsums[sparse_col_indexes[i] -1]) / double(table_sum)) *
            distinctiveness[i];
    }
    List ret(3);
    ret[0] = pmi;
    ret[1] = distinctiveness;
    ret[2] = saliency;
    return ret;
}
