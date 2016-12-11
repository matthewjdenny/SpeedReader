#include <RcppArmadillo.h>
#include <cmath>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
double Fast_Sparse_Mutual_Information(
        arma::vec rows,
        arma::vec cols,
        arma::vec vals,
        arma::vec colsums,
        arma::vec rowsums,
        int num_entries
){

    double mutual_information = 0;
    // loop over non-zero entries
    for(int i = 0; i < num_entries; ++i){
        double temp = std::log((vals[i]/(colsums[cols[i]]*rowsums[rows[i]])));
        if(!std::isfinite(temp)){
            temp = 0;
        }
        mutual_information += vals[i] * temp;
    }
    return mutual_information;
}




// [[Rcpp::export]]
List Fast_Sparse_Mutual_Information_Full(
        arma::vec rows,
        arma::vec cols,
        arma::vec vals,
        arma::vec colsums,
        arma::vec rowsums,
        int num_entries
){

    arma::vec column_contribution = arma::zeros(colsums.n_elem);

    double mutual_information = 0;
    // loop over non-zero entries
    for(int i = 0; i < num_entries; ++i){
        double temp = std::log((vals[i]/(colsums[cols[i]]*rowsums[rows[i]])));
        if(!std::isfinite(temp)){
            temp = 0;
        }
        mutual_information += vals[i] * temp;
        column_contribution[cols[i]] += vals[i] * temp;
    }

    List to_return(2);
    to_return[0] = mutual_information;
    to_return[1] = column_contribution;
    return to_return;
}
