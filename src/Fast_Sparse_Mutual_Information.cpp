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
