#include <RcppArmadillo.h>
#include <cmath>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
List Col_and_Row_Sums(
        arma::mat joint_dist
){
    List to_return(2);
    arma::mat colsums = sum(joint_dist,0);
    arma::mat rowsums = sum(joint_dist,1);

    to_return[0] = colsums;
    to_return[1] = colsums;
    return to_return;
}
