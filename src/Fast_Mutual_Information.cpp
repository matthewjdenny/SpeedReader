#include <RcppArmadillo.h>
#include <cmath>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
double Fast_Mutual_Information(
        arma::mat joint_dist,
        arma::vec non_zero_cols
){
    // need to modify to take sparse matrix objects
    //takes an already-normalized matrix
    double mutual_information = 0;
    int num_rows = joint_dist.n_rows;
    arma::mat colsums = sum(joint_dist,0);
    arma::mat rowsums = sum(joint_dist,1);
    for(int i = 0; i < num_rows; ++i){
        int cur_total = 0;
        int cur_max = non_zero_cols[i];
        int j = 0;
        while(cur_total < cur_max){
            double temp = log((joint_dist(i,j)/(colsums[j]*rowsums[i])));
            if(!std::isfinite(temp)){
                temp = 0;
            }
            mutual_information += joint_dist(i,j) * temp;
            if(joint_dist(i,j) > 0){
                cur_total += 1;
            }
            j +=1;
        }
    }
    return mutual_information;
}
