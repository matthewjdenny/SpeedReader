#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
arma::vec calculate_ACMI_contribution(
        double dist_sum,
        arma::vec colsums,
        arma::vec rowsums,
        int num_cols,
        arma::vec column_contributions,
        arma::vec row_index_counts,
        arma::mat joint,
        int total_non_zeros,
        double full_MI){

    arma::vec ACMI_contribution = arma::zeros(num_cols);

    // loop over row pairs and
    for(int i = 0; i < num_cols; ++i){
        // only do calculation with non-zero col sum
        if (colsums[i] > 0) {
            double D = dist_sum /double(dist_sum - colsums[i]);

            int cur_non_zeros = 0;

            // get the differences in row sums
            double x1 =  (rowsums[0] - joint(0,i));
            double x2 =  (rowsums[1] - joint(1,i));
            //renormalize
            x1 = x1 / (x1 + x2);
            x2 = x2 / (x1 + x2);
            // determine whether each row normalizing constant has gotten
            // bigger or smaller
            double D_1 = rowsums[0]/x1;
            double D_2 = rowsums[1]/x2;

            double sub_term_1 = 0;
            if (joint(0,i) > 0) {
                cur_non_zeros += 1;
                sub_term_1 = std::log(D_1) * (row_index_counts[0] - 1);
            } else {
                sub_term_1 = std::log(D_1) * row_index_counts[0];
            }

            double sub_term_2 = 0;
            if (joint(1,i) > 0) {
                cur_non_zeros += 1;
                sub_term_2 = std::log(D_2) * (row_index_counts[1] - 1);
            } else {
                sub_term_2 = std::log(D_2) * row_index_counts[1];
            }



            //double term1 = std::log(D) * (total_non_zeros - cur_non_zeros);
            double term2 =  sub_term_1 + sub_term_2;

            double change = D * (0 - term2 - column_contributions[i]);

            if ( i < 10) {
                Rcpp::Rcout  << i << " D1 : " <<  D_1  << " D2 : " <<  D_2 <<std::endl;
                Rcpp::Rcout  << i << " sub terms: " <<  sub_term_1 << " " << sub_term_2 <<std::endl;
                Rcpp::Rcout  << i << " D : " <<   D <<std::endl;
                Rcpp::Rcout  << i << " C : " <<  column_contributions[i] <<std::endl;
                Rcpp::Rcout  << i << " Change : " <<  change <<std::endl;
            }

            ACMI_contribution[i] = change;
        }
    }

    return ACMI_contribution;
}


// [[Rcpp::export]]
arma::vec calculate_unique_MI_contribution(
        arma::vec colsums,
        arma::vec rowsums,
        int num_cols,
        int num_rows,
        arma::mat joint,
        arma::vec column_type_counts,
        double dist_sum){

    arma::vec MI_heldout = arma::zeros(num_cols);

    // loop over each column we are going to hold out
    for(int i = 0; i < num_cols; ++i){

        // first we need to determine adjustment for column and row sums for
        // the other columns and rows. All column adjustments will be the same,
        // row adjustments will be different.
        double joint_adjustment = dist_sum / (dist_sum - colsums[i]);
        arma::vec row_adjustment = arma::zeros(num_rows);
        for(int j = 0; j < num_rows; ++j){
            row_adjustment[j] = (rowsums[j] - joint(j,i));
        }
        double denom = arma::accu(row_adjustment);
        for(int j = 0; j < num_rows; ++j){
            row_adjustment[j] = row_adjustment[j]/denom;
        }

        // Rcpp::Rcout  << i << row_adjustment <<std::endl;

        double mutual_information = 0;
        for(int k = 0; k < num_cols; ++k){
            for(int j = 0; j < num_rows; ++j){
                double temp = std::log((joint(j,k)/(colsums[k] * row_adjustment[j])));
                if(!std::isfinite(temp)){
                    temp = 0;
                }
                if (k != i) {
                    mutual_information += column_type_counts[k] * (joint_adjustment * joint(j,k) * temp);
                } else {
                    if (column_type_counts[k] > 1) {
                        mutual_information += (column_type_counts[k] - 1) * (joint_adjustment * joint(j,k) * temp);
                    }
                }
            }
        }

        MI_heldout[i] = mutual_information;

    }

    return MI_heldout;
}




