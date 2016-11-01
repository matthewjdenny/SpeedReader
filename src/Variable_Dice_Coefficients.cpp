#include <RcppArmadillo.h>
#include <string>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

namespace mjd {
    std::vector<std::string>remove_duplicates(std::vector<std::string> ngrams){
        // sort
        // int len = ngrams.size();
        std::sort(ngrams.begin(),ngrams.end());
        auto last = std::unique(ngrams.begin(), ngrams.end());
        // v now holds {1 2 3 4 5 6 7 x x x x x x}, where 'x' is indeterminate
        ngrams.erase(last, ngrams.end());
        return ngrams;
    }

    //original
    // std::vector<std::string>remove_duplicates(std::vector<std::string> ngrams){
    //     int current = 0;
    //     while (current < ngrams.size()) {
    //         int k = current + 1;
    //         while (k < ngrams.size()) {
    //             if (ngrams[k] == ngrams[current]) {
    //                 ngrams.erase(ngrams.begin() + k);
    //             }
    //             k += 1;
    //         }
    //         current += 1;
    //     }
    //     return ngrams;
    // }
}

// [[Rcpp::export]]
List Variable_Dice_Coefficients(
        int number_of_lines,
        List Lines,
        int number_of_lines2,
        List Lines2,
        int Dice_Terms,
        bool rem_duplicates){

    arma::mat linewise_dice_coefficients = arma::zeros(number_of_lines, number_of_lines2);
    arma::mat both_in_a = arma::zeros(number_of_lines, number_of_lines2);
    arma::mat both_in_b = arma::zeros(number_of_lines, number_of_lines2);
    arma::mat ngrams_a = arma::zeros(number_of_lines, number_of_lines2);
    arma::mat ngrams_b = arma::zeros(number_of_lines, number_of_lines2);
    arma::mat both = arma::zeros(number_of_lines, number_of_lines2);

    for(int i = 0; i < number_of_lines; ++i){
        std::vector<std::string> line1 = Lines[i];
        //allocate vector to hold bigrams
        std::vector<std::string> bigrams_1 = line1;
        //take out the first element since we will have one less bigram than
        //unigrams

        //generate bigrams only if the line has more than one term
        if(bigrams_1.size() > (Dice_Terms - 1)) {
            // only erase terms if ngram length is atleast 1
            if(Dice_Terms > 1) {
                bigrams_1.erase(bigrams_1.begin(),bigrams_1.begin() + (Dice_Terms - 1));
            }

            // create the line, then add to it.

            for(int k = 0; k < bigrams_1.size(); ++k){
                std::string cur = line1[k];
                if(Dice_Terms > 1) {
                    for(int l = 1; l < (Dice_Terms-1); ++l){
                        cur += line1[k+l];
                    }
                }
                bigrams_1[k] = cur;
            }
        }

        //Rcpp::Rcout << "Grams 1 Before: " <<  bigrams_1.size()  <<std::endl;
        std::vector<std::string> bigrams_1a = mjd::remove_duplicates(bigrams_1);
        //Rcpp::Rcout << "Grams 1 After: " <<  bigrams_1a.size()  <<std::endl;
        // for(int i = 0; i < bigrams_1.size(); ++i){
        //     Rcpp::Rcout <<  bigrams_1[i]  <<std::endl;
        // }

        for(int j = 0; j < number_of_lines2; ++j){
            std::vector<std::string> line2 = Lines2[j];
            //allocate vector to hold bigrams
            std::vector<std::string> bigrams_2 = line2;
            //take out the first element since we will have one less bigram than
            //unigrams
            if( bigrams_2.size() > (Dice_Terms - 1)) {
                if(Dice_Terms > 1) {
                    bigrams_2.erase(bigrams_2.begin(),bigrams_2.begin() + (Dice_Terms - 1));
                }
                for(int k = 0; k < bigrams_2.size(); ++k){
                    std::string cur = line2[k];
                    if(Dice_Terms > 1) {
                        for(int l = 1; l < (Dice_Terms-1); ++l){
                            cur += line2[k+l];
                        }
                    }
                    bigrams_2[k] = cur;
                }
            }
            //Rcpp::Rcout << "Grams 2 Before: " <<  bigrams_2.size()  <<std::endl;
            std::vector<std::string> bigrams_2a = mjd::remove_duplicates(bigrams_2);
            //Rcpp::Rcout << "Grams 2 After: " <<  bigrams_2a.size()  <<std::endl;
            // figure out which bigrams are in both sets
            arma::vec both_vec = arma::zeros(bigrams_1a.size());
            for(int k = 0; k < bigrams_1a.size(); ++k){
                // get matches in current doc
                for(int l = 0; l < bigrams_2a.size(); ++l){
                    if(bigrams_1a[k] == bigrams_2a[l]) {
                        //Rcpp::Rcout << "Bigram 1: " << bigrams_1a[k] << "Bigram 2: " << bigrams_2a[l] <<std::endl;
                        both_vec[k] = 1;
                        break;
                    }
                }
            }
            double both_count = arma::sum(both_vec);

            //Rcpp::Rcout << "Both: " <<  both_count  <<std::endl;
            double dice = (2*both_count)/double(bigrams_1a.size() + bigrams_2a.size());
            Rcpp::Rcout << "Dice Coefficient: " <<  dice  <<std::endl;
            linewise_dice_coefficients(i,j) = dice;
            both_in_a(i,j) = both_count/double(bigrams_1a.size());
            both_in_b(i,j) = both_count/double(bigrams_2a.size());
            ngrams_a(i,j) = double(bigrams_1a.size());
            ngrams_b(i,j) = double(bigrams_2a.size());
            both(i,j) = both_count;
        }
    }
    List to_return(6);
    to_return[0] = linewise_dice_coefficients;
    to_return[1] = both_in_a;
    to_return[2] = both_in_b;
    to_return[3] = ngrams_a;
    to_return[4] = ngrams_b;
    to_return[5] = both;
    return to_return;
}

