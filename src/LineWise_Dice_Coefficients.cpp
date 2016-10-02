#include <RcppArmadillo.h>
#include <string>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
arma::mat LineWise_Dice_Coefficients(
        int number_of_lines,
        List Lines,
        int number_of_lines2,
        List Lines2){

    arma::mat linewise_dice_coefficients = arma::zeros(number_of_lines, number_of_lines2);

    for(int i = 0; i < number_of_lines; ++i){
        std::vector<std::string> line1 = Lines[i];
        //allocate vector to hold bigrams
        std::vector<std::string> bigrams_1 = line1;
        //take out the first element since we will have one less bigram than
        //unigrams

        //generate bigrams only if the line has more than one term
        if( bigrams_1.size() > 1) {
            bigrams_1.erase(bigrams_1.begin());
            for(int k = 0; k < bigrams_1.size(); ++k){
                bigrams_1[k] = line1[k] + line1[k+1];
            }
        }
        for(int j = 0; j < number_of_lines2; ++j){
            std::vector<std::string> line2 = Lines2[j];
            //allocate vector to hold bigrams
            std::vector<std::string> bigrams_2 = line2;
            //take out the first element since we will have one less bigram than
            //unigrams
            if( bigrams_2.size() > 1) {
                bigrams_2.erase(bigrams_2.begin());
                //generate bigrams
                for(int k = 0; k < bigrams_2.size(); ++k){
                    bigrams_2[k] = line2[k] + line2[k+1];
                }
            }
            // figure out which bigrams are in both sets
            double both_count = 0;
            for(int k = 0; k < bigrams_1.size(); ++k){
                for(int l = 0; l < bigrams_2.size(); ++l){
                    if(bigrams_1[k] == bigrams_2[l]) {
                         //Rcpp::Rcout << "Bigram 1: " << bigrams_1[k] << "Bigram 2: " << bigrams_2[l] <<std::endl;
                        both_count += 1;
                    }
                }
            }
            double dice = (2*both_count)/double(bigrams_1.size() + bigrams_2.size());
            linewise_dice_coefficients(i,j) = dice;
        }
    }
    return linewise_dice_coefficients;
}

