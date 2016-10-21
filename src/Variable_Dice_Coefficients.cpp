#include <RcppArmadillo.h>
#include <string>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
arma::mat Variable_Dice_Coefficients(
        int number_of_lines,
        List Lines,
        int number_of_lines2,
        List Lines2,
        int Dice_Terms){

    arma::mat linewise_dice_coefficients = arma::zeros(number_of_lines, number_of_lines2);

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
            // figure out which bigrams are in both sets
            double both_count = 0;
            for(int k = 0; k < bigrams_1.size(); ++k){
                // get matches in current doc
                for(int l = 0; l < bigrams_2.size(); ++l){
                    if(bigrams_1[k] == bigrams_2[l]) {
                        //Rcpp::Rcout << "Bigram 1: " << bigrams_1[k] << "Bigram 2: " << bigrams_2[l] <<std::endl;
                        both_count += 1;
                        break;
                    }
                }
            }
            double dice = (2*both_count)/double(bigrams_1.size() + bigrams_2.size());
            linewise_dice_coefficients(i,j) = dice;
        }
    }
    return linewise_dice_coefficients;
}

