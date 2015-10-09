#include <RcppArmadillo.h>
#include <string>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
List Count_Words(
    int number_of_documents,
    List Document_Words,
    arma::vec Document_Lengths
){
  List to_return(3);
  int total_unique_words = 0;
  arma::vec unique_word_counts = arma::zeros(250000);
  std::vector<std::string> unique_words(250000);
  
  for(int n = 0; n < number_of_documents; ++n){
    Rcpp::Rcout << "Current Document: " << n << std::endl;
    int length = Document_Lengths[n];
    if(length > 0){
      std::vector<std::string> current = Document_Words[n];
      for(int i = 0; i < length; ++i){
        int already = 0;
        int counter = 0;
        while(already == 0){
          if(counter == total_unique_words){
            unique_words[counter] = current[i];
            unique_word_counts[counter] += 1;
            total_unique_words += 1;
            already = 1;
          }else{
            if(unique_words[counter] == current[i]){
              unique_word_counts[counter] += 1;
              already  = 1;
            }
          }
          counter +=1;
        }
      }
    }
  }
  
  arma::vec word_counts = arma::zeros(total_unique_words);
  std::vector<std::string> words(total_unique_words);
  for(int i = 0; i < total_unique_words; ++i){
    word_counts[i] = unique_word_counts[i];
    words[i] = unique_words[i];
  }
  
  to_return[0] = total_unique_words;
  to_return[1] = words;
  to_return[2] = word_counts;
  return to_return;
}
