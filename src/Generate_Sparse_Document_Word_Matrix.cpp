#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
List Generate_Sparse_Document_Term_Matrix(
        int number_of_documents,
        int number_of_unique_words,
        std::vector<std::string> unique_words,
        List Document_Words,
        arma::vec Document_Lengths,
        List Document_Word_Counts,
        int total_terms
){

    arma::vec document_indices = arma::zeros(total_terms);
    arma::vec term_indices = arma::zeros(total_terms);
    arma::vec counts = arma::zeros(total_terms);
    int total_counter = 0;
    for(int n = 0; n < number_of_documents; ++n){
        Rcpp::Rcout << "Current Document: " << n << std::endl;
        int length = Document_Lengths[n];
        if(length > 0){
            std::vector<std::string> current = Document_Words[n];
            arma::vec current_counts = Document_Word_Counts[n];
            for(int i = 0; i < length; ++i){
                int already = 0;
                int counter = 0;
                while(already == 0){
                    if(counter == number_of_unique_words ){
                        already = 1;
                    }else{
                        if(unique_words[counter] == current[i]){
                            document_indices[total_counter] = n+1;
                            term_indices[total_counter] = counter+1;
                            counts[total_counter] = current_counts[i];
                            total_counter +=1;
                            already = 1;
                        }
                        counter +=1;
                    }

                }
            }
        }
    }
    List to_return(3);
    to_return[0] = document_indices;
    to_return[1] = term_indices;
    to_return[2] = counts;
    //return
    return to_return;

}

