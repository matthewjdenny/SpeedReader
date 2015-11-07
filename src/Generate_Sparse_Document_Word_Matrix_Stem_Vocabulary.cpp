#include <RcppArmadillo.h>
#include <string>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
List Generate_Sparse_Document_Term_Matrix_Stem_Vocabulary(
        int number_of_documents,
        int number_of_unique_words,
        std::vector<std::string> unique_words,
        List Document_Words,
        arma::vec Document_Lengths,
        List Document_Word_Counts,
        int total_terms,
        std::vector<std::string> stem_lookup,
        arma::vec starts,
        arma::vec ends,
        int lookup_size
){

    arma::vec document_indices = arma::zeros(total_terms);
    arma::vec term_indices = arma::zeros(total_terms);
    arma::vec counts = arma::zeros(total_terms);
    //int lookup_size = starts.n_elem;
    int stems_found = 0;
    Rcpp::Rcout << "Lookup Size: " << lookup_size << std::endl;
    int total_counter = 0;
    for(int n = 0; n < number_of_documents; ++n){
        Rcpp::Rcout << "Current Document: " << n << std::endl;
        int length = Document_Lengths[n];
        if(length > 0){
            std::vector<std::string> current = Document_Words[n];
            arma::vec current_counts = Document_Word_Counts[n];
            for(int i = 0; i < length; ++i){
                // figure out which stem the current string has
                std::string str = current[i];
                std::string stem = str.substr(0,3);
                //Rcpp::Rcout << "Stem: " << stem << std::endl;
                int found_stem = 0;
                int stem_counter = 0;
                int stem_exists = 1;
                int start = -1;
                int end = -1;
                while(found_stem == 0){
                    if(lookup_size == stem_counter){
                        stem_exists = 0;
                        found_stem = 1;
                    }else{
                        std::string compare = stem_lookup[stem_counter];
                        if(compare.compare(stem) == 0){
                            start = starts[stem_counter];
                            end = ends[stem_counter];
                            found_stem = 1;
                            stems_found += 1;
                        }
                        stem_counter +=1;
                    }
                }

                // only try to add a term if its stem exists
                if(stem_exists == 1){
                    //Rcpp::Rcout << "Found Stem: " << total_counter << std::endl;
                    int already = 0;
                    while(already == 0){
                        if(start == end){
                            already = 1;
                        }else{
                            std::string comp1 = unique_words[start];
                            std::string comp2 = current[i];
                            if(comp1.compare(comp2) == 0){
                                document_indices[total_counter] = n+1;
                                term_indices[total_counter] = start+1;
                                counts[total_counter] = current_counts[i];
                                total_counter +=1;
                                already = 1;
                            }
                            start +=1;
                        }
                    }
                }
            }
        }
    }
    //remove excess zeros
    Rcpp::Rcout << "Total Counter: " << total_counter << std::endl;
    Rcpp::Rcout << "Stems Found: " << stems_found<< std::endl;
    arma::vec ret_document_indices = arma::zeros(total_counter);
    arma::vec ret_term_indices = arma::zeros(total_counter);
    arma::vec ret_counts = arma::zeros(total_counter);
    for(int t = 0; t < total_counter; ++t){
        ret_document_indices[t] = document_indices[t];
        ret_term_indices[t] = term_indices[t];
        ret_counts[t] = counts[t];
    }

    List to_return(3);
    to_return[0] = ret_document_indices;
    to_return[1] = ret_term_indices;
    to_return[2] = ret_counts;
    //return
    return to_return;

}

