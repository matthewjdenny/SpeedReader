// [[Rcpp::plugins(cpp11)]]
#include <RcppArmadillo.h>
#include <unordered_map>
#include <chrono>
#include <thread>
//[[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(BH)]]
using namespace Rcpp;

// [[Rcpp::export]]
arma::mat reference_dist_distance(
        arma::vec ref_dist_i,
        arma::vec ref_dist_j,
        arma::vec ref_dist_v,
        arma::vec target_dist_i,
        arma::vec target_dist_j,
        arma::vec target_dist_v,
        int num_ref_dists,
        int num_documents,
        arma::vec term_weights){

    arma::mat distances = arma::zeros(num_documents,num_ref_dists);

    int cur = 0;
    int num_entry = ref_dist_i.n_elem;
    int num_lookups = target_dist_i.n_elem;

    // allocate a vector to hold n-grams
    std::vector<std::unordered_map<int,double>> lookups(num_ref_dists);

    // hash term weights
    Rcpp::Rcout << "Hashing Term Weights: " << std::endl;
    std::unordered_map<int,double> term_weight_map;
    int twlen = term_weights.n_elem;
    for (int i = 0; i < twlen; ++i) {
        int temp1 = i;
        double temp2 = term_weights[i];
        std::pair<int,double> to_insert(temp1,temp2);
        term_weight_map.insert(to_insert);
    }


    //loop through documents and form ngrams/hash them
    for (int i = 0; i < num_ref_dists; ++i) {
        Rcpp::Rcout << "Hashing Reference Distribution: " << i << std::endl;
        std::unordered_map<int,double> lookup;
        while (ref_dist_i[cur] == i) {
            int temp1 = ref_dist_j[cur];
            double temp2 = ref_dist_v[cur];
            std::pair<int,double> to_insert(temp1,temp2);
            lookup.insert(to_insert);
            cur += 1;
            if (cur == num_entry) {
                break;
            }
        }
        lookups[i] = lookup;
    }


    // figure out its contribution to each of the distances
    for (int j = 0; j < num_ref_dists; ++j) {
        Rcpp::Rcout << "Checking Against Reference Distribution: " << j << " of " << num_ref_dists << std::endl;
        // get the current hash map
        std::unordered_map<int,double> current_lookup = lookups[j];
        //no loop through the target documents
        double current_distance = 0;
        int cur_i = 0;
        for(int i = 0; i < num_lookups; ++i){
            if (cur_i == target_dist_i[i]) {
                // see if we can find hte current row index in the current hashmap
                std::unordered_map<int,double>::const_iterator got = current_lookup.find(target_dist_j[i]);
                std::unordered_map<int,double>::const_iterator term_weight = term_weight_map.find(target_dist_j[i]);
                double weight = term_weight->second;
                if (got == current_lookup.end()) {
                    current_distance += weight * std::pow(target_dist_v[i],2);
                } else {
                    double temp = got->second;
                    if (i < 4) {
                        Rcpp::Rcout << "Value: " << temp << " weight " << weight << std::endl;
                    }
                    current_distance += weight * std::pow((target_dist_v[i] - temp),2);
                }
            } else {
                //insert into the
                distances(cur_i,j) = std::sqrt(current_distance);
                //reset everything
                current_distance = 0;
                cur_i += 1;
                // see if we can find hte current row index in the current hashmap
                std::unordered_map<int,double>::const_iterator got = current_lookup.find(target_dist_j[i]);
                std::unordered_map<int,double>::const_iterator term_weight = term_weight_map.find(target_dist_j[i]);
                double weight = term_weight->second;
                if (got == current_lookup.end()) {
                    current_distance += weight * std::pow(target_dist_v[i],2);
                } else {
                    double temp = got->second;
                    current_distance += weight * std::pow((target_dist_v[i] - temp),2);
                }
            }
        }
    }



    return distances;
}

