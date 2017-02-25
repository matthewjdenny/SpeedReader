// [[Rcpp::plugins(cpp11)]]
#include <RcppArmadillo.h>
#include <string>
#include <unordered_set>
#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string/classification.hpp>
//[[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(BH)]]
using namespace Rcpp;

namespace mjd {
    arma::vec calculate_metrics(arma::vec which_a_in_b,
                                   arma::vec which_b_in_a,
                                   int ngram_size) {


        arma::vec m_seq_1 = arma::zeros(which_a_in_b.size());
        int cur_match = 0;
        int counter = 0;
        for(int i = 0; i < which_a_in_b.size(); ++i){
            if (which_a_in_b[i] == 1) {
                cur_match += 1;
            } else {
                if (cur_match != 0) {
                    m_seq_1[counter] = cur_match;
                    cur_match = 0;
                    counter += 1;
                }
            }
        }
        if (counter > 0) {
            m_seq_1 = m_seq_1.subvec(0,(counter-1));
        } else {
            m_seq_1 = m_seq_1.subvec(0,0);
        }

        arma::vec m_seq_2 = arma::zeros(which_b_in_a.size());
        cur_match = 0;
        counter = 0;
        for(int i = 0; i < which_b_in_a.size(); ++i){
            if (which_b_in_a[i] == 1) {
                cur_match += 1;
            } else {
                if (cur_match != 0) {
                    m_seq_2[counter] = cur_match;
                    cur_match = 0;
                    counter += 1;
                }
            }
        }
        if (counter > 0) {
            m_seq_2 = m_seq_2.subvec(0,(counter-1));
        } else {
            m_seq_2 = m_seq_2.subvec(0,0);
        }

        arma::vec n_seq_1 = arma::zeros(which_a_in_b.size());
        cur_match = 0;
        counter = 0;
        for(int i = 0; i < which_a_in_b.size(); ++i){
            if (which_a_in_b[i] == 0) {
                cur_match += 1;
            } else {
                if (cur_match != 0) {
                    n_seq_1[counter] = cur_match;
                    cur_match = 0;
                    counter += 1;
                }
            }
        }
        if (counter > 0) {
            n_seq_1 = n_seq_1.subvec(0,(counter-1));
        } else {
            n_seq_1 = n_seq_1.subvec(0,0);
        }

        arma::vec n_seq_2 = arma::zeros(which_b_in_a.size());
        cur_match = 0;
        counter = 0;
        for(int i = 0; i < which_b_in_a.size(); ++i){
            if (which_b_in_a[i] == 0) {
                cur_match += 1;
            } else {
                if (cur_match != 0) {
                    n_seq_2[counter] = cur_match;
                    cur_match = 0;
                    counter += 1;
                }
            }
        }
        if (counter > 0) {
            n_seq_2 = n_seq_2.subvec(0,(counter-1));
        } else {
            n_seq_2 = n_seq_2.subvec(0,0);
        }

        //blockiness can be captured through state transition probabilities,


        double ag = 1 - (arma::mean(n_seq_2)/double(which_b_in_a.size()));
        if (ag == 1) {
            ag = 0;
        }

        double dg = 1 - (arma::mean(n_seq_1)/double(which_a_in_b.size()));
        if (dg == 1) {
            dg = 0;
        }

        double as = 1 - (arma::sum(n_seq_2)/double(which_b_in_a.size()));
        double ds = 1 - (arma::sum(n_seq_1)/double(which_a_in_b.size()));

        double prop_deletions = (ngram_size + 1 )*n_seq_1.size()/double(which_a_in_b.size());
        double prop_additions = (ngram_size + 1 )*n_seq_2.size()/double(which_b_in_a.size());

        if(n_seq_1[0] == 0) {
            prop_deletions = 0;
        }
        if(n_seq_2[0] == 0) {
            prop_additions = 0;
        }

        double prop_changes = (prop_deletions + prop_additions)/double(2);


        arma::vec ret = arma::zeros(37);

        ret[0] = ag;
        ret[1] = dg;
        ret[2] = as;
        ret[3] = ds;
        ret[4] = arma::mean(n_seq_2);
        ret[5] = arma::mean(n_seq_1);
        ret[6] = (as + ds)/double(2);
        ret[7] = (arma::sum(n_seq_2) + arma::sum(n_seq_1)) / double(n_seq_2.size() + n_seq_1.size());
        ret[8] = prop_deletions;
        ret[9]  = prop_additions;
        ret[10] = prop_changes;
        ret[11] = m_seq_1.size();
        ret[12] = arma::max(m_seq_1);
        ret[13] = arma::min(m_seq_1);
        ret[14] = arma::mean(m_seq_1);
        ret[15] = arma::median(m_seq_1);
        ret[16] = arma::var(m_seq_1);
        ret[17] = n_seq_1.size();
        ret[18] = arma::max(n_seq_1);
        ret[19] = arma::min(n_seq_1);
        ret[20] = arma::mean(n_seq_1);
        ret[21] = arma::median(n_seq_1);
        ret[22] = arma::var(n_seq_1);
        ret[23] = which_a_in_b.size();
        ret[24] = m_seq_2.size();
        ret[25]= arma::max(m_seq_2);
        ret[26] = arma::min(m_seq_2);
        ret[27] = arma::mean(m_seq_2);
        ret[28] = arma::median(m_seq_2);
        ret[29] = arma::var(m_seq_2);
        ret[30] = n_seq_2.size();
        ret[31] = arma::max(n_seq_2);
        ret[32] = arma::min(n_seq_2);
        ret[33] = arma::mean(n_seq_2);
        ret[34] = arma::median(n_seq_2);
        ret[35] = arma::var(n_seq_2);
        ret[36] = which_b_in_a.size();

        return ret;
    }

    arma::vec calculate_similarity_only(arma::vec which_a_in_b,
                                        arma::vec which_b_in_a) {
        arma::vec ret = arma::zeros(2);
        ret[0] = arma::mean(which_a_in_b);
        ret[1] = arma::mean(which_b_in_a);
        return ret;
    }

}



// [[Rcpp::export]]
arma::mat Efficient_Block_Sequential_String_Set_Hash_Comparison(
        List documents,
        int num_docs,
        arma::mat comparison_inds,
        int ngram_length,
        bool ignore_documents,
        arma::vec to_ignore){

    // allocate a vector to hold n-grams
    std::vector<std::vector<std::string>> ngrams(num_docs);
    std::vector<std::unordered_set<std::string>> dictionaries(num_docs);

    // store output 37 is the current number of metrics we compute
    int num_comparisons = comparison_inds.n_rows;
    arma::mat comparison_metrics =  arma::zeros(num_comparisons,37);

    int ignore_counter = 0;
    int cur_check = to_ignore[0];

    //loop through documents and form ngrams/hash them
    for(int i = 0; i < num_docs; ++i){
        // if we are ignoring documents then we check to see if we hash first
        bool hash = true;
        if (ignore_documents) {
            // if we are on a document we are skipping
            if (cur_check == i) {
                hash = false;
                ignore_counter += 1;
            }
        }

        if (hash) {
            std::unordered_set<std::string> dictionary;
            std::vector<std::string> doc = documents[i];
            //allocate vector to hold bigrams
            std::vector<std::string> cur_ngrams = doc;

            if(cur_ngrams.size() > (ngram_length - 1)) {
                // only erase terms if ngram length is atleast 1
                if(ngram_length > 1) {
                    cur_ngrams.erase(cur_ngrams.begin(),cur_ngrams.begin() + (ngram_length - 1));
                }
            }

            //populate ngrams and hashmap
            int cur_length = (ngram_length-1);
            if ((ngram_length-1) > cur_ngrams.size()) {
                cur_length = cur_ngrams.size();
            }
            for(int k = 0; k < cur_ngrams.size(); ++k){
                std::string cur = doc[k];
                if(ngram_length > 1) {
                    for(int l = 1; l < cur_length; ++l){
                        cur += doc[k+l];
                    }
                }
                cur_ngrams[k] = cur;
                dictionary.insert(cur);

            }

            ngrams[i] = cur_ngrams;
            dictionaries[i] = dictionary;
        }
    }

    Rcpp::Rcout << "Hashing Complete..." << std::endl;

    // LOOP OVER ALL COMPARISONS
    for(int i = 0; i < num_comparisons; ++i){

        if (i % 1000 == 0) {
            Rcpp::Rcout << "Current Comparison: " << i << " of " << num_comparisons << std::endl;
        }

        std::unordered_set<std::string> dictionary1 = dictionaries[comparison_inds(i,0)];
        std::unordered_set<std::string> dictionary2 = dictionaries[comparison_inds(i,1)];

        std::vector<std::string> ngrams_1 = ngrams[comparison_inds(i,0)];
        std::vector<std::string> ngrams_2 = ngrams[comparison_inds(i,1)];

        arma::vec which_a_in_b = arma::zeros(ngrams_1.size());
        arma::vec which_b_in_a = arma::zeros(ngrams_2.size());

        for(int k = 0; k < ngrams_1.size(); ++k){
            std::unordered_set<std::string>::const_iterator got = dictionary2.find(ngrams_1[k]);
            if (got != dictionary2.end()) {
                which_a_in_b[k] = 1;
            }
        }

        for(int k = 0; k < ngrams_2.size(); ++k){
            std::unordered_set<std::string>::const_iterator got = dictionary1.find(ngrams_2[k]);
            if (got != dictionary1.end()) {
                which_b_in_a[k] = 1;
            }
        }

        // now calculate comparison metrics
        arma::vec temp1 = mjd::calculate_metrics(
            which_a_in_b,
            which_b_in_a,
            ngram_length);
        comparison_metrics.row(i) = arma::trans(temp1);

    }

    return comparison_metrics;
}


// [[Rcpp::export]]
arma::mat Efficient_Block_Hash_Ngrams(
        std::vector<std::string> documents,
        int num_docs,
        arma::mat comparison_inds,
        int ngram_length,
        bool ignore_documents,
        arma::vec to_ignore){

    // allocate a vector to hold n-grams
    std::vector<std::vector<std::string>> ngrams(num_docs);
    std::vector<std::unordered_set<std::string>> dictionaries(num_docs);

    // store output 37 is the current number of metrics we compute
    int num_comparisons = comparison_inds.n_rows;
    arma::mat comparison_metrics =  arma::zeros(num_comparisons,2);

    int ignore_counter = 0;
    int cur_check = to_ignore[0];

    //loop through documents and form ngrams/hash them
    for(int i = 0; i < num_docs; ++i){
        bool hash = true;
        if (ignore_documents) {
            // if we are on a document we are skipping
            if (cur_check == i) {
                hash = false;
                ignore_counter += 1;
                cur_check = to_ignore[ignore_counter];
            }
        }

        if (hash) {
            std::unordered_set<std::string> dictionary;
            std::string temp = documents[i];

            std::vector<std::string> doc;
            boost::algorithm::split(doc, temp, boost::algorithm::is_any_of(" "));
            //allocate vector to hold bigrams
            std::vector<std::string> cur_ngrams = doc;

            if(cur_ngrams.size() >= ngram_length ) {
                // only erase terms if ngram length is atleast 1
                if (ngram_length > 1) {
                    cur_ngrams.erase(cur_ngrams.begin(),cur_ngrams.begin() + (ngram_length - 1));
                }
            } else {
                // if the document is not atleast as big as the ngram length,
                // then set the ngram vector to be a length of one vector.
                cur_ngrams.erase(cur_ngrams.begin(),cur_ngrams.begin() + (cur_ngrams.size() - 1));
            }

            //populate ngrams and hashmap
            int cur_length = ngram_length;
            if (ngram_length > doc.size()) {
                cur_length = doc.size();
            }

            for (int k = 0; k < cur_ngrams.size(); ++k) {
                std::string cur = doc[k];
                if (ngram_length > 1) {
                    for (int l = 1; l < cur_length; ++l) {
                        cur += doc[k+l];
                    }
                }

                //Rcpp::Rcout << cur << std::endl;
                cur_ngrams[k] = cur;
                dictionary.insert(cur);
            }

            ngrams[i] = cur_ngrams;
            dictionaries[i] = dictionary;
        }
    }

    Rcpp::Rcout << "Hashing Complete..." << std::endl;

    // LOOP OVER ALL COMPARISONS
    for(int i = 0; i < num_comparisons; ++i){

        if (i % 10000 == 0) {
            Rcpp::Rcout << "Current Comparison: " << i << " of " << num_comparisons << std::endl;
        }

        std::unordered_set<std::string> dictionary1 = dictionaries[comparison_inds(i,0)];
        std::unordered_set<std::string> dictionary2 = dictionaries[comparison_inds(i,1)];

        std::vector<std::string> ngrams_1 = ngrams[comparison_inds(i,0)];
        std::vector<std::string> ngrams_2 = ngrams[comparison_inds(i,1)];

        arma::vec which_a_in_b = arma::zeros(ngrams_1.size());
        arma::vec which_b_in_a = arma::zeros(ngrams_2.size());

        for(int k = 0; k < ngrams_1.size(); ++k){
            std::unordered_set<std::string>::const_iterator got = dictionary2.find(ngrams_1[k]);
            if (got != dictionary2.end()) {
                which_a_in_b[k] = 1;
            }
        }

        for(int k = 0; k < ngrams_2.size(); ++k){
            std::unordered_set<std::string>::const_iterator got = dictionary1.find(ngrams_2[k]);
            if (got != dictionary1.end()) {
                which_b_in_a[k] = 1;
            }
        }

        // now calculate comparison metrics
        arma::vec temp1 = mjd::calculate_similarity_only(
            which_a_in_b,
            which_b_in_a);
        comparison_metrics.row(i) = arma::trans(temp1);

    }

    return comparison_metrics;
}



// [[Rcpp::export]]
arma::mat String_Input_Sequential_String_Set_Hash_Comparison(
        std::vector<std::string> documents,
        int num_docs,
        arma::mat comparison_inds,
        int ngram_length,
        bool ignore_documents,
        arma::vec to_ignore){

    // allocate a vector to hold n-grams
    std::vector<std::vector<std::string>> ngrams(num_docs);
    std::vector<std::unordered_set<std::string>> dictionaries(num_docs);

    // store output 37 is the current number of metrics we compute
    int num_comparisons = comparison_inds.n_rows;
    arma::mat comparison_metrics =  arma::zeros(num_comparisons,37);

    int ignore_counter = 0;
    int cur_check = to_ignore[0];

    //loop through documents and form ngrams/hash them
    for (int i = 0; i < num_docs; ++i) {
        // if we are ignoring documents then we check to see if we hash first
        bool hash = true;
        if (ignore_documents) {
            // if we are on a document we are skipping
            if (cur_check == i) {
                hash = false;
                ignore_counter += 1;
                cur_check = to_ignore[ignore_counter];
            }
        }

        if (hash) {
            std::unordered_set<std::string> dictionary;
            std::string temp = documents[i];

            std::vector<std::string> doc;
            boost::algorithm::split(doc, temp, boost::algorithm::is_any_of(" "));
            //allocate vector to hold bigrams
            std::vector<std::string> cur_ngrams = doc;

            if(cur_ngrams.size() >= ngram_length ) {
                // only erase terms if ngram length is atleast 1
                if (ngram_length > 1) {
                    cur_ngrams.erase(cur_ngrams.begin(),cur_ngrams.begin() + (ngram_length - 1));
                }
            } else {
                // if the document is not atleast as big as the ngram length,
                // then set the ngram vector to be a length of one vector.
                cur_ngrams.erase(cur_ngrams.begin(),cur_ngrams.begin() + (cur_ngrams.size() - 1));
            }

            //populate ngrams and hashmap
            int cur_length = ngram_length;
            if (ngram_length > doc.size()) {
                cur_length = doc.size();
            }

            for (int k = 0; k < cur_ngrams.size(); ++k) {
                std::string cur = doc[k];
                if (ngram_length > 1) {
                    for (int l = 1; l < cur_length; ++l) {
                        cur += doc[k+l];
                    }
                }

                //Rcpp::Rcout << cur << std::endl;
                cur_ngrams[k] = cur;
                dictionary.insert(cur);
            }

            ngrams[i] = cur_ngrams;
            dictionaries[i] = dictionary;
        }
    }

    Rcpp::Rcout << "Hashing Complete..." << std::endl;

    // LOOP OVER ALL COMPARISONS
    for(int i = 0; i < num_comparisons; ++i){

        if (i % 1000 == 0) {
            Rcpp::Rcout << "Current Comparison: " << i << " of " << num_comparisons << std::endl;
        }

        std::unordered_set<std::string> dictionary1 = dictionaries[comparison_inds(i,0)];
        std::unordered_set<std::string> dictionary2 = dictionaries[comparison_inds(i,1)];

        std::vector<std::string> ngrams_1 = ngrams[comparison_inds(i,0)];
        std::vector<std::string> ngrams_2 = ngrams[comparison_inds(i,1)];

        arma::vec which_a_in_b = arma::zeros(ngrams_1.size());
        arma::vec which_b_in_a = arma::zeros(ngrams_2.size());

        for(int k = 0; k < ngrams_1.size(); ++k){
            std::unordered_set<std::string>::const_iterator got = dictionary2.find(ngrams_1[k]);
            if (got != dictionary2.end()) {
                which_a_in_b[k] = 1;
            }
        }

        for(int k = 0; k < ngrams_2.size(); ++k){
            std::unordered_set<std::string>::const_iterator got = dictionary1.find(ngrams_2[k]);
            if (got != dictionary1.end()) {
                which_b_in_a[k] = 1;
            }
        }

        // now calculate comparison metrics
        arma::vec temp1 = mjd::calculate_metrics(
            which_a_in_b,
            which_b_in_a,
            ngram_length);
        comparison_metrics.row(i) = arma::trans(temp1);

    }

    return comparison_metrics;
}

