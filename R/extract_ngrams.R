extract_ngrams <- function(tokenized_document,
                           ngram_lengths,
                           remove_punctuation,
                           remove_numeric,
                           lemmatize,
                           lowercase){
    ngram_list <- vector(mode = "list", length = length(ngram_lengths))
    names(ngram_list) <- paste0(ngram_lengths,"_grams",sep = "")

    # loop over lengths
    for (i in 1:length(ngram_lengths)) {
        if (nrow(tokenized_document) < ngram_lengths[i]) {
            cat("Cannot extract N-Grams of length:",ngram_lengths[i],
                "as the document only contains:",nrow(tokenized_document),"\n")
        } else {
            cat("Extracting ",ngram_lengths[i],"-Grams...\n",sep = "")
            # create a vector to store ngrams in:
            ngrams <- rep("",length = (nrow(tokenized_document) -
                                           ngram_lengths[i] + 1))
            # counter to keep track of the number we actually find
            ngram_counter <- 1
            # use out conditionals to reduce the number of logical checks that have
            # to be performed (I know its messy)
            if (remove_punctuation) {
                if (remove_numeric) {
                    if (lemmatize) {
                        # no punct, no numbers, yes lemmas
                        for (j in 1:length(ngrams)) {
                            end <- j + ngram_lengths[i] - 1
                            cur_sum <- sum(tokenized_document$punctuation[j:end] +
                                               tokenized_document$numeric[j:end])
                            if (cur_sum == 0) {
                                ngrams[ngram_counter] <- paste0(
                                    tokenized_document$lemma[j:end],collapse = "_")
                                ngram_counter <- ngram_counter + 1
                            }
                        }
                    } else {
                        # no punct, no numbers, no lemmas
                        for (j in 1:length(ngrams)) {
                            end <- j + ngram_lengths[i] - 1
                            cur_sum <- sum(tokenized_document$punctuation[j:end] +
                                               tokenized_document$numeric[j:end])
                            if (cur_sum == 0) {
                                ngrams[ngram_counter] <- paste0(
                                    tokenized_document$word[j:end],collapse = "_")
                                ngram_counter <- ngram_counter + 1
                            }
                        }
                    }
                } else {
                    if (lemmatize) {
                        # no punct, yes numbers, yes lemmas
                        for (j in 1:length(ngrams)) {
                            end <- j + ngram_lengths[i] - 1
                            cur_sum <- sum(tokenized_document$punctuation[j:end])
                            if (cur_sum == 0) {
                                ngrams[ngram_counter] <- paste0(
                                    tokenized_document$lemma[j:end],collapse = "_")
                                ngram_counter <- ngram_counter + 1
                            }
                        }
                    } else {
                        # no punct, yes numbers, no lemmas
                        for (j in 1:length(ngrams)) {
                            end <- j + ngram_lengths[i] - 1
                            cur_sum <- sum(tokenized_document$punctuation[j:end])
                            if (cur_sum == 0) {
                                ngrams[ngram_counter] <- paste0(
                                    tokenized_document$word[j:end],collapse = "_")
                                ngram_counter <- ngram_counter + 1
                            }
                        }
                    }
                }
            } else {
                if (remove_numeric) {
                    if (lemmatize) {
                        # yes punct, no numbers, yes lemmas
                        for (j in 1:length(ngrams)) {
                            end <- j + ngram_lengths[i] - 1
                            cur_sum <- sum(tokenized_document$numeric[j:end])
                            if (cur_sum == 0) {
                                ngrams[ngram_counter] <- paste0(
                                    tokenized_document$lemma[j:end],collapse = "_")
                                ngram_counter <- ngram_counter + 1
                            }
                        }
                    } else {
                        # yes punct, no numbers, no lemmas
                        for (j in 1:length(ngrams)) {
                            end <- j + ngram_lengths[i] - 1
                            cur_sum <- sum(tokenized_document$numeric[j:end])
                            if (cur_sum == 0) {
                                ngrams[ngram_counter] <- paste0(
                                    tokenized_document$word[j:end],collapse = "_")
                                ngram_counter <- ngram_counter + 1
                            }
                        }
                    }
                } else {
                    if (lemmatize) {
                        # yes punct, yes numbers, yes lemmas
                        for (j in 1:length(ngrams)) {
                            end <- j + ngram_lengths[i] - 1
                            ngrams[ngram_counter] <- paste0(
                                tokenized_document$lemma[j:end],collapse = "_")
                            ngram_counter <- ngram_counter + 1

                        }
                    } else {
                        # yes punct, yes numbers, no lemmas
                        for (j in 1:length(ngrams)) {
                            end <- j + ngram_lengths[i] - 1
                            ngrams[ngram_counter] <- paste0(
                                tokenized_document$word[j:end],collapse = "_")
                            ngram_counter <- ngram_counter + 1
                        }
                    }
                }
            }# end of messy conditionals

            # subset the vector of ngrams to only those we are keeping, then store
            # them.
            ngrams <- ngrams[1:ngram_counter]

            # if we are lowercasing, do so now
            if (lowercase) {
                ngrams <- tolower(ngrams)
            }
            ngram_list[[i]] <- ngrams
        }
    } # end of loop ngram lengths

    return(ngram_list)
}
