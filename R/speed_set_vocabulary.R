#' A function the reorgaizes vocabulary to speed up document term matrix formation using a string stem dictionary.
#'
#' @param vocab A vocabulary list object returned by the count_words() function.
#' @param term_frequency_threshold A threshold below which all words appearing fewer than that many times in the corpus will be removed. Defaults to 0 in which case no words will be removed.
#' @param cores The number of cores we wish to use for parallelization to speed up computation. Defaults to 1.
#' @return A vocabulary list object.
#' @export
speed_set_vocabulary <- function(vocab,
                                 term_frequency_threshold = 0,
                                 cores = 1){

    cat("Removing terms from vocabulary that appear less than",
        term_frequency_threshold, "times in the data...\n")
    #remove infrequently used terms
    to_remove <- which(vocab$word_counts < term_frequency_threshold)
    cat("Initial vocabulary size:",vocab$total_unique_words,"\n")
    if(length(to_remove) > 0){
        vocab$unique_words <- vocab$unique_words[-to_remove]
        vocab$word_counts <- vocab$word_counts[-to_remove]
        vocab$total_unique_words <- length(vocab$word_counts)
    }
    cat("Reduced vocabulary size:",vocab$total_unique_words,"\n")

    cat("Sorting vocabulary alphabetically...\n")
    # sort vocabulary alphabetically
    ordering <- order(vocab$unique_words)
    Sorted_Vocabulary <- vocab$unique_words[ordering]
    cat("Removing terms that are fewer than three characters long...\n")
    #remove overly short words

    numchars <- as.numeric(sapply(Sorted_Vocabulary,nchar))
    cat("Removing:",Sorted_Vocabulary[which(numchars < 3)],"\n")
    Sorted_Vocabulary <- Sorted_Vocabulary[which(numchars > 2)]

    # get first three characters of every string
    get_first_three <- function(i){
        substr(Sorted_Vocabulary[i],1,3)
    }
    cat("Extracting the first 3 characters of each term, this may take a while...\n")
    first_3_chars <- parallel::mclapply(1:length(Sorted_Vocabulary),
                                        get_first_three,
                                        mc.cores = cores)
    first_3_chars <- unlist(first_3_chars)

    #get unique first three characters
    unique_first_3 <- unique(first_3_chars)

    lookup <- data.frame(stem = unique_first_3,
                         stringsAsFactors = FALSE)
    cat("Generating flattened tree lookup...\n")
    # get spans of each stem in vocabulary
    first_time_used <- rep(0,nrow(lookup))
    counter <- 1
    for(i in 1:length(first_3_chars)){
        if(unique_first_3[counter] == first_3_chars[i]){
            first_time_used[counter] <- i
            counter <- counter + 1
            if (counter > length(unique_first_3)) {
                break
            }
        }
    }

    last_time_used <- rep(0,nrow(lookup))
    for(i in 1:(length(first_time_used) - 1)){
        last_time_used[i] <- first_time_used[i+1] -1
    }
    last_time_used[length(last_time_used)] <- length(first_3_chars)

    count <- last_time_used - first_time_used + 1
    lookup <- cbind(lookup,count,first_time_used,last_time_used)
    ordered_lookup <- lookup[order(lookup$count,decreasing = T),]
    cat("Returning speed-set vocabulary...\n")
    vocabulary <- list(vocabulary = Sorted_Vocabulary,
                       stems = ordered_lookup$stem,
                       stem_count = ordered_lookup$count,
                       stem_first_use = ordered_lookup$first_time_used,
                       stem_last_use = ordered_lookup$last_time_used,
                       type = "stem-lookup")

    return(vocabulary)
}

