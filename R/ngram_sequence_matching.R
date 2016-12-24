#' @title N-Gram Sequence Matching
#' @description Calculates the positions of n-grams in two document versions
#' which match an ngram in the other version.
#'
#' @param document_1 A string (or a character vector) representing the earlier
#' document version.
#' @param document_2 A string (or a character vector) representing the later
#' document version.
#' @param ngram_size The length of n-grams to be compared
#' @param use_hashmap Defaults to FALSE. If TRUE, then a hashmap is used for
#' faster lookup and comparisons.
#' @param tokenized_strings_provided Defaults to FALSE. If TRUE, then
#' pre-tokenized strings are expected as character vectors.
#' @return A List object.
#' @export
ngram_sequence_matching <- function(document_1,
                                    document_2,
                                    ngram_size,
                                    use_hashmap = FALSE,
                                    tokenized_strings_provided = FALSE){

    ptm <- proc.time()

    if (tokenized_strings_provided) {
        if (!use_hashmap) {
            document_1 <- doc
            document_2 <- doc
        }
    } else {
        if (length(document_1) > 1) {
            doc <- paste0(document_1,collapse = " ")
        } else {
            doc <- document_1
        }

        doc <- stringr::str_replace_all(doc, "[\\s]+", " ")[[1]]
        doc <- stringr::str_split(doc, " ")[[1]]
        document_1 <- doc

        if (length(document_2) > 1) {
            doc <- paste0(document_2,collapse = " ")
        } else {
            doc <- document_2
        }
        doc <- stringr::str_replace_all(doc, "[\\s]+", " ")[[1]]
        doc <- stringr::str_split(doc, " ")[[1]]
        document_2 <- doc
    }


    if (use_hashmap) {
        if (ngram_size == 1) {
            res <- Sequential_Token_Set_Hash_Comparison(document_1,
                                                        document_2)

        } else {
            res <- Sequential_string_Set_Hash_Comparison(document_1,
                                                         document_2,
                                                         ngram_size)
        }

    } else {
        res <- Sequential_Raw_Term_Dice_Matches(document_1,
                                                document_2,
                                                ngram_size)
    }


    # we can also calculate some metrics on these like the average length of
    # contiguous match sequence
    m_seq_1 <- NULL
    cur_match <- 0
    ms <- res[[1]]
    for (i in 1:length(ms)) {
        if (ms[i] == 1) {
            cur_match <- cur_match + 1
        } else {
            if (cur_match != 0) {
                m_seq_1 <- c(m_seq_1,cur_match)
                cur_match <- 0
            }
        }
    }

    # deal with case of no matches
    if(is.null(m_seq_1)) {
        m_seq_1 <- 0
    }

    m_seq_2 <- NULL
    cur_match <- 0
    ms <- res[[2]]
    for (i in 1:length(ms)) {
        if (ms[i] == 1) {
            cur_match <- cur_match + 1
        } else {
            if (cur_match != 0) {
                m_seq_2 <- c(m_seq_2,cur_match)
                cur_match <- 0
            }
        }
    }

    # deal with case of no matches
    if(is.null(m_seq_2)) {
        m_seq_2 <- 0
    }

    # now look for non-matches
    n_seq_1 <- NULL
    cur_match <- 0
    ms <- res[[1]]
    for (i in 1:length(ms)) {
        if (ms[i] == 0) {
            cur_match <- cur_match + 1
        } else {
            if (cur_match != 0) {
                n_seq_1 <- c(n_seq_1,cur_match)
                cur_match <- 0
            }
        }
    }

    # deal with case of no matches
    if(is.null(n_seq_1)) {
        n_seq_1 <- 0
    }

    n_seq_2 <- NULL
    cur_match <- 0
    ms <- res[[2]]
    for (i in 1:length(ms)) {
        if (ms[i] == 0) {
            cur_match <- cur_match + 1
        } else {
            if (cur_match != 0) {
                n_seq_2 <- c(n_seq_2,cur_match)
                cur_match <- 0
            }
        }
    }

    # deal with case of no matches
    if(is.null(n_seq_2)) {
        n_seq_2 <- 0
    }


    # blockiness can be captured through state transition probabilities,


    ag <- 1 - (mean(n_seq_2)/length(res[[2]]))
    if (ag == 1) {
        ag <- 0
    }

    dg <- 1 - (mean(n_seq_1)/length(res[[1]]))
    if (dg == 1) {
        dg <- 0
    }

    as <- 1 - sum(res[[2]])/length(res[[2]])
    ds <- 1 - sum(res[[1]])/length(res[[1]])

    prop_deletions <- (ngram_size + 1 )*length(n_seq_1)/length(res[[1]])
    prop_additions <- (ngram_size + 1 )*length(n_seq_2)/length(res[[2]])

    if(n_seq_1[1] == 0) {
        prop_deletions <- 0
    }
    if(n_seq_2[1] == 0) {
        prop_additions <- 0
    }
    prop_changes <- mean(prop_deletions, prop_additions)



    mss <- data.frame(addition_granularity = ag,
                      deletion_granularity = dg,
                      addition_scope = as,
                      deletion_scope = ds,
                      average_addition_size = mean(n_seq_2),
                      average_deletion_size = mean(n_seq_1),
                      scope = mean(as, ds),
                      average_edit_size = mean(c(n_seq_2, n_seq_1)),
                      prop_deletions = prop_deletions,
                      prop_additions  = prop_additions,
                      prop_changes = prop_changes,
                      num_match_blocks_v1 = length(m_seq_1),
                      max_match_length_v1 = max(m_seq_1),
                      min_match_length_v1 = min(m_seq_1),
                      mean_match_length_v1 = mean(m_seq_1),
                      median_match_length_v1 = median(m_seq_1),
                      match_length_variance_v1 = var(m_seq_1),
                      num_nonmatch_blocks_v1 = length(n_seq_1),
                      max_nonmatch_length_v1 = max(n_seq_1),
                      min_nonmatch_length_v1 = min(n_seq_1),
                      mean_nonmatch_length_v1 = mean(n_seq_1),
                      median_nonmatch_length_v1 = median(n_seq_1),
                      nonmatch_length_variance_v1 = var(n_seq_1),
                      total_ngrams_v1 = length(res[[1]]),
                      num_match_blocks_v2 = length(m_seq_2),
                      max_match_length_v2 = max(m_seq_2),
                      min_match_length_v2 = min(m_seq_2),
                      mean_match_length_v2 = mean(m_seq_2),
                      median_match_length_v2 = median(m_seq_2),
                      match_length_variance_v2 = var(m_seq_2),
                      num_nonmatch_blocks_v2 = length(n_seq_2),
                      max_nonmatch_length_v2 = max(n_seq_2),
                      min_nonmatch_length_v2 = min(n_seq_2),
                      mean_nonmatch_length_v2 = mean(n_seq_2),
                      median_nonmatch_length_v2 = median(n_seq_2),
                      nonmatch_length_variance_v2 = var(n_seq_2),
                      total_ngrams_v2 = length(res[[2]]))

    result <- list(matches_version_1 = res[[1]],
                   matches_version_2 = res[[2]],
                   ngram_length = ngram_size,
                   match_sequence_statistics = mss,
                   addition_granularity = ag,
                   deletion_granularity = dg,
                   addition_scope = as,
                   deletion_scope = ds,
                   average_addition_size = mean(n_seq_2),
                   average_deletion_size = mean(n_seq_1),
                   scope = mean(as, ds),
                   average_edit_size = mean(c(n_seq_2, n_seq_1)),
                   prop_deletions = prop_deletions,
                   prop_additions  = prop_additions,
                   prop_changes = prop_changes)
    t2 <- proc.time() - ptm
    cat("Complete in:",t2[[3]],"seconds...\n")
    return(result)
}

