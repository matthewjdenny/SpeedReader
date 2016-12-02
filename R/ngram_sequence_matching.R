#' @title N-Gram Sequence Matching
#' @description Calculates the positions of n-grams in two document versions
#' which match an ngram in the other version.
#'
#' @param document_1 A string (or a character vector) representing the earlier
#' document version.
#' @param document_2 A string (or a character vector) representing the later
#' document version.
#' @param ngram_size The length of n-grams to be compared
#' @return A List object.
#' @export
ngram_sequence_matching <- function(document_1,
                                    document_2,
                                    ngram_size){

    ptm <- proc.time()

    doc <- paste0(document_1,collapse = " ")
    doc <- stringr::str_replace_all(doc, "[\\s]+", " ")[[1]]
    doc <- stringr::str_split(doc, " ")[[1]]
    document_1 <- list(doc = doc)
    doc <- paste0(document_2,collapse = " ")
    doc <- stringr::str_replace_all(doc, "[\\s]+", " ")[[1]]
    doc <- stringr::str_split(doc, " ")[[1]]
    document_2 <- list(doc = doc)

    res <- Sequential_Raw_Term_Dice_Matches(document_1,
                                               document_2,
                                               ngram_size)

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

    mss <- data.frame(num_match_blocks = c(length(m_seq_1),length(m_seq_2)),
                      max_match_length = c(max(m_seq_1),max(m_seq_2)),
                      min_match_length = c(min(m_seq_1),min(m_seq_2)),
                      mean_match_length = c(mean(m_seq_1),mean(m_seq_2)),
                      median_match_length = c(median(m_seq_1),median(m_seq_2)),
                      match_length_variance = c(var(m_seq_1),var(m_seq_2)),
                      num_nonmatch_blocks = c(length(n_seq_1),length(n_seq_2)),
                      max_nonmatch_length = c(max(n_seq_1),max(n_seq_2)),
                      min_nonmatch_length = c(min(n_seq_1),min(n_seq_2)),
                      mean_nonmatch_length = c(mean(n_seq_1),mean(n_seq_2)),
                      median_nonmatch_length = c(median(n_seq_1),median(n_seq_2)),
                      nonmatch_length_variance = c(var(n_seq_1),var(n_seq_2)),
                      total_ngrams = c(length(res[[1]]), length(res[[2]])))

    row.names(mss) <- c("Version_1", "Version_2")

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

    prop_deletions <- 2 * length(n_seq_1)/length(res[[1]])
    prop_additions <- 2 * length(n_seq_2)/length(res[[2]])
    prop_changes <- mean(prop_deletions, prop_additions)

    mss <- as.data.frame(t(mss))
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

