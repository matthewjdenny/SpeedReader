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

    mss <- data.frame(num_match_blocks = c(length(m_seq_1),length(m_seq_2)),
                      max_block_length = c(max(m_seq_1),max(m_seq_2)),
                      min_block_length = c(min(m_seq_1),min(m_seq_2)),
                      mean_block_length = c(mean(m_seq_1),mean(m_seq_2)),
                      median_block_length = c(median(m_seq_1),median(m_seq_2)),
                      total_blocks = c(length(res[[1]]), length(res[[2]])))

    result <- list(matches_document_1 = res[[1]],
                   matches_document_2 = res[[2]],
                   ngram_length = ngram_size,
                   match_sequence_statistics = mss)
    t2 <- proc.time() - ptm
    cat("Complete in:",t2[[3]],"seconds...\n")
    return(result)
}

