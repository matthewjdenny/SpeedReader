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


    result <- list(matches_document_1 = res[[1]],
                   matches_document_2 = res[[2]],
                   ngram_length = ngram_size)
    t2 <- proc.time() - ptm
    cat("Complete in:",t2[[3]],"seconds...\n")
    return(result)
}

