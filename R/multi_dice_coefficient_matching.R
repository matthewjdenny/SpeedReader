#' @title Multiple N-Gram Lngth Dice Coefficient Document Matching
#' @description Calculate N-Gram wise Dice coefficients for different N-Gram
#' Lengths.
#'
#' @param document_1 A vector of strings (one per line or one per sentence), or
#' a list of vectors of tokens  (one per line or one per sentence).
#' @param document_2 Same as document_1, will be used for comparison.
#' @param ngram_sizes A numeric vector of N-Gram lengths for us in calculating
#' Dice coefficients.
#' @return A data.frame with Dice coefficients based on different N-Gram lengths.
#' @export
multi_dice_coefficient_matching <- function(document_1,
                                            document_2,
                                            ngram_sizes = c(1:50)){

    ptm <- proc.time()
    cat("Whitespace tokenizing (if necessary)...\n")
    if (class(document_1) != "list") {
        doc <- vector(mode = "list",length = length(document_1))
        for (i in 1:length(doc)) {
            cur <- stringr::str_split(tolower(document_1[i]),"[\\s]+")[[1]]
            doc[[i]] <- cur
        }
        document_1 <- doc
    }

    if (class(document_2) != "list") {
        doc <- vector(mode = "list",length = length(document_2))
        for (i in 1:length(doc)) {
            cur <- stringr::str_split(tolower(document_2[i]),"[\\s]+")[[1]]
            doc[[i]] <- cur
        }
        document_2 <- doc
    }

    doc <- paste0(unlist(document_1),collapse = " ")
    doc <- stringr::str_replace_all(doc, "[\\s]+", " ")[[1]]
    doc <- stringr::str_split(doc, " ")[[1]]
    document_1 <- list(doc = doc)
    doc <- paste0(unlist(document_2),collapse = " ")
    doc <- stringr::str_replace_all(doc, "[\\s]+", " ")[[1]]
    doc <- stringr::str_split(doc, " ")[[1]]
    document_2 <- list(doc = doc)


    Dice_coefs <- data.frame(ngram_size = ngram_sizes,
                             dice_coef = rep(0,length(ngram_sizes)))
    # get dice coefficients
    cat("Calculating Dice coefficients..\n")
    for(i in 1:length(ngram_sizes)) {
        cat("Calculating Dice coefficient for n-gram size:",ngram_sizes[i],"\n")
        dice_value <- Variable_Dice_Coefficients(
            length(document_1),
            document_1,
            length(document_2),
            document_2,
            ngram_sizes[i])

        Dice_coefs[i,2] <- dice_value
    }

    t2 <- proc.time() - ptm
    cat("Complete in:",t2[[3]],"seconds...\n")
    return(Dice_coefs)
}
