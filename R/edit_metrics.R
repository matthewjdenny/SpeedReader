#' @title Calculate Edit Metrics Between Two Document Versions
#' @description Calculate Scope and Granularity of Document Edits.
#'
#' @param document_1 A vector of strings (one per line or one per sentence), or
#' a list of vectors of tokens  (one per line or one per sentence).
#' @param document_2 Same as document_1, will be used for comparison.
#' @param ngram_sizes A numeric vector of N-Gram lengths for us in calculating
#' Dice coefficients.
#' @return A data.frame with Dice coefficients based on different N-Gram lengths.
#' @export
edit_metrics <- function(document_1,
                         document_2,
                         ngram_sizes = c(1:50)){

    dice <- multi_dice_coefficient_matching(document_1 = document_1,
                                            document_2 = document_2,
                                            ngram_sizes = ngram_sizes,
                                            remove_duplicates = TRUE)

    # cacluate scope
    scope <- 1 - mean(dice$dice_coef)

    # calculate granularity
    max_dice <- max(dice$dice_coef)
    min_dice <- min(dice$dice_coef)
    K <- length(dice$dice_coef)
    min_ind <- which(dice$dice_coef == max_dice)
    min_ind <- min_ind[length(min_ind)]
    theoretical_max <- seq(max_dice,min_dice,length.out = (K - min_ind))
    denominator <- (theoretical_max - min_dice)*(K - 1 - min_ind)
    granularity <- 0

    if (max_dice != min_dice) {
        counter <- 1
        for (i in (min_ind + 1):(K-1)) {
            granularity <- granularity + max((theoretical_max[counter] - dice$dice_coef[i])/denominator[counter],0)
            counter <- counter + 1
        }
    }

    # return everything
    ret <- list(granularity = granularity,
                scope = scope,
                dice_coefficients = dice)

    return(ret)
}
