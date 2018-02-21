#' @title Reference distribtuion distances
#' @description Calculates the euclidean distance (up to a proportionality)
#' between document term distributions and a set of reference distributions.
#'
#' @param category_reference_distribution A simple_triplet_matrix where each row
#' represents the distribution over terms in a particular category. These can
#' be normalized or raw counts.
#' @param document_term_matrix A simple_triplet_matrix where each row represents
#' a document and each column, a term in the vocabulary. The columns in both
#' matrices should match up.
#' @param inverse_frequency_weighting If TRUE, then distances are weighted by
#' the inverse of the term's aggregate count in the document term
#' matrix. This means that differences in more frequently occuring terms will
#' have less weight than those for less frequently appearing terms. Defaults to
#' TRUE.
#' @param large_matrix Defaults to FALSE. If TRUE, then a method that is robust
#' to large matrices will be used. Set this if you get an erro of the form:
#' "'i, j, nrow, ncol' invalid type".
#' @return A dataframe with distances of each document to each reference
#' distribution. The last column indicates the closest reference distribtuion
#' for each document.
#' @export
reference_distribution_distance <- function(category_reference_distribution,
                                           document_term_matrix,
                                           inverse_frequency_weighting = TRUE,
                                           large_matrix = FALSE) {

    if (ncol(category_reference_distribution) != ncol(document_term_matrix)) {
        stop("category_reference_distribution and document_term_matrix must have the same vocabulary.")
    }

    # generate term weights (which are 1 if no weighting)
    term_weights <- rep(1,ncol(document_term_matrix))
    if (inverse_frequency_weighting) {
        if (large_matrix) {
            counts <- table(document_term_matrix$j)
            cs <- as.numeric(counts)
        } else {
            cs <- slam::col_sums(document_term_matrix)
        }

        # replace any zero column sums with 1's so we don't get weird errors
        zeros <- which(cs == 0)
        if (length(zeros) > 0) {
            cs[zeros] <- 1
        }
        term_weights <- 1/cs
    }

    # normalize the rows of each
    cat("Normalizing rows of category_reference_distribution...\n")
    rsums <- slam::row_sums(category_reference_distribution)
    for (i in 1:nrow(category_reference_distribution)) {
        temp <- category_reference_distribution[i,]/rsums[i]
        inds <- which(category_reference_distribution$i == i)
        category_reference_distribution$v[inds] <- temp$v
    }

    cat("Normalizing rows of document_term_matrix...\n")
    if (large_matrix) {
        counts <- table(document_term_matrix$i)
        rsums <- as.numeric(counts)
    } else {
        rsums <- slam::row_sums(document_term_matrix)
    }

    start <- 1
    end <- 1
    for (i in 1:nrow(document_term_matrix)) {
        if (i %% 10000 == 0) {
            cat("Current Document:",i,"\n")
        }
        len <- length(document_term_matrix$i)
        while (document_term_matrix$i[end] == i) {
            end <- end + 1
            if (end > len) {
                break
            }
        }
        document_term_matrix$v[start:(end-1)] <- document_term_matrix$v[start:(end-1)]/rsums[i]
        start <- end
    }

    # now we calculate
    distances <- reference_dist_distance(
        category_reference_distribution$i - 1,
        category_reference_distribution$j - 1,
        category_reference_distribution$v,
        document_term_matrix$i - 1,
        document_term_matrix$j - 1,
        document_term_matrix$v,
        nrow(category_reference_distribution),
        nrow(document_term_matrix),
        term_weights
    )

    cat("Finding minimum distance categories for each document...\n")
    # find the minimum distance
    if (!is.null(rownames(category_reference_distribution))) {
        names <- rownames(category_reference_distribution)
        minimum_distance_category <- rep("",nrow(document_term_matrix))
    } else {
        names <- 1:nrow(category_reference_distribution)
        minimum_distance_category <- rep(0,nrow(document_term_matrix))
    }

    for (i in 1:nrow(document_term_matrix)) {
        if (i %% 10000 == 0) {
            cat("Current Document:",i,"\n")
        }
        ind <- which(distances[i,] == min(distances[i,]))[1]
        minimum_distance_category[i] <- names[ind]
    }

    to_return <- as.data.frame(distances)
    colnames(to_return) <- names
    to_return$minimum_distance_category <- minimum_distance_category

    return(to_return)
}
