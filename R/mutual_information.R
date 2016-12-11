#' @title Mutual Information
#' @description Calculate Mutual Information
#'
#' @param joint_dist A matrix of class "simple_triplet_matrix"
#' or a standard dense matrix.
#' @param normalized Defaults to FALSE. If TRUE, then the function expects a
#' matrix which sums to one. WARNING -- this function will not check to see if
#' the matrix sums  to one if TRUE (for speed reasons).
#' @param non_zero_column_entries Defaults to NULL, otherwise, a vector of
#' length equal to the number of rows in the joint_dist recording the number of
#' non-zero column entries for that row. Can greatly speed up computation. Must
#' be supplied if normalized = TRUE.
#' @return The mutual information of the joint distribution.
#' @export
mutual_information <- function(joint_dist,
                               normalized = FALSE,
                               non_zero_column_entries = NULL){

    ptm <- proc.time()

    # deal with case where we have a dense matrix
    if (class(joint_dist) == "matrix") {

        # deal with case where we have an already normalized matrix
        if (!is.null(non_zero_column_entries)) {
            if (normalized) {
                MI <- Fast_Mutual_Information(joint_dist,
                                              non_zero_column_entries)
            } else {
                joint_dist <- joint_dist/sum(joint_dist)
                MI <- Fast_Mutual_Information(joint_dist,
                                              non_zero_column_entries)
            }
        } else {
            MI <- Mutual_Information(joint_dist)
        }

    }

    # deal with the case where we have a sparse matrix
    if (class(joint_dist) == "simple_triplet_matrix") {

        cat("Normalizing, calculating row and column sums...\n")
        #noramlize joint dist and take row and column sums
        joint_dist$v <- joint_dist$v/sum(joint_dist$v)
        normalized_column_sums <- slam::col_sums(joint_dist)
        normalized_row_sums <- slam::row_sums(joint_dist)

        i <- joint_dist$i - 1
        j <- joint_dist$j - 1
        v <- joint_dist$v
        num_entries <- length(i)

        cat("Calculating Mutual Information...\n")
        MI <- Fast_Sparse_Mutual_Information(
            i,
            j,
            v,
            normalized_column_sums,
            normalized_row_sums,
            num_entries)

    }


    t2 <- proc.time() - ptm
    cat("Complete in:",t2[[3]],"seconds...\n")
    return(MI)
}
