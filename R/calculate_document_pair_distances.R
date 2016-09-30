#' @title Document Distances
#' @description Calculate distances between pairs of documents.
#'
#' @param document_term_matrix A sparse matrix object of class
#' "simple_triplet_matrix", or a dense matrix object, with documents as rows and
#' vocabulary entries as columns.
#' @param document_indicies A numeric vector of length two (document_a_row_index,
#' document_b_row_index), or a list object with each entry containing a vector
#' of length two as described above.
#' @param distance_method Can be one of "cosine" or "euclidean". Defaults to
#' "cosine".
#' @return A vector of document pair distances
#' @export
calculate_document_pair_distances <- function(document_term_matrix,
                                              document_indicies,
                                              distance_method = "cosine"){

  ptm <- proc.time()

  # get the number of comparisons
  num_comparisons <- 1
  if (class(document_indicies) == "list") {
    num_comparisons <- length(document_indicies)
  } else if ((class(document_indicies) == "numeric") &
             (length(document_indicies) == 2)){
    document_indicies <- list(first_pair = document_indicies)
  } else {
    stop("You have provided document_indicies in the wrong form...")
  }

  pair_distances <- rep(0, num_comparisons)

  for (i in 1:num_comparisons) {
    cat("Currently working on document pair",i,"of",num_comparisons,"\n")
    indicies <- document_indicies[[i]]
    cat("Extracting rows...\n")
    # extract rows, this will take a while
    if (class(document_term_matrix) == "simple_triplet_matrix") {
      document_1 <- as.numeric(as.matrix(document_term_matrix[indicies[1],]))
    } else {
      document_1 <- document_term_matrix[indicies[1],]
    }
    if (class(document_term_matrix) == "simple_triplet_matrix") {
      document_2 <- as.numeric(as.matrix(document_term_matrix[indicies[2],]))
    } else {
      document_2 <- document_term_matrix[indicies[2],]
    }

    data <- rbind(document_1,document_2)

    cat("Calculating pairwise distance...\n")
    # calculate the document similarity matrix
    simil <- proxy::simil(data, method = distance_method)

    simil <- as.matrix(simil)
    distances2 <- proxy::pr_simil2dist(simil)
    pair_distances[i] <- distances2[2,1]
  }
  t2 <- proc.time() - ptm
  cat("Complete in:",t2[[3]],"seconds...\n")
  return(pair_distances)
}

