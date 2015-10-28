#' SpeedReader: functions to facilitate high performance text processing in R.
#'
#' @section SpeedReader functions:
#' To use this package, You will first want to check out the generate_document_term_vectors() function which will take raw data and transform it into document term vectors. You will then likely want to generate a document term matri using either generate_document_term_matrix() or generate_sparse_large_document_term_matrix() depending on the number of documents you are dealing with. After that, this package provides a number of other functions for simple pointwise mututal information based analysis as will eventualy link to the MALLET libraries for topic modeling with billions of tokens.
#'
#' @docType package
#' @name SpeedReader
NULL
#> NULL

#' @import methods
NULL

#' @importFrom grDevices dev.off pdf rgb
NULL


#' @useDynLib SpeedReader
#' @importFrom Rcpp sourceCpp
NULL
#> NULL
