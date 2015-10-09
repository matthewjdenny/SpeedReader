#' A function to unlist and concatenate a subset of a matrix/data.frame
#'
#' @param dataframe_chunk A subset of a dataframe or matrix we wish to stick into a vector
#' @return A vector of words
#' @export
unlist_and_concatenate <- function(dataframe_chunk){
  result <- c(unlist(dataframe_chunk))
  narm <- which(is.na(result))
  if(length(narm) > 0){
    result <- result[-narm]
  }
  return(as.character(result))
}
