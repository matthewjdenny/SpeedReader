#' An experimental function to efficiently generate a vocabulary in parallel
#' from output produced by the ngrams() function. This will only work for users
#' with GNU coreutils > 8.13 as the sort --parallel option is used.
#'
#' @param ngrams An optional list object output by the ngrams() function.
#' @param input_directory An optional input directory where blocked output from
#' th ngrams() function is stored as .Rdata files.
#' @param combine_ngrams Logical indicating whether simple ngrams should be
#' combined together when forming the vocabulary. If FALSE, then separate
#' vocabularies will be generated for each ngram length. Defaults to FALSE.
#' @param cores The number of cores to be used for parallelization.
#' @return Returns a list object with the vocabulary (sorted by frequency) and
#' and word counts.
#' @export
count_ngrams <- function(ngrams,
                         input_directory,
                         combine_ngrams,
                         cores) {

}
