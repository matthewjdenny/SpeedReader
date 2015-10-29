#' A function the returns the file paths to two example raw datasets for testing
#'
#' @param source Can be either "bill tsvs" in which case the paths to the five example bill tsv files are returned, or "test sparse doc-term", in which case the paths to two Rdata files necessary to test the generate_sparse_large_document_term_matrix() are returned.
#' @return A vector of file paths
#' @export
get_file_paths <- function(source = c("bill tsvs","test sparse doc-term")){
    source <- source[1]
    if(source == "bill tsvs"){
        f1 <- system.file("extdata", "bill1.phrases.tsv", package = "SpeedReader")[1]
        f2 <- system.file("extdata", "bill2.phrases.tsv", package = "SpeedReader")[1]
        f3 <- system.file("extdata", "bill3.phrases.tsv", package = "SpeedReader")[1]
        f4 <- system.file("extdata", "bill4.phrases.tsv", package = "SpeedReader")[1]
        f5 <- system.file("extdata", "bill5.phrases.tsv", package = "SpeedReader")[1]

        files <- c(f1,f2,f3,f4,f5)
    }
    if(source == "test sparse doc-term"){
        f1 <- system.file("extdata", "Block_1.Rdata", package = "SpeedReader")[1]
        f2 <- system.file("extdata", "Block_2.Rdata", package = "SpeedReader")[1]

        files <- c(f1,f2)
    }
    return(files)
}
