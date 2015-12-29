library(SpeedReader)
context("CoreNLP.")

test_that("CoreNLP Works", {

    skip_on_cran()
    skip("Requires Huge download...")
    # dont run since this is too large of a file
    # download_corenlp(version = "3.5.2")

    directory <- system.file("extdata", package = "SpeedReader")[1]

    Tokenized <- corenlp(
        documents = NULL,
        document_directory = directory,
        delete_intermediate_files = TRUE,
        syntactic_parsing = FALSE,
        coreference_resolution =FALSE,
        additional_options = "",
        return_raw_output = FALSE)

    fp<- get_file_paths("raw text")
    t1 <- paste0(readLines(fp[1]),collapse = " ")
    t2 <- paste0(readLines(fp[2]),collapse = " ")
    documents <- c(t1,t2)

    Tokenized2 <- corenlp(
        documents = documents,
        document_directory = NULL,
        delete_intermediate_files = TRUE,
        syntactic_parsing = FALSE,
        coreference_resolution =FALSE,
        additional_options = "",
        return_raw_output = FALSE)


    fp<- get_file_paths("raw text")
    t1 <- readLines(fp[1])
    t2 <- readLines(fp[2])
    documents <- list(t1,t2)

    Tokenized3 <- corenlp(
        documents = documents,
        document_directory = NULL,
        delete_intermediate_files = TRUE,
        syntactic_parsing = FALSE,
        coreference_resolution =FALSE,
        additional_options = "",
        return_raw_output = FALSE)


})
