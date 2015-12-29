library(SpeedReader)
context("CoreNLP.")

test_that("CoreNLP Works", {

    skip_on_cran()
    skip("Requires Huge download...")
    # dont run since this is too large of a file
    download_corenlp(version = "3.5.2")

    directory <- system.file("extdata", package = "SpeedReader")[1]
    Tokenized <- corenlp(
        documents = NULL,
        document_directory = directory,
        version = "3.5.2")

})
