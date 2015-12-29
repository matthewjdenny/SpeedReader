library(SpeedReader)
context("CoreNLP.")

test_that("CoreNLP Works", {

    skip_on_cran()
    skip("Requires Huge download...")
    # dont run since this is too large of a file
    download_corenlp(version = "3.5.2")
    file.remove(paste("filenames.txt",sep = ""))
    file.remove(paste("filenames.txt.xml",sep = ""))

    directory <- system.file("extdata", package = "SpeedReader")[1]
    opts = "-ner.model english.all.3class"
    opts = "-ner.model english.muc.7class"
    opts = "-ner.model english.conll.4class"

    Tokenized <- corenlp(
        documents = NULL,
        document_directory = directory,
        delete_intermediate_files = TRUE,
        syntactic_parsing = FALSE,
        coreference_resolution =FALSE,
        ner_model = "english.muc.7class",
        additional_options = "",
        return_raw_output = FALSE)



})
