library(SpeedReader)
context("MALLET LDA.")

test_that("MALLET LDA Works", {

    skip_on_cran()
    skip("Requires Huge download...")

    files <- get_file_paths(source = "test sparse doc-term")

    sdtm <- generate_sparse_large_document_term_matrix(
        file_list = files,
        file_directory = NULL,
        vocabulary = NULL,
        maximum_vocabulary_size = -1,
        using_document_term_counts = TRUE)

    test_results <- mallet_lda(documents = sdtm,
                           document_directory = NULL,
                           vocabulary = NULL,
                           topics = 10,
                           iterations = 1000,
                           burnin = 10,
                           alpha = 1,
                           beta = 0.01,
                           hyperparameter_optimization_interval = 5,
                           num_top_words = 20,
                           optional_arguments = "",
                           tokenization_regex = '[\\p{L}\\p{N}\\p{P}]+',
                           cores = 1,
                           delete_intermediate_files = FALSE)

    data("sotu_corp")

    dtm <- quanteda::dfm(sotu_corp$text)
    sdtm <- convert_quanteda_to_slam(dtm)

    to_rem <- grep("[^A-Za-z]+",sdtm$dimnames$Terms)
    sdtm <- sdtm[,-to_rem]
    doc_term_counts <- slam::row_sums(sdtm)

    setwd("~/Desktop")
    test_results <- mallet_lda(documents = sdtm,
                               topics = 10,
                               iterations = 1000,
                               burnin = 10,
                               alpha = 1,
                               beta = 0.01,
                               hyperparameter_optimization_interval = 5,
                               num_top_words = 20,
                               optional_arguments = "",
                               tokenization_regex = '[\\p{L}\\p{N}\\p{P}]+',
                               cores = 1,
                               delete_intermediate_files = TRUE,
                               use_phrases = FALSE)





})
