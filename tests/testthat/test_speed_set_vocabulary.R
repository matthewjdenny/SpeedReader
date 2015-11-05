library(SpeedReader)
context("Speed Set Vocabulary")

test_that("That speed set vocabulary returns same result", {
    # load data
    # data(document_term_vector_list)
    # data(document_term_count_list)
    files <- get_file_paths(source = "test sparse doc-term")

    vocab_test <- generate_sparse_large_document_term_matrix(
        file_list = files,
        file_directory = NULL,
        vocabulary = NULL,
        maximum_vocabulary_size = -1,
        using_document_term_counts = TRUE,
        generate_sparse_term_matrix = FALSE,
        large_vocabulary = TRUE,
        term_frequency_threshold = 0,
        save_vocabulary_to_file = FALSE)


    sdtm <- generate_sparse_large_document_term_matrix(
        file_list = files,
        file_directory = NULL,
        vocabulary = NULL,
        maximum_vocabulary_size = -1,
        using_document_term_counts = TRUE,
        generate_sparse_term_matrix = TRUE,
        large_vocabulary = TRUE,
        term_frequency_threshold = 0,
        save_vocabulary_to_file = FALSE)

    expect_equal(35424, ncol(sdtm))
    expect_equal(5, nrow(sdtm))

    cat("\n")
    data(document_term_vector_list)
    data(document_term_count_list)
    count <- count_words(document_term_vector_list,
                         maximum_vocabulary_size = 1000000,
                         document_term_count_list = document_term_count_list)

    keep <- which(count$unique_words %in% vocab_test$vocabulary)
    condensed <- sum(count$word_counts[keep])

    expect_equal(condensed, sum(sdtm))


})
