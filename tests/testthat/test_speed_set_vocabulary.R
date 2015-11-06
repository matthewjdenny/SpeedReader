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
    cs <- vocab_test$vocabulary[which(slam::col_sums(sdtm) == 0)]
    cat("Missing Terms:",cs,"\n\n\n\n")

    cat("\n")
    data(document_term_vector_list)
    data(document_term_count_list)
    count <- count_words(document_term_vector_list,
                         maximum_vocabulary_size = 1000000,
                         document_term_count_list = document_term_count_list)

    keep <- which(count$unique_words %in% vocab_test$vocabulary)
    cat(length(keep),"\n")
    expect_equal(length(keep), 35424)

    condensed <- sum(count$word_counts[keep])
    second_sum <- sum(sdtm)
    cat(condensed, "\n")
    cat(second_sum,"\n")
    expect_equal(condensed, second_sum)
})

test_that("That speed set in parallel works", {
    # load data
    # data(document_term_vector_list)
    # data(document_term_count_list)
    files <- get_file_paths(source = "test sparse doc-term")

    sdtm2 <- generate_sparse_large_document_term_matrix(
        file_list = files,
        file_directory = NULL,
        vocabulary = NULL,
        maximum_vocabulary_size = -1,
        using_document_term_counts = TRUE,
        generate_sparse_term_matrix = TRUE,
        large_vocabulary = TRUE,
        term_frequency_threshold = 0,
        save_vocabulary_to_file = FALSE,
        parallel = TRUE,
        cores = 1)
})
