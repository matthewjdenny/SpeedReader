library(SpeedReader)
context("TFIDF")

test_that("That tfidf works", {
    # load data
    # data(document_term_vector_list)
    # data(document_term_count_list)
    files <- get_file_paths(source = "test sparse doc-term")

    sdtm <- generate_sparse_large_document_term_matrix(
        file_list = files,
        file_directory = NULL,
        vocabulary = NULL,
        maximum_vocabulary_size = -1,
        using_document_term_counts = TRUE)

    test_tfidf <- tfidf(sdtm,
                      colnames(sdtm),
                      remove_documents_with_no_terms = FALSE,
                      only_calculate_corpus_level_statistics = TRUE,
                      display_rankings = TRUE,
                      top_words_to_display = 40)



})
