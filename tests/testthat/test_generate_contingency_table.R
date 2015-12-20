library(SpeedReader)
context("generate contingency table")

test_that("That generating a contingency table works", {
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

    metadata <- data.frame(party = c("Dem","Dem","Rep","Rep","Dem"),
                           type = c(1,1,1,1,0),
                           stringsAsFactors = FALSE)

    test <- get_unique_values_and_counts(
        metadata,
        variable_names = colnames(metadata)[1],
        threshold = 2)$values

    test2 <- generate_contingency_table(metadata,
                                        sdtm,
                                        vocabulary = NULL,
                                        variables_to_use = c("party","type"),
                                        threshold = 0)


})
