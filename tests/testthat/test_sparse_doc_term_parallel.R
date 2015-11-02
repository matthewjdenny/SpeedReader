library(SpeedReader)
context("Generate Sparse Document Term Matrix")

test_that("That document term matricies have the right dimensions and sum", {

    data(document_term_vector_list)
    data(document_term_count_list)
    count <- count_words(document_term_vector_list,
                         maximum_vocabulary_size = 1000000,
                         document_term_count_list = document_term_count_list)

    aggregate_vocabulary = count$unique_words

    files <- get_file_paths(source = "test sparse doc-term")

    test <- sparse_doc_term_parallel(file = files[1],
                                     aggregate_vocabulary = aggregate_vocabulary)


})


