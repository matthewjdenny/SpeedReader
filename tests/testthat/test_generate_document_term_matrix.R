library(SpeedReader)
context("Generate Document Term Matrix")

test_that("That document term matricies have the right dimensions and sum", {
    # load data
    data(document_term_vector_list)
    data(document_term_count_list)

    cat("\n")
    doc_term1 <- generate_document_term_matrix(document_term_vector_list)

    expect_equal(5, nrow(doc_term1))
    expect_equal(35522, ncol(doc_term1))

    cat("\n")
    doc_term2 <- generate_document_term_matrix(document_term_vector_list,
                                               document_term_count_list = document_term_count_list)

    cat("\n")
    count <- count_words(document_term_vector_list,
                          maximum_vocabulary_size = 1000000,
                          document_term_count_list = document_term_count_list)

    expect_equal(5, nrow(doc_term2))
    expect_equal(35522, ncol(doc_term2))
    expect_equal(sum(count$word_counts), sum(doc_term2))
    expect_equal(count$word_counts, as.numeric(colSums(doc_term2)))

    cat("\n")
    doc_term3 <- generate_document_term_matrix(document_term_vector_list,
                                               vocabulary = count$unique_words)

    expect_equal(35522, ncol(doc_term3))

    cat("\n")
    doc_term4 <- generate_document_term_matrix(document_term_vector_list,
                                               vocabulary = count$unique_words,
                                               document_term_count_list = document_term_count_list)

    expect_equal(sum(count$word_counts), sum(doc_term4))
    expect_equal(count$word_counts, as.numeric(colSums(doc_term4)))

})
