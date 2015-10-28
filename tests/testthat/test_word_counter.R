library(SpeedReader)
context("Word Counter")

test_that("word counter gets the right counts", {
    # load data
    data(document_term_vector_list)
    data(document_term_count_list)
    cat("\n")
    count1 <- count_words(document_term_vector_list,
                            maximum_vocabulary_size = 1000000)
    cat("\n")

    count2 <- count_words(document_term_vector_list,
                          maximum_vocabulary_size = -1)

    expect_equal(count1$unique_words, count2$unique_words)
    cat("\n")

    count3 <- count_words(document_term_vector_list,
                          maximum_vocabulary_size = 1000000,
                          document_term_count_list = document_term_count_list)
    expect_equal(69825, sum(count3$word_counts))
    expect_equal(count1$total_unique_words, count3$total_unique_words)
    cat("\n")

    count4 <- count_words(document_term_vector_list,
                          maximum_vocabulary_size = -1,
                          existing_vocabulary = count3$unique_words,
                          existing_word_counts = count3$word_counts,
                          document_term_count_list = document_term_count_list)

    expect_equal(2*69825, sum(count4$word_counts))
})
