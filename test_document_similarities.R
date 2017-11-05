library(SpeedReader)
context("document_similarities")

test_that("That doc similarities works", {
    skip("this takes a bit too long")

    data("congress_bills")

    # Generate similarity metrics:
    results <- document_similarities(documents = congress_bills,
                                     ngram_size = 5,
                                     prehash = T)

    results2 <- document_similarities(documents = congress_bills,
                                     ngram_size = 5,
                                     prehash = T,
                                     document_block_size = 40,
                                     add_ngram_comparisons = NULL)

    results <- results[order(results$doc_1_ind, results$doc_2_ind),]
    results2 <- results2[order(results2$doc_1_ind, results2$doc_2_ind),]

    expect_equal(as.character(results), as.character(results2))

})



test_that("That adding n-grams works", {
    skip("this is just to check that adding n-grams works as expected")

    data("congress_bills")

    # Generate similarity metrics:
    results <- document_similarities(documents = congress_bills,
                                     ngram_size = 5,
                                     prehash = T,
                                     add_ngram_comparisons = c(1,2))


})

