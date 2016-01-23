library(SpeedReader)
context("NGrams")

test_that("That parallel and regular ngram extraction works", {
    # load data
    data("Processed_Text")

    cat("\n")
    system.time({
        NGrams <- ngrams(tokenized_documents = Processed_Text,
                         ngram_lengths = c(1,2,3),
                         remove_punctuation = TRUE,
                         remove_numeric = TRUE,
                         lowercase = TRUE,
                         parallel = FALSE,
                         cores = 2)
    })

    skip("This will not work on travis")
    cat("\n")
    system.time({
        NGrams2 <- ngrams(tokenized_documents = Processed_Text,
                         ngram_lengths = c(1,2,3),
                         remove_punctuation = TRUE,
                         remove_numeric = TRUE,
                         lowercase = TRUE,
                         parallel = TRUE,
                         cores = 3)
    })

    expect_equal(NGrams, NGrams2)

})
