library(SpeedReader)
context("Generate Document Term Vectors")

test_that("We are reading in the right number of ", {
    f1 <- system.file("extdata", "bill1.phrases.tsv", package = "SpeedReader")[1]
    f2 <- system.file("extdata", "bill2.phrases.tsv", package = "SpeedReader")[1]
    f3 <- system.file("extdata", "bill3.phrases.tsv", package = "SpeedReader")[1]
    f4 <- system.file("extdata", "bill4.phrases.tsv", package = "SpeedReader")[1]
    f5 <- system.file("extdata", "bill5.phrases.tsv", package = "SpeedReader")[1]

    files <- list(f1,f2,f3,f4,f5)
    cat("\n")
    system.time({documents <- generate_document_term_vectors(input = files, data_type = "csv", csv_separator = "\t",csv_word_column = 1,csv_count_column = 2,csv_header = TRUE,keep_sequence = FALSE)})

    # save this stuff as example data
    # document_term_vector_list <- documents[[1]]
    # document_term_count_list <- documents[[2]]
    # devtools::use_data(document_term_vector_list)
    # devtools::use_data(document_term_count_list)

    expect_equal(69825, sum(unlist(sapply(documents[[2]],sum))))
})



