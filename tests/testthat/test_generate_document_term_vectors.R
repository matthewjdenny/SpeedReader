library(SpeedReader)
context("Generate Document Term Vectors")

test_that("We are reading in the right number of ", {
    files <- get_file_paths(source = "bill tsvs")
    cat("\n")
    system.time({documents <- generate_document_term_vectors(input = files, data_type = "csv", csv_separator = "\t",csv_word_column = 1,csv_count_column = 2,csv_header = TRUE,keep_sequence = FALSE)})

    # save this stuff as example data
    # document_term_vector_list <- documents[[1]]
    # document_term_count_list <- documents[[2]]
    # devtools::use_data(document_term_vector_list)
    # devtools::use_data(document_term_count_list)

    expect_equal(69825, sum(unlist(sapply(documents[[2]],sum))))
})



