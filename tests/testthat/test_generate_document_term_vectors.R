library(SpeedReader)
context("Generate Document Term Vectors")

test_that("We are reading in the right number of ", {
    files <- get_file_paths(source = "bill tsvs")
    cat("\n")
    system.time({documents <- generate_document_term_vectors(
        input = files,
        data_type = "csv",
        csv_separator = "\t",
        csv_word_column = 1,
        csv_count_column = 2,
        csv_header = TRUE,
        keep_sequence = FALSE)})

    # save this stuff as example data
    # document_term_vector_list <- documents[[1]]
    # document_term_count_list <- documents[[2]]
    # devtools::use_data(document_term_vector_list)
    # devtools::use_data(document_term_count_list)

    expect_equal(69825, sum(unlist(sapply(documents[[2]],sum))))

    #now provide input as a vetor of strings:
    docs <- rep("One of the most common things we might want to do is read in and clean a raw input text file. To do this, we will want to make use of two functions, the first of these will clean and individual string, removing any characters that are not letters, lowercasing everything, and getting rid of additional spaces between words before tokenizing the resulting text and retur12ning a 12345667 vector of indiv!!idual words:",10)
    system.time({documents <- generate_document_term_vectors(
        input = docs,
        data_type = "string",
        tokenization_method = "RegEx",
        keep_sequence = FALSE)})
    str <- clean_document_text(text = docs[1])

    expect_equal(length(unique(str)), length(documents$document_term_count_list[[1]]))
})



