library(SpeedReader)
context("Generate Document Term Vectors")

test_that("We are reading in the right number of ", {
    # only for testing on my computer
    files <- get_file_paths(source = "bill tsvs")
    cat("\n")
#     system.time({generate_blocked_document_term_vectors(input = files,
#         output_stem = "Block",
#         data_directory =  "~/Desktop/",
#         block_size = 3,
#         data_type = "csv",
#         csv_separator = "\t",
#         csv_word_column = 1,
#         csv_count_column = 2,
#         csv_header = TRUE,
#         keep_sequence = FALSE)})

})
