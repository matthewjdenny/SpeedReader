library(SpeedReader)
context("Clean text with simple regular expressions.")

test_that("All different types of text input yield the same thing", {

    # generate different pieces of example text
    str_example <- "One of the most common things we might want to do is read in and clean a raw input text file. To do this, we will want to make use of two functions, the first of these will clean and individual string, removing any characters that are not letters, lowercasing everything, and getting rid of additional spaces between words before tokenizing the resulting text and retur12ning a 12345667 vector of indiv!!idual words:"

    vec_example <- stringr::str_split(str_example, " ")[[1]]

    df_example <- data.frame(text = vec_example,
                             cov = 1:length(vec_example),
                             stringsAsFactors = F)

    df2_example <- data.frame(text = vec_example,
                             stringsAsFactors = F)

    bad_df_example <- data.frame(text = vec_example,
                             cov = 1:length(vec_example),
                             stringsAsFactors = T)

    # run on each
    str <- clean_document_text(text = str_example)
    vec <- clean_document_text(text = vec_example)
    df <- clean_document_text(text = df_example[,1])
    df2 <- clean_document_text(text = df2_example)
    bad <- clean_document_text(text = bad_df_example)
    # make sure we get the same thing.
    expect_equal(str, vec)
    expect_equal(str, df)
    expect_equal(str, df2)
    expect_equal(str, bad)

})
