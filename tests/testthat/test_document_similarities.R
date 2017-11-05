library(SpeedReader)
context("document similarities")

test_that("make sure it handles weird cases", {

    # generate different pieces of example text
    str1 <- "One of the most common things we might want to do is read in and clean a raw input text file.   "

    str2 <- "One of the most common things we might want to do is"

    str3<- "want to do is read in and clean a raw input text file. To do this, we will want to make use of two functions of"

    str4 <- "One of the most common"

    str5 <- "One of the most"

    docs <- c(str1,str2,str3,str4,str5)

    doc_pairs <- t(combn(1:5,2))

    results <- document_similarities(filenames = NULL,
                                     documents = docs,
                                     input_directory = NULL,
                                     ngram_size = 5,
                                     output_directory = NULL,
                                     doc_pairs = doc_pairs,
                                     cores = 1,
                                     max_block_size = 100000,
                                     prehash = T,
                                     ngram_match_only = TRUE)

    results2 <- document_similarities(filenames = NULL,
                                     documents = docs,
                                     input_directory = NULL,
                                     ngram_size = 5,
                                     output_directory = NULL,
                                     doc_pairs = doc_pairs,
                                     cores = 1,
                                     max_block_size = 100000,
                                     prehash = T,
                                     ngram_match_only = FALSE)





})
