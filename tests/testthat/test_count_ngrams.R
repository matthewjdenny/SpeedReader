library(SpeedReader)
context("Count NGrams")

test_that("That parallel ngram counting works", {
    # load data
   skip("I am not sure we can write files on travis")
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

    system.time({
        NGram_Counts <- count_ngrams(ngrams = NGrams,
                                 input_directory = NULL,
                                 file_list = NULL,
                                 combine_ngrams = FALSE,
                                 cores = 1,
                                 mac_brew = FALSE)
    })

})
