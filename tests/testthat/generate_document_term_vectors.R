
f1 <- system.file("extdata", "bill0.phrases.tsv", package = "SpeedReader")[1]
f2 <- system.file("extdata", "bill1.phrases.tsv", package = "SpeedReader")[1]
f3 <- system.file("extdata", "bill2.phrases.tsv", package = "SpeedReader")[1]
f4 <- system.file("extdata", "bill3.phrases.tsv", package = "SpeedReader")[1]
f5 <- system.file("extdata", "bill4.phrases.tsv", package = "SpeedReader")[1]
f6 <- system.file("extdata", "bill5.phrases.tsv", package = "SpeedReader")[1]
f7 <- system.file("extdata", "bill6.phrases.tsv", package = "SpeedReader")[1]

files <- list(f1,f2,f3,f4,f5,f6,f7)

documents <- generate_document_term_vectors(input = files, data_type = "csv", csv_separator = "\t",csv_word_column = 2,csv_count_column = 8,csv_header = TRUE,keep_sequence = FALSE)


