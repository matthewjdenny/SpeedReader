#' A function to generate document term vectors from a variety of inputs.
#'
#' @param input A list of strings, term vectors, raw documents, or csv files you wish to turn into document term vectors.
#' @param data_type The type of data provided to the function.
#' @param data_directory Optional argument specifying where the data is stored.
#' @param tokenization_method Will eventually implement both an R-based and a Stanford CoreNLP based tokenization strategy. Currently not available.
#' @param return_result A logical value indicating whether the resulting list object should be returned.
#' @param csv_separator Defaults to "," but can be set to "\t" for tab separated values.
#' @param csv_word_column  If you are providing one csv file per document, then you must specify the index of the column that contains the words. Defaults to NULL.
#' @param csv_count_column For memory efficiency, you may want to store only the counts of unique words in csv files. If your data include counts, then you must specify the index of the column that contains the counts. Defaults to NULL.
#' @return A document term vector list.
#' @export
generate_document_term_vectors <- function(
    input,
    data_type = c("string","term vector","raw text","csv"),
    data_directory = NULL,
    tokenization_method = c("RegEx","CoreNLP"),
    return_result = FALSE,
    csv_separator = ",",
    csv_word_column = NULL,
    csv_count_column = NULL){







    return(NULL)
}
