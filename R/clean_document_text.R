#' A function which cleans the raw text of a document provided either as a single string, a vector of strings, or a column of a data.frame.
#'
#' @param text The raw text of a document the user wishes to clean. Can be supplied as either a single string, a vector of strings, or a column from a data.frame.
#' @param remove_regex A regular expression specifying the characters the user would like to exclude from the final text string. This function works by replacing those terms with spaces and then splitting the resulting string on those spaces. Defaults to removing all characters that are not uper or lowercase letters or spaces (as a regex, this is "[^a-zA-Z\\s]").
#' @return A document-term vector with ordering preserved.
#' @export
clean_document_text <- function(text,
                                remove_regex = "[^a-zA-Z\\s]"){

    # if the user provided a vector of strings, then collapse it before
    # further preprocessing
    if(class(text) == "character"){
        text <- paste0(text,collapse = " ")
    }

    # text was a column from a dataframe, then turn it into a string
    if(class(text) == "data.frame"){
        text <- paste0(text[,1],collapse = " ")
    }

    if(class(text) != "character" | length(text) > 1){
        cat("You have supplied an invalid input!")
    }
    # Lowercase
    temp <- tolower(text)
    #' Remove everything that is not a number or letter (may want to keep more
    #' stuff in your actual analyses).
    temp <- stringr::str_replace_all(temp,remove_regex, " ")
    # Shrink down to just one white space
    temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
    # Split it
    temp <- stringr::str_split(temp, " ")[[1]]
    # Get rid of trailing "" if necessary
    indexes <- which(temp == "")
    if(length(indexes) > 0){
        temp <- temp[-indexes]
    }
    return(temp)
}
