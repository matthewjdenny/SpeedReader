#' Runs Stanford CoreNLP on a collection of documents
#'
#' @param tokenized_documents An optional list object output by the corenlp()
#' or corenlp_blocked() functions containing tokenized document dataframes (one
#' per document).
#' @param tokenized_documents_directory An optional directory path to a
#' directory contianing CoreNLP_Output_x.Rdata files output by the corenlp()
#' or corenlp_blocked() functions. Cannot be supplied in addition to the
#' 'tokenized_documents' argument.
#' @param file_list An optional list of CoreNLP_Output_x.Rdata  files to be used
#' if tokenized_documents_directory option is specified. Can be useful if the
#' user only wants to process a subset of documents in the directory such as
#' when the corpus is extremely large.
#' @param ngram_lengths A vector of N-gram lengths (in tokens) to be returned by
#' the function. Defaults to c(1,2,3) which returns all unigrams, bigrams and
#' trigrams.
#' @param remove_punctuation Removes any N-Grams with atleast one token
#' containing one or more punctuation characters: [!"#$%&'()*+,\-./:;<=>?@[\\\]
#' ^_`{|}~]
#' @param remove_numeric Removes any N-Grams with atleast one token containing
#' one or more numerals (0-9).
#' @param JK_filtering Defaults to FALSE. If TRUE then bigrams and trigrams will be
#' extracted, and filtered according to the tag patterns described in Justeson,
#' John S., and Slava M. Katz. "Technical terminology: some linguistic properties
#' and an algorithm for identification in text." Natural language engineering
#' 1.01 (1995): 9-27. Available: https://brenocon.com/JustesonKatz1995.pdf.
#' The POS tag patterns used are: AN, NN, AAN, ANN, NAN, NNN, NPN. Note that
#' this method requires input produced by the corenlp() or corenlp_blocked()
#' functions.
#' @param verb_filtering Defaults to FALSE. If TRUE, then short verb phrases
#' will be extracted in a manner similar to that described in JK_filtering
#' above. The POS tag patterns used are: VN, VAN, VNN, VPN, ANV, VDN. Note that
#' this method requires input produced by the corenlp() or corenlp_blocked()
#' functions.
#' @param phrase_extraction Defaults to FALSE. If TRUE, then full phrases of
#' arbitrary length will be extracted following the procedure described in Denny,
#' O'Connor, and Wallach (2016). This method will produce the most phrases, of
#' highest quality, but will take significantly longer than other methods. Not
#' currently implemented. Note that this method requires input produced by the
#' corenlp() or corenlp_blocked() functions.
#' @param return_tag_patterns Defaults to FALSE. If TRUE and either JK_filtering
#' = TRUE, verb_filtering = TRUE, or phrase_extraction = TRUE, then the tag
#' pattern matched in forming the n-gram/phrase will be returned as an
#' accompanying vector.
#' @return Returns a list of lists (one list per document) with entries for n-grams
#' of each size specified in the ngram_lengths argument. May also return metadata
#' if return_tag_patterns = TRUE.
#' @examples
#' \dontrun{
#' directory <- system.file("extdata", package = "SpeedReader")[1]
#' Ngrams <- ngrams()
#' }
#' @export
ngrams <- function(tokenized_documents = NULL,
                   tokenized_documents_directory = NULL,
                   file_list = NULL,
                   ngram_lengths = c(1,2,3),
                   remove_punctuation = TRUE,
                   remove_numeric = TRUE,
                   JK_filtering = FALSE,
                   verb_filtering = FALSE,
                   phrase_extraction = FALSE,
                   return_tag_patterns = FALSE){

    cat("Starting N-Gram extraction at:",toString(Sys.time()),"\n")

    # save the current working directory
    currentwd <- getwd()
    NGrams <- NULL



    USING_EXTERNAL_FILES <- FALSE
    #check to make sure that we have the right kind of input
    if(!is.null(tokenized_documents) & is.null(tokenized_documents_directory)){
        if(class(tokenized_documents) != "list" & class(tokenized_documents) != "character"){
            stop("You must provide a 'documents' object as either a vector of strings (one per document) or a list of string vectors (one entry per document)...")
        }

    }else if(is.null(tokenized_documents) & !is.null(tokenized_documents_directory)){
        setwd(tokenized_documents_directory)
        USING_EXTERNAL_FILES <- TRUE
    }else{
        stop("You must specify either a valid documents object or a valid document_directory directory path (but not both)...")
    }


    substrRight <- function(x, n){
        substr(x, nchar(x)-n+1, nchar(x))
    }

    # prepare text to be used
    if(USING_EXTERNAL_FILES){
        if(is.null(file_list)){
            documents <- list.files()
            #only use files with a .txt ending
            endings <- as.character(sapply(documents,substrRight,6))
            txtfiles <- which(endings == ".Rdata")
            if(length(txtfiles) > 0){
                documents <- documents[txtfiles]
            }else{
                stop("Did not find any valid .Rdata files in the specified directory...")
            }
        }else{
            documents <- file_list
        }
    }

    numdocs <- length(documents)
    Processed_Text <- vector(length = numdocs, mode = "list")
    #names(Processed_Text) <- documents
    filenames <- rep("",numdocs)


    cat("Completed running N-GRam extraction at:",toString(Sys.time()),"\n")
    # reset the working directory
    setwd(currentwd)
    # return the list of data frames
    return(NGrams)
}



