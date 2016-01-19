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
#' The POS tag patterns used are: AN, NN, AAN, ANN, NAN, NNN, NPN.
#' @param verb_filtering Defaults to FALSE. If TRUE, then short verb phrases
#' will be extracted in a manner similar to that described in JK_filtering
#' above. The POS tag patterns used are: VN, VAN, VNN, VPN, ANV, VDN.
#' @param phrase_extraction Defaults to FALSE. If TRUE, then full phrases of
#' arbitrary length will be extracted following the procedure described in Denny,
#' O'Connor, and Wallach (2016). This method will produce the most phrases, of
#' highest quality, but will take significantly longer than other methods. Not
#' currently implemented.
#' @param return_tag_patterns Defaults to FALSE. If TRUE and either JK_filtering
#' = TRUE, verb_filtering = TRUE, or phrase_extraction = TRUE, then the tag
#' pattern matched in forming the n-gram/phrase will be returned as an
#' accompanying vector.
#' @param lemmatize If TRUE, then n-grams are constructed out of lemmatized
#' tokens, Defaults to FALSE.
#' @param lowercase If TRUE, all n-grams are lowercased before being returned.
#' Defaults to TRUE.
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
                   return_tag_patterns = FALSE,
                   lemmatize = FALSE,
                   lowercase = FALSE){

    cat("Starting N-Gram extraction at:",toString(Sys.time()),"\n")

    # save the current working directory
    currentwd <- getwd()

    # check to make sure the user knows what they are extracting
    EXTRACT_NGRAMS <- TRUE
    if(is.null(ngram_lengths)){
        EXTRACT_NGRAMS <- FALSE
    }

    USING_EXTERNAL_FILES <- FALSE
    #check to make sure that we have the right kind of input
    if(!is.null(tokenized_documents) & is.null(tokenized_documents_directory)){
        if(class(tokenized_documents) != "list"){
            stop("You must provide a 'tokenized_documents' object as a list of dataframes produced by the corenlp() or corenlp_blocked() functions...")
        }
    }else if(is.null(tokenized_documents) & !is.null(tokenized_documents_directory)){
        setwd(tokenized_documents_directory)
        USING_EXTERNAL_FILES <- TRUE
    }else{
        stop("You must specify either a valid tokenized_documents object or a valid tokenized_documents_directory directory path (but not both)...")
    }

    # get all of the file names
    substrRight <- function(x, n){
        substr(x, nchar(x)-n+1, nchar(x))
    }

    # prepare text to be used
    if(USING_EXTERNAL_FILES){
        if(is.null(file_list)){
            tokenized_documents <- list.files()
            #only use files with a .txt ending
            endings <- as.character(sapply(tokenized_documents,substrRight,6))
            files <- which(endings == ".Rdata")
            if(length(files) > 0){
                tokenized_documents <- tokenized_documents[files]
            }else{
                stop("Did not find any valid .Rdata files in the specified directory...")
            }
        }else{
            tokenized_documents <- file_list
        }
    }

    numdocs <- length(tokenized_documents)
    NGrams <- NULL

    # now do actual extractions
    if(USING_EXTERNAL_FILES){
        Processed_Text <- NULL
        # if we are using external files, generate ngrams for the curent block,
        # then save them to disk
        for(i in 1:numdocs){
            #load the POS tagged data
            load(tokenized_documents[i])
            cur_numdocs <- length(Processed_Text)
            # create list object to store results
            NGrams  <- vector(length = cur_numdocs, mode = "list")
            for(j in 1:cur_numdocs){
                # initialize list object
                current <- list()
                current$ngrams <- NULL
                current$jk_filtered <- NULL
                current$verb_filtered <- NULL
                current$phrases <- NULL
                current$ngram_lengths <- ngram_lengths
                current$remove_punctuation <- remove_punctuation
                current$remove_numeric <- remove_numeric

                if(EXTRACT_NGRAMS){
                    current$ngrams <- extract_ngrams(
                        tokenized_document = tokenized_documents[[j]],
                        ngram_lengths = ngram_lengths,
                        remove_punctuation = remove_punctuation,
                        remove_numeric = remove_numeric,
                        lemmatize = lemmatize,
                        lowercase = lowercase)
                }
                if(JK_filtering){
                    current$jk_filtered <- extract_jk(
                        tokenized_document = tokenized_documents[[j]],
                        lemmatize = lemmatize,
                        lowercase = lowercase
                    )
                }
                if(verb_filtering){
                    current$verb_filtered <- extract_verbs(
                        tokenized_document = tokenized_documents[[j]],
                        lemmatize = lemmatize,
                        lowercase = lowercase
                    )
                }
                if(phrase_extraction){
                    stop("phrase_extraction not currently implemented")
                    # current$phrases <- extract_phrases(
                    #     tokenized_document = tokenized_documents[[j]],
                    #     lemmatize = lemmatize,
                    #     lowercase = lowercase
                    # )
                }
                # save everthing into the list object
                NGrams[[j]] <- current
            }
            save(NGrams,file = paste("NGram_Extractions_",i,".Rdata"))
        }
    }else{
        # if we are using internal data, generate ngrams for all documents and
        # return list object
        NGrams  <- vector(length = numdocs, mode = "list")
        for(i in 1:numdocs){
            # initialize list object
            current <- list()
            current$ngrams <- NULL
            current$jk_filtered <- NULL
            current$verb_filtered <- NULL
            current$phrases <- NULL
            current$ngram_lengths <- ngram_lengths
            current$remove_punctuation <- remove_punctuation
            current$remove_numeric <- remove_numeric

            if(EXTRACT_NGRAMS){
                current$ngrams <- extract_ngrams(
                    tokenized_document = tokenized_documents[[i]],
                    ngram_lengths = ngram_lengths,
                    remove_punctuation = remove_punctuation,
                    remove_numeric = remove_numeric,
                    lemmatize = lemmatize,
                    lowercase = lowercase)
            }
            if(JK_filtering){
                current$jk_filtered <- extract_jk(
                    tokenized_document = tokenized_documents[[i]],
                    lemmatize = lemmatize,
                    lowercase = lowercase
                )
            }
            if(verb_filtering){
                current$verb_filtered <- extract_verbs(
                    tokenized_document = tokenized_documents[[i]],
                    lemmatize = lemmatize,
                    lowercase = lowercase
                )
            }
            if(phrase_extraction){
                stop("phrase_extraction not currently implemented")
                # current$phrases <- extract_phrases(
                #     tokenized_document = tokenized_documents[[i]],
                #     lemmatize = lemmatize,
                #     lowercase = lowercase
                # )
            }
            # save everthing into the list object
            NGrams[[i]] <- current
        }
    }

    cat("Completed running N-GRam extraction at:",toString(Sys.time()),"\n")
    # reset the working directory
    setwd(currentwd)
    # return the list of data frames
    return(NGrams)
}



