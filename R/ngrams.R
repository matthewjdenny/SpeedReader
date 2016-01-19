#' Extracts N-Grams and phrases from a collection od documents that has been preprocessed by the corenlp() function.
#'
#' @param tokenized_documents An optional list object output by the corenlp()
#' or corenlp_blocked() functions containing tokenized document dataframes (one
#' per document).
#' @param tokenized_documents_directory An optional directory path to a
#' directory contianing CoreNLP_Output_x.Rdata files output by the corenlp()
#' or corenlp_blocked() functions. Cannot be supplied in addition to the
#' 'tokenized_documents' argument.
#' @param output_directory If a tokenized_documents_directory is provided, an
#' alternate output directory may be provided in which case n-gram extractions
#' for each block are saved in the alternative directory If NULL, then
#' output_directory will be set to tokenized_documents_directory.
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
#' @param parallel Logical: should documents be processed in parallel? Defaults
#' to FALSE.
#' @param cores Number of cores to be used if parallel = TRUE, defaults to 2.
#' @examples
#' \dontrun{
#' data("Processed_Text")
#' NGrams <- ngrams(tokenized_documents = Processed_Text,
#'                  ngram_lengths = c(1,2,3),
#'                  remove_punctuation = TRUE,
#'                  remove_numeric = TRUE,
#'                  lowercase = TRUE,
#'                  parallel = FALSE,
#'                  cores = 1)
#' }
#' @export
ngrams <- function(tokenized_documents = NULL,
                   tokenized_documents_directory = NULL,
                   output_directory = NULL,
                   file_list = NULL,
                   ngram_lengths = c(1,2,3),
                   remove_punctuation = TRUE,
                   remove_numeric = TRUE,
                   JK_filtering = FALSE,
                   verb_filtering = FALSE,
                   phrase_extraction = FALSE,
                   return_tag_patterns = FALSE,
                   lemmatize = FALSE,
                   lowercase = FALSE,
                   parallel = FALSE,
                   cores = 2){

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
    if (!is.null(tokenized_documents) & is.null(tokenized_documents_directory)) {
        if (class(tokenized_documents) != "list") {
            stop("You must provide a 'tokenized_documents' object as a list of dataframes produced by the corenlp() or corenlp_blocked() functions...")
        }
    }else if (is.null(tokenized_documents) & !is.null(tokenized_documents_directory)) {
        setwd(tokenized_documents_directory)
        USING_EXTERNAL_FILES <- TRUE
        if (is.null(output_directory)) {
            cat("Since output_directory was NULL, setting output_directory to:",
                tokenized_documents_directory, "\n")
            output_directory <- tokenized_documents_directory
        }
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
                cat("Found",length(files),
                    "valid .Rdata files in directory. Here are a few examples:\n")
                print(head(tokenized_documents))
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
        # if we are using external files, generate ngrams for the curent block,
        # then save them to disk
        if (parallel) {
            cat("Extracting N-Grams from",numdocs,"blocks of documents on",
                cores,"cores. This may take a while...\n")
            cl <- parallel::makeCluster(getOption("cl.cores", cores))

            success <- parallel::clusterApplyLB(cl = cl,
                x = 1:numdocs,
                fun = ngrams_single_block,
                ngram_lengths = ngram_lengths,
                remove_punctuation = remove_punctuation,
                remove_numeric = remove_numeric,
                lemmatize = lemmatize,
                lowercase = lowercase,
                tokenized_documents = tokenized_documents,
                EXTRACT_NGRAMS = EXTRACT_NGRAMS,
                JK_filtering = JK_filtering,
                verb_filtering = verb_filtering,
                phrase_extraction = phrase_extraction,
                tokenized_documents_directory = tokenized_documents_directory,
                output_directory = output_directory)
            # stop the cluster when we are done
            parallel::stopCluster(cl)
        } else {
            for(i in 1:numdocs){
                cat("Currently working on block",i,"of",numdocs,"\n")
                success <- ngrams_single_block(i = i,
                ngram_lengths = ngram_lengths,
                remove_punctuation = remove_punctuation,
                remove_numeric = remove_numeric,
                lemmatize = lemmatize,
                lowercase = lowercase,
                tokenized_documents = tokenized_documents,
                EXTRACT_NGRAMS = EXTRACT_NGRAMS,
                JK_filtering = JK_filtering,
                verb_filtering = verb_filtering,
                phrase_extraction = phrase_extraction,
                tokenized_documents_directory = tokenized_documents_directory,
                output_directory = output_directory)
            }
        }
    }else{
        # if we are using internal data, generate ngrams for all documents and
        # return list object
        if (parallel) {
            cat("Extracting N-Grams from",numdocs,"documents on",
                cores,"cores. This may take a while...\n")
            cl <- parallel::makeCluster(getOption("cl.cores", cores))

            NGrams <- parallel::clusterApplyLB(cl = cl,
                x = 1:numdocs,
                fun = ngrams_single_document,
                ngram_lengths = ngram_lengths,
                remove_punctuation = remove_punctuation,
                remove_numeric = remove_numeric,
                lemmatize = lemmatize,
                lowercase = lowercase,
                tokenized_documents = tokenized_documents,
                EXTRACT_NGRAMS = EXTRACT_NGRAMS,
                JK_filtering = JK_filtering,
                verb_filtering = verb_filtering,
                phrase_extraction = phrase_extraction)
            # stop the cluster when we are done
            parallel::stopCluster(cl)
        } else {
            NGrams  <- vector(length = numdocs, mode = "list")
            for(i in 1:numdocs){
                cat("Currently working on document",i,"of",numdocs,"\n")
                NGrams[[i]] <- ngrams_single_document(
                    j = i,
                    ngram_lengths = ngram_lengths,
                    remove_punctuation = remove_punctuation,
                    remove_numeric = remove_numeric,
                    lemmatize = lemmatize,
                    lowercase = lowercase,
                    tokenized_documents = tokenized_documents,
                    EXTRACT_NGRAMS = EXTRACT_NGRAMS,
                    JK_filtering = JK_filtering,
                    verb_filtering = verb_filtering,
                    phrase_extraction = phrase_extraction)
            }
        } # end of parallel conditional
    } # end of blocks conditional

    cat("Completed running N-GRam extraction at:",toString(Sys.time()),"\n")
    # reset the working directory
    setwd(currentwd)
    # return the list of data frames
    return(NGrams)
}



