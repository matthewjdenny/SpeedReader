ngrams_single_block <- function(i,
                                ngram_lengths,
                                remove_punctuation,
                                remove_numeric,
                                lemmatize,
                                lowercase,
                                tokenized_documents,
                                EXTRACT_NGRAMS,
                                JK_filtering,
                                verb_filtering,
                                phrase_extraction,
                                tokenized_documents_directory,
                                output_directory){
    setwd(tokenized_documents_directory)
    Processed_Text <- NULL
    #load the POS tagged data
    load(tokenized_documents[i])
    cur_numdocs <- length(Processed_Text)
    # create list object to store results
    NGrams  <- vector(length = cur_numdocs, mode = "list")
    for(j in 1:cur_numdocs){
        cat("Currently working on document",j,"of",cur_numdocs,"\n")
        # save everthing into the list object
        NGrams[[j]] <- ngrams_single_document(
            j = 1,
            ngram_lengths = ngram_lengths,
            remove_punctuation = remove_punctuation,
            remove_numeric = remove_numeric,
            lemmatize = lemmatize,
            lowercase = lowercase,
            tokenized_documents = Processed_Text[j],
            EXTRACT_NGRAMS = EXTRACT_NGRAMS,
            JK_filtering = JK_filtering,
            verb_filtering = verb_filtering,
            phrase_extraction = phrase_extraction)
    }
    setwd(output_directory)
    save(NGrams,file = paste("NGram_Extractions_",i,".Rdata", sep = ""))
    return("Success!")
}
