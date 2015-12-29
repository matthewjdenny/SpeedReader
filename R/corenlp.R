#' Runs Stanford CoreNLP on a collection of documents
#'
#' @param documents An optional list of character vectors or a vector of strings, with one entry per dcument. These documents will be run through CoreNLP.
#' @param document_directory An optional directory path to a directory contianing only .txt files (one per document) to be run through CoreNLP. Cannot be supplied in addition to the 'documents' argument.
#' @param delete_intermediate_files Logical indicating whether intermediate files produced by CoreNLP should be deleted. Defaults to TRUE, but can be set to FALSE and the xml output of CoreNLP will be saved.
#' @param syntactic_parsing Logical indicating whether syntactic parsing should be included as an option. Defaults to FALSE. Caution, enabling this argument may greatly increase runtime. If TRUE, output will automatically be return in raw format.
#' @param coreference_resolution Logical indicating whether coreference resolution should be included as an option. Defaults to FALSE. Caution, enabling this argument may greatly increase runtime. If TRUE, output will automatically be return in raw format.
#' @param ner_model The model to be used for named entity resolution. Can be one of 'english.all.3class', 'english.muc.7class', or 'english.conll.4class'. Defaults to 'english.all.3class'. These models are described in greater detail at teh following webpage: http://nlp.stanford.edu/software/CRF-NER.shtml#Models.
#' @param additional_options An optional string specifying additional options for CoreNLP. May cause unexpected behavior, use at your own risk!
#' @param return_raw_output Defaults to FALSE, if TRUE, then CoreNLP output is not parsed and raw list objects are returned.
#' @param version The version of Core-NLP to download. Defaults to '3.5.2'. Newer versions of CoreNLP will be made available at a later date.
#' @return Does not return anything.
#' @export
corenlp <- function(documents = NULL,
                    document_directory = NULL,
                    delete_intermediate_files = TRUE,
                    syntactic_parsing = FALSE,
                    coreference_resolution = FALSE,
                    ner_model = c("english.all.3class",
                                  "english.muc.7class",
                                  "english.conll.4class"),
                    additional_options = "",
                    return_raw_output = FALSE,
                    version = "3.5.2"){

    # save the current working directory
    currentwd <- getwd()

    ner_model <- ner_model[1]

    if(syntactic_parsing){
        return_raw_output <- TRUE
    }
    if(coreference_resolution){
        return_raw_output <- TRUE
    }

    USING_EXTERNAL_FILES <- FALSE
    #check to make sure that we have the right kind of input
    if(!is.null(documents) & is.null(document_directory)){
        if(class(documents) != "list" & class(documents) != "character"){
            stop("You must provide a 'documents' object as either a vector of strings (one per document) or a list of string vectors (one entry per document)...")
            success <- dir.create("corenlp_intermediate_files",showWarnings = FALSE)
            if(!success){
                file.remove("./corenlp_intermediate_files")
                success <- dir.create("corenlp_intermediate_files")
            }
            if(!success){
                stop("Could not create the intermdiate file directory necessary to use coreNLP. This is likely due to a file premission error. Make usre you have permission to create files or run your R session as root.")
            }
            setwd("./corenlp_intermediate_files")
        }
    }else if(is.null(documents) & !is.null(document_directory)){
        setwd(document_directory)
        USING_EXTERNAL_FILES <- TRUE
    }else{
        stop("You must specify either a valid documents object or a valid document_directory directory path (but not both)...")
    }


    #check to see that we have the selected version of corenlp installed
    test1 <- system.file("extdata",paste("stanford-corenlp-",version,".jar",sep = ""), package = "SpeedReader")[1]
    test2 <- system.file("extdata",paste("stanford-corenlp-",version,"-models.jar",sep = ""), package = "SpeedReader")[1]
    test3 <- system.file("extdata","xom.jar", package = "SpeedReader")[1]

    if(test1 != "" & test2 != "" & test3 != ""){
        cat("Found both CoreNLP JAR files...\n")
    }else{
        cat("CoreNLP Jar files not found, downloading...\n")
        download_corenlp(version = version)
    }

    substrRight <- function(x, n){
        substr(x, nchar(x)-n+1, nchar(x))
    }

    # prepare text to be used
    if(USING_EXTERNAL_FILES){
        documents <- list.files()
        #only use files with a .txt ending
        endings <- as.character(sapply(documents,substrRight,4))
        txtfiles <- which(endings == ".txt")
        if(length(txtfiles) > 0){
            documents <- documents[txtfiles]
        }else{
            stop("Did not find any valid .txt files in the specified directory...")
        }
    }

    numdocs <- length(documents)
    Processed_Text <- vector(length = numdocs, mode = "list")
    #names(Processed_Text) <- documents
    filenames <- rep("",numdocs)

    if(USING_EXTERNAL_FILES){
        filenames <- documents
    }else{
        for(i in 1:numdocs){
            filenames[i] <- paste("file",j,".txt",sep = "")
            text <- documents[i]
            #write each document to file
            write.table(text,
                        file = paste("file",j,".txt",sep = ""),
                        quote = FALSE,
                        row.names = F,
                        col.names= F,
                        sep = " " )
        }
    }

    # write filenames table to file so that it can be used by coreNLP
    write.table(filenames,
                file = "filenames.txt",
                quote = FALSE,
                row.names = F,
                col.names= F,
                sep = "\n" )

    #include options
    parse <- ""
    if(syntactic_parsing){
        parse <- ",parse"
    }
    dcoref <- ""
    if(coreference_resolution){
        dcoref <- ",dcoref"
    }

    # run corenlp
    directory <- system.file("extdata", package = "SpeedReader")[1]
    p2 <- pipe(paste('java -cp "', directory, '/*" -Xmx2g edu.stanford.nlp.pipeline.StanfordCoreNLP -annotators tokenize,ssplit,pos,lemma,ner',parse,dcoref," -ner.model ",ner_model," ",additional_options, ' -filelist filenames.txt',sep = ""),"r")
    close(p2)

    for(i in 1:numdocs){
        #read everything in
        if(USING_EXTERNAL_FILES){
            data <- XML::xmlParse(paste(filenames[i],".xml",sep = ""))
            if(delete_intermediate_files){
                file.remove(paste(filenames[i],".xml",sep = ""))
            }
        }else{
            data <- XML::xmlParse(paste("file",i,".txt.xml",sep = ""))
            if(delete_intermediate_files){
                file.remove(paste("file",i,".txt.xml",sep = ""))
            }
        }

        if(delete_intermediate_files){
            file.remove(paste("filenames.txt",sep = ""))
        }

        # turn into a list of sentences
        xml_data <- XML::xmlToList(data)[[1]][[1]]

        if(return_raw_output){
            Processed_Text[[i]] <- xml_data
        }else{
            #get number of tokens
            numtokens <- 0
            for(j in 1:length(xml_data)){
                numtokens <- numtokens + length(xml_data[[j]][[1]])
            }
            cat("Reading in:", numtokens, "tokens from document:",i,"of", numdocs,"\n")

            token_data <- data.frame(word = rep("",numtokens),
                                     lemma = rep("",numtokens),
                                     POS = rep("",numtokens),
                                     NER = rep("",numtokens),
                                     sentence = rep(0,numtokens),
                                     document = rep(i,numtokens),
                                     stringsAsFactors = FALSE)


            #loop through every sentence in document
            token_counter <- 1
            for(j in 1:length(xml_data)){
                for(k in 1:length(xml_data[[j]][[1]])){
                    token_data$word[token_counter] <- xml_data[[j]][[1]][[k]]$word
                    token_data$lemma[token_counter] <- xml_data[[j]][[1]][[k]]$lemma
                    token_data$POS[token_counter] <- xml_data[[j]][[1]][[k]]$POS
                    token_data$NER[token_counter] <- xml_data[[j]][[1]][[k]]$NER
                    token_data$sentence[token_counter] <- j
                    token_counter <- token_counter + 1
                }
            }
            Processed_Text[[i]] <- token_data
        }
    }

    # reset the working directory
    setwd(currentwd)
    # return the list of data frames
    return(Processed_Text)
}



