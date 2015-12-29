#' Runs Stanford CoreNLP on a collection of documents
#'
#' @param documents An optional list of character vectors or a vector of strings, with one entry per dcument. These documents will be run through CoreNLP.
#' @param document_directory An optional directory path to a directory contianing only .txt files (one per document) to be run through CoreNLP. Cannot be supplied in addition to the 'documents' argument.
#' @param version The version of Core-NLP to download. Defaults to '3.5.2'.
#' @return Does not return anything.
#' @export
corenlp <- function(documents = NULL,
                    document_directory = NULL,
                    version = "3.5.2"){

    # save the current working directory
    currentwd <- getwd()

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

    # run corenlp
    directory <- system.file("extdata", package = "SpeedReader")[1]
    p2 <- pipe(paste('java -cp "', directory, '/*" -Xmx2g edu.stanford.nlp.pipeline.StanfordCoreNLP -annotators tokenize,ssplit,pos,lemma,ner -filelist filenames.txt',sep = ""),"r")
    close(p2)

    for(i in 1:numdocs){
        #read everything in
        if(USING_EXTERNAL_FILES){
            data <- XML::xmlParse(paste(filenames[i],".xml",sep = ""))
        }else{
            data <- XML::xmlParse(paste("file",i,".txt.xml",sep = ""))
        }

        xml_data <- XML::xmlToList(data)[[1]][[1]]

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
            for(k in 1:length(xml_data[[j]])){
                token_data$word[token_counter] <- xml_data[[j]][[k]]$word
                token_data$lemma[token_counter] <- xml_data[[j]][[k]]$lemma
                token_data$POS[token_counter] <- xml_data[[j]][[k]]$POS
                token_data$NER[token_counter] <- xml_data[[j]][[k]]$NER
                token_data$sentence[token_counter] <- j
                token_counter <- token_counter + 1
            }
        }

        Processed_Text[[i]] <- token_data
    }
    return(Processed_Text)
}



