mallet_lda <- function(documents = NULL,
                       document_directory = NULL,
                       vocabulary = NULL,
                       topics = 10,
                       iterations = 100,
                       alpha = 1,
                       beta = 0.1,
                       hyperparameter_optimization_interval = 0,
                       num_top_words = 20,
                       optional_arguments = "",
                       tokenization_regex = '[\\p{L}\\p{N}\\p{P}]+',
                       cores = 1,
                       delete_intermediate_files = TRUE){

    ###############################
    #### Step 0: Preliminaries ####
    ###############################
    if(hyperparameter_optimization_interval == 0 & is.null(vocabulary_size)){
        stop("You must provide the vocabulary_size if you are not using hyperparameter optimization.")
    }

    if(hyperparameter_optimization_interval == 0){
        beta <- beta * length(vocabulary)
    }

    # save the current working directory
    currentwd <- getwd()

    USING_EXTERNAL_FILES <- FALSE
    #check to make sure that we have the right kind of input
    if(!is.null(documents) & is.null(document_directory)){
        if(class(documents) == "list" | class(documents) == "character"){
            # create an intermediate directory
            success <- dir.create("mallet_intermediate_files",showWarnings = FALSE)
            if(!success){
                file.remove("./mallet_intermediate_files")
                success <- dir.create("mallet_intermediate_files")
            }
            if(!success){
                stop("Could not create the intermdiate file directory necessary to use coreNLP. This is likely due to a file premission error. Make usre you have permission to create files or run your R session as root.")
            }
            setwd("./mallet_intermediate_files")

            if(class(documents) == "list"){
                # deal with the case where we got a list of term vectors
                temp <- rep("", length(documents))
                for(i in 1:length(temp)){
                    temp[i] <- paste0(documents[[i]],collapse = " ")
                }
                documents <- temp
            }
        }else if(class(documents) == "matrix"){
            if(is.null(vocabulary)){
                vocabulary <- colnames(documents)
                cat("No vocabulary supplied, using column names of document term matrix...\n")
            }
            if(length(vocabulary) != ncol(documents)){
                stop(paste("Length of vocabulary:",length(vocabulary),"is not equal to the number of columns in the document term matrix:",ncol(documents)))
            }

            #populate a string vector of documents from dtm
            cat("Populating document vector from document term matrix...\n")
            printseq_counter <- 1
            if(nrow(documents) > 9999){
                printseq <- round(seq(1,nrow(documents), length.out = 1001)[2:1001],0)
            }else if(nrow(documents) > 199){
                printseq <- round(seq(1,nrow(documents), length.out = 101)[2:101],0)
            }else{
                printseq <- 1:nrow(documents)
            }
            temp_docs <- rep("",nrow(documents))
            for(i in 1:length(temp_docs)){
                if(printseq[printseq_counter] == i){
                    cat(printseq_counter,"/",length(printseq)," complete...\n",sep = "")
                }
                str <- NULL
                colindexes <- which(documents[i,] > 0)
                if(length(colindexes) > 0){
                    for(k in 1:length(colindexes)){
                        str <- c(str,
                                 rep(vocabulary[colindexes[k]],
                                     documents[i,colindexes[k]]))
                    }
                    temp  <- paste0(str,collapse = " ")
                }else{
                    temp <- ""
                }
                temp_docs[i] <- temp
            }
            documents <- temp_docs


        }else if(class(documents) == "simple_triplet_matrix"){
            if(is.null(vocabulary)){
                vocabulary <- colnames(documents)
                cat("No vocabulary supplied, using column names of document term matrix...\n")
            }
            if(length(vocabulary) != ncol(documents)){
                stop(paste("Length of vocabulary:",length(vocabulary),"is not equal to the number of columns in the document term matrix:",ncol(documents)))
            }

            #populate a string vector of documents from dtm
            cat("Populating document vector from document term matrix...\n")
            printseq_counter <- 1
            if(nrow(documents) > 9999){
                printseq <- round(seq(1,nrow(documents), length.out = 1001)[2:1001],0)
            }else if(nrow(documents) > 199){
                printseq <- round(seq(1,nrow(documents), length.out = 101)[2:101],0)
            }else{
                printseq <- 1:nrow(documents)
            }
            temp_docs <- rep("",nrow(documents))
            for(i in 1:length(temp_docs)){
                if(printseq[printseq_counter] == i){
                    cat(printseq_counter,"/",length(printseq)," complete...\n",sep = "")
                }
                str <- NULL
                colindexes <- which(documents$i == i)
                if(length(colindexes) > 0){
                    for(k in 1:length(colindexes)){
                        str <- c(str,
                                 rep(vocabulary[colindexes[k]],
                                     documents$v[colindexes[k]]))
                    }
                    temp  <- paste0(str,collapse = " ")
                }else{
                    temp <- ""
                }
                temp_docs[i] <- temp
            }
            documents <- temp_docs


        }else{
            stop("You must provide a 'documents' object as either a vector of strings (one per document),a list of string vectors (one entry per document), or a dense (or sparse) document-term matrix...")
        }
    }else if(is.null(documents) & !is.null(document_directory)){
        USING_EXTERNAL_FILES <- TRUE
        substrRight <- function(x, n){
            substr(x, nchar(x)-n+1, nchar(x))
        }

        # prepare text to be used
        documents <- list.files(path = document_directory)
        #only use files with a .txt ending
        endings <- as.character(sapply(documents,substrRight,4))
        txtfiles <- which(endings == ".txt")
        if(length(txtfiles) > 0){
            documents <- documents[txtfiles]
        }else{
            stop("Did not find any valid .txt files in the specified directory...")
        }
        #read in documents
        temp_docs <- rep("",length(documents))
        for(i in 1:length(documents)){
            temp_docs[i] <- paste0(readLines(documents[i]), collapse = " ")
        }
        document_names <- documents
        documents <- temp_docs
    }else{
        stop("You must specify either a valid documents object or a valid document_directory directory path (but not both)...")
    }

    # deal with different kinds of data -- turn it into a vector of strings (one string per document)
    #check to see that we have the selected version of corenlp installed
    test1 <- system.file("extdata","/mallet.jar", package = "SpeedReader")[1]
    test2 <- system.file("extdata","/mallet-deps.jar", package = "SpeedReader")[1]

    if(test1 != "" & test2 != "" & test3 != ""){
        cat("Found both MALLET JAR files...\n")
    }else{
        cat("MALLET Jar files not found, downloading...\n")
        download_mallet(version = version)
    }

    num_docs <- length(documents)
    directory <- system.file("extdata", package = "SpeedReader")[1]

    ##############################################
    #### Step 1: Output documents to tsv file ####
    ##############################################
    cat("Outputing documents in correct format for MALLET...\n")

    # CSV format -- 1 line per document:
    # doc_id\t\tdoc_text
    data <- matrix("",nrow = num_docs,ncol = 3)
    for(i in 1:num_docs){
        data[i,1] <- i
        data[i,2] <- "PLACEHOLDER"
        data[i,3] <- documents[i]
    }
    cat("Writing corpus to file...")

    # write the data to file:
    write.table(data, file = "mallet_input_corpus.csv", quote = FALSE, row.names = F,col.names= F, sep = "\t" )

    ############################################
    #### Step 2: Preprocess data for MALLET ####
    ############################################

    # prepare the data for use with Mallet's LDA routine
    prepare_data <- paste("java -server -Xmx3g -classpath ",directory,"/* cc.mallet.classify.tui.Csv2Vectors --keep-sequence --token-regex '",tokenization_regex,"' --output mallet_corpus.dat --input mallet_input_corpus.csv --print-output > stdout_intake.txt 2>&1", sep = "")
    p <- pipe(prepare_data,"r")
    close(p)

    ####################################
    #### Step 3: Run LDA via MALLET ####
    ####################################

    # Now run LDA
    if(hyperparameter_optimization_interval != 0){
        run_mallet <- paste("java -server -Xmx10g -classpath ",directory,"/* cc.mallet.topics.tui.Vectors2Topics --input mallet_corpus.dat --output-state output_state.txt.gz --output-topic-keys topic-keys.txt --xml-topic-report topic-report.xml --xml-topic-phrase-report topic-phrase-report.xml --output-doc-topics doc-topics.txt --num-topics ",topics,"--num-iterations ",iterations," --output-state-interval ",floor(iterations/5)," --num-threads ",cores," --optimize-interval ",hyperparameter_optimization_interval," --optimize-burn-in ",hyperparameter_optimization_interval," > stdout.txt 2>&1&", sep = "")
    }else{
        run_mallet <- paste("java -server -Xmx10g -classpath ",directory,"/* cc.mallet.topics.tui.Vectors2Topics --input mallet_corpus.dat --output-state output_state.txt.gz --output-topic-keys topic-keys.txt --xml-topic-report topic-report.xml --xml-topic-phrase-report topic-phrase-report.xml --output-doc-topics doc-topics.txt --num-topics ",topics,"--num-iterations ",iterations," --output-state-interval ",floor(iterations/5)," --num-threads ",cores," --beta ",beta," > stdout.txt 2>&1&", sep = "")
    }

    p <- pipe(run_mallet,"r")
    close(p)

    #######################################
    #### Step 4: Read the data back in ####
    #######################################

    LDA_Results <- vector(mode = "list", length = 4)



    ###############################################
    #### Step 5: Cleanup and Return Everything ####
    ###############################################

    # remove
    if(delete_intermediate_files){
        setwd("..")
        unlink("./mallet_intermediate_files")
    }

    #reset the working directory
    setwd(currentwd)

    return(LDA_Results)

}
