#' Runs Stanford CoreNLP on a collection of .txt files and processes them in blocks of a specified size, saving intermediate results to disk. Designed to function on very large corpora.
#'
#' @param output_directory The path to a directory where the user would like CoreNLP output to be stored. Output will be saved to this directory in .Rdata files named CoreNLP_Output_1.Rdata ... CoreNLP_Output_N.Rdata
#' @param document_directory A directory path to a directory contianing .txt files (one per document) to be run through CoreNLP.
#' @param file_list An optional list of .txt files to be used. Can be useful if the user only wants to process a subset of documents in the directory such as when the corpus is extremely large.
#' @param block_size The number of docuemnts to be processed at a time. Defaults to 1000.
#' @param syntactic_parsing Logical indicating whether syntactic parsing should be included as an option. Defaults to FALSE. Caution, enabling this argument may greatly increase runtime. If TRUE, output will automatically be return in raw format.
#' @param coreference_resolution Logical indicating whether coreference resolution should be included as an option. Defaults to FALSE. Caution, enabling this argument may greatly increase runtime. If TRUE, output will automatically be return in raw format.
#' @param additional_options An optional string specifying additional options for CoreNLP. May cause unexpected behavior, use at your own risk!
#' @param return_raw_output Defaults to FALSE, if TRUE, then CoreNLP output is not parsed and raw list objects are returned.
#' @param version The version of Core-NLP to download. Defaults to '3.5.2'. Newer versions of CoreNLP will be made available at a later date.
#' @param parallel Logical indicating whether CoreNLP should be run in parallel.
#' @param cores The number of cores to be used if CoreNLP is being run in parallel.
#' @return Does not return anything, saves all output to disk.
#' @export
corenlp_blocked <- function(output_directory,
                            document_directory,
                            file_list = NULL,
                            block_size = 1000,
                            syntactic_parsing = FALSE,
                            coreference_resolution = FALSE,
                            additional_options = "",
                            return_raw_output = FALSE,
                            version = "3.5.2",
                            parallel = FALSE,
                            cores = 1){

    currentwd <- getwd()
    setwd(document_directory)
    output_directory <- check_directory_name(output_directory)

    substrRight <- function(x, n){
        substr(x, nchar(x)-n+1, nchar(x))
    }

    if(is.null(file_list)){
        documents <- list.files()
        #only use files with a .txt ending
        endings <- as.character(sapply(documents,substrRight,4))
        txtfiles <- which(endings == ".txt")
        if(length(txtfiles) > 0){
            documents <- documents[txtfiles]
        }else{
            stop("Did not find any valid .txt files in the specified directory...")
        }
    }else{
        documents <- file_list
    }

    # get the number of documents and number of blocks
    num_docs <- length(documents)
    num_blocks <- ceiling(num_docs/block_size)

    # if parallel
    single_block <- function(i){
        cat("Curently working on block:",i,"of",num_blocks,"...\n")
        # get the appropriate file list
        start <- block_size*(i-1) + 1
        end <- block_size*i
        current_file_list <- documents[start:end]

        #run corenlp on block of documents
        Processed_Text <- corenlp(
            document_directory = document_directory,
            file_list = current_file_list,
            syntactic_parsing = syntactic_parsing ,
            coreference_resolution = coreference_resolution,
            additional_options = additional_options,
            return_raw_output = return_raw_output,
            version = version,
            block = i)

        save(Processed_Text,
             file = paste(output_directory,
                          "CoreNLP_Output_",i,".Rdata",sep = ""))
    }



    if(parallel){
        download_corenlp(version = version)
        #intitalizes snowfall session
        snowfall::sfInit(parallel = TRUE, cpus = cores)

        #check to see if we are running in parallel
        if(snowfall::sfParallel())
            cat( "Running in parallel mode on", snowfall::sfCpus(), "nodes.\n" )
        else
            cat( "Running in sequential mode.\n" )

        #export all packages and libraries currently loaded in workspace
        for (i in 1:length(.packages())){
            eval(call("sfLibrary", (.packages()[i]), character.only=TRUE))
        }

        # apply our problem across the cluster using hte indexes we have determined and load balancing
        # Export a list of R data objects
        snowfall::sfExportAll()
        snowfall::sfClusterApplyLB(1:num_blocks,single_block)

        #stop the cluster when we are done -- this is very important and must be done manually every time
        snowfall::sfStop()
    }else{
        # now loop
        for(i in 1:num_blocks){
            cat("Curently working on block:",i,"of",num_blocks,"...\n")
            # get the appropriate file list
            start <- block_size*(i-1) + 1
            end <- block_size*i
            current_file_list <- documents[start:end]

            #run corenlp on block of documents
            Processed_Text <- corenlp(
                document_directory = document_directory,
                file_list = current_file_list,
                syntactic_parsing = syntactic_parsing ,
                coreference_resolution = coreference_resolution,
                additional_options = additional_options,
                return_raw_output = return_raw_output,
                version = version,
                block = i)

            save(Processed_Text,
                 file = paste(output_directory,
                              "CoreNLP_Output_",i,".Rdata",sep = ""))
        }
    }



    setwd(currentwd)
}



