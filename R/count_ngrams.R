#' An experimental function to efficiently generate a vocabulary in parallel
#' from output produced by the ngrams() function. Cores > 1 will only work for
#' users with GNU coreutils > 8.13 as the sort --parallel option is used. If you
#' have an older version use cores = 1.
#'
#' @param ngrams An optional list object output by the ngrams() function.
#' @param input_directory An optional input directory where blocked output from
#' th ngrams() function is stored as .Rdata files.
#' @param file_list An optional vector of file names to be used. Useful if you
#' only want to work on a subset of the input.
#' @param combine_ngrams Logical indicating whether simple ngrams should be
#' combined together when forming the vocabulary. If FALSE, then separate
#' vocabularies will be generated for each ngram length. Defaults to FALSE.
#' @param cores The number of cores to be used for parallelization.
#' @param mac_brew An option to use alternate versions of shell commands that
#' are compatible with GNU coretools as installed via "brew install coretools".
#' Simple adds a "g" infront of commands.
#' @return Returns a list object with the vocabulary (sorted by frequency) and
#' and word counts.
#' @export
count_ngrams <- function(ngrams = NULL,
                         input_directory = NULL,
                         file_list = NULL,
                         combine_ngrams = FALSE,
                         cores = 2,
                         mac_brew = FALSE) {

    current_directory <- getwd()

    if (!is.null(ngrams) & !is.null(input_directory)) {
        stop("You may only specify one of ngrams or input_directory!.")
    }

    USING_EXTERNAL_FILES <- FALSE
    if (!is.null(ngrams)) {
        if (class(ngrams) != "list") {
            stop("You must provide a list object output by the ngrams() function.")
        }
        input_directory <- current_directory
    } else {
        USING_EXTERNAL_FILES <- TRUE
        setwd(input_directory)
    }

    # get all of the file names
    substrRight <- function(x, n){
        substr(x, nchar(x) - n + 1, nchar(x))
    }

    # find external files
    if (USING_EXTERNAL_FILES) {
        if (is.null(file_list)) {
            ngrams <- list.files()
            #only use files with a .txt ending
            endings <- as.character(sapply(ngrams,substrRight,6))
            files <- which(endings == ".Rdata")
            if (length(files) > 0) {
                ngrams <- ngrams[files]
                cat("Found",length(files),
                    "valid .Rdata files in directory. Here are a few examples:\n")
                print(head(ngrams))
            } else {
                stop("Did not find any valid .Rdata files in the specified directory...")
            }
        } else {
            ngrams <- file_list
        }
    }

    # if we are using external files read in each block one by one and add them
    # to the output.
    if (USING_EXTERNAL_FILES) {
        NGrams <- NULL
        # loop over external files
        for (i in 1:length(ngrams)) {
            cat("Currently working on block",i,"of",length(ngrams),"...\n")
            # load in the file
            load(ngrams[i])
            if (i == 1) {
                filenames <- output_ngrams(NGrams = NGrams,
                                           combine_ngrams = combine_ngrams,
                                           directory = input_directory)
            } else {
                add <- output_ngrams(NGrams = NGrams,
                                     combine_ngrams = combine_ngrams,
                                     directory = input_directory)
                if (length(add$filenames) > length(filenames$filenames)) {
                  filenames <- add
                }
            }


        }
    } else {
        cat("Outputting",length(ngrams),
            "to text files. This may take a while...\n")
        filenames <- output_ngrams(NGrams = ngrams,
                                   combine_ngrams = combine_ngrams,
                                   directory = input_directory)
    }

    vocab_and_counts <- vector(mode = "list",
                               length = length(filenames$filenames))
    names(vocab_and_counts) <- filenames$types
    # now we generate wordcount for each of the files
    for (i in 1:length(filenames$filenames)) {
        cat("Currently generating vocabulary and counts for",filenames$types[i],
            "in parallel on",cores,"cores (job",i,"of",
            length(filenames$filenames),") starting at:",
            toString(Sys.time()),"\n")
        if (mac_brew) {
            if (cores == 1) {
                command <- paste("gtr -sc 'A-Za-z\\_' '\\012' < ",
                                 filenames$filenames[i],
                                 " | gsort | guniq -c > Vocab_",i,".txt",
                                 sep = "")
            } else {
                command <- paste("gtr -sc 'A-Za-z\\_' '\\012' < ",
                filenames$filenames[i],
                " | gsort --parallel=",cores," | guniq -c > Vocab_",i,".txt",
                sep = "")
            }
        } else {
            if (cores == 1) {
                command <- paste("tr -sc 'A-Za-z\\_' '\\012' < ",
                                 filenames$filenames[i],
                                 " | sort | uniq -c > Vocab_",i,".txt",
                                 sep = "")
            } else {
                command <- paste("tr -sc 'A-Za-z\\_' '\\012' < ",
                 filenames$filenames[i],
                 " | sort --parallel=",cores," | uniq -c > Vocab_",i,".txt",
                 sep = "")
            }
        }

        p1 <- pipe(command,"r")
        close(p1)

        cat("Reading in vocabulary and extracting terms and counts...\n")
        #now read in the results and save them
        cur_vocab <- read.delim(file = paste("Vocab_",i,".txt",sep = ""),
                                sep = "\n",
                                header = TRUE,
                                stringsAsFactors = FALSE)

        #remove the intermediate files
        p2 <- pipe(paste("rm Vocab_",i,".txt",sep = ""),"r")
        close(p2)
        p3 <- pipe(paste("rm ",filenames$filenames[i],sep = ""),"r")
        close(p3)

        cat("Extracting terms and counts...\n")

        get_count <- function(str){
            return(as.numeric(stringr::str_extract_all(str,"[0-9]+")[[1]]))
        }

        get_term <- function(str){
            temp <- stringr::str_split(str," ")[[1]]
            return(temp[length(temp)])
        }

        counts <- sapply(cur_vocab[,1],get_count, USE.NAMES = FALSE)
        terms <- sapply(cur_vocab[,1],get_term, USE.NAMES = FALSE)

        vocab_and_counts[[i]] <- list(term = terms,
                                      count = counts)
        cat("Documents contain",length(counts),"unique",filenames$types[i],
            "and a total of",sum(counts),"of these terms...\n")
        cat("Ending current job at:",toString(Sys.time()),"\n")
    }

    return(vocab_and_counts)
}
