#' Checks the java version on your computer and downloads Stanford CoreNLP jar files for use with this package.
#'
#' @param version THe version of Core-NLP to download. Defaults to '3.5.2'
#' @return Does not return anything.
#' @export
download_corenlp <- function(version = "3.5.2"){
    # determine if the user has a new enough version of Java (1.8 or higher)
    version <- system("java -version", intern = TRUE)

    Version_OK <- function(){
        x <- readline(prompt = "Is the java version greater than or equal to  1.8? (yes/no)")
        x <- as.character(x)
        if(is.na(x)){
            stop("Please provide a valid string.")
        }
        ok <- FALSE
        if(tolower(x) == "yes"){
            ok <- TRUE
        }else if(tolower(x) == "no"){

        }else{
            stop("You must provide an answer of either 'yes' or 'no'.")
        }
        return(ok)
    }

    ok <- Version_OK()
    if(!ok){
        stop("You must have java version 1.8 or higher installed on your computer. Please update your java version by visiting the following website and then retry download. Website: http://www.oracle.com/technetwork/java/javase/downloads/index.html -- Make sure to select the JDK option from this page and then download the newest version.")
    }

    # get the right file names
    directory <- system.file("extdata", package = "SpeedReader")[1]
    f1 <- paste(directory,"/stanford-corenlp-",version,".jar",sep = "")
    f2 <- paste(directory,"/stanford-corenlp-",version,"-models.jar",sep = "")

    url <- "http://mjdenny.com/SpeedReader/JAR_Files/"
    web1 <- paste(url,"stanford-corenlp-",version,".jar",sep = "")
    web2 <- paste(url,"stanford-corenlp-",version,"-models.jar",sep = "")

    # download the two jar files associated with the selected version
    download.file(url = web1, destfile = f1, method = "auto")
    download.file(url = web2, destfile = f2, method = "auto")
}

