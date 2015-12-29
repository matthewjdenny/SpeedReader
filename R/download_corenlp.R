#' Checks the java version on your computer and downloads Stanford CoreNLP jar files for use with this package.
#'
#' @param version The version of Core-NLP to download. Defaults to '3.5.2'
#' @return Does not return anything.
#' @export
download_corenlp <- function(version = "3.5.2"){
    # determine if the user has a new enough version of Java (1.8 or higher)
    system("java -version", intern = TRUE)

    Version_OK <- function(){
        x <- readline(prompt = "Is the java version greater than or equal to  1.8? (yes/no): ")
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
    f3 <- paste(directory,"/stanford-corenlp-",version,"-sources.jar",sep = "")
    f4 <- paste(directory,"/stanford-corenlp-",version,"-javadoc.jar",sep = "")
    f5 <- paste(directory,"/ejml-0.23.jar",sep = "")
    f6 <- paste(directory,"/javax.json-api-1.0-sources.jar",sep = "")
    f7 <- paste(directory,"/javax.json.jar",sep = "")
    f8 <- paste(directory,"/joda-time-2.1-sources.jar",sep = "")
    f9 <- paste(directory,"/joda-time.jar",sep = "")
    f10 <- paste(directory,"/jollyday-0.4.7-sources.jar",sep = "")
    f11 <- paste(directory,"/jollyday.jar",sep = "")
    f12 <- paste(directory,"/protobuf.jar",sep = "")
    f13 <- paste(directory,"/xom-1.2.10-src.jar",sep = "")
    f14 <- paste(directory,"/xom.jar",sep = "")

    url <- "http://mjdenny.com/SpeedReader/JAR_Files/"
    web1 <- paste(url,"stanford-corenlp-",version,".jar",sep = "")
    web2 <- paste(url,"stanford-corenlp-",version,"-models.jar",sep = "")
    web3 <- paste(url,"stanford-corenlp-",version,"-sources.jar",sep = "")
    web4 <- paste(url,"stanford-corenlp-",version,"-javadoc.jar",sep = "")
    web5 <- paste(url,"/ejml-0.23.jar",sep = "")
    web6 <- paste(url,"/javax.json-api-1.0-sources.jar",sep = "")
    web7 <- paste(url,"/javax.json.jar",sep = "")
    web8 <- paste(url,"/joda-time-2.1-sources.jar",sep = "")
    web9 <- paste(url,"/joda-time.jar",sep = "")
    web10 <- paste(url,"/jollyday-0.4.7-sources.jar",sep = "")
    web11 <- paste(url,"/jollyday.jar",sep = "")
    web12 <- paste(url,"/protobuf.jar",sep = "")
    web13 <- paste(url,"/xom-1.2.10-src.jar",sep = "")
    web14 <- paste(url,"/xom.jar",sep = "")

    # download the two jar files associated with the selected version
    cat("Downloading JAR files...\n" )
    cat("File 1 of 14...\n")
    download.file(url = web1, destfile = f1, method = "auto")
    cat("File 2 of 14...\n")
    download.file(url = web2, destfile = f2, method = "auto")
    cat("File 3 of 14...\n")
    download.file(url = web3, destfile = f3, method = "auto")
    cat("File 4 of 14...\n")
    download.file(url = web4, destfile = f4, method = "auto")
    cat("File 5 of 14...\n")
    download.file(url = web5, destfile = f5, method = "auto")
    cat("File 6 of 14...\n")
    download.file(url = web6, destfile = f6, method = "auto")
    cat("File 7 of 14...\n")
    download.file(url = web7, destfile = f7, method = "auto")
    cat("File 8 of 14...\n")
    download.file(url = web8, destfile = f8, method = "auto")
    cat("File 9 of 14...\n")
    download.file(url = web9, destfile = f9, method = "auto")
    cat("File 10 of 14...\n")
    download.file(url = web10, destfile = f10, method = "auto")
    cat("File 11 of 14...\n")
    download.file(url = web11, destfile = f11, method = "auto")
    cat("File 12 of 14...\n")
    download.file(url = web12, destfile = f12, method = "auto")
    cat("File 13 of 14...\n")
    download.file(url = web13, destfile = f13, method = "auto")
    cat("File 14 of 14...\n")
    download.file(url = web14, destfile = f14, method = "auto")
    cat("Downloads complete!\n")

    #check to see that the download worked
    test1 <- system.file("extdata",paste("stanford-corenlp-",version,".jar",sep = ""), package = "SpeedReader")[1]
    test2 <- system.file("extdata",paste("stanford-corenlp-",version,"-models.jar",sep = ""), package = "SpeedReader")[1]

    if(test1 != "" & test2 != ""){
        cat("JAR file downloads appear to have been successful!\n")
    }else{
        stop("It appears that one or more of the files did not download successfully...\n")
    }
}

