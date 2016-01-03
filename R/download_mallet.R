#' Checks the java version on your computer and downloads MALLET jar files for use with this package.
#'
#' @return Does not return anything.
#' @export
download_mallet <- function(){
    # determine if the user has a new enough version of Java (1.8 or higher)
    system("java -version", intern = TRUE)
    cat("You must have java version 1.8 or higher installed on your computer. You may update your java version by visiting the following website and then retry download. Website: http://www.oracle.com/technetwork/java/javase/downloads/index.html -- Make sure to select the JDK option from this page and then download the newest version.")

    # get the right file names
    directory <- system.file("extdata", package = "SpeedReader")[1]
    f1 <- paste(directory,"/mallet.jar",sep = "")
    f2 <- paste(directory,"/mallet-deps.jar",sep = "")

    url <- "http://mjdenny.com/SpeedReader/JAR_Files/"
    web1 <- paste(url,"mallet.jar",sep = "")
    web2 <- paste(url,"mallet-deps.jar",sep = "")

    # download the two jar files associated with the selected version
    cat("Downloading JAR files...\n" )
    cat("File 1 of 2...\n")
    download.file(url = web1, destfile = f1, method = "auto")
    cat("File 2 of 2...\n")
    download.file(url = web2, destfile = f2, method = "auto")
    cat("Downloads complete!\n")

    #check to see that the download worked
    test1 <- system.file("extdata","/mallet.jar", package = "SpeedReader")[1]
    test2 <- system.file("extdata","/mallet-deps.jar", package = "SpeedReader")[1]

    if(test1 != "" & test2 != ""){
        cat("JAR file downloads appear to have been successful!\n")
    }else{
        stop("It appears that one or more of the files did not download successfully...\n")
    }
}

