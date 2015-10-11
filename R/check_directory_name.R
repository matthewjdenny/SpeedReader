#' A function to ensure that a directory name is in the proper format to be pasted together with a file name. It adds a trailling / if necessary.
#'
#' @param directory A string containing a directory path that we wish to check.
#' @return A string object that is a directory name with a trailing slash, which is what will be required if we wish to paste it and a file name together.
#' @export
check_directory_name <- function(directory){

    if(length(directory) != 1){
        stop("You must provide a single string that is a valid path to a directory.")
    }
    substrRight <- function(x, n){
        substr(x, nchar(x)-n+1, nchar(x))
    }

    lastchar <- substrRight(directory,1)

    if(lastchar == "/"){
        #we are all set
    }else{
        #add a trailing slash to we save to right place
        directory <- paste(directory,"/",sep = "")
    }
    return(directory)
}

