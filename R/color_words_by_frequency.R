#' A function to generate LaTeX output from a dataframe containing words and their frequencies.
#'
#' @param word_frequency_dataframe A dataframe, preferrably produced by order_by_counts(), or with the same structure.
#' @param min_black Defaults to 30 and indicates how light the text for infrequently used words can become.
#' @param print_first Defualts to printing the first 40 words. If set to -1, then all words will be printed.
#' @param supress_reminder If TRUE then no reminder to use the xcolor pacakge will be provided.
#' @return Concatenates output to the console, no output is returned at present.
#' @export
color_words_by_frequency <- function(word_frequency_dataframe,
                                     min_black = 30,
                                     print_first = 40,
                                     supress_reminder = T){
  # check to make sure that there are as many rows in the dataframe we are
  # passing in as there are rows we want to print and fix if this is not the
  # case.
  if(print_first > nrow(word_frequency_dataframe)){
    print_first = nrow(word_frequency_dataframe)
  }
  # if print_first == -1 then print everything
  if(print_first == -1){
    print_first = nrow(word_frequency_dataframe)
  }


  # first we determine the range of colors
  top <- max(word_frequency_dataframe[1:print_first,2])
  bottom <- min(word_frequency_dataframe[1:print_first,2])
  levels <- bottom:top
  colors <- seq(from=min_black,100, length.out = length(levels))

  # now we deteremine font_widths <- c("m","b","bx")
  if(length(levels) > 1){
    first <- floor(length(levels)/3)
    second <- floor((length(levels) - first)/2)
    widths <- rep("m",length(levels))
    for(i in 1:first){
      widths[i] <- "bx"
    }
    for(i in (first+1):second){
      widths[i] <- "b"
    }
    widths <- rev(widths)
  }else{
    widths <- rep("m",length(levels))
  }

  # now we loop through the word frequency dataframe and cat it to the console.
  for(i in 1:print_first){
    cur <- word_frequency_dataframe[i,]
    cat("\\fontseries{",widths[which(levels == cur[,2])],"}\\selectfont\\textcolor{black!",colors[which(levels == cur[,2])],"}{",cur[,1],"}",sep = "")
    # unless this is that last word we are printing, comma separate.
    if(i != print_first){
      cat(", ")
    }
  }
  if(supress_reminder){
    cat("\n\n & \n\n")
  }else{
    # remind the user to use the xcolor package
    cat("\n\n\n\nRemember to include \\usepackage{xcolor} in your LaTeX document preamble.\n")
  }
}



