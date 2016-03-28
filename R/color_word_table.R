#' A function to generate LaTeX output from a dataframe containing covariates and top words.
#'
#' @param word_table A dataframe with optional covariates and then some number of fields each of which is a top word.
#' @param covariate_columns The columns (if any) in the word table(s) that contain covariates which will be printed verbatim.
#' @param word_columns The columns in the word table(s) that contain the actual words we wish to print.
#' @param min_black Defaults to 30 and indicates how light the text for infrequently used words can become.
#' @param print_first Defualts to printing the first 40 rows. If set to -1, then all rows will be printed.
#' @param all_same If TRUE then the color of all words will be the same and at the maximum value.
#' @param remove_words A user supplied list of stopwords that should be removed before printing top words.
#' @param second_table if not NULL, then two tables are output side by side, following the same formatting and row widths.
#' @param max_char_width This sepcifies the maximum number of characters that can be displayed per line (in order to avoid lines in the table spilling over). Defaults to 60 which is appropriate for the two column PNAS style.
#' @param bold_covariates If TRUE then any covariate values provided will be output in bold font to make them easier to distinguish from topic top words.
#' @return Concatenates output to the console, no output is returned at present.
#' @export
color_word_table <- function(word_table,
                             covariate_columns  = 1,
                             word_columns = 4:13,
                             min_black = 30,
                             print_first = 60,
                             all_same = FALSE,
                             remove_words = "",
                             second_table = NULL,
                             max_char_width = 60,
                             bold_covariates = T){

  # helper function
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  remove_last_two_chars <- function(x){
    substr(x, 1, nchar(x)- 2)
  }
  # check to make sure that there are as many rows in the dataframe we are
  # passing in as there are rows we want to print and fix if this is not the
  # case.
  print_first2 <- print_first
  if(print_first > nrow(word_table)){
    print_first = nrow(word_table)
  }
  # if print_first == -1 then print everything
  if(print_first == -1){
    print_first = nrow(word_table)
  }

  all_words <- unlist_and_concatenate(word_table[,word_columns])

  remove_indexes <- which(all_words %in% remove_words)
  if(length(remove_indexes) > 0){
    all_words <- all_words[-remove_indexes]
  }
  word_frequency_dataframe <- order_by_counts(all_words)
  unqiue_words <- as.character(word_frequency_dataframe[,1])

  # first we determine the range of colors
  top <- max(word_frequency_dataframe[,2])
  bottom <- min(word_frequency_dataframe[,2])
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

  #if we provided a second table, prepare it as well
  if(!is.null(second_table)){
    if(print_first > nrow(second_table)){
      print_first2 = nrow(second_table)
    }
    # if print_first == -1 then print everything
    if(print_first2 == -1){
      print_first2 = nrow(second_table)
    }

    all_words2 <- unlist_and_concatenate(second_table[,word_columns])

    remove_indexes2 <- which(all_words2 %in% remove_words)
    if(length(remove_indexes2) > 0){
      all_words2 <- all_words2[-remove_indexes2]
    }
    word_frequency_dataframe2 <- order_by_counts(all_words2)
    unqiue_words2 <- as.character(word_frequency_dataframe2[,1])

    # first we determine the range of colors
    top2 <- max(word_frequency_dataframe2[,2])
    bottom2 <- min(word_frequency_dataframe2[,2])
    levels2 <- bottom2:top2
    colors2 <- seq(from=min_black,100, length.out = length(levels2))

    # now we deteremine font_widths <- c("m","b","bx")
    if(length(levels2) > 1){
      first2 <- floor(length(levels2)/3)
      second2 <- floor((length(levels2) - first2)/2)
      widths2 <- rep("m",length(levels2))
      for(i in 1:first2){
        widths2[i] <- "bx"
      }
      for(i in (first2+1):second2){
        widths2[i] <- "b"
      }
      widths2 <- rev(widths2)
    }else{
      widths2 <- rep("m",length(levels2))
    }
  }

  # now we loop through the word frequency dataframe and cat it to the console.
  toprint = max(print_first,print_first2)
  for(i in 1:toprint){
    if(i <= print_first){
      covars <- as.character(word_table[i,covariate_columns])
      words <- as.character(word_table[i,word_columns])
      line <- NULL
      justwords <- NULL
      # add covariates to table
      if(length(covars) > 0){
        for(j in 1:length(covars)){
          if(bold_covariates){
            line <- paste(line,"\\textbf{",covars[j],"} & ",sep = "")
          }else{
            line <- paste(line,covars[j]," & ",sep = "")
          }
        }
        justwords <- paste(justwords,covars[j],sep = " ")
      }
      if(length(words) > 0){
        for(j in 1:length(words)){
          if(words[j] %in% remove_words){
            # do nothing because it is a stopword
          }else{
            index <- which(unqiue_words == words[j])
            if(!all_same){
              addition <-paste(line,"\\fontseries{",widths[which(levels == word_frequency_dataframe[index,2])],"}\\selectfont\\textcolor{black!",colors[which(levels == word_frequency_dataframe[index,2])],"}{",words[j],"}",sep = "")
            }else{
              addition <-paste(line,words[j],sep = "")
            }
            justwords <- paste(justwords,words[j],", ",sep = "" )
            # unless this is that last word we are printing, comma separate.
            if(j != length(words)){
              addition <-paste(addition,", ",sep = "")
            }

            line_width <- nchar(justwords)
            if(line_width <=max_char_width){
              line <- addition
            }
          }
        }
        # check to see if we have a trailing ", "
        if(substrRight(line,2) == ", "){
          line <- remove_last_two_chars(line)
        }
        cat(line)
      }
    }

    # if we only supplied one dataset then we are done
    if(is.null(second_table)){
      cat("\\\\ \n")
    }else{
      cat("\n &\n")
      if(i <= print_first2){
        covars2 <- as.character(second_table[i,covariate_columns])
        words2 <- as.character(second_table[i,word_columns])
        line2 <- NULL
        justwords2 <- NULL
        # add covariates to table
        if(length(covars2) > 0){
          for(j in 1:length(covars2)){
            if(bold_covariates){
              line2 <- paste(line2,"\\textbf{",covars2[j],"} & ",sep = "")
            }else{
              line2 <- paste(line2,covars2[j]," & ",sep = "")
            }
          }
          justwords2 <- paste(justwords2,covars2[j],sep = " ")
        }
        if(length(words2) > 0){
          for(j in 1:length(words2)){
            if(words2[j] %in% remove_words){
              # do nothing because it is a stopword
            }else{
              index2 <- which(unqiue_words2 == words2[j])
              if(!all_same){
                addition2 <-paste(line2,"\\fontseries{",widths2[which(levels2 == word_frequency_dataframe2[index2,2])],"}\\selectfont\\textcolor{black!",colors2[which(levels2 == word_frequency_dataframe2[index2,2])],"}{",words2[j],"}",sep = "")
              }else{
                addition2 <-paste(line2,words2[j],sep = "")
              }
              justwords2 <- paste(justwords2,words2[j],", ",sep = "" )

              # unless this is that last word we are printing, comma separate.
              if(j != length(words2)){
                addition2 <-paste(addition2,", ",sep = "")
              }

              line_width2 <- nchar(justwords2)
              if(line_width2 <= max_char_width){
                line2 <- addition2
              }
            }
          }
          # check to see if we have a trailing ", "
          if(substrRight(line2,2) == ", "){
            line2 <- remove_last_two_chars(line2)
          }
          cat(line2)
        }
      }
      cat("\\\\ \n")
    }
  }
  # remind the user to use the xcolor package
  cat("\n\n\n\nRemember to include \\usepackage{xcolor} in your LaTeX document preamble.\n")
}



