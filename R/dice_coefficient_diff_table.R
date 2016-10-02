#' @title Lines In Both Documents via Dice Coefficients
#' @description Calculate termwise Deice coefficeints for pairs of lines and
#' determine which lines are in both documents
#'
#' @param document_1 A vector of strings (one per line or one per sentence), or
#' a list of vectors of tokens  (one per line or one per sentence).
#' @param document_2 Same as document_1, will be used for comparison.
#' @param dice_matching_results Output from dice_coefficient_line_matching(),
#' with dice_matrix included.
#' @param output_string Logical indicating whether a string vector should be
#' returned. If TRUE, then all output is in string vector, if FALSE, the output
#' is concatenated to the screen.
#' @param compare_consecutive_line_pairs Logical indicating whether consecutive
#' pairs of lines should be compared. Defaults to TRUE.
#' @return A character vector (optionally).
#' @export
dice_coefficient_diff_table <- function(document_1,
                                        document_2,
                                        dice_matching_results,
                                        output_string = FALSE,
                                        compare_consecutive_line_pairs = TRUE){

    if (class(document_1) != "list") {
        doc <- vector(mode = "list",length = length(document_1))
        for (i in 1:length(doc)) {
            cur <- stringr::str_split(tolower(document_1[i]),"[\\s]+")[[1]]
            doc[[i]] <- cur
        }
        document_1 <- doc
    }

    if (class(document_2) != "list") {
        doc <- vector(mode = "list",length = length(document_2))
        for (i in 1:length(doc)) {
            cur <- stringr::str_split(tolower(document_2[i]),"[\\s]+")[[1]]
            doc[[i]] <- cur
        }
        document_2 <- doc
    }

    if (compare_consecutive_line_pairs) {
        # combine consecutive lines
        document_1a <- document_1[-length(document_1)]
        for (i in 1:length(document_1a)) {
            document_1a[[i]] <- c(document_1[[i]],document_1[[i+1]])
        }
        document_2a <- document_2[-length(document_2)]
        for (i in 1:length(document_2a)) {
            document_2a[[i]] <- c(document_2[[i]],document_2[[i+1]])
        }
        document_1 <- document_1a
        document_2 <- document_2a
    }

    # extract results
    document_1_in_document_2 <- dice_matching_results$document_1_in_document_2
    document_2_in_document_1 <- dice_matching_results$document_2_in_document_1
    dice_matrix <- dice_matching_results$dice_matrix
    threshold <- dice_matching_results$threshold

    # get non_matching lines
    nonmatching_1 <- 1:length(document_1)
    nonmatching_2 <- 1:length(document_2)
    if(length(document_1_in_document_2) >0){
        nonmatching_1 <- nonmatching_1[-document_1_in_document_2]
    }
    if(length(document_2_in_document_1) >0){
        nonmatching_2 <- nonmatching_2[-document_2_in_document_1]
    }

    #define latex headers a footers
    top <- c("\\documentclass{article}",
             "\\usepackage{float,geometry,color,xcolor,longtable,tabularx}",
             "\\definecolor{blue}{RGB}{51,51,153}",
             "\\definecolor{red}{RGB}{153,0,51}",
             "\\setlength\\voffset{-1in}",
             "\\setlength\\hoffset{-1in}",
             "\\setlength\\topmargin{.5in}",
             "\\setlength\\oddsidemargin{.5in}",
             "\\setlength\\textheight{9.8in}",
             "\\setlength\\textwidth{7.5in}",
             "\\setlength\\footskip{.7cm}",
             "\\setlength\\headheight{0cm}",
             "\\setlength\\headsep{0cm}",
             "\\begin{document}")
    header1 <- c("\\begin{longtable}{m{3.5in}|m{3.5in}}",
                "Deleted Line from Document 1 & Best Match From Document 2 \\\\",
                "\\hline\\hline \\\\")

    header2 <- c("\\begin{longtable}{m{3.5in}|m{3.5in}}",
                 "Added Line from Document 2 & Best Match From Document 1 \\\\",
                 "\\hline\\hline \\\\")

    footer <- c("\\hline",
                "\\end{longtable}")
    end <- "\\end{document}"

    # deal with document 1
    table_body <- NULL
    for (i in nonmatching_1) {
        cur <- NULL
        cur_line <- document_1[[i]]
        # find the best match to compare against
        max_ind <- which(dice_matrix[i,] == max(dice_matrix[i,]))[1]
        best_match_doc <- document_2[[max_ind]]
        if(length(max_ind) > 0) {
            for (j in 1:length(cur_line)) {
                found <- sum(grepl(cur_line[j],
                                   best_match_doc,
                                   ignore.case = TRUE,
                                   fixed = TRUE)) > 0
                if (found) {
                    cur  <- paste(cur," ",cur_line[j], sep = "")
                } else {
                    cur  <- paste(cur," {\\color{red}\\textbf{",cur_line[j],"}}", sep = "")
                }
            }
            # break column
            cur  <- paste(cur,"  & ", sep = "")

            if (max(dice_matrix[i,]) < 0.4) {
                for (j in 1:length(best_match_doc)) {
                    found <- sum(grepl(best_match_doc[j],
                                       cur_line,
                                       ignore.case = TRUE,
                                       fixed = TRUE)) > 0
                    if (found) {
                        cur  <- paste(cur," ",best_match_doc[j], sep = "")
                    } else {
                        cur  <- paste(cur," {\\color{blue}\\textbf{",best_match_doc[j],"}}", sep = "")
                    }
                }
            }

            cur  <- paste(cur," \\\\", sep = "")
            cur <- stringr::str_replace_all(cur,"[\\$]","")
            table_body <- c(table_body, cur, "\\hline")
        }
    }


    # deal with document 1
    table_body2 <- NULL
    for (i in nonmatching_2) {
        cur <- NULL
        cur_line <- document_2[[i]]
        # find the best match to compare against
        max_ind <- which(dice_matrix[,i] == max(dice_matrix[,i]))[1]
        best_match_doc <- document_1[[max_ind]]
        if(length(max_ind) > 0) {
            for (j in 1:length(cur_line)) {
                found <- sum(grepl(cur_line[j],
                                   best_match_doc,
                                   ignore.case = TRUE,
                                   fixed = TRUE)) > 0
                if (found) {
                    cur  <- paste(cur," ",cur_line[j], sep = "")
                } else {
                    cur  <- paste(cur," {\\color{blue}\\textbf{",cur_line[j],"}}" ,sep = "")
                }
            }
            # break column
            cur  <- paste(cur,"  & ", sep = "")
            if (max(dice_matrix[,i]) < 0.4) {
                for (j in 1:length(best_match_doc)) {
                    found <- sum(grepl(best_match_doc[j],
                                       cur_line,
                                       ignore.case = TRUE,
                                       fixed = TRUE)) > 0
                    if (found) {
                        cur  <- paste(cur," ",best_match_doc[j], sep = "")
                    } else {
                        cur  <- paste(cur," {\\color{red}\\textbf{",best_match_doc[j],"}}", sep = "")
                    }
                }
            }
            cur  <- paste(cur," \\\\", sep = "")
            cur <- stringr::str_replace_all(cur,"[\\$]","")
            table_body2 <- c(table_body2, cur, "\\hline")
        }
    }

    # now make tables and output them or not

    table_1 <- c(header1,table_body,footer, " "," "," ")
    table_2 <- c(header2,table_body2,footer, " "," "," ")

    document <- c(top,table_1,table_2,end)

    if (output_string) {
        return(document)
    } else {
        cat(table_1,sep = "\n")
        cat(table_2,sep = "\n")
    }

}
