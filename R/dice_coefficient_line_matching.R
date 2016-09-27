#' @title Lines In Both Documents via Dice Coefficients
#' @description Calculate termwise Deice coefficeints for pairs of lines and
#' determine which lines are in both documents
#'
#' @param document_1 A vector of strings (one per line or one per sentence), or
#' a list of vectors of tokens  (one per line or one per sentence).
#' @param document_2 Same as document_1, will be used for comparison.
#' @param threshold A value between 0 and 1 that denotes the Dice coefficient
#' threshold for considdering a line as being in both documents.
#' @param return_dice_matrix Logical indicating whether the Dice coefficient
#' matrix should be returned. Defualts to FALSE.
#' @return A list with two vectors, each giving the inidices of the lines
#' in document 1/2 that are in the other document based on the dice coefficient
#' threshold.
#' @export
dice_coefficient_line_matching <- function(document_1,
                                           document_2,
                                           threshold = 0.8,
                                           return_dice_matrix = FALSE){

    ptm <- proc.time()
    cat("Whitespace tokenizing (if necessary)...\n")
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

    # get dice coefficients
    cat("Calculating Dice coefficients..\n")
    dice_matrix <- LineWise_Dice_Coefficients(
        length(document_1),
        document_1,
        length(document_2),
        document_2)

    # lines in document one in document two
    keep_1 <- which(apply(dice_matrix,1,max) >= threshold)

    keep_2 <- which(apply(dice_matrix,2,max) >= threshold)

    t2 <- proc.time() - ptm
    cat("Complete in:",t2[[3]],"seconds...\n")
    if (return_dice_matrix) {
        return(list(document_1_in_document_2 = keep_1,
                    docuemnt_2_in_document_1 = keep_2,
                    dice_matrix = dice_matrix))
    } else {
        return(list(document_1_in_document_2 = keep_1,
                    docuemnt_2_in_document_1 = keep_2))
    }

}
