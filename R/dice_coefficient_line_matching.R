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
#' matrix should be returned. Defualts to TRUE.
#' @param compare_consecutive_line_pairs Logical indicating whether consecutive
#' pairs of lines should be compared. Defaults to TRUE.
#' @return A list with two vectors, each giving the inidices of the lines
#' in document 1/2 that are in the other document based on the dice coefficient
#' threshold.
#' @export
dice_coefficient_line_matching <- function(document_1,
                                           document_2,
                                           threshold = 0.8,
                                           return_dice_matrix = TRUE,
                                           compare_consecutive_line_pairs = TRUE){

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
    }

    # get dice coefficients
    cat("Calculating Dice coefficients..\n")
    if (compare_consecutive_line_pairs) {
        dice_matrix <- LineWise_Dice_Coefficients(
            length(document_1a),
            document_1a,
            length(document_2a),
            document_2a)
    } else {
        dice_matrix <- LineWise_Dice_Coefficients(
            length(document_1),
            document_1,
            length(document_2),
            document_2)
    }


    if (compare_consecutive_line_pairs) {
        # lines in document one in document two
        keep_1 <- which(apply(dice_matrix,1,max) >= threshold)
        if (length(keep_1) > 0 & length(document_1) > 1) {
            keep_1a <- keep_1 + 1
            keep_1 <- unique(c(keep_1,keep_1a))
            keep_1 <- keep_1[order(keep_1, decreasing = FALSE)]
        }

        keep_2 <- which(apply(dice_matrix,2,max) >= threshold)
        if (length(keep_2) > 0 & length(document_2) > 1) {
            keep_2a <- keep_2 + 1
            keep_2 <- unique(c(keep_2,keep_2a))
            keep_2 <- keep_2[order(keep_2, decreasing = FALSE)]
        }

    } else {
        # lines in document one in document two
        keep_1 <- which(apply(dice_matrix,1,max) >= threshold)

        keep_2 <- which(apply(dice_matrix,2,max) >= threshold)
    }


    t2 <- proc.time() - ptm
    cat("Complete in:",t2[[3]],"seconds...\n")
    if (return_dice_matrix) {
        return(list(document_1_in_document_2 = keep_1,
                    docuemnt_2_in_document_1 = keep_2,
                    dice_matrix = dice_matrix,
                    threshold = threshold))
    } else {
        return(list(document_1_in_document_2 = keep_1,
                    docuemnt_2_in_document_1 = keep_2))
    }

}
