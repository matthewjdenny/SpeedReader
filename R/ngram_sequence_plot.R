#' @title N-Gram Sequence Matching
#' @description Calculates the positions of n-grams in two document versions
#' which match an ngram in the other version.
#'
#' @param ngram_sequnce_results A List object produced by the
#' `ngram_sequence_matching()` function.
#' @param document Can be 1 or 2, the index of the document to plot.
#' @param columns Defaults to NA, in which case the number of columns will be
#' set automatically. Can be set to any integer number by the user.
#' @param include_title_legend Defaults to TRUE in which cas a title and legend
#' are include in the plot. Can be set to FALSE for multi-plotting.
#' @param return_plot_object Logical defaults to FALSE. If TRUE, then the plot
#' is not printed, only returned.
#' @param custom_title Optional custom plot title.
#' @return A plot
#' @export
ngram_sequnce_plot <- function(ngram_sequnce_results,
                               document = 1,
                               columns = NULL,
                               include_title_legend = TRUE,
                               return_plot_object = FALSE,
                               custom_title = NULL) {

    ngram_length <- ngram_sequnce_results$ngram_length

    if (document == 1) {
        matches <- ngram_sequnce_results$matches_document_1
    }
    if (document == 2) {
        matches <- ngram_sequnce_results$matches_document_2
    }

    # recode matches as text variable
    m <- rep("Match", length(matches))
    m[which(matches == 0)] <- "Non-Match"
    matches <- m

    if (is.null(columns)) {
      columns <- min(length(matches), 50)
    }

    rn <- NULL
    numrows <- ceiling(length(matches)/columns)
    for (i in 1:numrows) {
       rn <- c(rn, rep(i,columns))
    }
    rn <- rn[1:length(matches)]

    cn <- rep(1:columns,ceiling(length(matches)/columns))
    cn <- cn[1:length(matches)]

    dat <- data.frame(match = matches,
                      col_number = cn,
                      row_number = rn)

    p <- ggplot2::ggplot(dat, ggplot2::aes(col_number, row_number)) +
        ggplot2::geom_tile(ggplot2::aes(fill = match), colour = "white") +
        ggplot2::scale_fill_manual(values = c("lightblue","magenta"))
    if (include_title_legend) {
        p <- p + ggplot2::theme(legend.position = "bottom",
                                axis.ticks.y = ggplot2::element_blank(),
                                axis.text.y = ggplot2::element_blank(),
                                legend.title = ggplot2::element_blank()) +
            ggplot2::labs(x = "", y = "") +
            ggplot2::scale_x_discrete(expand = c(0, 0)) +
            ggplot2::scale_y_reverse(expand = c(0,0)) +
            ggplot2::ggtitle(paste(ngram_length,"-Gram Matches", sep = ""))
    } else {
        p <- p + ggplot2::theme(legend.position = "none",
                                axis.ticks.y = ggplot2::element_blank(),
                                axis.text.y = ggplot2::element_blank(),
                                legend.title = ggplot2::element_blank()) +
            ggplot2::labs(x = "", y = "") +
            ggplot2::scale_x_discrete(expand = c(0, 0)) +
            ggplot2::scale_y_reverse(expand = c(0,0))
    }

    if (!is.null(custom_title)) {
        p <- p + ggplot2::ggtitle(custom_title)
    }

    if (return_plot_object) {
        return(p)
    } else {
        print(p)
    }
}


