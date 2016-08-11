subsume_ngrams <- function(ranked_terms,
                           document_term_matrix,
                           term_clusters_to_output = 500,
                           top_terms_to_search = 200,
                           correlation_threshold = 0.9) {

    if (class(document_term_matrix) == "simple_triplet_matrix"){
        document_term_matrix <- sparse_to_dense_matrix(document_term_matrix)
    }

    # we will use this data-structure to store all terms in term cluster with
    # descriptive metadata
    ranked_term_cluster_list <- vector(mode = "list",
                                       length = term_clusters_to_output)
    # use the existing dataset as a template and repopulate
    ranked_term_clusters <- ranked_terms[1:term_clusters_to_output,]
    # allocate another vector to record the number of terms combined which we
    # will return to the user
    terms_subsumed <- rep(0,term_clusters_to_output)

    clusters_returned <- 1

    # loop over term_clusters_to_output and populate
    for (i in 1:term_clusters_to_output) {
        # print(i)
        # print(paste("nrow:", nrow(ranked_terms)))
        # get the focal term
        focal_term <- ranked_terms[1,1]
        focal_term_metadata <- ranked_terms[1,2:ncol(ranked_terms)]

        # find all terms that subsume the focal term
        max_row <- min(top_terms_to_search, nrow(ranked_terms))

        if (max_row == 0) {
            break
        } else {
            inds <- 1 # becasue we look for subterms of the current term
            for (j in 2:max_row) {
                if (grepl(focal_term, ranked_terms[j,1])) {
                    inds <- c(inds,j)
                }
            }
            # now get the actual terms
            larger_terms <- ranked_terms[inds,1]
            # now loop over max_length terms and find all of their subterms
            for (j in 1:length(larger_terms)) {
                cur <- larger_terms[j]
                for (k in 2:max_row) {
                    if (grepl(ranked_terms[k,1], cur)) {
                        inds <- c(inds,k)
                    }
                }
            }

            # get the unique term indicies
            inds <- unique(inds)

            # if there is only one term in the cluster, then add it to the
            # appropriate data structures and move on.
            if (length(inds) == 1) {
                # only the first term can be in the cluster
                in_cluster <- 1
                correlations <- 1
                in_cluster_indicator <- 1
                largest_term <- focal_term
            } else {
                # if there are more than one terms in the cluster
                # now get the sub-doc-term-matrix
                sub_dt <- document_term_matrix[,inds]
                # get the correlations to the focal term
                correlations <- sapply(1:length(inds),
                                       get_correlation,
                                       dtm = sub_dt)

                # determine which terms qualify for the cluster based on threshold
                in_cluster <- inds[which(correlations > correlation_threshold)]
                in_cluster_indicator <- rep(0,length(correlations))
                in_cluster_indicator[which(correlations > correlation_threshold)] <- 1

                # get the largest term
                lt <- ranked_terms[in_cluster,1]
                nchars <- as.numeric(sapply(lt,nchar))
                largest_term <- lt[which(nchars == max(nchars))[1]]
            }

            # now generate the data we are going put in the object we return
            ranked_term_clusters[i,1] <- largest_term
            ranked_term_clusters[i,2:ncol(ranked_terms)] <- focal_term_metadata

            # record the nubmer of terms subsumed
            terms_subsumed[i] <- length(in_cluster)

            # create a cluster dataset to stick in the list
            cluster_data <- data.frame(term = ranked_terms[inds,1],
                                       correlations = correlations,
                                       in_cluster = in_cluster_indicator,
                                       stringsAsFactors = FALSE)
            ranked_term_cluster_list[[i]] <- cluster_data

            # remove the terms inclued in the cluster and keep going as long as
            # there is still input data
            clusters_returned <- clusters_returned + 1
            if (nrow(ranked_terms) == length(in_cluster)) {
                break
            } else {
                ranked_terms <- ranked_terms[-in_cluster,]
                document_term_matrix <- document_term_matrix[,-in_cluster]
            }
        }
    }

    ranked_term_clusters <- cbind(ranked_term_clusters,terms_subsumed)
    # if we ran out of data to make new clusters then subset the data
    ranked_term_clusters <- ranked_term_clusters[1:clusters_returned,]
    ranked_term_cluster_list <- ranked_term_cluster_list[1:clusters_returned]

    # determine if there are any missing rows
    rmr <- c(which(is.na(rownames(ranked_term_clusters))),
             which(is.null(rownames(ranked_term_clusters))),
             which(is.na(ranked_term_clusters[,1])))
    if(length(rmr) > 0) {
        cat("Removing NA rows:",rmr,"\n")
        ranked_term_clusters <- ranked_term_clusters[-rmr,]
        ranked_term_cluster_list <- ranked_term_cluster_list[-rmr]
    }

    # get the correct names for everything
    print(ranked_term_clusters[,1])
    rownames(ranked_term_clusters) <- ranked_term_clusters[,1]
    names(ranked_term_cluster_list) <- ranked_term_clusters[,1]

    cat("N-Gram Subsumption Complete!")
    # return everything
    return(list(ranked_term_clusters = ranked_term_clusters,
                ranked_term_cluster_list = ranked_term_cluster_list))
}

n_terms <- function (str) {
    length(stringr::str_split(str, " ")[[1]])
}

get_correlation <- function(ind,dtm) {
    current <- cor(dtm[,1], dtm[,ind])
    # deal with case of constant data which should be correlated at 1
    if (is.na(current)) {
      current <- 1
    }
    return(current)
}

# test <- subsume_ngrams(ranked_terms,
#                        document_term_matrix,
#                        term_clusters_to_output = 20,
#                        top_terms_to_search = 200,
#                        correlation_threshold = 0.9)

# # find the document term matrix associated with the top 200 terms
# inds <- rep(0, 200)
# vocab <- colnames(doc_term_two_plus)
# terms <- healthcare_113[[1]]$term
# for (i in 1:200) {
#     print(i)
#     inds[i] <- which(vocab == terms[i])
# }
# # subset to the documents about healthcare c(607,608)
# rows <- c(as.numeric(attr(party_topic_two_plus, "document_indices")[[607]]),
#           as.numeric(attr(party_topic_two_plus, "document_indices")[[608]]))
# #reduced matrix
# document_term_matrix <- doc_term_two_plus[rows, inds]
# ranked_terms <- healthcare_113[[1]]
# setwd("~/Dropbox/Research/Congressional_Bill_Language/data/Analysis_1993-2014")
# load("Phrase_Subsumption_Testing.Rdata")
