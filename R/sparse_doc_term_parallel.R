sparse_doc_term_parallel <- function(file){
    load(file)
    current_document_lengths <- unlist(lapply(document_term_vector_list, length))
    cat("Total terms in current block:",sum(current_document_lengths),"\n")
    current_dw <- generate_document_term_matrix(document_term_vector_list,
                                                vocabulary = aggregate_vocabulary,
                                                document_term_count_list = document_term_count_list)

    #turn into simple triplet matrix and rbind to what we already have
    current_dw <- slam::as.simple_triplet_matrix(current_dw)
    return(curren_dw)
}
