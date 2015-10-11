#' A function to generate a sparse large document term matrix in blocks from a list document term vector lists stored as .Rdata object on disk. This function is designed to work on very large corpora that would be computationally intractable to generate a document term matrix for using standard methods.
#'
#' @param file_directory
#' @param
#' @return A sparse document term matrix object.
#' @export
sparse_large_document_term_matrix <- function(file_directory,
                                              file_list,
                                              aggregate_vocabulary = NULL){


    setwd("~/Dropbox/Research/Congressional_Bill_Language/data/CBP_Data_For_Analysis")
    load("Master_Vocab_And_Counts_Ordered.Rdata")
    to_count <- c("tokens",
                  "JK",
                  "JKV",
                  "NP",
                  "N",
                  "N{2}",
                  "N{3}",
                  "N{4,}",
                  "A+N+",
                  "A*N+(L(N+)R)+",
                  "A*N+P(D|A)*N+",
                  "A*N+(P(D|A)*N){2}",
                  "A*N+(P(D|A)*N){3}",
                  "A*N+(P(D|A)*N){4}",
                  "VN",
                  "VPN",
                  "ANV",
                  "VAN",
                  "VNN",
                  "NVV",
                  "VDN",
                  "NN",
                  "AN",
                  "AAN",
                  "ANN",
                  "NAN",
                  "NNN",
                  "NPN")

    #generate separate R obects to save for each kind of vocab
    Document_Term_Matrix_List <- vector(length=28, mode = "list")
    maxblock = 908
    maxblockmax = 67

    #loop over bill blocks to add to matricies
    for(i in 0:908){
        cat("Loading Bill Block Number:",i,"\n")
        setwd("~/Dropbox/Research/Congressional_Bill_Language/data/CBP_Data_For_Analysis")
        load(paste("CBP_Bill_Word_Type_Vectors_",i,".Rdata",sep = ""))

        if(i == maxblock){
            maxind  <- maxblockmax
        }else{
            maxind = 100
        }

        # Bill_Word_Type_Vectors
        #loop over term types
        for(j in 1:28){
            cat("Currently Constructing matrix for",to_count[j], "for block",i,"\n" )
            bill_words <- Bill_Word_Type_Vectors[[j]]
            vocab <- Master_Vocab_And_Counts_Ordered[[j]]$Vocabulary
            num_unique_words <- length(vocab)
            number_of_bills <-maxind
            Bill_Lengths <- rep(0,number_of_bills)
            for(n in 1:number_of_bills){
                Bill_Lengths[n] <- length(bill_words[[n]])
            }
            cat("Total Terms in Block:",sum(Bill_Lengths),"\n")

            dwn <- Generate_Document_Word_Matrix(number_of_bills,
                                                 num_unique_words,
                                                 vocab,
                                                 bill_words,
                                                 Bill_Lengths)

            #turn into simple triplet matrix and rbind to what we already have
            dwm <- as.simple_triplet_matrix(dwm)
            if(i == 0){
                Document_Term_Matrix_List[[j]] <- dwn
            }else{
                Document_Term_Matrix_List[[j]] <- rbind(Document_Term_Matrix_List[[j]],dwm)
            }
        }
    }

    #get the names right
    for(k in 1:28){
        colnames(Document_Term_Matrix_List[[k]]) <- Master_Vocab_And_Counts_Ordered[[k]]$Vocabulary
    }
    names(Document_Term_Matrix_List) <- to_count

    #save
    save(Document_Term_Matrix_List, file = "Document_Term_Matrix_List.Rdata")

}
