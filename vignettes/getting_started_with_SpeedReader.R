## ----eval=FALSE, fig.width=6, fig.height=4, fig.align ='center'----------
#  # Load in example data:
#  data("congress_bills")
#  
#  # Use the quanteda::dfm function to generate a document-term matrix:
#  quanteda_dtm <- quanteda::dfm(congress_bills)
#  
#  # Convert to a slam::simple_triplet_matrix object:
#  dtm <- convert_quanteda_to_slam(quanteda_dtm)

## ----eval=FALSE, fig.width=6, fig.height=4, fig.align ='center'----------
#  # Start by loading the package:
#  library(SpeedReader)

## ----eval=FALSE, fig.width=6, fig.height=4, fig.align ='center'----------
#  # First, we will want to set our working directory to folder where all of the
#  # bill text is located. For me, this looks like:
#  directory <- "~/Desktop/Bill_Data"
#  # but it will likely be different for you. It is a good idea to save this path
#  # as a separate string, as we will need to provide it to the
#  # document_similarities() function later. Now we can go ahead and set out
#  # directory:
#  setwd(directory)
#  
#  # Once we have done that, we will want to get a list of all files contained in
#  # this directory. Alternatively you can create this character vector of file
#  # names manually, or read it in. The point is that we should be left with a
#  # character vector containing all of the names of the .txt files associated
#  # with all of the documents we want to compare, and no other file names. You
#  # will want to double check this vector is correct.
#  files <- list.files()

## ----eval=FALSE, fig.width=6, fig.height=4, fig.align ='center'----------
#  results <- document_similarities(filenames = files,
#                                   input_directory = directory,
#                                   ngram_size = 5,
#                                   parallel = FALSE,
#                                   prehash = T)

## ----eval=FALSE, fig.width=6, fig.height=4, fig.align ='center'----------
#  # Load in the Congressional Bills:
#  data("congress_bills")
#  
#  # Generate similarity metrics:
#  results <- document_similarities(documents = congress_bills,
#                                   ngram_size = 5,
#                                   parallel = FALSE,
#                                   prehash = T)

## ----eval=FALSE, fig.width=6, fig.height=4, fig.align ='center'----------
#  # Get the filenames:
#  directory <- "~/Desktop/Bill_Data"
#  setwd(directory)
#  files <- list.files()
#  
#  # Break them apart into their constituent parts and form a data.frame:
#  metadata <- data.frame(chamber = rep("",length(files)),
#                         bill = rep("",length(files)),
#                         version = rep("",length(files)),
#                         stringsAsFactors = FALSE)
#  # Generate the metadata:
#  for (i in 1:length(files)) {
#      # split up the file names:
#      temp <- stringr::str_split(files[i],"(-|\\.)")[[1]]
#      # save the relevant parts:
#      metadata[i,] <- temp[2:4]
#  }
#  
#  # Now find all document pairs:
#  doc_pairs <- NULL
#  # Start with the 20 HR bills:
#  for (i in 1:20) {
#      cur <- which(metadata$chamber == "HR" & metadata$bill == i)
#  
#      if (length(cur) > 1) {
#          temp <- t(combn(cur,2))
#          doc_pairs <- rbind(doc_pairs,temp)
#      }
#  }
#  
#  # Move on to the 21 S bills:
#  for (i in 1:21) {
#      cur <- which(metadata$chamber == "S" & metadata$bill == i)
#      if (length(cur) > 1) {
#          temp <- t(combn(cur,2))
#          doc_pairs <- rbind(doc_pairs,temp)
#      }
#  }
#  
#  
#  # Generate similarity metrics:
#  results <- document_similarities(filenames = files,
#                                   input_directory = directory,
#                                   doc_pairs = doc_pairs,
#                                   ngram_size = 5,
#                                   parallel = FALSE,
#                                   prehash = T)

## ----eval=TRUE, fig.width=7, fig.height=4, fig.align ='center'-----------
# Load the package:
library(SpeedReader)

# Load in the Congressional Bills:
data("congress_bills")

# Find the locations of overlapping n-gram matches and mismatches in the 
# document pair.
matches <- ngram_sequence_matching(congress_bills[29],
                                   congress_bills[30],
                                   ngram_size = 5)

# Generate a plot of these matches and mismatches:
ngram_sequnce_plot(matches,
                   custom_title = "Example Comparison of 103-HR-5-IH and 103-HR-5-EH.")

