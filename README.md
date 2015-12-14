# SpeedReader [![Travis-CI Build Status](https://travis-ci.org/matthewjdenny/SpeedReader.svg?branch=master)](https://travis-ci.org/matthewjdenny/SpeedReader)
An R package that provides functions to facilitate high performance text processing in R.

## Overview
This package will eventually provide a number of functions to:

  * Read in and clean text data.
  * Preprocess it using R functions to remove stopwords and other undesireable text.
  * Run your text through Stanford's CoreNLP libraries to POS tag it and find named entities.
  * Count terms.
  * Generate document term matrices
  * Perform analyses including PMI, TF-IDF and topic modeling using MALLET. 

The unifying theme of these functions is that they are designed to be easy
to use, and to operate on up to tens of billions of tokens over hundreds of millions of 
documents without requiring a massive map-reduce cluster with terabytes of RAM. I have decided
to produce an R package since these are functions I use quite frequently andthey have been replicated
in several projects.

## Installation

### Requirements for using C++ code with R

Note that if you are using a Mac, you will need to start by making sure you have Xcode + developer tools installed or you will not be able to compile the C++ code that is used in the samplers for this package. You will need to go here: <https://developer.apple.com/xcode/downloads/> and then select the link to the additional downloads page which will prompt you to enter you apple ID. This will let you download the developer tools. This requirement is not unique to this package, but is necessary for all packages that use Rcpp.  
  
If you are using a Windows machine, you will need to make sure you have the latest release of R (3.2.0+) and will also need to install `Rtools` (v33 or higher, available here <http://cran.r-project.org/bin/windows/Rtools/>)  before you can use any packages with C++ code in them. It is also highly advised that you use [RStudio](http://www.rstudio.com/) to download and install the package as it seems to play nicer with Rcpp under Windows. You may also want to visit [this blog post](https://cdrv.wordpress.com/2013/01/12/getting-compilers-to-work-with-rcpp-rcpparmadillo/) which has more information on making C++ work with R under Windows. 
  
If you are using a Linux distro, make sure you have a C++ complier installed, but in general, you should not run into as many issues. 

More generally, I suggest you check out this [tutorial on using C++ with R](http://www.mjdenny.com/Rcpp_Intro.html). It goes over some of the code used in this package and also covers a number of potential prolems you might run into when trying to compile C++ code on your computer, so it is a good reference. 

### Installing The Package
  
To install this package from Github, you will need to Hadley Wickham's devtools package installed.

    install.packages("devtools")
    library("devtools")
    
Now we can install from Github using the following line:

    devtools::install_github("matthewjdenny/SpeedReader")

I have  had success installing with R 3.2.0+ installed but please email me if you hit any issues.

### Functions

The SpeedReader package currently provides the following functions to aid in the preprocessing of large text corpora.

* `generate_document_term_vectors()` -- A function to ingest raw text data, either as .txt files, as R objects with one string per document, as R objects with a term vector per document, or as csv/tsv files with a column of unique words and (optionally) their counts. If providing raw text, cleaning and tokenization is currently provided using the included `clean_document_text()` function which makes use of regular expressions, but cleaning and NER will eventually be provided using Standford's CoreNLP libraries.
* `generate_blocked_document_term_vectors()` -- A function to automate generating and saving to disk blocks of documents for corpora that are too large to fit in memory. Automatically formats data for downstream use in large scale text manipulation functions.
* `count_words()` -- A function to count words in a provided document term vector list. Has the option to continue adding to a previously generated vocabulary/count object.
* `generate_document_term_matrix()` -- A function to generate a document term matrix from a term-vector list object returned by `generate_document_term_vectors()`. Provides lots of options and will automatically generate a vocabulary if none is provided. Provides and option to return a sparse document-term matrix.
* `generate_sparse_large_document_term_matrix()` -- The main function provided by the package. Will generate very large (sparse) document term matrices from very large vocabularies, in parallel, in a memory efficient manner. 
* `sparse_to_dense_matrix()` -- A helpful function for converting sparse matrix objects to dense matrix objects. Use with caution on large sparse matrices!
* `tfidf()` -- Calculates and displays TF-IDF scores for a given document term matrix.

The SpeedReader package also provides the following utility functions:

* `unlist_and_concatenate()` -- A function to un-list and concatenate a subset of a matrix/data.frame
* `order_by_counts()` -- A function to generate an ordered word count dataframe from a raw vector of words.
* `multi_plot()` -- An implementation of matplot with nice coloring and automatic legend generation.
* `kill_zombies()` -- A function which takes no arguments and kills zombie R processes if the user is using a UNIX based machine.
* `estimate_plots()` -- A function to parameter estimate plots with 95 percent confidence bounds for up to two models we wish to compare.
* `distinct_words()` -- A function to find (semi)-distinct words in a list of term vectors.
* `combine_document_term_matrices()` -- A function to combine multiple document term matrices into a single aggregate document term matrix.
* `color_words_by_frequency()` -- A function to generate LaTeX output from a dataframe containing words and their frequencies. With shading based on word frequency.
* `color_word_table()` -- A function to generate LaTeX output from a dataframe containing covariates and top words.
* `clean_document_text()` -- A function which cleans the raw text of a document provided either as a single string, a vector of strings, or a column of a data.frame.
* `topic_coherence()` -- A function to calculate topic coherence for a given topic using the formulation in "Optimizing Semantic Coherence in Topic Models" [available here:](http://dirichlet.net/pdf/mimno11optimizing.pdf).
