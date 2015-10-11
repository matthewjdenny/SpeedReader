# SpeedReader
An R package that provides functions to facilitate high performance text processing in R.

## Overview
This package will eventually provide a number of functions to read in and clean text data,
count words, generate document term matrices, and perform analyses including PMI, TF-IDF and
topic modeling. The unifying theme of these functions is that they are designed to be easy
to use, and to operate on up to tens of billions of tokens over hundreds of millions of 
documents without requiring a massive map-reduce cluster with terabytes of RAM. I have decided
to produce an R package since these are functions I use quite frequently andthey have been replicated
in several projects.

