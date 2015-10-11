# SpeedReader
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



