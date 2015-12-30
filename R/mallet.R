mallet <- function(documents = NULL,
                   document_directory = NULL,
                   topics = 10,
                   iterations = 100,
                   alpha = 1,
                   beta = 0.1,
                   vocabulary_size = NULL,
                   hyperparameter_optimization_interval = 0,
                   num_top_words = 20,
                   optional_arguments = "",
                   tokenization_regex = '[\\p{L}\\p{N}\\p{P}]+',
                   cores = 1){

    if(hyperparameter_optimization_interval == 0 & is.null(vocabulary_size)){
        stop("You must provide the vocabulary_size if you are not using hyperparameter optimization.")
    }

    if(hyperparameter_optimization_interval == 0){
        beta <- beta * vocabulary_size
    }
    # deal with different kinds of data -- turn it into a vector of strings (one string per document)
#     str <- NULL
#     for(k in 1:ncol(cur$matrix)){
#         if(cur$matrix[j,k] >0){
#             str <- c(str,rep(cur$vocab$words[k],cur$matrix[j,k]))
#         }
#     }
#     temp  <- paste0(str,collapse = " ")

    num_docs <- length(documents)

    directory <- system.file("extdata", package = "SpeedReader")[1]
    # MALL_LOCATION <- "~/Documents/Mallet-master/dist/"
    # MALLET <- "~/Documents/Mallet-master/lib/*"

    # CSV format -- 1 line per document:
    # doc_id\t\tdoc_text

    cat("Outputing documents in correct format for MALLET...\n")

    data <- matrix("",nrow = num_docs,ncol = 3)
    for(i in 1:num_docs){
        data[i,1] <- i
        data[i,2] <- i
        data[i,3] <- documents[i]
    }
    cat("Writing corpus to file...")

    # create an intermediate directory
    success <- dir.create("mallet_intermediate_files",showWarnings = FALSE)
    if(!success){
        file.remove("./mallet_intermediate_files")
        success <- dir.create("mallet_intermediate_files")
    }
    if(!success){
        stop("Could not create the intermdiate file directory necessary to use coreNLP. This is likely due to a file premission error. Make usre you have permission to create files or run your R session as root.")
    }
    setwd("./mallet_intermediate_files")

    # write the data to file:
    write.table(data, file = "mallet_input_corpus.csv", quote = FALSE, row.names = F,col.names= F, sep = "\t" )

    prepare_data <- paste("java -server -Xmx3g -classpath ",directory,"/* cc.mallet.classify.tui.Csv2Vectors --keep-sequence --token-regex '",tokenization_regex,"' --output mallet_corpus.dat --input mallet_input_corpus.csv --print-output > stdout_intake.txt 2>&1", sep = "")
    p <- pipe(prepare_data,"r")
    close(p)

    ####################################################################
    if(hyperparameter_optimization_interval != 0){
        run_mallet <- paste("java -server -Xmx10g -classpath ",directory,"/* cc.mallet.topics.tui.Vectors2Topics --input mallet_corpus.dat --output-state output_state.txt.gz --output-topic-keys topic-keys.txt --xml-topic-report topic-report.xml --xml-topic-phrase-report topic-phrase-report.xml --output-doc-topics doc-topics.txt --num-topics ",topics,"--num-iterations ",iterations," --output-state-interval ",floor(iterations/5)," --num-threads ",cores," --optimize-interval ",hyperparameter_optimization_interval," --optimize-burn-in ",hyperparameter_optimization_interval," > stdout.txt 2>&1&", sep = "")
    }else{
        run_mallet <- paste("java -server -Xmx10g -classpath ",directory,"/* cc.mallet.topics.tui.Vectors2Topics --input mallet_corpus.dat --output-state output_state.txt.gz --output-topic-keys topic-keys.txt --xml-topic-report topic-report.xml --xml-topic-phrase-report topic-phrase-report.xml --output-doc-topics doc-topics.txt --num-topics ",topics,"--num-iterations ",iterations," --output-state-interval ",floor(iterations/5)," --num-threads ",cores," --beta ",beta," > stdout.txt 2>&1&", sep = "")
    }

    p <- pipe(run_mallet,"r")
    close(p)



    # list of all option that can be added, for reference
    # A tool for estimating, saving and printing diagnostics for topic models, such as LDA.
    # --help TRUE|FALSE
    # Print this command line option usage information.  Give argument of TRUE for longer documentation
    # Default is false
    # --prefix-code 'JAVA CODE'
    # Java code you want run before any other interpreted code.  Note that the text is interpreted without modification, so unlike some other Java code options, you need to include any necessary 'new's when creating objects.
    # Default is null
    # --config FILE
    # Read command option values from a file
    # Default is null
    # --input FILENAME
    # The filename from which to read the list of training instances.  Use - for stdin.  The instances must be FeatureSequence or FeatureSequenceWithBigrams, not FeatureVector
    # Default is null
    # --input-model FILENAME
    # The filename from which to read the binary topic model. The --input option is ignored. By default this is null, indicating that no file will be read.
    # Default is null
    # --input-state FILENAME
    # The filename from which to read the gzipped Gibbs sampling state created by --output-state. The original input file must be included, using --input. By default this is null, indicating that no file will be read.
    # Default is null
    # --output-model FILENAME
    # The filename in which to write the binary topic model at the end of the iterations.  By default this is null, indicating that no file will be written.
    # Default is null
    # --output-state FILENAME
    # The filename in which to write the Gibbs sampling state after at the end of the iterations.  By default this is null, indicating that no file will be written.
    # Default is null
    # --output-model-interval INTEGER
    # The number of iterations between writing the model (and its Gibbs sampling state) to a binary file.  You must also set the --output-model to use this option, whose argument will be the prefix of the filenames.
    # Default is 0
    # --output-state-interval INTEGER
    # The number of iterations between writing the sampling state to a text file.  You must also set the --output-state to use this option, whose argument will be the prefix of the filenames.
    # Default is 0
    # --inferencer-filename FILENAME
    # A topic inferencer applies a previously trained topic model to new documents.  By default this is null, indicating that no file will be written.
    # Default is null
    # --evaluator-filename FILENAME
    # A held-out likelihood evaluator for new documents.  By default this is null, indicating that no file will be written.
    # Default is null
    # --output-topic-keys FILENAME
    # The filename in which to write the top words for each topic and any Dirichlet parameters.  By default this is null, indicating that no file will be written.
    # Default is null
    # --num-top-words INTEGER
    # The number of most probable words to print for each topic after model estimation.
    # Default is 20
    # --show-topics-interval INTEGER
    # The number of iterations between printing a brief summary of the topics so far.
    # Default is 50
    # --topic-word-weights-file FILENAME
    # The filename in which to write unnormalized weights for every topic and word type.  By default this is null, indicating that no file will be written.
    # Default is null
    # --word-topic-counts-file FILENAME
    # The filename in which to write a sparse representation of topic-word assignments.  By default this is null, indicating that no file will be written.
    # Default is null
    # --diagnostics-file FILENAME
    # The filename in which to write measures of topic quality, in XML format.  By default this is null, indicating that no file will be written.
    # Default is null
    # --xml-topic-report FILENAME
    # The filename in which to write the top words for each topic and any Dirichlet parameters in XML format.  By default this is null, indicating that no file will be written.
    # Default is null
    # --xml-topic-phrase-report FILENAME
    # The filename in which to write the top words and phrases for each topic and any Dirichlet parameters in XML format.  By default this is null, indicating that no file will be written.
    # Default is null
    # --output-topic-docs FILENAME
    # The filename in which to write the most prominent documents for each topic, at the end of the iterations.  By default this is null, indicating that no file will be written.
    # Default is null
    # --num-top-docs INTEGER
    # When writing topic documents with --output-topic-docs, report this number of top documents.
    # Default is 100
    # --output-doc-topics FILENAME
    # The filename in which to write the topic proportions per document, at the end of the iterations.  By default this is null, indicating that no file will be written.
    # Default is null
    # --doc-topics-threshold DECIMAL
    # When writing topic proportions per document with --output-doc-topics, do not print topics with proportions less than this threshold value.
    # Default is 0.0
    # --doc-topics-max INTEGER
    # When writing topic proportions per document with --output-doc-topics, do not print more than INTEGER number of topics.  A negative value indicates that all topics should be printed.
    # Default is -1
    # --num-topics INTEGER
    # The number of topics to fit.
    # Default is 10
    # --num-threads INTEGER
    # The number of threads for parallel training.
    # Default is 1
    # --num-iterations INTEGER
    # The number of iterations of Gibbs sampling.
    # Default is 1000
    # --num-icm-iterations INTEGER
    # The number of iterations of iterated conditional modes (topic maximization).
    # Default is 0
    # --no-inference true|false
    # Do not perform inference, just load a saved model and create a report. Equivalent to --num-iterations 0.
    # Default is false
    # --random-seed INTEGER
    # The random seed for the Gibbs sampler.  Default is 0, which will use the clock.
    # Default is 0
    # --optimize-interval INTEGER
    # The number of iterations between reestimating dirichlet hyperparameters.
    # Default is 0
    # --optimize-burn-in INTEGER
    # The number of iterations to run before first estimating dirichlet hyperparameters.
    # Default is 200
    # --use-symmetric-alpha true|false
    # Only optimize the concentration parameter of the prior over document-topic distributions. This may reduce the number of very small, poorly estimated topics, but may disperse common words over several topics.
    # Default is false
    # --alpha DECIMAL
    # SumAlpha parameter: sum over topics of smoothing over doc-topic distributions. alpha_k = [this value] / [num topics]
    # Default is 5.0
    # --beta DECIMAL
    # Beta parameter: smoothing parameter for each topic-word. beta_w = [this value]
    # Default is 0.01
}
