sparse_to_dense_matrix <- function(x, ...){
    nr <- x$nrow
    nc <- x$ncol
    ## old line: y <- matrix(vector(typeof(x$v), nr * nc), nr, nc)
    y <- matrix(0, nr, nc)  ##
    y[cbind(x$i, x$j)] <- x$v
    dimnames(y) <- x$dimnames
    y
}
