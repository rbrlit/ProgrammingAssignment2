## These two functions compute an inverted matrix. They allow to cache the computed
## inverted matrix, so the lengthy operation does not have to be repeated.

## This function creates a matrix object and stores three functions
## necessary for caching the inverted matrix.

makeCacheMatrix <- function(mat = matrix()){
        inv <<- NULL
        get <- function() mat
        save.inverted <- function(x) inv <<- x
        get.inverted <- function() inv
        list(get = get, save.inverted = save.inverted, get.inverted = get.inverted)
}


## This function computes the inverted matrix based on the matrix created by
## the previous function. But before computing it checks whether the inverted
## matrix has already been computed.

cacheSolve <- function(x, ...){
        inv <- x$get.inverted()
        if (!is.null(inv)){
                message("Getting cached matrix")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat)
        x$save.inverted(inv)
        inv
}
