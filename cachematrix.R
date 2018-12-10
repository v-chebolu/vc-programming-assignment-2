## The following two functions together implement caching of the inverse
## of a matrix. Use them as follows:
##
## - first call makeCacheMatrix with the matrix as argument, 
##   and save result to a variable, say, cm.
##
## - call cacheSolve(cm) to get the matrix's inverse. It will compute
##   the inverse only if a cached inverse value is not available.
##
## - if there is a need to work with a new matrix, call cm$set() with
##   the new matrix as input argument. Subsequent calls to CacheSolve(cm)
##   will provide the inverse of this new matrix.
## 

## makeCacheMatrix:
## Accept a matrix as input argument, and return a matrix-like entity
## that facilitates caching of matrix inverse. The return value is 
## actually a list of functions that get the matrix, set the matrix, 
## get the inverse, and set the inverse.
##
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}


## cacheSolve:
## Accept a matrix-like entity returned by makeCacheMatrix as an input argument
## and return the inverse of the underlying matrix. If a valid cached inverse
## value exists, just return it. If not, compute and cache the inverse, and also
## return it.
## The first argument to this function is a matrix-like entity returned by
## makeCacheMatrix. Remaining arguments are passed unchanged as the arguments
## after the matrix argument to solve(), the matrix inverse computation function.
##
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("Getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
