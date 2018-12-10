## The following two functions together implement caching of the inverse
## of a matrix. Use them as follows:
##
## - first call makeCacheMatrix with the matrix as argument, 
##   and save result to a variable, say cm.
##
## - call cacheSolve(cm) to get the matrix's inverse. It will compute
##   the inverse only if a cached inverse value is not available.
##
## - if there is a need to work with a new matrix, call cm$set() with
##   the new matrix as input argument. Subsequent calls to CacheSolve()
##   will provide the inverse of this new matrix.
## 

## makeCacheMatrix:
## Accept a matrix as input argument, and return a matrix-like entity
## that facilitates caching of matrix inverse - a list of functions that
## get the matrix, set the matrix, get the inverse, and set the inverse.
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
## Get the inverse of the matrix-like entity returned by the makeCacheMatrix
## function. See if a cahced inverse value is available, and if so, return 
## it. If not, calculate the inverse, save it in cache, and return it.
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
