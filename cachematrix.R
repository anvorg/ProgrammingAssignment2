## This function creates a special "matrix" object that can cache its inverse.
##
## @param x a square invertible matrix
##
## @return list containing a function to: a) set the value of the matrix, b)
## get the value of the matrix, c) set the value of the inverse, d) get the
## value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix
## has not changed), then cachedSolve should retrive the inverse from the cache.
##
## @param x a square invertible matrix
##
## @return the inverse of 'x'.

cacheSolve <- function(x, ...) {
    ## Gets the cached inverse
    i <- x$getinverse()
    
    ## Checks if it exists
    if (!is.null(i)) {
        ## Returns the cached inverse
        return(i)
    }
    
    ## The inverse has not been calculated yet
    data <- x$get()
    i <- solve(data, ...)
    
    ## Cache the inverse matrix for future references
    x$setinverse(i)
    
    i
}
