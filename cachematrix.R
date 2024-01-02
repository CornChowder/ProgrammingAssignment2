## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    xx <- NULL
    set <- function(y) {
        x <<- y
        xx <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) xx <<- inverse
    getinverse <- function() xx
    list(set = set, get = get, setinverse = setinvers, getinverse = getinverse)
}


## cacheSolve: calculates the inverse of a "matrix" created by makeCacheMatrix
## Checks to see if the "matrix" already has an inverse otherwise calculates it

cacheSolve <- function(x, ...) {
    xx <- x$getinverse()
    if (!is.null(xx)) {
        message("getting cached data")
        return(xx)
    }
    mat <- x$get()
    xx <- solve(x, ...)
    x$setinverse(xx)
    xx
}
