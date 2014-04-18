## Together, these functions can cache the inverse of a matrix, check if the
## result has been cached, and find the inverse of a matrix if the result
## has not already been cached.

## makeCacheMatrix creates a special matrix object that is capable of caching
## the inverse of a matrix to save for later use.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inverse <<- solve
    getInverse <- function() inverse
    
## The list created below holds the functions created above so cachSolve can
## use them.
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## cacheSolve checks to see if the inverse of a matrix has already been cached.
## I the inverse is not null, it returns the inverse. If the inverse has not
## been cached, it computes the inverse using solve() and caches the result
## for subsequent use.

cacheSolve <- function(x, ...) {
    
## Checks to see if inverse has already been cached and returns it if it has.
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    
## Finds inverse of matrix and caches it if the inverse has not already
## been cached.
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
