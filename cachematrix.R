## Creates a special matrix that can cache it's inverse

makeCacheMatrix <- function(x = numeric()) {
    
    
    cache <- NULL
    
    # setter for the matrix
    setMatrix <- function(newValue) {
        x <<- newValue
        cache <<- NULL
    }
    
    # getter for the matrix
    getMatrix <- function() {
        x
    }
    
    cacheInverse <- function(solve) {
        cache <<- solve
    }
    
    getInverse <- function() {
        cache
    }
    
    list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve will retrieve the inverse from the cache.
cacheSolve <- function(y, ...) {
    inverted <- y$getInverse()
    if(!is.null(inverted)) {
        message("From Cache!")
        return(inverted)
    }
    # otherwise get the matrix, caclulate the inverse and store it in
    # the cache
    data <- y$getMatrix()
    inverted <- solve(data)
    y$cacheInverse(inverted)
    
    # return the inverse
    inverted
}
