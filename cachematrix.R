## The functions helps calculating the inverse of a matrix in an effective way

## Example of usage:
## exampleMatrix <- matrix(c(3,1,2,1),nrow=2,ncol=2) # Creates an example matrix
## cachableMatrix <- makeCacheMatrix(exampleMatrix) # Creates a cachable matrix object from the example matrix
## cacheSolve(cachableMatrix) # Prints the inverted example matrix to the screen if called twice it uses the
##                              the cached version the second time

## makeCacheMatrix is called with a matrix as argument and returns a specially designed matrix object
## which can cache its inverse if the inverse has already been calculated

makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        set <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) invMatrix <<- solve
        getInverse <- function() invMatrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above by using this 
## as argument. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## retrieve the inverse from the cache instead of being calculated again.

cacheSolve <- function(x, ...) {
        invMatrix <- x$getInverse()
        if(!is.null(invMatrix)) {
                message("Using cached data to generate inversed matrix")
                return(invMatrix)
        }
        data <- x$get()
        invMatrix <- solve(data, ...)
        x$setInverse(invMatrix)
        invMatrix
}
