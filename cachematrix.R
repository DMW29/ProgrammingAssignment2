## Computationally intensive operations consume resources and time.
## When the object on which these calculations are made remain unchanged,
## it is best to cache the calulation result.
## This function will calculate the inverse of a square, invertible matrix
## and cache it into memory.

## makeCacheMatrix has as its input a square, invertible matrix and outputs
## a matrix object that caches the input matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) i <<- solve
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve has as its input a square, invertible matrix. The output
## of this function will return the cached inverse if it exists, or will
# or create a new cache inverse for later retreival if it does not.

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
