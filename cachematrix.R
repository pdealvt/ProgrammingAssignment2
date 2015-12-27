## Assignment
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
## Your assignment is to write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse, which will use functions to store the inverse matrix in cache.

makeCacheMatrix <- function(x = matrix()) {
        cacheInverse <- NULL
        set <- function(y) {
                x <<- y
                cacheInverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) cacheInverse <<- inverse
        getInverse <- function() cacheInverse
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.

## Next, determine the specal matrix's inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseFUN <- x$getInverse()
        if(!is.null(inverseFUN)) {
                message("getting cached data")
                return(inverseFUN)
        }
        data <- x$get()
        inverseFUN <- solve(data, ...)
        x$setInverse(inverseFUN)
        inverseFUN
}
