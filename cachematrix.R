## The makeCacheMatrix function creates a special "matrix" object that 
## can cache its inverse.
## The inverse of the special "matrix" is computed by cacheSolve function.

## Create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## 'x' is a matrix.
        ## Create a special "matrix" object from 'x' that can cache its inverse.
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Compute the inverse of the special "matrix" returned by makeCacheMatrix
## function. If the inverse has already been calculated (and the matrix has 
## not changed), then the cacheSolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'.
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
