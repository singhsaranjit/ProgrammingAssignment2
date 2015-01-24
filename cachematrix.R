## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.

## Create a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    # Initialize inverse to NULL.
    i <- NULL

    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i

    # Return a list of set, get, setinverse and getinverse functions.
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Compute the inverse of a matrix. If the inverse is already calculated and stored in cache, get the value from cache.
cacheSolve <- function(x, ...) {
    
    # If inverse is already cached, get value from cache and exit.
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }

    # Calculate inverse of matrix, cache inverse and return value.
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)

    i

}
