## These functions are used to compute the inverse of a matrix.  By calling
## cacheSolve, it computes the inverse and then using the functions in makeCacheMatrix
## it will cache the inverse so that the next time you call it, it won't have
## to compute the inverse again, but rather, returns the cached value.


## This function creates an object that can cache the inverse of a matrix.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function computes the inverse of a matrix.  It stores it in a cache
## so the next time you call it, it will return it from the cache instead of
## having to compute the inverse again
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
