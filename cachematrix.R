## The "cacheMatrix" implemented here is a matrix that keeps a cached version of its inverse.
## It is created via makeCacheMatrix() and inverted via cacheSolve()

## Create a cacheMatrix object, which contains a cache spot for its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        # Any changes to the matrix are made via this function, so changing the matrix clears the cache
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(theInv) inv <<- theInv  # sets the cached inverse inv to the value of theInv
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Calculate and cache the inverse of an invertible matrix x, return the cached version instead,
## if it has already been calculated.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # Only reaches here if there is no cached inverse
    data <- x$get()
    inv <- solve(data) # inverse gets calculated here
    x$setinv(inv)      # and cached here
    inv
}
