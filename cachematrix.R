# The functions found in this file are used to: 1) create an object that stores
# an invertable matrix and a cached copy of its inverse; and 2) to update
# the copy of the inverse stored in the cache.

# This function creates a special CacheMatrix object that stores the value of
# an invertable matrix, and its inverse, in its environment
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        # store a new matrix object and invalidate the cached inverse
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    # return a list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# This function takes a CacheMatrix object as an argument and returns it's inverse. The
# inverse is pulled from the cache if already set, and recalculated and stored back to
# that cache, if not.
cacheSolve <- function(x, ...) {

    # checked for the cached inverse and return it if it exists
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }

    # the inverse wasn't already cached; calculate it; store it in the object's
    # cache, and return it
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
