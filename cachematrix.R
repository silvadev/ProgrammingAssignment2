## This script set and get (with caching) a Inverse of a matrix.

## The below function creates a special object that manipulate and stores in 
## cache a inverse of a matrix.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x   <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inv <<- solve
    getInverse <- function() inv
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The below function retrieve cached matrix inverse if exists, else not, the
## inverse is calculated and stored in cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the Inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("[INFO] Inverse of this matrix is cached.")
        message("[INFO] Retrieving...")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
