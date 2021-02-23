## This script set and get (with caching) a inverse of a matrix.

## The below function creates a special object that manipulate and stores in 
## cache a inverse of a matrix.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x   <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The below function retrieve cached matrix inverse if exists, else not, the inverse is calculated and stored in cache.
cacheinverse <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("[INFO] Inverse of this matrix is cached.")
        message("[INFO] Retrieving...")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
