## These two functions are used to cache and compute the inverse of a matrix


## This function creates a special "matrix" object that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(m) {
        x <<- m;
        inverse <<- NULL;
    }
    get <- function() return(x);
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() return(inverse);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then it
## retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
    inverse <- x$getinv()
    if(!is.null(inverse)) {
        message("fetching the cached data...")
        return(inverse)
    }
    data <- x$get()
    invserse <- solve(data, ...)
    x$setinv(inverse)
    return(inverse)
}
