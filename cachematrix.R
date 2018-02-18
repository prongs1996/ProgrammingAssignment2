## These two functions are used to cache and compute the inverse of a matrix


## This function creates a special "matrix" object that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(m){
    x <<- m
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inverse <<- solveMatrix
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##  This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then it
## retrieves the inverse from the cache

## make sure you are passing as input what you received from makeCacheMatrix

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("fetching cached data...")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse      
}
