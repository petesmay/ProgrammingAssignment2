## Author: petesmay

## Function: makeCacheMatrix 
##   Creates a 'special' matrix object able to store it's inverse.
## Returns: a list of functions operable on this object:
##         - set(<matrix>) stores the specified matrix
##         - get() returns the previously stored matrix, or a default
##                 1x1 matrix
##         - setInverse(<matrix>) stores an inverse matrix
##         - getInverse() returns the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  ## variable to store the inverse matrix
  inv <- NULL
  
  ## function to store the specified matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  ## function to return the stored matrix
  get <- function(){
    return (x)
  }
  
  ## function to set the inverse matrix
  setInverse <- function(y){
    inv <<- y
  }
  
  ## function to get the inverse matrix
  getInverse <- function(){
    return (inv)
  }
  
  ## Sets and returns a list of functions for use with
  ## objects of this CacheMatrix type
  list (set = set, 
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}



## Function: cacheSolve
##   Calculates the inverse of the specified matrix 'x'. The
##   inverse result is cached for future reference.
## Note: As per instructions, the supplied matrix is assumed to
##       always be invertible. No error handling is applied.
## Returns: the inverse matrix of 'x'

cacheSolve <- function(x, ...) {
  ## Try to use the cached inverse
  inv <- x$getInverse()
  if(!is.null(inv)){
    return (inv)
  }
  ## calculate inverse from stored matrix and store
  storedmat <- x$get()
  inv <- solve(storedmat, ...)
  x$setInverse(inv)
  
  return (inv)
}
