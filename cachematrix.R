## Author: petesmay

## Put comments here that give an overall description of what your
## functions do

## Function: makeCacheMatrix 
##   Creates a 'special' matrix object able to store it's inverse.
##   Returns: a list of functions operable on this object:
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
  
  ## Sets and returns a list of functions for use with
  ## objects of this CacheMatrix type
  list (set = set, get = get)
}


## Write a short comment describing this function

## Function: cacheSolve

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  return (solve(x))
}
