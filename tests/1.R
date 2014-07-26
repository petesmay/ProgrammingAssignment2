# Author: Peter May

m1 <- matrix(c(1,2,3,4), nrow=2, ncol=2)

## Function: Tests for matrix equality
## Returns: True if m1 and m2 are matrices of equal size, with all elements equal
matrixEquals <- function(m1, m2){
  return (is.matrix(m1) && is.matrix(m2) && dim(m1) == dim(m2) && all.equal(m1, m2))
}

## Test: Checks that an empty matrix is retrievable
test.makeCacheEmpty <- function(){
  m <- makeCacheMatrix()
  checkTrue(matrixEquals(m$get(), matrix()))
}

## Test: Checks that a matrix can be set and retrieved
test.makeCacheSet <- function(){
  m <- makeCacheMatrix()
  m$set(m1)
  checkTrue(matrixEquals(m$get(), m1))
}

## Test: Checks that the returned matrix from CacheSolve is the inverse
##       by multiplying it my the original matrix and comparing to the
##       identity matrix
test.checkInverseMatrix <- function(){
  ## Check we get the identity matrix
  checkTrue(matrixEquals(cacheSolve(m1) %*% m1, diag(2)))
}