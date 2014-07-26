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

test.makeCacheSetInverse <- function(){
  m <- makeCacheMatrix()
  m$set(m1)
  m$setInverse(solve(m1))
  ## check against the identity matrix
  checkTrue(matrixEquals(m$get() %*% m$getInverse(), diag(2)))
}

## Test: Checks that the inverse is calculated correctly and that it is
##       cached. Checks by testing original matrix multiplied with its
##       inverse equals the identity matrix
test.cacheSolveCached <- function(){
  m <- makeCacheMatrix()
  m$set(m1)
  inv <- cacheSolve(m)
  ## check inverse(m) * m = identity
  checkTrue(matrixEquals(inv %*% m$get(), diag(2)))
  ## check cached inverse(m) * m = identity
  checkTrue(matrixEquals(m$getInverse() %*% m$get(), diag(2)))
}

## Test: Checks that if the matrix is changed, the inverse is recomputed
test.cacheSolveChanged <- function(){
  m <- makeCacheMatrix()
  m$set(m1)
  inv <- cacheSolve(m)
  ## check cached inverse(m) * m = identity
  checkTrue(matrixEquals(m$getInverse() %*% m$get(), diag(2)))
  ## change the original matrix and check that the inverse is NULL
  m$set(matrix(c(5,6,7,8), nrow=2, ncol=2))
  checkEquals(m$getInverse(), NULL)
  ## now check that a new inverse is calculated when asked
  newinv <- cacheSolve(m)
  checkTrue(matrixEquals(m$getInverse() %*% m$get(), diag(2)))
}