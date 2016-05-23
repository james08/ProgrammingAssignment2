## This file contains two functions to handle the inversion, 
## caching and retrieval of a matrix.

## This function represents a matrix that is able
## to cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  setInverse <- function(i) inv <<- i
  getInverse <- function() inv
  checkEquals <- function(a = matrix()) a == x
  list(get = get, getInverse = getInverse, setInverse = setInverse,
       checkEquals = checkEquals)
}


## This function will return the inverse of the matrix
## parameter passed to it. It will save time and resource
## by first checking if the inverse of this matrix has
## already been cached before recalculating and 
## caching the inverse.

cacheSolve <- function(x, ...) {
  if(is.null(x$getInverse())) {
    print("calculating inverse...")
    i <- x$get()
    x$setInverse(solve(i))
  }
  x$getInverse()
}