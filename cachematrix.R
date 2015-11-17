## Functions for wrapping a matrix to cache the calculation of the inverse of the matrix.

## Given a matrix m, creates an object wrapping it so as to be able to cache the calculation of
## the inverse. Intended to be used with cacheSolve e.g.
## m = diag(2, nrow=4, ncol=4)
## cm = makeCacheMatrix(m)
## cacheSolve(cm)

makeCacheMatrix <- function(m = matrix()) {
  cachedInverse <- NULL
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function() m
  setCachedInverse <- function(inverse) cachedInverse <<- inverse
  getCachedInverse <- function() cachedInverse
  list(set = set, get = get,
       setCachedInverse = setCachedInverse,
       getCachedInverse = getCachedInverse)
}


## Returns the inverse of a matrix wrapped using makeCacheMatrix. See makeCacheMatrix.

cacheSolve <- function(cacheMatrix, ...) {
  inverse <- cacheMatrix$getCachedInverse()
  
  if(is.null(inverse)) {
    m <- cacheMatrix$get()
    inverse <- solve(m, ...)
    cacheMatrix$setCachedInverse(inverse)
  }
  
  inverse
}
