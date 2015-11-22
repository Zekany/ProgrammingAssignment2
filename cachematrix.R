## Put comments here that give an overall description of what your
## functions do

## This function caches the inverse of a matrix, so it doesn't have to be calculated
## multiple times.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <-function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This one returns the matrix from the previous function.

cacheSolve <- function(x, ...) {
  invfunc <- x$getinverse()
  if(!is.null(invfunc)) {
    return(invfunc)
  }
  result <-x$get()
  invfunc <- solve(result, ...)
  x$setInverse(invfunc)
  invfunc
}