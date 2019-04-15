## create a function with input the matrix x
## These two functions cache the inverse of a matrix

## This function creates an  object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse, getInverse = getInverse)
}

#This function computes the inverse of the special matrix returned by makeCacheMatrix. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
