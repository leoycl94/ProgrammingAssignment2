## it creates a list containing functions to set the value of the matrix and set the value of inverse of the matrix

## This function creates a matrix which can cache the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function () x
  setInverse <- function (inverse) inv <<- inverse
  getInverse <- function () inv
  list (set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getinverse)
}


## This function computes the inverse of the matrix created by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse ()
  if (!is.null (inv)) {
    message ("getting cached data")
    return (inv)
  }
  mat <- x$get ()
  inv <- solve (mat, ...)
  x$setInverse (inv)
  inv
}


