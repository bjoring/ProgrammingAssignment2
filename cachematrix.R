## This calculates the inverse of a matrix if needed, and
## if matrix has already been calculated, returns the
## cached value for that matrix

## makeCacheMatrix makes a vector of functions to set and
## get the inverse matrix. Returns a vector.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve calculates and returns the inverse of a matrix
## or returns the cached inverse matrix if previously solved

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("Getting cached data...")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}
