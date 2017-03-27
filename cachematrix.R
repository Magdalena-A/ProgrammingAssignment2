## The following pair of functions calculates the inverse of a matrix, and caches the result.
## If the inverse for the matrix has already been cached, the functions will retrieve the inverse from
## cache instead of re-calculating it.

## The function makeCacheMatrix creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## The function cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve will 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
