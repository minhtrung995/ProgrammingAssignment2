## these codes arr for caching the inverse of a matrix rather than compute it repeatedly

## create cache matrix object the invert of the matrix for calculating
makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInvert <- function(invert) inv <- invert
  getInvert <- function() inv
  list(set = set, get = get, setInvert = setInvert, getInvert = getInvert)
}

## check/set and return cache
cacheSolve <- function(x,...) {
  inv <- x$getInvert()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInvert(inv)
  inv
}
