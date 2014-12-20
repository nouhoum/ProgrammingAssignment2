## The two following functions work in conjunction to compute and cache the inverse of 
## a matrix which can be expensive computation. 

## This function creates a special matrix which can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This functions compute the inverse of a "special" matrix returned by the function "makeCacheMatrix".
cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  i
}
