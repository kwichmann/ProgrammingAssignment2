## These functions allow using cached values for the inverse of a square matrix

## makeCacheMatrix sets up a matrix cache with four functions.
## 
## For instance mat <- makeCacheMatrix(x) makes a matrix cache named mat
## with a cached matrix equal to the matrix x, and with four functions:
##
##    mat$set(x)    sets the cached matrix to the matrix x
##    mat$get()     gets the cached matrix
##    mat$setinv(x) sets the cached matrix inverse to the matrix x
##    mat$getinv()  gets the cached matrix inverse

makeCacheMatrix <- function(x = matrix()) {  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Calling the cacheSolve function on a matrix cache checks if the most
## recently cached matrix has been inverted before. If not, the calculation
## is performed and cached. If yes, the cached version is retrieved instead.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
