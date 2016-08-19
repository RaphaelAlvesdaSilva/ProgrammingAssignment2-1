## Cache the inverse of a matrix.
  
## Standard square invertible matrix
x <- matrix(data = c(1,3,2,4) ,nrow = 2, ncol = 2)

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}


## Use m <- makeCacheMatrix(x) to create the Cache Matrix
## m <- makeCacheMatrix(x)

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data.")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
  m
}

## Use cacheSolve(m) to get the square invertible matrix
## cacheSolve(m)
