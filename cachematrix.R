## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix creates a list of functions that 
## (1) set the value of a matrix, 
## (2) get the value of a matrix,
## (3) set the value of the inverse,
## (4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setmatrix <- function(y) {
    x <<- y
    i <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inv) i <<- inv 
  getinverse <- function() i
  list(getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function cacheSolve calculates the inverse of the matrix, using the functions in makeCacheMatrix
## It skips the computation using the cache, if the inverse has already been calculated

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("Returning value from cache")
    return(i)
  } else {
    data <- x$getmatrix()
    i <- solve(data)
    x$setinverse(i)
    return(i)
  }
}



