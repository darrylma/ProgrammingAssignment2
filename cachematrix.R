## Put comments here that give an overall description of what your
## functions do

## Creates a special matrix that can cache its inverse
## Input: matrix
## Return: list of functions

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  # Sets the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # Gets the matrix
  get <- function() {
    x
  }
  
  # Sets the inverse of the matrix
  setInverse <- function(i) {
    inverse <<- i
  }
  
  # Sets the inverse of the matrix
  getInverse <- function() {
    inverse
  }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## If the inverse of the matrix has not been cached, the function calculates 
## the inverse of the matrix and sets the value of the inverse in the cache. 
## Else function will simply retrieve cached inverse
## Input: matrix
## Return: inverse of matrix

cacheSolve <- function(x, ...) {
  ## Get inverse of matrix
  inverse <- x$getInverse()
  
  ## If inverse of matrix is cached, return cached inverse
  if (!is.null(inverse)) {
    message("geting cached data")
    return(inverse)
  }
  
  ## Get matrix object
  data <- x$get()
  
  ## Calculate inverse of matrix
  inverse <- solve(data)
  
  ## Set inverse of matrix
  x$setInverse(inverse)
  inverse
}
