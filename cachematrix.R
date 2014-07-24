## Put comments here that give an overall description of what your
## functions do

## function makeCacheMatrix and cacheSolve that are used to create a special
## object that stores a matrix and cache's its inverse matrix


## function makeMatrix is a function that creates a special "matrix", but
## it is in fact a list containing four functions:
## 1 - function set(), which sets the value of the matrix
## 2 - function get(), which gets the value of the matrix
## 3 - function setinverse(), which sets the value of the inverse matrix
## 4 - function getinverse(), which gets the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## function cacheSolve calculates the inverse of the "special" matrix created
## with the function makeCacheMatrix

## However it first verifies if the inverse matrix was already calculated, if so
## it prints the message 'getting cached data' and returns the inverse matrix,
## skipping the computation. Otherwise, it calculates the inverse of the matrix
## and sets it in the cache with the function setinverse 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}