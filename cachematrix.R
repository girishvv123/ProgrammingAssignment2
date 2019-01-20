## Put comments here that give an overall description of what your
# This function creates a special "matrix" object that can cache its inverse
## functions do

## Write a short comment describing this function
# caching the matrix inverse
# calculating inverse of a matrix is very costly
# so cache it on first request 
# and return inverse from cache on subsequent requests

makeCacheMatrix <- function(x = matrix()) {

  # declare inverse to null
  inverseOfAMatrix <- NULL
  # set function
  set <- function(y){
    x <<- y
    inverseOfAMatrix <<- NULL
  }
  # get function
  get <- function(){
    x
  }
  #setter for setting Inverse of matrix 
  setInverse <- function(inverseOfMatrix){
    inverseOfAMatrix <<- inverseOfMatrix
  }
  # getter to get inverse of a matrix
  getInverse <- function(){
    inverseOfAMatrix
  }
  # return all these functions as a list
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
# this function resolves whether inverse of a matrix 
# is already in cache or not
# if it is in cache it is returned
# else from the matrix inverse is retrieved using 
# appropriate get function
# that inverse value is stored in cache and returned
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
