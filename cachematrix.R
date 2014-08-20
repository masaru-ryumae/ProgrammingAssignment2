## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
################################################################################
# Function name: makeCacheMatrix
# File location: Anywhere accessible
# Date:          08/19/2014
# Version:       0.1
# Summary:       This function defines various member functions that can be called from 
#		 cacheSolve function, and utilizes lexical scoping for variables. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(matrix) m <<- matrix
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
################################################################################
# Function name: cacheSolve
# File location: Anywhere accessible
# Date:          08/19/2014
# Version:       0.1
# Summary:       This function gets the matrix inverse, but if the cache data
#		 exists, then it will not recompute, just get the cache value.
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
