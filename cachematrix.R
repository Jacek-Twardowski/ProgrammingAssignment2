## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse.

## Let's follow the suggested framework for vector caching
makeCacheMatrix <- function(x = matrix()) { # empty matrix
  
  matrix_inv <- NULL # initialize NULL inverse matrixe
  set <- function(y) {
    x <<- y # new matrix input
    matrix_inv <<- NULL #we have to reset the old inverse to NULL
  }
  get <- function() x #get input matrix
  setinverse <- function(inverse) matrix_inv <<- inverse #set actual "value" for inverse
  getinverse <- function() matrix_inv #get actual "value" of inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## Write a short comment describing this function
##function is computing inverse matrix of the input
##if the input matrix was not changed and inverse was already calculated than get
##cahced version of inversed matrix
cacheSolve <- function(x, ...) {
  
  m <- x$getinverse() # apply defined makeCacheMatrix() getinverse function
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  } #get inversed matrix
  data <- x$get() #if changed input than we have to calculate new inverse 
  m <- solve(data,...) #calculate the new inverse using solve()
  x$setinverse(m) #set the new inverse
  m #return the result
}
