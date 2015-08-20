## FileName: cacheMatrix.R
## Author: Dr Roger D. Peng
## Last Modified By: Christopher Yang
## Last Modified Date: August 20, 2015
##
## Assumptions: the matrix supplied is always invertible.
##
## The first function, makeCacheMatrix creates a special "matrix:, which is really 
## a list containing a function to
##   set the inverse of the matrix
##   get the inverse of the matrix
##   set the value of the inverse
##   get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  #Set: set the input matrix, clear the inverse matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ##Get: get the input matrix
  get <- function() x
  
  ##Setinverse: set the inverse matrix
  setinverse <- function(inverse) inv <<- inverse
  
  ##Getinverse: get the inverse matrix
  getinverse <- function() inv
  
  ##Define the functions into a list
  list(set = set, get = get,
      setinv = setinverse,
      getinv = getinverse)
}


##The following function calculates the inverse of the special "matrix" created with the 
##above function. However, it first checks to see if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates 
##the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()

  ## If there is cached data then simply return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Otherwise retrieve the matrix, compute the inverse and set the value in cache
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  
  ## Return with the inverse
  inv
}

## How to use:
##> matrixTest <- matrix(1:4, nrow=2, ncol=2)
##> matrixResult <- makeCacheMatrix(matrixTest)
##
## Display the input matrix
##> matrixResult$get()
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4
##
## Display the inverse matrix - should be NULL as the inverse has not been computed yet
##> matrixResult$getinv()
##NULL
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##
## Use the cacheSolve matrix to get the inverse 
##> cacheSolve(matrixResult)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##
## Calling matrixResult$getinv() should now give us the same results above (not NULL)
##> matrixResult$getinv()
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##
## Calling cacheSolve(matrixResult) again should produce an extra message as we're now
## getting the inverse from the cache and not recomputing it
##> cacheSolve(matrixResult)
##getting cached data
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##