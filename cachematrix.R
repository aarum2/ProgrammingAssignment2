## I have created two functions by the name makeCacheMatrix and cacheSolve. The cacheSolve
## function will solve the matrix and calculate the inverse of it which is cached and 
## returned when it is called the second time to avoid double work.


## The function makeCacheMatrix will set the cache using set and get methods as shown below.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x

  setinverse <- function(inverse) i <<- inverse
  
  getinverse <- function() i
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
## The function cacheSolve will check if the result is already cahed and returns it. It will compute the inverse if it was not caluculated before.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting the cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}