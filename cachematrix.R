## Solve a matrix - cache the inverse to 
##      help speed things up on future calls.
## 
## Author - Jeffrey D. Young - Oct 24th, 2015

## Create a matrix and cache it's inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  # input to function should be a matrix
  if(!is.matrix(x)) {
    message("Error: x is not a matrix")
    return()
  }

  inv <- NULL

  # function to create initial matrix and NULL inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # function to return matrix data
  get <- function() x
  
  # function to set inverse in cache
  setinverse <- function(solve) inv <<- solve
  
  # function to return inverse from cache
  getinverse <- function() inv

  # return object with four functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Solve a special matrix - use cached inverse
## if available, otherwise solve and set 
## inverse in cache.
cacheSolve <- function(x, ...) {
  # get cached inverse if available
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # if inverse is not available, retrieve matrix, 
  # compute inverse and set in cache
  data <- x$get()          # retrieve matrix data
  inv <- solve(data, ...)  # compute inverse
  x$setinverse(inv)        # set inverse in cache
  inv                      # return inverse
}
