## The following functions are used to create a cached matrix object
## and to solve the inverse of a matrix

## makeCahceMatrix creates an object that is a list of four functions
## IT can be used to set and get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function solves the inverse of a given makeCacheMatrix object
## It checks whether the inverse has been cached and retrieves the value
## If not, it solves for the inverse and returns that calculation

cacheSolve <- function(x, ...) {
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
