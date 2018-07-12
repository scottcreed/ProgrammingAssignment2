## This project caches a matrix and its inverse

## makeCacheMatrix takes a single argument of type matrix.
##  It also defines get, set, setinverse, and getinverse functions
##  within its scope.

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


## cacheSolve will return the cached inverse of a matrix or will
##  calculate the inverse and cache it for future reference.

cacheSolve <- function(x) {
        i <- x$getinverse()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
