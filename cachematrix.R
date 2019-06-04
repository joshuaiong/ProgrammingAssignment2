## function creates Caching the Inverse of a Matrix


## function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL
  set <- function(y) {
    x <<- y
    iv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) iv <<- inverse
  getinverse <- function() iv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix
cacheSolve <- function(x, ...) {
  iv <- x$getinverse()
  if (!is.null(iv)) {
    message("getting cached data")
    return(iv)
  }
  data <- x$get()
  iv <- solve(data, ...)
  x$setinverse(iv)
  iv
}
