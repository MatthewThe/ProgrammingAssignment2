## These functions will create a special matrix class which 
## can cache the inverse as long as the matrix is not changed

## This function creates a "special" matrix from a normal matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv) 
}


## This function returns the inverse of the "special" matrix;
## calculating it the inverse has not been calculated (or the
## matrix has been updated), or returning the cached inverse
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message('getting cached inverse')
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
