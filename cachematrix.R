## makeCacheMatrix and cacheSolve are two functions that work together to return the inverse of a matrix.
## if the inverse of a matrix x has already been computed,
## these function return the outcome from cache instead of calculating it again.


## within makeCacheMatrix, x is the matrix that is to be inverted.
## with the function makeCacheMatrix, a list of four functions are created (set, get, setinv_matr and getinv_matr)
## that are to be used by cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv_matr <- function(inv_matr) m <<- inv_matr
  getinv_matr <- function() m
  list(set = set, get = get,
       setinv_matr = setinv_matr,
       getinv_matr = getinv_matr)
}

## within cacheSolve, x2 is an object that is returned by makeCacheMatrix
## cacheSolve uses the four functions that are created by makeCacheMatrix to either calculate
## the inverse of the matrix, or retrieve it from cache, if it has already been calculated before.


cacheSolve <- function(x2, ...) {
  m <- x2$getinv_matr()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x2$get()
  m <- solve(data, ...)
  x2$setinv_matr(m)
  m
}
