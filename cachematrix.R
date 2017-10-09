## Objective of the following functions is to cache the inverse of a matrix so that we do not need to compute the inverse repeatedly  
## This is done using two functions

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(get = get,
       setinv = setinv,
       getinv = getinv)
}

## The function below computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
