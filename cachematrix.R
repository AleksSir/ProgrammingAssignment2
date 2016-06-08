## Put comments here that give an overall description of what your
## functions do: First function makes it possible to cache an inverse of an object
## Second one solves the inverse, but first checks whether it's solved already

## Write a short comment describing this function:
## Allows you to set and get data

makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  set <- function(y) {
    x <<- y
    matinv <<- NULL
  }
  get <- function() x
  getinv <- function() matinv
  setinv <- function(z) matinv <<- z 
  list(set = set,get = get, getinv = getinv, setinv = setinv)

}


## Write a short comment describing this function
## First checkes whether a cache was made for the inverse, if not make one

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matinv <- x$getinv()
  if(!is.null(matinv)) {
    return(matinv)
  }
  mat <- x$get()
  matinv <- solve(mat, ...)
  x$setinv(matinv)
  matinv
}