## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly 
## The following pair of functions will get and/or cache the inverse of a matrix. 


## The makeCacheMatrix function creates a list of function to 
## set and get the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
inverse0 <- NULL
  set <- function(y) {
    x <<- y
    inverse0 <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverse0 <<- inverse
  getInverse <- function() inverse0
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cachSolve function returns the inverse of a matrix.
## It looks for the value, first, of the inverse if it has already been solved.
## If not, it will proceed to get the inverse of the function.
## After solving, the setInverse() function will set the cached value of the
## Matrix inverse.

 
cacheSolve <- function(x, ...) {
    inverse0 <- x$getInverse()
  if (!is.null(inverse0)) {
    message("getting cached data")
    return(inverse0)
  }
  mat <- x$get()
  inverse0 <- solve(mat, ...)   ## Return a matrix that is the inverse of 'x'
  x$setInverse(inverse0) 
  inverse0
}
