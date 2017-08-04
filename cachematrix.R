## These functions calculates the inverse of a matrix and caches the result u
## so that cached reslt can be used later

## This function will create a list of functions 
## that are basically responsible for setting and getting matrix and .

makecachematrix <- function(x = matrix()) {
  inv <- NULL
  setmat <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getmat <- function() x
  setinv <- function(invcal) inv <<- invcal
  getinv <- function() inv
  list(setmat = setmat, getmat = getmat,
       setinv = setinv,
       getinv = getinv)
}


## Returns inverse of matrix either by calculation or from cached result.

cachesolve <- function(x){
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$getmat()
  inv <- solve(data)
  x$setinv(inv)
  inv
}