## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinver <- function(inverse) m <<- inverse
  getinver <- function() m
  list(set = set, get = get, setinverse=setinver,getinverse=getinver)
}


## Write a short comment describing this function
#  This function cachesolve() computes the inverse of the matrix returned by makeCacheMatrix. 
#  If the inverse has already been calculated then cacheSolve retrieve the inverse from the cache saved.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinver()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinver(m)
  m
}
