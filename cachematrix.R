## Function “makeCacheMatrix” creates a special “matrix” object that can cache its inverse. 
## get is a function that returns the vector x stored in the main function.
## set is a function that changes the vector stored in the main function.
## setinverse and getinverse are functions related to set and get by storing the value of the input 
## in a variable m into the main function makeCacheMatrix (setinverse) and return it (getinverse).

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Function “cacheSolve” computes the inverse of the special “matrix” (which is the input of cachemean) returned by makeCacheMatrix above. 
## If the inverse was calculated or is unchanged, then it will retrieve the inverse from the cache. 
## If not, it gets the matrix stored with makeCacheMatrix, the inverse, and stores it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
