## There are two functions: makeCacheMatrix() and cachSolve
## the first one makes a special matrix which contains several functions
## the second uses these functions to either get the already solved inverse,
## or calculates it and than stores it

## setting up the special matrix

makeCacheMatrix <- function(x = matrix()) {
  
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## getting the already solved inverse, or solves it and than stores it

cacheSolve <- function(x, ...) {

  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
